#this code assigns wards + distance to nearest ward border for all parcels in the assessor panel 

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---- Load ----
land_values    <- sfarrow::st_read_parquet("../input/land_values_geo.parquet", show_col_types = FALSE) 
ward_panel <- st_read("../input/ward_panel.gpkg")
alderman_panel <- read_csv("../input/alderman_panel.csv") 

# -------------------------------------------------------------------
# 1) Choose one geometry per pin (most recent)
# -------------------------------------------------------------------
pins_latest <- land_values %>%
  group_by(pin10) %>%
  slice_max(tax_year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pin10)  # keep only id + geometry

# -------------------------------------------------------------------
# 2) Assign ward for EACH ward-map year (1998 / 2014 / 2015 in your gpkg)
#    This does NOT iterate over the full panel; just unique pins.
# -------------------------------------------------------------------
pin_year_ward <- st_join(
  pins_latest,
  ward_panel %>% select(year, ward),
  left  = TRUE,
  join  = st_within
) %>%
  st_drop_geometry() %>%
  # In case any pin falls just outside due to slivers, snap to nearest ward that year
  bind_rows({
    missing <- anti_join(
      expand.grid(pin10 = pins_latest$pin10, year = unique(ward_panel$year)) %>% as_tibble(),
      ., by = c("pin10","year")
    )
    if (nrow(missing) == 0L) return(tibble())
    # nearest ward per year
    map_dfr(split(missing, missing$year), function(df_y) {
      y <- unique(df_y$year)
      pts <- pins_latest %>% semi_join(df_y, by = "pin10")
      polys <- ward_panel %>% filter(year == y) %>% select(year, ward)
      nn <- st_nearest_feature(pts, polys)
      tibble(pin10 = pts$pin10, year = y, ward = polys$ward[nn])
    })
  }) %>%
  distinct(pin10, year, .keep_all = TRUE)

# -------------------------------------------------------------------
# 3) Build ward internal border lines and ward pairs per year (simple loop)
# -------------------------------------------------------------------
years <- sort(unique(ward_panel$year))

build_borders_one_year <- function(polys) {
  polys <- polys %>% select(ward) %>% st_transform(3435)
  # heal geometry without requiring lwgeom
  polys$geometry <- st_buffer(st_make_valid(polys), 0)
  
  nb <- st_touches(polys)
  pairs <- tibble(
    i = rep(seq_len(nrow(polys)), lengths(nb)),
    j = unlist(nb)
  ) %>% filter(i < j)
  
  if (nrow(pairs) == 0L) {
    return(st_sf(ward_a = integer(), ward_b = integer(),
                 ward_pair = character(),
                 geometry = st_sfc(crs = 3435)))
  }
  
  segs <- purrr::pmap_dfr(pairs, function(i, j) {
    g <- suppressWarnings(st_intersection(st_geometry(polys[i, ]), st_geometry(polys[j, ])))
    if (length(g) == 0L) return(NULL)
    g <- st_collection_extract(g, "LINESTRING")
    if (length(g) == 0L) return(NULL)
    st_sf(ward_a = polys$ward[i], ward_b = polys$ward[j], geometry = g)
  })
  
  segs %>%
    mutate(ward_pair = paste0(pmin(ward_a, ward_b), "-", pmax(ward_a, ward_b))) %>%
    st_cast("MULTILINESTRING")
}

ward_borders <- purrr::map_dfr(
  years,
  ~ build_borders_one_year(ward_panel %>% filter(year == .x)) %>% mutate(year = .x)
)

# De-duplicate while keeping sf+CRS
ward_borders <- ward_borders %>%
  group_by(year, ward_pair) %>%
  summarise(do_union = TRUE, .groups = "drop")

# Safety: ensure CRS is set (should already be 3435)
if (is.na(st_crs(ward_borders))) st_crs(ward_borders) <- 3435


# -------------------------------------------------------------------
# 4) MODIFIED: Calculate a single, definitive nearest border for each parcel
#    This replaces the old year-by-year loop which was the source of the bug.
# -------------------------------------------------------------------
message("Calculating definitive nearest border for each unique parcel...")

# First, create a wide history of ward assignments for each pin
pin_ward_history <- pin_year_ward %>%
  distinct(pin10, year, .keep_all = TRUE) %>%
  tidyr::pivot_wider(names_from = year, values_from = ward, names_prefix = "ward_")

# Separate pins into treated (ward changed in 2015) and control (ward did not)
# We focus on the 2015 remap as that's the basis of the downstream analysis.
pin_groups <- pin_ward_history %>%
  mutate(
    # Note: Your data uses 2003 as the 'before' map for the 2015 event.
    is_treated = !is.na(ward_2003) & !is.na(ward_2015) & ward_2003 != ward_2015
  ) %>%
  select(pin10, ward_2003, ward_2015, is_treated)

treated_pins <- pin_groups %>% filter(is_treated)
control_pins <- pin_groups %>% filter(!is_treated)

# --- Process TREATED parcels ---
# For these, the relevant border is the one they actually crossed.
# We'll calculate their distance to that specific border from the "before" map (2003).
if (nrow(treated_pins) > 0) {
   treated_borders <- treated_pins %>%
    mutate(
      ward_pair = paste0(pmin(ward_2003, ward_2015), "-", pmax(ward_2003, ward_2015))
    ) %>%
    # MODIFIED: Coerce the spatial data to a tibble to force a non-spatial join.
    left_join(
      ward_borders %>% filter(year == 2003) %>% select(ward_pair, border_geom = geometry),
      by = "ward_pair"
    )
  
  pts_treated <- pins_latest %>% semi_join(treated_borders, by = "pin10") %>% st_transform(3435)
  borders_treated_geom <- st_as_sf(treated_borders, sf_column_name = "border_geom", crs = 3435)
  
  # Filter out cases where the border didn't exist in the 'before' map
  valid_borders <- borders_treated_geom %>% filter(!st_is_empty(border_geom))
  
  # MODIFIED: Drop geometry from the right-hand side for the attribute join.
  valid_pts <- pts_treated %>% semi_join(valid_borders %>% st_drop_geometry(), by = "pin10")
  
  # Ensure order matches for st_distance
  valid_pts <- valid_pts[match(valid_borders$pin10, valid_pts$pin10),]
  
  distances <- st_distance(valid_pts, valid_borders, by_element = TRUE)
  
  treated_info <- valid_borders %>%
    st_drop_geometry() %>%
    select(pin10, ward_pair) %>%
    mutate(dist_to_boundary_ft = as.numeric(units::set_units(distances, "ft")))
} else {
  treated_info <- tibble(pin10=character(), ward_pair=character(), dist_to_boundary_ft=numeric())
}

# --- Process CONTROL parcels ---
# For these, the relevant border is their closest border on the most recent map (2015).
if (nrow(control_pins) > 0) {
  pts_control <- pins_latest %>% semi_join(control_pins, by = "pin10") %>% st_transform(3435)
  borders_2015 <- ward_borders %>% filter(year == 2015)
  
  nearest_idx <- st_nearest_feature(pts_control, borders_2015)
  distances <- st_distance(pts_control, borders_2015[nearest_idx,], by_element = TRUE)
  
  control_info <- tibble(
    pin10 = pts_control$pin10,
    ward_pair = borders_2015$ward_pair[nearest_idx],
    dist_to_boundary_ft = as.numeric(units::set_units(distances, "ft"))
  )
} else {
  control_info <- tibble(pin10=character(), ward_pair=character(), dist_to_boundary_ft=numeric())
}

# --- Combine and create the final pin-level mapping table ---
pin_border_map <- bind_rows(treated_info, control_info)

# -------------------------------------------------------------------
# 5) MODIFIED: Merge the new, corrected border info back to the main panel
# -------------------------------------------------------------------
message("Merging corrected border information into main panel...")

map_year_for <- function(tax_year) {
  case_when(
    tax_year <= 2003 ~ 1998L,
    tax_year > 2003 & tax_year <= 2014 ~ 2003L,
    TRUE ~ 2015L
  )
}

land_values_aug <- land_values %>%
  # First, create the 'ward_map_year' column needed for the join
  mutate(ward_map_year = map_year_for(tax_year)) %>%
  # Now, join the correct ward assignment for that map year
  left_join(
    pin_year_ward %>% rename(ward_map_year = year),
    by = c("pin10", "ward_map_year")
  ) %>%
  # Finally, join the static, corrected border information
  left_join(pin_border_map, by = "pin10") %>%
  # Ensure distinct rows
  distinct(pin10, tax_year, .keep_all = TRUE)



# -------------------------------------------------------------------
# 7) Merge in alderman 
# -------------------------------------------------------------------
# land_values_aug <- st_read_parquet("../output/land_values_aug.parquet")


alderman_year <- alderman_panel %>%
  mutate(
    month = as.yearmon(month),
    tax_year = as.integer(format(as.Date(month), "%Y")),
    ward     = as.integer(ward)
  ) %>%
  group_by(ward, tax_year, alderman) %>%
  summarise(
    months_held = n(),
    last_month  = max(month), 
    .groups = "drop"
  ) %>%
  arrange(ward, tax_year, desc(months_held), desc(last_month)) %>%
  group_by(ward, tax_year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(ward, tax_year, alderman)


land_values_aug <- land_values_aug %>%
  mutate(ward = as.integer(ward)) %>%
  left_join(alderman_year, by = c("ward", "tax_year"))

# -------------------------------------------------------------------
# 8) Save outputs
# -------------------------------------------------------------------
sfarrow::st_write_parquet(land_values_aug, "../output/land_values_aug.parquet")
st_write(ward_borders, "../output/ward_borders.gpkg", delete_dsn = TRUE)


# arrow::write_feather(st_drop_geometry(pin_ward_map), "../output/pin_ward_map.feather")

