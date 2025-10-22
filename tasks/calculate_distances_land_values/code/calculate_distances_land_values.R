# --- Land/Building Values: assign wards + distance to nearest ward border for all parcels ---

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
#setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/calculate_distances_land_values/code")
source("../../setup_environment/code/packages.R")

message("==> Loading inputs...")
# ---- Load ----
land_values    <- sfarrow::st_read_parquet("../input/land_values_geo.parquet", show_col_types = FALSE) 
ward_panel <- st_read("../input/ward_panel.gpkg")
alderman_panel <- read_csv("../input/alderman_panel.csv") 
message(sprintf("    land_values rows: %s | ward_panel rows: %s | alderman_panel rows: %s",
                nrow(land_values), nrow(ward_panel), nrow(alderman_panel)))

# -------------------------------------------------------------------
# 1) Choose one geometry per pin (most recent)
# -------------------------------------------------------------------
message("==> Step 1: Picking one geometry per pin (most recent tax_year)...")
pins_latest <- land_values %>%
  group_by(pin10) %>%
  slice_max(tax_year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pin10, geometry)
message(sprintf("    pins_latest: %s unique pins", nrow(pins_latest)))

# -------------------------------------------------------------------
# 2) Assign ward for EACH ward-map year (1998 / 2014 / 2015 in your gpkg)
#    This does NOT iterate over the full panel; just unique pins.
# -------------------------------------------------------------------
message("==> Step 2: Assigning wards by map year (1998/2014/2015)...")
if (st_crs(pins_latest) != st_crs(ward_panel)) {
  message("    CRS mismatch detected; transforming ward_panel to pins_latest CRS...")
  ward_panel <- st_transform(ward_panel, st_crs(pins_latest))
}

years <- sort(unique(ward_panel$year))
message(sprintf("    Years found in ward_panel: %s", paste(years, collapse = ", ")))

pin_year_ward <- purrr::map_dfr(
  years,
  function(y) {
    message(sprintf("    -> Year %s: dissolving polygons and assigning by nearest...", y))
    polys <- ward_panel %>%
      dplyr::filter(year == y) %>%
      dplyr::select(year, ward) %>%
      st_make_valid() %>%
      dplyr::group_by(year, ward) %>%                   # dissolve to one geom per ward
      dplyr::summarise(geometry = st_union(geom), .groups = "drop")
    
    # one unique ward per pin by construction
    nn <- st_nearest_feature(pins_latest, polys)
    tibble(
      pin10 = pins_latest$pin10,
      year  = y,
      ward  = polys$ward[nn]
    )
  }
)

# Strong uniqueness checks
stopifnot(!anyDuplicated(pin_year_ward[, c("pin10","year")]))
stopifnot(nrow(pin_year_ward) == nrow(pins_latest) * length(years))
message(sprintf("    pin_year_ward complete: %s rows (expected %s)",
                nrow(pin_year_ward), nrow(pins_latest) * length(years)))


# -------------------------------------------------------------------
# 3) Build ward internal border lines and ward pairs per year (simple loop)
# -------------------------------------------------------------------
message("==> Step 3: Building ward internal border lines per year...")
years <- sort(unique(ward_panel$year))

build_borders_one_year <- function(polys) {
  message(sprintf("    [borders] polygons: %s", nrow(polys)))
  polys <- polys %>% select(ward) %>% st_transform(3435)
  # heal geometry without requiring lwgeom
  polys$geometry <- st_buffer(st_make_valid(polys), 0)
  
  nb <- st_touches(polys)
  pairs <- tibble(
    i = rep(seq_len(nrow(polys)), lengths(nb)),
    j = unlist(nb)
  ) %>% filter(i < j)
  message(sprintf("    [borders] touching pairs: %s", nrow(pairs)))
  
  if (nrow(pairs) == 0L) {
    message("    [borders] no touching pairs, returning empty sf")
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
  message(sprintf("    [borders] raw line segments: %s", nrow(segs)))
  
  segs %>%
    mutate(ward_pair = paste0(pmin(ward_a, ward_b), "-", pmax(ward_a, ward_b))) %>%
    st_cast("MULTILINESTRING")
}

ward_borders <- purrr::map_dfr(
  years,
  ~ {
    message(sprintf("    -> Building borders for year %s ...", .x))
    build_borders_one_year(ward_panel %>% filter(year == .x)) %>% mutate(year = .x)
  }
)

# De-duplicate while keeping sf+CRS
message("    Unioning border segments within (year, ward_pair)...")
ward_borders <- ward_borders %>%
  group_by(year, ward_pair) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING")
message(sprintf("    ward_borders final rows: %s", nrow(ward_borders)))

# Safety: ensure CRS is set (should already be 3435)
if (is.na(st_crs(ward_borders))) {
  message("    ward_borders missing CRS; assigning EPSG:3435")
  st_crs(ward_borders) <- 3435
}

# -------------------------------------------------------------------
# 4) MODIFIED: Calculate a single, definitive nearest border for each parcel
#    This replaces the old year-by-year loop which was the source of the bug.
# -------------------------------------------------------------------
message("==> Step 4: Calculating definitive nearest border for each unique parcel...")

# First, create a wide history of ward assignments for each pin
pin_ward_history <- pin_year_ward %>%
  tidyr::pivot_wider(names_from = year, values_from = ward, names_prefix = "ward_")

pin_groups <- pin_ward_history %>%
  mutate(is_treated = !is.na(ward_2014) & !is.na(ward_2015) & ward_2014 != ward_2015) %>%
  select(pin10, ward_1998, ward_2014, ward_2015, is_treated)

treated_pins  <- pin_groups %>% filter(is_treated)
control_pins  <- pin_groups %>% filter(!is_treated)
message(sprintf("    treated pins: %s | control pins: %s", nrow(treated_pins), nrow(control_pins)))

# --- Process TREATED parcels ---
# For these, the relevant border is the one they actually crossed.
# -- Treated: distance to the border they crossed, measured on the 2014 map
if (nrow(treated_pins) > 0) {
  message("    Computing treated distances (using 2014 borders)...")
  treated_borders <- treated_pins %>%
    mutate(ward_pair = paste0(pmin(ward_2014, ward_2015), "-", pmax(ward_2014, ward_2015))) %>%
    left_join(
      ward_borders %>% filter(year == 2014) %>% select(ward_pair, border_geom = geometry),
      by = "ward_pair"
    )
  
  pts_treated <- pins_latest %>% semi_join(treated_borders, by = "pin10") %>% st_transform(3435)
  borders_treated_geom <- st_as_sf(treated_borders, sf_column_name = "border_geom", crs = 3435)
  
  valid_borders <- borders_treated_geom %>% filter(!st_is_empty(border_geom))
  valid_pts <- pts_treated %>% semi_join(valid_borders %>% st_drop_geometry(), by = "pin10")
  message(sprintf("       valid treated pins: %s", nrow(valid_pts)))
  
  # No manual reordering needed if you pass geometry explicitly:
  distances <- st_distance(valid_pts, st_geometry(valid_borders), by_element = TRUE)
  message("       treated distances computed.")
  
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
  message("    Computing control distances (nearest 2015 border)...")
  pts_control <- pins_latest %>% semi_join(control_pins, by = "pin10") %>% st_transform(3435)
  borders_2015 <- ward_borders %>% filter(year == 2015)
  
  nearest_idx <- st_nearest_feature(pts_control, borders_2015)
  distances <- st_distance(pts_control, borders_2015[nearest_idx,], by_element = TRUE)
  message("       control distances computed.")
  
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
stopifnot(!anyDuplicated(pin_border_map$pin10))
message(sprintf("    pin_border_map rows: %s", nrow(pin_border_map)))

st_write(pin_border_map, "../output/pin_border_map.gpkg", delete_dsn = TRUE)


# -------------------------------------------------------------------
# 5) MODIFIED: Merge the new, corrected border info back to the main panel
# -------------------------------------------------------------------
message("==> Step 5: Joining wards & borders back to repeated cross-section...")

map_year_for <- function(tax_year) {
  dplyr::case_when(
    tax_year <= 2003 ~ 1998L,
    tax_year <= 2014 ~ 2014L, 
    TRUE             ~ 2015L
  )
}

land_values_aug <- land_values %>%
  mutate(ward_map_year = map_year_for(tax_year)) %>%
  left_join(
    pin_year_ward %>% rename(ward_map_year = year),
    by = c("pin10","ward_map_year"),
    relationship = "many-to-one"   # one row per (pin10, ward_map_year) on RHS
  ) %>%
  left_join(pin_border_map, by = "pin10", relationship = "many-to-one") %>%  # one border per pin
  distinct(pin10, tax_year, .keep_all = TRUE)

message(sprintf("    land_values_aug rows: %s", nrow(land_values_aug)))

# land_values_aug %>%
#   count(tax_year) %>%
#   arrange(tax_year) %>%
#   print(n = 50)
# 
# land_values_aug %>%
#   count(pin10, tax_year) %>%
#   filter(n > 1)  # should be zero rows

# -------------------------------------------------------------------
# 7) Merge in alderman 
# -------------------------------------------------------------------
# land_values_aug <- st_read_parquet("../output/land_values_aug.parquet")
message("==> Step 7: Merging alderman (ward x tax_year)...")

alderman_year <- alderman_panel %>%
  mutate(
    month    = zoo::as.yearmon(month),
    tax_year = as.integer(format(as.Date(month), "%Y")),
    ward     = as.integer(ward)
  ) %>%
  group_by(ward, tax_year, alderman) %>%
  summarise(months_held = dplyr::n(), last_month = max(month), .groups = "drop") %>%
  arrange(ward, tax_year, desc(months_held), desc(last_month)) %>%
  group_by(ward, tax_year) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(ward, tax_year, alderman)

land_values_aug <- land_values_aug %>%
  mutate(ward = as.integer(ward)) %>%
  left_join(alderman_year, by = c("ward","tax_year"), relationship = "many-to-one")

message("    alderman merge complete.")


# -------------------------------------------------------------------
# 8) Save outputs
# -------------------------------------------------------------------
message("==> Step 8: Writing outputs...")
sfarrow::st_write_parquet(land_values_aug, "../output/land_values_aug.parquet")
st_write(ward_borders, "../output/ward_borders.gpkg", delete_dsn = TRUE)
message("==> Done.")
# arrow::write_feather(st_drop_geometry(pin_ward_map), "../output/pin_ward_map.feather")
