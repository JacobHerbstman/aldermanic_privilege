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

UNIQUE_MAP_YEARS <- c(1998, 2003, 2015, 2024)

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

years <- UNIQUE_MAP_YEARS
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
years <- UNIQUE_MAP_YEARS

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
  # Add ward_a and ward_b to group_by to preserve them for the join
  group_by(year, ward_pair, ward_a, ward_b) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTILINESTRING")
message(sprintf("    ward_borders final rows: %s", nrow(ward_borders)))

# Safety: ensure CRS is set (should already be 3435)
if (is.na(st_crs(ward_borders))) {
  message("    ward_borders missing CRS; assigning EPSG:3435")
  st_crs(ward_borders) <- 3435
}

# -------------------------------------------------------------------
# 4) MODIFIED: Calculate nearest border for each parcel *within each map year*
#    This replaces the 2014-2015 event logic.
# -------------------------------------------------------------------
message("==> Step 4: Calculating nearest border for each (pin, year) pair...")

# We already have pin_year_ward from Step 2. Get pin geometries.
pts_geom <- pins_latest %>% st_transform(3435)

# We already have ward_borders from Step 3.
# Ensure it has ward_a, ward_b for filtering.
if (!all(c("ward_a", "ward_b") %in% names(ward_borders))) {
  stop("Step 3 must preserve ward_a and ward_b in ward_borders")
}

years <- UNIQUE_MAP_YEARS
pin_border_map_list <- list()

for (y in years) {
  message(sprintf("Processing year: %s", y))
  
  # Get all borders for this year
  borders_y <- ward_borders %>% filter(year == y)
  
  # Get all pins with their ward assignment *for this year*
  pts_y <- pts_geom %>%
    inner_join(
      pin_year_ward %>% filter(year == y),
      by = "pin10"
    )
  
  # Loop over each pin for this year
  # --- NEW VECTORIZED APPROACH ---
  # Group pins by their ward and run one vectorized query per ward
  # (replaces the pin-by-pin loop)
  pin_year_info <- pts_y %>%
    group_by(ward, year) %>% # 'year' is from the inner_join
    group_modify(function(pins_in_ward_group, key) {
      
      # This is your new progress message
      message(sprintf("     ...processing year %s, ward %s (%s pins)", 
                      key$year, key$ward, nrow(pins_in_ward_group)))
      
      # Filter borders to ONLY those touching this group's ward
      relevant_borders <- borders_y %>%
        filter(ward_a == key$ward | ward_b == key$ward)
      
      if (nrow(relevant_borders) == 0) {
        # This ward has no internal borders, return NAs
        return(tibble(
          pin10 = pins_in_ward_group$pin10,
          ward_pair = NA_character_,
          dist_to_boundary_ft = NA_real_
        ))
      }
      
      # Run ONE vectorized nearest_feature call for all pins in this group
      nearest_idx <- st_nearest_feature(pins_in_ward_group, relevant_borders)
      
      # Run ONE vectorized distance call
      distances <- st_distance(pins_in_ward_group, 
                               relevant_borders[nearest_idx, ], 
                               by_element = TRUE)
      
      # Return the results for this group
      tibble(
        pin10 = pins_in_ward_group$pin10,
        ward_pair = relevant_borders$ward_pair[nearest_idx],
        dist_to_boundary_ft = as.numeric(units::set_units(distances, "ft"))
      )
      
    }, .keep = FALSE) %>% # .keep=FALSE is fine, we just want the new tibble
    ungroup()
  
  pin_border_map_list[[as.character(y)]] <- pin_year_info
}

pin_border_map <- bind_rows(pin_border_map_list)

stopifnot(!anyDuplicated(pin_border_map[, c("pin10", "year")]))
message(sprintf(" pin_border_map rows: %s", nrow(pin_border_map)))

# Save this new dynamic map
# Note: This is no longer a spatial file
write_csv(pin_border_map, "../output/pin_border_map.csv")

# -------------------------------------------------------------------
# 5) MODIFIED: Merge the new, corrected border info back to the main panel
# -------------------------------------------------------------------
message("==> Step 5: Joining wards & borders back to repeated cross-section...")

map_year_for <- function(tax_year) {
  dplyr::case_when(
    tax_year <= 2002 ~ 1998L,
    tax_year <= 2014 ~ 2003L,
    tax_year <= 2023 ~ 2015L,
    TRUE             ~ 2024L
  )
}

land_values_aug <- land_values %>%
  mutate(ward_map_year = map_year_for(tax_year)) %>%
  left_join(
    pin_year_ward %>% rename(ward_map_year = year),
    by = c("pin10","ward_map_year"),
    relationship = "many-to-one"  # one row per (pin10, ward_map_year) on RHS
  ) %>%
  # --- THIS JOIN IS NOW DYNAMIC ---
  left_join(
    pin_border_map %>% rename(ward_map_year = year), 
    by = c("pin10", "ward_map_year"), 
    relationship = "many-to-one"
  ) %>%
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
  select(-ward.y) %>% 
  rename(ward = ward.x) %>%
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
