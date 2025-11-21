# This script calculates signed distances from parcels to ward boundaries
# and assigns aldermen based on construction year and redistricting events
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# Configuration parameters for redistricting handling
REDISTRICTING_2003 <- as.Date("2003-05-01") # 2003 map becomes active
REDISTRICTING_2015 <- as.Date("2015-05-01") # 2015 map becomes active
REDISTRICTING_2024 <- as.Date("2024-05-01") # 2024 map becomes active
BOUNDARY_YEARS     <- c(1998L, 2003L, 2015L, 2024L)

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

cat("Loading parcel data...\n")
parcels <- st_read("../input/geocoded_residential_data.gpkg") %>%
  # Note: This filter excludes older multifamily buildings if they exist in the input
  filter(!is.na(yearbuilt), yearbuilt >= 1999 & yearbuilt <= 2025) %>%
  mutate(construction_date = as.Date(paste0(yearbuilt, "-06-15")))

cat("Loading ward boundaries...\n")
ward_panel <- st_read("../input/ward_panel.gpkg")

cat("Loading alderman panel...\n")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv")

cat("Loading alderman strictness scores... \n")
alderman_scores <- read_csv("../input/aldermen_strictness_scores.csv")

if (st_crs(parcels) != st_crs(ward_panel)) {
  message("CRS mismatch detected. Transforming parcels CRS to match ward boundaries.")
  parcels <- st_transform(parcels, st_crs(ward_panel))
}

cat("Loading zoning data...\n")
zoning_data <- st_read("../input/zoning_data_clean.gpkg")

if (st_crs(zoning_data) != st_crs(ward_panel)) {
  zoning_data <- st_transform(zoning_data, st_crs(ward_panel))
}

# Spatial join for zoning
# Note: This works for multifamily too, as they are now point geometries
parcels <- parcels %>%
  st_join(
    zoning_data %>% dplyr::select(
      zone_code, floor_area_ratio, lot_area_per_unit, maximum_building_height
    ),
    left = TRUE, largest = TRUE
  )

# -----------------------------------------------------------------------------
# 2. REDISTRICTING LOGIC FUNCTIONS
# -----------------------------------------------------------------------------
get_boundary_year <- function(construction_date) {
  case_when(
    construction_date < REDISTRICTING_2003 ~ 1998L,
    construction_date < REDISTRICTING_2015 ~ 2003L,
    construction_date < REDISTRICTING_2024 ~ 2015L,
    TRUE                                   ~ 2024L
  )
}

# -----------------------------------------------------------------------------
# 3. WARD ASSIGNMENT
# -----------------------------------------------------------------------------

cat("Assigning wards to parcels based on construction year...\n")

assign_ward_and_distances <- function(parcel_batch) {
  results <- parcel_batch %>%
    mutate(boundary_year = get_boundary_year(construction_date)) %>%
    ungroup()
  
  results_with_wards <- results %>%
    st_join(
      ward_panel %>%
        filter(year %in% unique(results$boundary_year)) %>%
        dplyr::select(year, ward, geom),
      suffix = c("", "_ward")
    ) %>%
    filter(boundary_year == year) %>%
    dplyr::select(-year) %>%
    rename(assigned_ward = ward)
  
  results_with_wards
}

batch_size <- 5000
n_batches <- ceiling(nrow(parcels) / batch_size)

cat(sprintf("Processing %d parcels in %d batches...\n", nrow(parcels), n_batches))

parcels_with_wards <- map_dfr(1:n_batches, function(i) {
  cat(sprintf("Processing batch %d of %d...\n", i, n_batches))
  
  start_idx <- (i - 1) * batch_size + 1
  end_idx <- min(i * batch_size, nrow(parcels))
  batch <- parcels[start_idx:end_idx, ]
  
  assign_ward_and_distances(batch)
})

# -----------------------------------------------------------------------------
# 4. IDENTIFY ADJACENT WARD PAIRS + BUILD SHARED BORDERS (ALL MAP YEARS)
# -----------------------------------------------------------------------------

cat("Identifying adjacent ward pairs...\n")

get_adjacent_ward_pairs <- function(ward_boundaries) {
  wards <- unique(ward_boundaries$ward)
  adjacent_pairs <- tibble()
  for (i in 1:(length(wards)-1)) {
    for (j in (i+1):length(wards)) {
      ward_a <- wards[i]; ward_b <- wards[j]
      geom_a <- ward_boundaries %>% filter(ward == ward_a) %>% st_geometry()
      geom_b <- ward_boundaries %>% filter(ward == ward_b) %>% st_geometry()
      touches <- st_touches(geom_a, geom_b, sparse = FALSE)[1,1]
      if (!touches) touches <- st_intersects(st_buffer(geom_a, 1), geom_b, sparse = FALSE)[1,1]
      if (touches) {
        adjacent_pairs <- bind_rows(adjacent_pairs,
                                    tibble(ward_a = ward_a, ward_b = ward_b,
                                           ward_pair = paste(min(ward_a, ward_b), max(ward_a, ward_b), sep = "_")))
      }
    }
  }
  adjacent_pairs
}

build_shared_borders <- function(ward_boundaries, pairs_df) {
  yr <- unique(ward_boundaries$year)[1]
  wb <- ward_boundaries %>%
    dplyr::select(ward, geom) %>% st_make_valid() %>%
    dplyr::group_by(ward) %>% dplyr::summarise(geometry = st_union(geom), .groups = "drop") %>%
    st_as_sf(crs = st_crs(ward_boundaries))
  
  out <- purrr::map_dfr(seq_len(nrow(pairs_df)), function(i) {
    wa <- pairs_df$ward_a[i]; wb_ <- pairs_df$ward_b[i]
    ga <- wb %>% dplyr::filter(ward == wa) %>% st_geometry()
    gb <- wb %>% dplyr::filter(ward == wb_) %>% st_geometry()
    shared <- suppressWarnings(st_intersection(st_boundary(ga), st_boundary(gb)))
    if (length(shared) == 0 || all(st_is_empty(shared))) return(NULL)
    gtypes <- unique(as.character(st_geometry_type(shared)))
    shared_lines <- if (all(gtypes %in% c("LINESTRING","MULTILINESTRING"))) {
      st_cast(shared, "LINESTRING")
    } else if ("GEOMETRYCOLLECTION" %in% gtypes) {
      suppressWarnings(st_collection_extract(shared, "LINESTRING"))
    } else return(NULL)
    if (length(shared_lines) == 0 || all(st_is_empty(shared_lines))) return(NULL)
    shared_lines <- shared_lines[as.numeric(st_length(shared_lines)) > 0, ]
    if (length(shared_lines) == 0) return(NULL)
    st_sf(year = yr, ward_a = wa, ward_b = wb_,
          ward_pair = paste(min(wa, wb_), max(wa, wb_), sep = "_"),
          geometry = shared_lines)
  })
  if (nrow(out) == 0) {
    return(st_sf(year = integer(), ward_a = integer(), ward_b = integer(),
                 ward_pair = character(), geometry = st_sfc(), crs = st_crs(ward_boundaries)))
  }
  st_as_sf(out, crs = st_crs(ward_boundaries))
}

YEARS_FOR_MAPS <- c(1998L, 2003L, 2015L, 2024L)

adjacent_pairs_all <- purrr::map_dfr(YEARS_FOR_MAPS, function(yy) {
  wb <- ward_panel %>% filter(year == yy)
  if (nrow(wb) == 0) return(tibble(year = yy, ward_a = integer(), ward_b = integer(), ward_pair = character()))
  ap <- get_adjacent_ward_pairs(wb)
  if (nrow(ap) == 0) return(tibble(year = yy, ward_a = integer(), ward_b = integer(), ward_pair = character()))
  mutate(ap, year = yy, .before = 1)
})

shared_borders_all <- purrr::map_dfr(YEARS_FOR_MAPS, function(yy) {
  wb <- ward_panel %>% filter(year == yy)
  ap <- adjacent_pairs_all %>% filter(year == yy) %>% dplyr::select(-year)
  if (nrow(wb) == 0 || nrow(ap) == 0) return(NULL)
  build_shared_borders(wb, ap)
})

# -----------------------------------------------------------------------------
# 5. CALCULATE DISTANCES TO WARD BOUNDARIES (SIMPLIFIED APPROACH)
# -----------------------------------------------------------------------------

cat("Calculating distances to ward boundaries (simplified approach)...\n")

calculate_boundary_distances_simple <- function(parcels_df, batch_size = 2000) {
  boundary_years <- unique(parcels_df$boundary_year)
  
  results_list <- purrr::map(boundary_years, function(by) {
    cat(sprintf("Processing boundary year %d...\n", by))
    year_parcels <- parcels_df %>% filter(boundary_year == by)
    if (nrow(year_parcels) == 0) return(NULL)
    
    borders_year <- shared_borders_all %>% filter(year == by)
    if (nrow(borders_year) == 0) return(NULL)
    
    n_batches <- ceiling(nrow(year_parcels) / batch_size)
    cat(sprintf("Processing %d parcels in %d batches...\n", nrow(year_parcels), n_batches))
    
    purrr::map_dfr(1:n_batches, function(batch_i) {
      cat(sprintf("  Distance batch %d of %d...\n", batch_i, n_batches))
      start_idx <- (batch_i - 1) * batch_size + 1
      end_idx   <- min(batch_i * batch_size, nrow(year_parcels))
      batch_parcels <- year_parcels[start_idx:end_idx, ]
      
      uw <- unique(batch_parcels$assigned_ward)
      edges <- borders_year %>% filter(ward_a %in% uw | ward_b %in% uw)
      if (nrow(edges) == 0) {
        batch_parcels$dist_to_boundary <- NA_real_
        batch_parcels$ward_pair <- NA_character_
        return(batch_parcels)
      }
      
      nearest_idx   <- st_nearest_feature(batch_parcels, edges)
      nearest_edges <- edges[nearest_idx, ]
      dists         <- st_distance(st_geometry(batch_parcels), st_geometry(nearest_edges), by_element = TRUE)
      
      neighbor <- ifelse(batch_parcels$assigned_ward == nearest_edges$ward_a,
                         nearest_edges$ward_b, nearest_edges$ward_a)
      
      batch_parcels$ward_pair        <- paste(pmin(batch_parcels$assigned_ward, neighbor),
                                              pmax(batch_parcels$assigned_ward, neighbor), sep = "_")
      batch_parcels$dist_to_boundary <- as.numeric(dists)
      batch_parcels
    })
  })
  
  bind_rows(results_list)
}

# Use simplified function
parcels_with_distances <- calculate_boundary_distances_simple(parcels_with_wards, batch_size = 2000)

cat("Distance calculation completed!\n")

# -----------------------------------------------------------------------------
# 6. ADD WARD PAIRS AND ALDERMAN INFO (EFFICIENT VERSION)
# -----------------------------------------------------------------------------

cat("Pre-processing alderman lookup tables...\n")
alderman_lookup <- alderman_panel %>%
  select(ward, month, alderman) %>%
  mutate(
    month_yearmon = as.yearmon(month, format = "%b %Y"),
    year = year(as.Date(month_yearmon)),
    yearmon_key = as.character(month_yearmon)
  )

# Create tenure lookup
alderman_tenure_lookup <- alderman_panel %>%
  mutate(month_yearmon = as.yearmon(month, format = "%b %Y")) %>%
  group_by(ward, alderman) %>%
  summarise(
    first_month = min(month_yearmon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(alderman_ward_key = paste(ward, alderman, sep = "_"))

# Efficient processing
final_dataset <- parcels_with_distances %>%
  mutate(
    construction_yearmon = as.yearmon(construction_date),
    yearmon_key = as.character(construction_yearmon)
  ) %>%
  left_join(alderman_lookup, 
            by = c("assigned_ward" = "ward", "yearmon_key")) %>%
  mutate(
    alderman_ward_key = paste(assigned_ward, alderman, sep = "_")
  ) %>%
  left_join(alderman_tenure_lookup, 
            by = "alderman_ward_key") %>%
  mutate(
    alderman_tenure_months = ifelse(
      !is.na(first_month) & !is.na(construction_yearmon),
      as.numeric((construction_yearmon - first_month) * 12),
      NA_real_
    )
  ) %>% 
  select(
    pin, geom,
    construction_year = yearbuilt, construction_date, boundary_year, dist_to_boundary,
    ward = assigned_ward, ward_pair,
    alderman = alderman.x, alderman_tenure_months,
    # NOTE: These columns were kept in your previous script (res + multi)
    arealotsf, areabuilding, bedroomscount, unitscount, storiescount, residential,
    
    # NOTE: The following columns were DROPPED in the previous geocoding script.
    # I have commented them out to prevent crashes. If you need them, 
    # add them to the select() in geocode_residential_data.R.
    # fullbathcount, halfbathcount, roomscount, 
    # construction_quality, central_heating, central_air, single_v_multi_family,
    
    zone_code 
  )

# -----------------------------------------------------------------------------
# 7. MERGE IN ALDERMAN STRICTNESS SCORES
# -----------------------------------------------------------------------------
final_dataset <- final_dataset %>%
  left_join(alderman_scores, by = "alderman") 

# -----------------------------------------------------------------------------
# 8. MAKE OUTCOME VARIABLES
# -----------------------------------------------------------------------------

final_dataset <- final_dataset %>%
  mutate(
    # Floor Area Ratio
    density_far = if_else(arealotsf > 0, areabuilding / arealotsf, NA_real_),
    # Lot Area Per Unit
    density_lapu = if_else(unitscount > 0, arealotsf / unitscount, NA_real_),
    
    # Building Coverage Ratio (requires stories; will be NA for multifamily where storiescount is NA)
    density_bcr = if_else(!is.na(storiescount) & storiescount > 0 & arealotsf > 0, 
                          (areabuilding / storiescount) / arealotsf, NA_real_),
    # Lot Size Per Story (will be NA for multifamily)
    density_lps = if_else(!is.na(storiescount) & storiescount > 0, arealotsf / storiescount, NA_real_),
    
    # Square Feet Per Unit
    density_spu = if_else(unitscount > 0, areabuilding / unitscount, NA_real_), 
    ## Dwelling units per acre (DUPAC)
    density_dupac = if_else(
      arealotsf > 0 & unitscount > 0,
      43560 * unitscount / arealotsf, 
      NA_real_)
  ) 

# -----------------------------------------------------------------------------
# 8.5. SAVE GEOSPATIAL OUTPUT
# -----------------------------------------------------------------------------
cat("Saving geospatial output before dropping geometry...\n")
st_write(final_dataset, "../output/parcels_with_geometry.gpkg", delete_dsn = TRUE)

# -----------------------------------------------------------------------------
# 9. SIGN DISTANCES BASED ON STRICTNESS 
# -----------------------------------------------------------------------------
final_dataset <- as_tibble(st_drop_geometry(final_dataset))

strictness_lookup <- final_dataset %>%
  group_by(ward, boundary_year) %>%
  summarise(strictness = mean(strictness, na.rm = TRUE), .groups = "drop")

final_dataset_signed <- final_dataset %>%
  mutate(
    wards_in_pair = str_split_fixed(ward_pair, "_", 2),
    ward_a = as.integer(wards_in_pair[, 1]),
    ward_b = as.integer(wards_in_pair[, 2]),
    other_ward = if_else(ward == ward_a, ward_b, ward_a)
  ) %>%
  left_join(strictness_lookup, by = c("other_ward" = "ward", "boundary_year" = "boundary_year")) %>%
  rename(strictness_own = strictness.x, strictness_neighbor = strictness.y) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance = dist_to_boundary * sign
  ) %>%
  filter(!is.na(signed_distance)) %>% 
  dplyr::select(-contains("wards_in_pair"))

cat("Final Dataset Created!\n")

# -----------------------------------------------------------------------------
# 10. SAVE OUTPUT
# -----------------------------------------------------------------------------

cat("Saving output...\n")

write_csv(final_dataset_signed, "../output/parcels_with_ward_distances.csv")

summary_stats <- final_dataset_signed %>%
  summarise(
    n_parcels = n(),
    n_wards = n_distinct(ward),
    n_ward_pairs = n_distinct(ward_pair, na.rm = TRUE),
    n_aldermen = n_distinct(alderman, na.rm = TRUE),
    mean_dist_to_boundary = mean(signed_distance, na.rm = TRUE),
    median_dist_to_boundary = median(signed_distance, na.rm = TRUE),
    .by = c(boundary_year, construction_year)
  ) %>% 
  arrange(construction_year)

cat("Task completed successfully!\n")
print(summary_stats)

write_csv(summary_stats, "../output/boundary_distance_summary.csv")

cat("Task completed successfully!\n")
cat(sprintf("Processed %d parcels across %d wards\n", 
            nrow(final_dataset), 
            n_distinct(final_dataset$ward)))
cat(sprintf("Found %d unique ward pairs\n", 
            n_distinct(final_dataset$ward_pair, na.rm = TRUE)))