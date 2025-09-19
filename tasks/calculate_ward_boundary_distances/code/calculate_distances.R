# This script calculates signed distances from parcels to ward boundaries
# and assigns aldermen based on construction year and redistricting events

## run this line when editing code in Rstudio 
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# Configuration parameters for redistricting handling
REDISTRICTING_RULE <- "construction_year"  # Options: "strict_2015", "gradual_2012_2015", "construction_year"
REDISTRICTING_CUTOFF <- as.Date("2015-05-01")  # For strict rule
REDISTRICTING_START <- as.Date("2012-01-01")   # For gradual rule
REDISTRICTING_END <- as.Date("2015-05-01")     # For gradual rule

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

cat("Loading parcel data...\n")
parcels <- st_read("../input/year_built_sample.gpkg") %>%
  filter(!is.na(yearbuilt), yearbuilt >= 2003 & yearbuilt <= 2014) %>%
  mutate(construction_date = as.Date(paste0(yearbuilt, "-06-15")))

cat("Loading ward boundaries...\n")
ward_panel <- st_read("../input/ward_panel.gpkg")

cat("Loading alderman panel...\n")
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv")

cat("Loading alderman strictness scores... \n")
alderman_scores <- read_csv("../input/alderman_strictness_scores.csv")

if (st_crs(parcels) != st_crs(ward_panel)) {
  message("CRS mismatch detected. Transforming parcels CRS to match ward boundaries.")
  parcels <- st_transform(parcels, st_crs(ward_panel))
}

cat("Loading zoning data...\n")
zoning_data <- st_read("../input/zoning_data_clean.gpkg")

if (st_crs(zoning_data) != st_crs(ward_panel)) {
  zoning_data <- st_transform(zoning_data, st_crs(ward_panel))
}

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

get_ward_boundaries_for_year <- function(construction_year, rule = REDISTRICTING_RULE) {
  if (rule == "strict_2015") {
    boundary_year <- ifelse(construction_year < 2015, 2014, 2015)
  } else if (rule == "gradual_2012_2015") {
    boundary_year <- case_when(
      construction_year <= 2012 ~ 2014,
      construction_year >= 2015 ~ 2015,
      TRUE ~ 2014
    )
  } else if (rule == "construction_year") {
    boundary_year <- case_when(
      construction_year <= 2014 ~ 2014,
      construction_year >= 2015 ~ 2015,
      TRUE ~ 2015
    )
  }
  return(boundary_year)
}

get_alderman_for_parcel <- function(ward_id, construction_date, alderman_data) {
  construction_yearmon <- as.yearmon(construction_date)
  
  alderman_match <- alderman_data %>%
    filter(ward == ward_id, month == construction_yearmon) %>%
    slice(1)
  
  if (nrow(alderman_match) == 0) {
    return(list(alderman = NA_character_, finance_chair = 0, zoning_chair = 0, budget_chair = 0))
  }
  
  return(list(
    alderman = alderman_match$alderman,
    finance_chair = alderman_match$finance_chair,
    zoning_chair = alderman_match$zoning_chair,
    budget_chair = alderman_match$budget_chair
  ))
}

# -----------------------------------------------------------------------------
# 3. WARD ASSIGNMENT
# -----------------------------------------------------------------------------

cat("Assigning wards to parcels based on construction year...\n")

assign_ward_and_distances <- function(parcel_batch) {
  results <- parcel_batch %>%
    mutate(boundary_year = get_ward_boundaries_for_year(yearbuilt, REDISTRICTING_RULE)) %>%
    ungroup()
  
  results_with_wards <- results %>%
    st_join(
      ward_panel %>% 
        filter(year %in% unique(results$boundary_year)) %>%
        select(year, ward, geom),
      suffix = c("", "_ward")
    ) %>%
    filter(boundary_year == year) %>%
    select(-year) %>%
    rename(assigned_ward = ward)
  
  return(results_with_wards)
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
# 4. IDENTIFY ADJACENT WARD PAIRS
# -----------------------------------------------------------------------------

cat("Identifying adjacent ward pairs...\n")

get_adjacent_ward_pairs <- function(ward_boundaries) {
  wards <- unique(ward_boundaries$ward)
  adjacent_pairs <- tibble()
  
  for (i in 1:(length(wards)-1)) {
    for (j in (i+1):length(wards)) {
      ward_a <- wards[i]
      ward_b <- wards[j]
      
      geom_a <- ward_boundaries %>% filter(ward == ward_a) %>% st_geometry()
      geom_b <- ward_boundaries %>% filter(ward == ward_b) %>% st_geometry()
      
      # Try st_touches first, then try with small buffer if no touch detected
      touches <- st_touches(geom_a, geom_b, sparse = FALSE)[1,1]
      
      if (!touches) {
        # If they don't touch, try with a small buffer (1 meter) to catch near-adjacent wards
        geom_a_buffered <- st_buffer(geom_a, 1)
        touches <- st_intersects(geom_a_buffered, geom_b, sparse = FALSE)[1,1]
      }
      
      if (touches) {
        adjacent_pairs <- bind_rows(adjacent_pairs, 
                                    tibble(ward_a = ward_a, ward_b = ward_b,
                                           ward_pair = paste(min(ward_a, ward_b), 
                                                             max(ward_a, ward_b), sep = "_")))
      }
    }
  }
  return(adjacent_pairs)
}

adjacent_pairs_2014 <- get_adjacent_ward_pairs(ward_panel %>% filter(year == 2014))
adjacent_pairs_2015 <- get_adjacent_ward_pairs(ward_panel %>% filter(year == 2015))

cat(sprintf("Found %d adjacent ward pairs for 2014\n", nrow(adjacent_pairs_2014)))
cat(sprintf("Found %d adjacent ward pairs for 2015\n", nrow(adjacent_pairs_2015)))


# -----------------------------------------------------------------------------
# 4.5. Build shared-border LINESTRINGs for each adjacent ward pair per year
# -----------------------------------------------------------------------------

build_shared_borders <- function(ward_boundaries, pairs_df) {
  yr <- unique(ward_boundaries$year)[1]
  
  # one valid polygon per ward
  wb <- ward_boundaries %>%
    dplyr::select(ward, geom) %>%
    st_make_valid() %>%
    dplyr::group_by(ward) %>%
    dplyr::summarise(geometry = st_union(geom), .groups = "drop") %>%
    st_as_sf(crs = st_crs(ward_boundaries))
  
  out <- purrr::map_dfr(seq_len(nrow(pairs_df)), function(i) {
    wa <- pairs_df$ward_a[i]; wb_ <- pairs_df$ward_b[i]
    
    ga <- wb %>% dplyr::filter(ward == wa) %>% st_geometry()
    gb <- wb %>% dplyr::filter(ward == wb_) %>% st_geometry()
    
    # intersect boundaries; can yield LINESTRING / MULTILINESTRING / GEOMETRYCOLLECTION / POINT / empty
    shared <- suppressWarnings(st_intersection(st_boundary(ga), st_boundary(gb)))
    
    if (length(shared) == 0 || all(st_is_empty(shared))) return(NULL)
    
    gtypes <- unique(as.character(st_geometry_type(shared)))
    
    # keep only linework
    shared_lines <-
      if (all(gtypes %in% c("LINESTRING", "MULTILINESTRING"))) {
        st_cast(shared, "LINESTRING")
      } else if ("GEOMETRYCOLLECTION" %in% gtypes) {
        suppressWarnings(st_collection_extract(shared, "LINESTRING"))
      } else {
        # POINT/MULTIPOINT or other: touching at a point only -> no shared border to use
        return(NULL)
      }
    
    if (length(shared_lines) == 0 || all(st_is_empty(shared_lines))) return(NULL)
    
    # drop zero-length bits
    shared_lines <- shared_lines[as.numeric(st_length(shared_lines)) > 0, ]
    if (length(shared_lines) == 0) return(NULL)
    
    st_sf(
      year      = yr,
      ward_a    = wa,
      ward_b    = wb_,
      ward_pair = paste(min(wa, wb_), max(wa, wb_), sep = "_"),
      geometry  = shared_lines
    )
  })
  
  if (nrow(out) == 0) return(st_sf(year = integer(), ward_a = integer(), ward_b = integer(),
                                   ward_pair = character(), geometry = st_sfc(), crs = st_crs(ward_boundaries)))
  st_as_sf(out, crs = st_crs(ward_boundaries))
}

shared_borders_2014 <- build_shared_borders(ward_panel %>% filter(year == 2014), adjacent_pairs_2014)
shared_borders_2015 <- build_shared_borders(ward_panel %>% filter(year == 2015), adjacent_pairs_2015)

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
    
    borders_year <- if (by == 2014) shared_borders_2014 else shared_borders_2015
    if (nrow(borders_year) == 0) return(NULL)
    
    n_batches <- ceiling(nrow(year_parcels) / batch_size)
    cat(sprintf("Processing %d parcels in %d batches...\n", nrow(year_parcels), n_batches))
    
    batch_results <- purrr::map_dfr(1:n_batches, function(batch_i) {
      cat(sprintf("  Distance batch %d of %d...\n", batch_i, n_batches))
      start_idx <- (batch_i - 1) * batch_size + 1
      end_idx   <- min(batch_i * batch_size, nrow(year_parcels))
      batch_parcels <- year_parcels[start_idx:end_idx, ]
      
      # keep only borders that involve any ward present in this batch
      uw <- unique(batch_parcels$assigned_ward)
      edges <- borders_year %>% filter(ward_a %in% uw | ward_b %in% uw)
      if (nrow(edges) == 0) {
        batch_parcels$dist_to_boundary <- NA_real_
        batch_parcels$ward_pair <- NA_character_
        return(batch_parcels)
      }
      
      # nearest shared border for each parcel
      nearest_idx <- st_nearest_feature(batch_parcels, edges)
      nearest_edges <- edges[nearest_idx, ]
      
      # distance to that shared border
      dists <- st_distance(st_geometry(batch_parcels), st_geometry(nearest_edges), by_element = TRUE)
      
      # identify the neighbor ward on that border
      neighbor <- ifelse(batch_parcels$assigned_ward == nearest_edges$ward_a,
                         nearest_edges$ward_b,
                         nearest_edges$ward_a)
      
      batch_parcels$ward_pair        <- paste(pmin(batch_parcels$assigned_ward, neighbor),
                                              pmax(batch_parcels$assigned_ward, neighbor), sep = "_")
      batch_parcels$dist_to_boundary <- as.numeric(dists)
      batch_parcels
    })
    
    batch_results
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
  select(ward, month, alderman, finance_chair, zoning_chair, budget_chair) %>%
  mutate(
    # Convert "Jan 2003" format to yearmon first, then extract year
    month_yearmon = as.yearmon(month, format = "%b %Y"),
    year = year(as.Date(month_yearmon)),
    yearmon_key = as.character(month_yearmon)
  )

# Create tenure lookup (calculate once per alderman-ward combo)
alderman_tenure_lookup <- alderman_panel %>%
  mutate(month_yearmon = as.yearmon(month, format = "%b %Y")) %>%
  group_by(ward, alderman) %>%
  summarise(
    first_month = min(month_yearmon, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(alderman_ward_key = paste(ward, alderman, sep = "_"))

# Simplified ward pair lookup (one record per ward)
ward_pair_simple <- bind_rows(
  adjacent_pairs_2014 %>% 
    select(ward_a, ward_pair) %>% 
    rename(ward = ward_a) %>% 
    mutate(boundary_year = 2014),
  adjacent_pairs_2014 %>% 
    select(ward_b, ward_pair) %>% 
    rename(ward = ward_b) %>% 
    mutate(boundary_year = 2014),
  adjacent_pairs_2015 %>% 
    select(ward_a, ward_pair) %>% 
    rename(ward = ward_a) %>% 
    mutate(boundary_year = 2015),
  adjacent_pairs_2015 %>% 
    select(ward_b, ward_pair) %>% 
    rename(ward = ward_b) %>% 
    mutate(boundary_year = 2015)
) %>%
  distinct() %>%
  group_by(ward, boundary_year) %>%
  summarise(ward_pair = first(ward_pair), .groups = "drop")

# Efficient processing
final_dataset <- parcels_with_distances %>%
  mutate(
    construction_yearmon = as.yearmon(construction_date),
    yearmon_key = as.character(construction_yearmon)
  ) %>%
  # Simple left joins instead of complex operations
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
    attom_id, geom,
    construction_year = yearbuilt, construction_date, boundary_year,
    ward = assigned_ward, ward_pair, dist_to_boundary,
    alderman = alderman.x, alderman_tenure_months,
    finance_chair, zoning_chair, budget_chair,
    arealotsf, areabuilding, bedroomscount, bathcount, bathpartialcount, roomscount, storiescount, unitscount, 
    assessorpriorsaleamount, deedlastsaleprice, construction, foundation, 
    zone_code, floor_area_ratio, lot_area_per_unit, maximum_building_height,
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
    # Building Coverage Ratio
    density_bcr = if_else(storiescount > 0 & arealotsf > 0, 
                          (areabuilding / storiescount) / arealotsf, NA_real_),
    # Lot Size Per Story
    density_lps = if_else(storiescount > 0, arealotsf / storiescount, NA_real_),
    # Square Feet Per Unit
    density_spu = if_else(unitscount > 0, areabuilding / unitscount, NA_real_)
  ) %>%
  filter(!is.na(dist_to_boundary) & !is.na(ward_pair) & !is.na(strictness_index)) 

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
  summarise(strictness_index = mean(strictness_index, na.rm = TRUE), .groups = "drop")

final_dataset_signed <- final_dataset %>%
  mutate(
    wards_in_pair = str_split_fixed(ward_pair, "_", 2),
    ward_a = as.integer(wards_in_pair[, 1]),
    ward_b = as.integer(wards_in_pair[, 2]),
    other_ward = if_else(ward == ward_a, ward_b, ward_a)
  ) %>%
  left_join(strictness_lookup, by = c("other_ward" = "ward", "boundary_year" = "boundary_year")) %>%
  rename(strictness_own = strictness_index.x, strictness_neighbor = strictness_index.y) %>%
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
# write_parquet(st_drop_geometry(final_dataset), "../output/parcels_with_ward_distances.parquet")

summary_stats <- final_dataset_signed %>%
  st_drop_geometry() %>%
  summarise(
    n_parcels = n(),
    n_wards = n_distinct(ward),
    n_ward_pairs = n_distinct(ward_pair, na.rm = TRUE),
    n_aldermen = n_distinct(alderman, na.rm = TRUE),
    mean_dist_to_boundary = mean(dist_to_boundary, na.rm = TRUE),
    median_dist_to_boundary = median(dist_to_boundary, na.rm = TRUE),
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
