# calculate_rent_distances.R
# Calculates signed distance to nearest ward boundary for rental listings.
# Optimized for large datasets using batching and spatial indexing.

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# -----------------------------------------------------------------------------
# 1. SETUP & ARGUMENTS
# -----------------------------------------------------------------------------
option_list <- list(
  make_option(c("-s", "--sample"),
    type = "character", default = "TRUE",
    help = "Run on sample? (TRUE/FALSE)"
  )
)
opt <- parse_args(OptionParser(option_list = option_list))
run_sample <- as.logical(opt$sample)

# Core CRS for distance calc (Illinois East ftUS)
crs_projected <- 3435

# Map Change Dates (Crucial for correct assignment)
# Map 1 (2003-2015): Ends May 2015
# Map 2 (2015-2023): Starts May 2015, Ends May 2023
# Map 3 (2023-Present): Starts May 2023
date_switch_2015 <- as.Date("2015-05-18") # Inauguration day 2015
date_switch_2023 <- as.Date("2023-05-15") # Inauguration day 2023

# -----------------------------------------------------------------------------
# 2. LOAD & PREP ANCILLARY DATA
# -----------------------------------------------------------------------------
message("Loading ancillary data...")

# Wards
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(crs_projected)

# Alderman Data
alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(month = as.yearmon(month))

strictness <- read_csv("../input/alderman_restrictiveness_scores_month_FEs.csv", show_col_types = FALSE) %>%
  select(alderman, strictness_index)

# Controls (for signing the border based on strictness)
# We need strictness scores attached to the ward-year level
# This is a bit complex since strictness is static per alderman, but aldermen move.
# We will attach strictness *after* the spatial join using the date/ward.

# -----------------------------------------------------------------------------
# 3. HELPER: BUILD BOUNDARY LINES
# -----------------------------------------------------------------------------
# We need line segments for each map version to calculate distance to *border*
get_boundaries <- function(ward_sf) {
  # Create shared boundaries by intersecting polygons
  # This is computationally expensive, so we do it once per map version

  # Buffer 0 to fix topology
  ward_sf <- st_buffer(ward_sf, 0)

  # Find touching pairs
  adj <- st_touches(ward_sf)

  edges <- imap_dfr(adj, function(nb, i) {
    if (length(nb) == 0) {
      return(NULL)
    }

    # Iterate through neighbors (only j > i to avoid duplicates)
    nb_valid <- nb[nb > i]
    if (length(nb_valid) == 0) {
      return(NULL)
    }

    map_dfr(nb_valid, function(j) {
      geom_i <- st_geometry(ward_sf[i, ])
      geom_j <- st_geometry(ward_sf[j, ])

      # Intersection is the shared border
      shared <- st_intersection(geom_i, geom_j)

      # Keep only lines (ignore points if corners touch)
      if (st_dimension(shared) != 1) {
        return(NULL)
      }

      tibble(
        ward_a = ward_sf$ward[i],
        ward_b = ward_sf$ward[j],
        geometry = shared
      )
    })
  }) %>%
    st_as_sf(crs = st_crs(ward_sf))

  return(edges)
}

message("Preparing ward maps...")
# Map 1: 2014 (Representative of pre-2015 era)
map_2014_poly <- ward_panel %>% filter(year == 2014)
map_2014_lines <- get_boundaries(map_2014_poly)

# Map 2: 2016 (Representative of 2015-2023 era)
map_2015_poly <- ward_panel %>% filter(year == 2016)
map_2015_lines <- get_boundaries(map_2015_poly)

# Map 3: 2024 (Representative of current era)
map_2023_poly <- ward_panel %>% filter(year == 2024)
map_2023_lines <- get_boundaries(map_2023_poly)

message(sprintf(
  "Generated boundaries: 2014 (%d), 2015 (%d), 2023 (%d)",
  nrow(map_2014_lines), nrow(map_2015_lines), nrow(map_2023_lines)
))

# -----------------------------------------------------------------------------
# 4. RENT PROCESSING FUNCTION
# -----------------------------------------------------------------------------

process_batch <- function(df_batch) {
  # 1. Prep
  # Filter invalid coords
  df_batch <- df_batch %>%
    filter(!is.na(latitude), !is.na(longitude), !is.na(file_date))

  if (nrow(df_batch) == 0) {
    return(NULL)
  }

  # Convert to SF
  pts <- st_as_sf(df_batch, coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(crs_projected)

  # 2. Split by Era
  pts_p1 <- pts %>% filter(file_date < date_switch_2015)
  pts_p2 <- pts %>% filter(file_date >= date_switch_2015, file_date < date_switch_2023)
  pts_p3 <- pts %>% filter(file_date >= date_switch_2023)

  # Helper to process a subset against a specific map
  calc_dist <- function(points, polys, lines) {
    if (nrow(points) == 0) {
      return(NULL)
    }

    # A. Assign Ward (Point in Polygon)
    # This is fast
    joined <- st_join(points, polys %>% select(ward), join = st_within)

    # Drop points outside Chicago wards
    joined <- joined %>% filter(!is.na(ward))
    if (nrow(joined) == 0) {
      return(NULL)
    }

    # B. Distance to Nearest Border (Spatial Index)
    # st_nearest_feature gives the index of the nearest line segment
    nearest_idx <- st_nearest_feature(joined, lines)

    # Get the geometry of the nearest lines
    nearest_geoms <- lines[nearest_idx, ]

    # Calculate element-wise distance (very fast)
    dists <- st_distance(joined, nearest_geoms, by_element = TRUE)

    # C. Metadata
    joined$dist_ft <- as.numeric(dists)
    joined$ward_pair_a <- nearest_geoms$ward_a
    joined$ward_pair_b <- nearest_geoms$ward_b

    # Identify neighbor ward
    joined <- joined %>%
      mutate(
        neighbor_ward = if_else(ward == ward_pair_a, ward_pair_b, ward_pair_a),
        ward_pair_id = paste(pmin(ward, neighbor_ward), pmax(ward, neighbor_ward), sep = "-")
      ) %>%
      select(-ward_pair_a, -ward_pair_b)

    return(joined)
  }

  # Run calculations
  res1 <- calc_dist(pts_p1, map_2014_poly, map_2014_lines)
  res2 <- calc_dist(pts_p2, map_2015_poly, map_2015_lines)
  res3 <- calc_dist(pts_p3, map_2023_poly, map_2023_lines)

  bind_rows(res1, res2, res3)
}

# -----------------------------------------------------------------------------
# 5. EXECUTE (STREAMING)
# -----------------------------------------------------------------------------
input_file <- "../input/chicago_rent_panel.parquet"

# Open dataset
ds <- arrow::open_dataset(input_file)

years <- 2014:2025
results_list <- list()

if (run_sample) {
  message("RUNNING ON SAMPLE MODE (1% by year)")
} else {
  message("RUNNING FULL DATASET (Batch Mode)")
}

# Set up progress bar for BOTH modes
pb <- txtProgressBar(min = 0, max = length(years), style = 3)

for (i in seq_along(years)) {
  yr <- years[i]

  # Read chunk for the year
  df_chunk <- ds %>%
    filter(year(file_date) == yr) %>%
    select(id, rent_price, building_type, beds, baths, sqft, laundry, gym, year_built, available_date, file_date, latitude, longitude) %>%
    collect()

  # Apply sampling if needed
  if (run_sample) {
    df_chunk <- df_chunk %>% slice_sample(prop = 0.01)
  }

  # Process chunk
  if (nrow(df_chunk) > 0) {
    res <- process_batch(df_chunk)
    if (!is.null(res)) results_list[[length(results_list) + 1]] <- res
  }

  gc()
  setTxtProgressBar(pb, i)
}
close(pb)

results_sf <- bind_rows(results_list)

# -----------------------------------------------------------------------------
# 6. POST-PROCESS: ALDERMAN & SIGNING
# -----------------------------------------------------------------------------
message("Attaching Alderman data and calculating signed distances...")

# We convert back to tibble for the merge
# Extract lat/lon before dropping geometry (transform back to WGS84 first)
final_df <- results_sf %>%
  st_transform(4326) %>% # Back to WGS84 for proper lat/lon
  mutate(
    longitude = st_coordinates(.)[, 1],
    latitude = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  as_tibble()

# 6a. Attach Alderman (Own & Neighbor)
# We need to lookup who was alderman at [ward, file_date]
# Use the alderman panel.
# Optimized lookup: Join on Ward + Month-Year

final_df <- final_df %>%
  mutate(month_join = as.yearmon(file_date))

ald_lookup <- alderman_panel %>%
  select(ward, month, alderman)

# Join Own
final_df <- final_df %>%
  left_join(ald_lookup, by = c("ward" = "ward", "month_join" = "month")) %>%
  rename(alderman_own = alderman)

# Join Neighbor
final_df <- final_df %>%
  left_join(ald_lookup, by = c("neighbor_ward" = "ward", "month_join" = "month")) %>%
  rename(alderman_neighbor = alderman)

# 6b. Attach Strictness
final_df <- final_df %>%
  left_join(strictness, by = c("alderman_own" = "alderman")) %>%
  rename(strictness_own = strictness_index) %>%
  left_join(strictness, by = c("alderman_neighbor" = "alderman")) %>%
  rename(strictness_neighbor = strictness_index)

# 6c. Sign the Distance
# Positive distance = Strict side (Higher strictness)
# Treatment = Strict.
final_df <- final_df %>%
  filter(!is.na(strictness_own), !is.na(strictness_neighbor)) %>%
  mutate(
    # If Own > Neighbor, we are on strict side -> Positive Distance
    # If Own < Neighbor, we are on lenient side -> Negative Distance
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ 0 # Equal strictness (drop?)
    ),
    signed_dist = dist_ft * sign
  ) %>%
  filter(sign != 0)

# -----------------------------------------------------------------------------
# 7. CLEAN BUILDING TYPES
# -----------------------------------------------------------------------------
message("Cleaning building types...")

final_df <- final_df %>%
  mutate(
    building_type_clean = case_when(
      # Single Family
      str_detect(building_type, regex("SFR|Single[- ]?family|house", ignore_case = TRUE)) ~ "single_family",

      # Multi Family (APT, Apartment, Multi-family, duplex, triplex, etc.)
      str_detect(building_type, regex("APT|Apartment|Multi[- ]?family|duplex|triplex|fourplex", ignore_case = TRUE)) ~ "multi_family",

      # Condo (CON, Condo, Condominium)
      str_detect(building_type, regex("CON|Condo|Condominium", ignore_case = TRUE)) ~ "condo",

      # Townhouse (TH, Townhouse)
      str_detect(building_type, regex("TH|Townhouse", ignore_case = TRUE)) ~ "townhouse",

      # Other/Unknown (COMM, NA, unknown)
      TRUE ~ "other"
    )
  )


# -----------------------------------------------------------------------------
# 8. SAVE
# -----------------------------------------------------------------------------
# Dynamically determine output filename
suffix <- if (run_sample) "_sample" else "_full"
output_path <- sprintf("../output/rent_with_ward_distances%s.parquet", suffix)

write_parquet(final_df, output_path)

message("Done! Saved ", nrow(final_df), " rows to ", output_path)
