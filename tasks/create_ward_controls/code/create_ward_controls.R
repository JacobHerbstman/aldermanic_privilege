## this code creates ward-level controls from the ACS by spatially joining block groups to wards
## Extended to 2006-2023 for full time coverage

source("../../setup_environment/code/packages.R")

# Set census api key if not already done
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Read ward panel for spatial boundaries
ward_panel <- st_read("../input/ward_panel.gpkg")

# Create output directory
dir.create("../output", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. DEFINE VARIABLES
# -----------------------------------------------------------------------------
acs_vars <- c(
  # Population and Race/Ethnicity
  total_population = "B01003_001",
  hispanic_population = "B03003_003", 
  black_population = "B03002_004",
  
  # Housing Tenure
  total_units = "B25003_001",
  owner_occupied = "B25003_002",
  
  # Income
  median_income = "B19013_001",
  
  # Average Household Size
  avg_household_size = "B25010_001"
)

# -----------------------------------------------------------------------------
# 2. DOWNLOAD ACS DATA (NO GEOMETRY) - EXTENDED PERIODS
# -----------------------------------------------------------------------------

# Function to download ACS data without geometry (your working version)
get_bg_data <- function(year_to_get) {
  get_acs(
    geography = "block group",
    variables = acs_vars,
    state = "IL",
    county = "Cook",
    year = year_to_get,
    survey = "acs5",
    output = "wide"
  ) %>%
    st_drop_geometry() %>%
    select(GEOID, ends_with("E")) %>% # Keep only GEOID and estimate columns
    rename_with(~ sub("E$", "", .), .cols = everything())
}

# Download data for multiple periods to cover 2006-2023
message("Downloading ACS data...")
early_period_data <- get_bg_data(2009) %>% mutate(period = "early")   # Covers ~2007-2011
pre_period_data <- get_bg_data(2014) %>% mutate(period = "pre")       # Covers ~2012-2016
post_period_data <- get_bg_data(2019) %>% mutate(period = "post")     # Covers ~2017-2021
recent_period_data <- get_bg_data(2022) %>% mutate(period = "recent") # Covers ~2020-2024

message("Downloaded data:")
message("- Early period (2009): ", nrow(early_period_data), " block groups")
message("- Pre-period (2014): ", nrow(pre_period_data), " block groups")
message("- Post-period (2019): ", nrow(post_period_data), " block groups")
message("- Recent period (2022): ", nrow(recent_period_data), " block groups")

# -----------------------------------------------------------------------------
# 3. GET BLOCK GROUP GEOMETRIES SEPARATELY
# -----------------------------------------------------------------------------

# Get block group geometries from a stable source
message("Getting block group geometries...")

bg_geometries <- 
  # Try 2020 first (usually most stable)
  get_acs(
    geography = "block group",
    variables = "B01003_001",  # Just population for geometry
    state = "IL",
    county = "Cook",
    year = 2019,
    survey = "acs5",
    geometry = TRUE
  ) %>%
  select(GEOID, geometry)

message("Got geometries for ", nrow(bg_geometries), " block groups")

# -----------------------------------------------------------------------------
# 4. PROCESS AND COMBINE DATA
# -----------------------------------------------------------------------------

# Combine all periods
combined_acs_data <- bind_rows(early_period_data, pre_period_data, post_period_data, recent_period_data)

# Add geometries to the data
bg_data_with_geom <- combined_acs_data %>%
  left_join(bg_geometries, by = "GEOID") %>%
  filter(!is.na(geometry)) %>%
  st_as_sf() %>%
  mutate(
    # Calculate derived variables
    land_area_sqkm = as.numeric(st_area(.)) / 1e6,
    homeownership_rate = ifelse(total_units > 0, owner_occupied / total_units, NA),
    population_density = ifelse(land_area_sqkm > 0, total_population / land_area_sqkm, NA),
    percent_black = ifelse(total_population > 0, black_population / total_population, NA),
    percent_hispanic = ifelse(total_population > 0, hispanic_population / total_population, NA)
  ) %>%
  # Filter out problematic block groups
  filter(!is.na(total_population), total_population > 0) %>%
  # Keep only what we need
  select(
    GEOID, period, total_population, homeownership_rate, 
    population_density, median_income, percent_black, 
    percent_hispanic, avg_household_size, geometry
  )

message("Combined data: ", nrow(bg_data_with_geom), " block group-period observations")

# -----------------------------------------------------------------------------
# 5. SPATIAL JOIN BLOCK GROUPS TO WARDS BY YEAR - EXTENDED RANGE
# -----------------------------------------------------------------------------

ward_controls_list <- list()

for (year_i in 2006:2023) {
  message("Processing spatial join for year ", year_i)
  
  # Get the right ACS period based on year
  period_to_use <- case_when(
    year_i <= 2011 ~ "early",
    year_i <= 2015 ~ "pre", 
    year_i <= 2019 ~ "post",
    year_i <= 2023 ~ "recent"
  )
  
  # Get block groups for this period
  year_bg_data <- bg_data_with_geom %>% 
    filter(period == period_to_use)
  
  # Get ward geometries for this year - use available years or extend
  if (year_i %in% unique(ward_panel$year)) {
    year_wards <- ward_panel %>%
      filter(year == year_i) %>%
      select(ward, year) %>%
      group_by(ward, year) %>%
      summarise(.groups = "drop")
  } else {
    # For years not in ward_panel, use the nearest available year
    nearest_year <- unique(ward_panel$year)[which.min(abs(unique(ward_panel$year) - year_i))]
    year_wards <- ward_panel %>%
      filter(year == nearest_year) %>%
      select(ward) %>%
      group_by(ward) %>%
      summarise(.groups = "drop") %>%
      mutate(year = year_i)
  }
  
  if (nrow(year_wards) == 0) {
    message("  - No ward geometries found for year ", year_i, ", skipping")
    next
  }
  
  # Ensure CRS compatibility
  if (st_crs(year_bg_data) != st_crs(year_wards)) {
    message("  - Transforming CRS for compatibility")
    year_bg_data <- st_transform(year_bg_data, st_crs(year_wards))
  }
  
  # Spatial join: find which ward each block group centroid falls into
  bg_centroids <- st_centroid(year_bg_data)
  
  bg_ward_joined <- st_join(bg_centroids, year_wards, join = st_within) %>%
    filter(!is.na(ward)) %>%  # Keep only block groups that fall within a ward
    st_drop_geometry()
  
  message("  - Successfully joined ", nrow(bg_ward_joined), " block groups to wards")
  
  # Aggregate to ward level using population weighting
  ward_year_controls <- bg_ward_joined %>%
    group_by(ward, year) %>%
    summarise(
      # Population-weighted averages
      homeownership_rate = weighted.mean(homeownership_rate, total_population, na.rm = TRUE),
      median_income = weighted.mean(median_income, total_population, na.rm = TRUE),
      percent_black = weighted.mean(percent_black, total_population, na.rm = TRUE),
      percent_hispanic = weighted.mean(percent_hispanic, total_population, na.rm = TRUE),
      avg_household_size = weighted.mean(avg_household_size, total_population, na.rm = TRUE),
      population_density = weighted.mean(population_density, total_population, na.rm = TRUE),
      
      # Ward totals
      total_population_ward = sum(total_population, na.rm = TRUE),
      n_block_groups = n_distinct(GEOID),
      
      .groups = "drop"
    ) %>%
    mutate(
      # Clean up problematic values
      across(where(is.numeric), ~ifelse(is.nan(.) | is.infinite(.), NA, .))
    )
  
  ward_controls_list[[as.character(year_i)]] <- ward_year_controls
}

# Combine all years
ward_controls <- bind_rows(ward_controls_list)

message("Ward controls created: ", nrow(ward_controls), " ward-year observations")
message("Year range: ", min(ward_controls$year), " to ", max(ward_controls$year))

# -----------------------------------------------------------------------------
# 6. QUALITY CHECKS
# -----------------------------------------------------------------------------

# # Check coverage - we should have 50 wards per year
# coverage_check <- ward_controls %>%
#   group_by(year) %>%
#   summarise(
#     wards_with_data = n_distinct(ward),
#     missing_wards = list(setdiff(1:50, unique(ward))),
#     avg_block_groups_per_ward = mean(n_block_groups, na.rm = TRUE),
#     total_population = sum(total_population_ward, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   mutate(missing_wards = map_chr(missing_wards, ~paste(.x, collapse = ", ")))
# 
# message("\nCoverage by year:")
# print(coverage_check)
# 
# # Summary statistics
# ward_summary_stats <- ward_controls %>%
#   summarise(
#     across(c(homeownership_rate, population_density, median_income, 
#              percent_black, percent_hispanic, avg_household_size),
#            list(mean = ~mean(., na.rm = TRUE),
#                 sd = ~sd(., na.rm = TRUE),
#                 min = ~min(., na.rm = TRUE),
#                 max = ~max(., na.rm = TRUE),
#                 missing = ~sum(is.na(.))),
#            .names = "{.col}_{.fn}")
#   )
# 
# # Check for any wards with very low population (might indicate problems)
# low_pop_wards <- ward_controls %>%
#   filter(total_population_ward < 1000) %>%
#   select(ward, year, total_population_ward, n_block_groups)
# 
# if (nrow(low_pop_wards) > 0) {
#   message("\nWarning: Found wards with very low population:")
#   print(low_pop_wards)
# }
# 
# # -----------------------------------------------------------------------------
# 7. SAVE OUTPUT
# -----------------------------------------------------------------------------

# Main ward controls file
write_csv(ward_controls, "../output/ward_controls.csv")

message("\nFiles saved:")
message("- ../output/ward_controls.csv")

message("\nScript completed successfully!")