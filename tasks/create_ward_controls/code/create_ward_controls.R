## this code creates ward-level controls from the ACS by spatially joining census tracts to wards
## Annual census tract data for 2006-2023 with proper tract boundary handling (2010 and 2020 changes)

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
# 2. DOWNLOAD ANNUAL CENSUS TRACT DATA
# -----------------------------------------------------------------------------

# Function to download annual tract data
get_tract_data <- function(year_to_get) {
  # For 2006-2008, use 2009 ACS 5-year (earliest available)
  # For 2009+, use actual year
  acs_year <- max(year_to_get, 2009)
  
  message("  - Downloading tract data for year ", year_to_get, " (using ACS ", acs_year, ")")
  
  get_acs(
    geography = "tract",
    variables = acs_vars,
    state = "IL",
    county = "Cook",
    year = acs_year,
    survey = "acs5",
    output = "wide"
  ) %>%
    st_drop_geometry() %>%
    select(GEOID, ends_with("E")) %>%
    rename_with(~ sub("E$", "", .), .cols = everything()) %>%
    mutate(year = year_to_get)  # Assign the target year
}

# Download data for all years
message("Downloading annual census tract data...")
all_tract_data <- map_dfr(2006:2023, get_tract_data)

message("Downloaded tract data for ", length(unique(all_tract_data$year)), " years")
message("Total tract-year observations: ", nrow(all_tract_data))

# -----------------------------------------------------------------------------
# 3. GET TRACT GEOMETRIES FOR ALL THREE PERIODS
# -----------------------------------------------------------------------------

message("Getting census tract geometries for all vintages...")

# Get pre-2010 tract geometries (2000-2009)
tract_geom_2000 <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "IL",
  county = "Cook",
  year = 2009,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(GEOID, geometry) %>%
  mutate(tract_vintage = "2000")

# Get 2010 tract geometries (2010-2019)
tract_geom_2010 <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "IL",
  county = "Cook",
  year = 2015,  # Use mid-period year for stability
  survey = "acs5",
  geometry = TRUE
) %>%
  select(GEOID, geometry) %>%
  mutate(tract_vintage = "2010")

# Get 2020 tract geometries (2020-2023)
tract_geom_2020 <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "IL",
  county = "Cook",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%
  select(GEOID, geometry) %>%
  mutate(tract_vintage = "2020")

message("Got geometries for:")
message("- 2000 vintage (2006-2009): ", nrow(tract_geom_2000), " census tracts")
message("- 2010 vintage (2010-2019): ", nrow(tract_geom_2010), " census tracts")
message("- 2020 vintage (2020-2023): ", nrow(tract_geom_2020), " census tracts")

# -----------------------------------------------------------------------------
# 4. PROCESS TRACT DATA WITH APPROPRIATE BOUNDARIES
# -----------------------------------------------------------------------------

# Function to assign tract geometries based on year
assign_tract_geometries <- function(tract_data_year) {
  year_val <- unique(tract_data_year$year)
  
  if (year_val <= 2009) {
    # Use 2000 vintage boundaries
    tract_data_year %>%
      left_join(tract_geom_2000 %>% select(GEOID, geometry), by = "GEOID") %>%
      filter(!is.na(geometry)) %>%
      mutate(tract_vintage_used = "2000")
  } else if (year_val <= 2019) {
    # Use 2010 vintage boundaries
    tract_data_year %>%
      left_join(tract_geom_2010 %>% select(GEOID, geometry), by = "GEOID") %>%
      filter(!is.na(geometry)) %>%
      mutate(tract_vintage_used = "2010")
  } else {
    # Use 2020 vintage boundaries
    tract_data_year %>%
      left_join(tract_geom_2020 %>% select(GEOID, geometry), by = "GEOID") %>%
      filter(!is.na(geometry)) %>%
      mutate(tract_vintage_used = "2020")
  }
}

# Process all data with appropriate geometries
tract_data_with_geom <- all_tract_data %>%
  group_by(year) %>%
  group_split() %>%
  map_dfr(assign_tract_geometries) %>%
  st_as_sf() %>%
  mutate(
    # Calculate derived variables
    land_area_sqkm = as.numeric(st_area(.)) / 1e6,
    homeownership_rate = ifelse(total_units > 0, owner_occupied / total_units, NA),
    population_density = ifelse(land_area_sqkm > 0, total_population / land_area_sqkm, NA),
    percent_black = ifelse(total_population > 0, black_population / total_population, NA),
    percent_hispanic = ifelse(total_population > 0, hispanic_population / total_population, NA)
  ) %>%
  # Filter out problematic tracts
  filter(!is.na(total_population), total_population > 0) %>%
  # Keep only what we need
  select(
    GEOID, year, tract_vintage_used, total_population, homeownership_rate, 
    population_density, median_income, percent_black, 
    percent_hispanic, avg_household_size, geometry
  )

message("Processed tract data: ", nrow(tract_data_with_geom), " tract-year observations")

# Check tract vintage usage
vintage_summary <- tract_data_with_geom %>%
  st_drop_geometry() %>%
  group_by(year, tract_vintage_used) %>%
  summarise(n_tracts = n(), .groups = "drop")

message("\nTract vintage usage by year:")
print(vintage_summary)

# -----------------------------------------------------------------------------
# 5. SPATIAL JOIN TRACTS TO WARDS BY YEAR
# -----------------------------------------------------------------------------

ward_controls_list <- list()

for (year_i in 2006:2023) {
  message("Processing spatial join for year ", year_i)
  
  # Get tract data for this year
  year_tract_data <- tract_data_with_geom %>% 
    filter(year == year_i)
  
  if (nrow(year_tract_data) == 0) {
    message("  - No tract data found for year ", year_i, ", skipping")
    next
  }
  
  # Get ward geometries for this year - use available years or extend
  if (year_i %in% unique(ward_panel$year)) {
    year_wards <- ward_panel %>%
      filter(year == year_i) %>%
      select(ward, year) %>%
      group_by(ward, year) %>%
      summarise(.groups = "drop")
  } else {
    # For years not in ward_panel, use the nearest available year
    available_years <- unique(ward_panel$year)
    nearest_year <- available_years[which.min(abs(available_years - year_i))]
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
  if (st_crs(year_tract_data) != st_crs(ward_panel)) {
    message("  - Transforming CRS for compatibility")
    year_tract_data <- st_transform(year_tract_data, st_crs(ward_panel))
  }
  
  # Spatial join using tract centroids
  tract_centroids <- st_centroid(year_tract_data)
  
  tract_ward_joined <- st_join(tract_centroids, year_wards, join = st_within) %>%
    filter(!is.na(ward)) %>%  # Keep only tracts that fall within a ward
    st_drop_geometry() %>% 
    select(-year.y) %>% 
    rename(year = year.x)
  
  tract_vintage <- unique(year_tract_data$tract_vintage_used)[1]
  message("  - Successfully joined ", nrow(tract_ward_joined), " tracts to wards (using ", tract_vintage, " vintage)")
  
  # Aggregate to ward level using population weighting
  ward_year_controls <- tract_ward_joined %>%
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
      n_tracts = n_distinct(GEOID),
      tract_vintage_used = first(tract_vintage_used),
      
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

# Check coverage - we should have 50 wards per year
coverage_check <- ward_controls %>%
  group_by(year) %>%
  summarise(
    wards_with_data = n_distinct(ward),
    missing_wards = list(setdiff(1:50, unique(ward))),
    avg_tracts_per_ward = mean(n_tracts, na.rm = TRUE),
    total_population = sum(total_population_ward, na.rm = TRUE),
    tract_vintage = first(tract_vintage_used),
    .groups = "drop"
  ) %>%
  mutate(missing_wards = map_chr(missing_wards, ~paste(.x, collapse = ", ")))

message("\nCoverage by year:")
print(coverage_check)

# Check for tract boundary changes around 2010 and 2020
tract_count_by_year <- ward_controls %>%
  group_by(year, tract_vintage_used) %>%
  summarise(
    total_tracts = sum(n_tracts, na.rm = TRUE),
    .groups = "drop"
  )

message("\nTotal tracts by year and vintage (should see changes in 2010 and 2020):")
print(tract_count_by_year)

# Summary statistics
ward_summary_stats <- ward_controls %>%
  summarise(
    across(c(homeownership_rate, population_density, median_income,
             percent_black, percent_hispanic, avg_household_size),
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE),
                min = ~min(., na.rm = TRUE),
                max = ~max(., na.rm = TRUE),
                missing = ~sum(is.na(.))),
           .names = "{.col}_{.fn}")
  )

# Check for any wards with very low population (might indicate problems)
low_pop_wards <- ward_controls %>%
  filter(total_population_ward < 1000) %>%
  select(ward, year, total_population_ward, n_tracts, tract_vintage_used)

if (nrow(low_pop_wards) > 0) {
  message("\nWarning: Found wards with very low population:")
  print(low_pop_wards)
}

# -----------------------------------------------------------------------------
# 7. SAVE OUTPUT
# -----------------------------------------------------------------------------

# Main ward controls file
write_csv(ward_controls, "../output/ward_controls.csv")

message("\nFiles saved:")
message("- ../output/ward_controls.csv")

message("\nScript completed successfully!")
