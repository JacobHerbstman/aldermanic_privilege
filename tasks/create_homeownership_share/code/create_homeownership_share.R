## This code creates a Block Group level panel (2010-2025)
## It fetches ACS demographics annually and spatially joins them to the correct Ward for each year.

source("../../setup_environment/code/packages.R")

# 1. SETUP & INPUTS
# -----------------------------------------------------------------------------
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Load the Ward Panel and ensure CRS is correct
ward_panel <- st_read("../input/ward_panel.gpkg") %>%
  st_transform(3435)

# Define ACS Variables
acs_vars <- c(
  total_population = "B01003_001",
  total_units      = "B25003_001",
  owner_occupied   = "B25003_002",
  median_income    = "B19013_001"
)

# 2. PRE-FETCH GEOMETRIES (The Fix)
# -----------------------------------------------------------------------------
# Instead of downloading shapes every loop (which caused your error),
# we download the "Master Map" for each Census era once.

message("Fetching 2010-era Block Group Shapes (using 2019)...")
bg_geo_2010 <- get_acs(
  geography = "block group", variables = "B01003_001", 
  state = "IL", county = "Cook", year = 2019, geometry = TRUE
) %>% 
  select(GEOID, geometry) %>% 
  st_transform(3435)

message("Fetching 2020-era Block Group Shapes (using 2022)...")
bg_geo_2020 <- get_acs(
  geography = "block group", variables = "B01003_001", 
  state = "IL", county = "Cook", year = 2022, geometry = TRUE
) %>% 
  select(GEOID, geometry) %>% 
  st_transform(3435)


# 3. THE "GRAND LOOP"
# -----------------------------------------------------------------------------
years_to_process <- 2013:2023
LATEST_ACS_YEAR <- 2023 # Update this as needed
final_panel_list <- list()

message(glue("Starting Annual Fetch & Join for years {min(years_to_process)} to {max(years_to_process)}..."))

for (y in years_to_process) {
  
  # --- A. Determine Data Year ---
  fetch_year <- min(y, LATEST_ACS_YEAR)
  message(glue("Panel Year: {y} | Using ACS Data from: {fetch_year}"))
  
  # --- B. Fetch Data Only (No Geometry) ---
  # This is fast and won't crash on shapefile errors
  current_data <- get_acs(
    geography = "block group",
    variables = acs_vars,
    state = "IL",
    county = "Cook",
    year = fetch_year, 
    survey = "acs5",
    geometry = FALSE  # <--- IMPORTANT: Data only!
  ) %>%
    select(GEOID, variable, estimate) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    mutate(
      homeownership_share = owner_occupied / total_units    
      )
  
  # --- C. Join to Correct Geography ---
  if (fetch_year < 2020) {
    # Use 2010 shapes for 2010-2019 data
    current_bgs <- left_join(bg_geo_2010, current_data, by = "GEOID")
  } else {
    # Use 2020 shapes for 2020+ data
    current_bgs <- left_join(bg_geo_2020, current_data, by = "GEOID")
  }
  
  # --- D. Select the Correct Ward Map ---
  current_wards <- ward_panel %>% filter(year == y)
  
  if (nrow(current_wards) == 0) {
    warning(glue("No Ward boundaries found for year {y}! Skipping..."))
    next
  }
  
  # --- E. Spatial Join (Centroids) ---
  bg_centroids <- st_centroid(current_bgs)
  
  joined_data <- st_join(bg_centroids, current_wards, join = st_within) %>%
    st_drop_geometry() %>%
    mutate(data_year = y) %>%
    select(
      GEOID,
      year = data_year,
      ward,
      homeownership_share,
      median_income,
      total_population,
      total_units
    )
  
  final_panel_list[[as.character(y)]] <- joined_data
}

# 4. COMBINE AND SAVE
# -----------------------------------------------------------------------------
final_bg_panel <- bind_rows(final_panel_list) %>%
  filter(!is.na(ward))

ward_year_panel <- final_bg_panel %>%
  group_by(ward, year) %>%
  summarize(
    # 1. Reconstruct the raw counts to get the true rate
    # (We sum the owner-occupied units and divide by the sum of total units)
    ward_total_units = sum(total_units, na.rm = TRUE),
    ward_owner_units = sum(total_units * homeownership_share, na.rm = TRUE),
    
    # 2. Calculate the Ward-Level Homeownership Rate
    ward_homeownership_rate = ward_owner_units / ward_total_units,
    
    # 3. Aggregate other useful controls (weighted by population/units)
    ward_total_population = sum(total_population, na.rm = TRUE),
    ward_median_income = weighted.mean(median_income, w = total_units, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  filter(ward_total_units > 0)

write_csv(final_bg_panel, "../output/ward_year_panel.csv")
