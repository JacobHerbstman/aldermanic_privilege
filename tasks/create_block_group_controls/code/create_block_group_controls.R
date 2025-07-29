## this code creates a panel of block-group level controls from the ACS

source("../../setup_environment/code/packages.R")

## set census api key if not already done
census_api_key(Sys.getenv("CENSUS_API_KEY"))

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
  
  # NEW: Average Household Size
  avg_household_size = "B25010_001"
)

# -----------------------------------------------------------------------------
# 2. DOWNLOAD ACS DATA FOR PRE- and POST-PERIODS
# -----------------------------------------------------------------------------
# Function to download and clean ACS data for a single year
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

# Download the pre-period (2009-2013) and post-period (2015-2019) data
pre_period_data <- get_bg_data(2014)
post_period_data <- get_bg_data(2019)


# -----------------------------------------------------------------------------
# 3. CREATE THE FULL 2010-2019 CONTROL PANEL
# -----------------------------------------------------------------------------
# Create a template with every block group for every year in the panel
panel_template <- expand.grid(
  GEOID = unique(c(pre_period_data$GEOID, post_period_data$GEOID)),
  year = 2010:2019,
  stringsAsFactors = FALSE
) %>%
  # Create a period key to join on
  mutate(period = if_else(year < 2015, "pre", "post"))

# Combine the pre- and post- data into one file with the same period key
combined_acs_data <- bind_rows(
  pre_period_data %>% mutate(period = "pre"),
  post_period_data %>% mutate(period = "post")
)

# Join the ACS data to the panel template
bg_controls_raw <- panel_template %>%
  left_join(combined_acs_data, by = c("GEOID", "period"))

# -----------------------------------------------------------------------------
# 4. CALCULATE FINAL CONTROL VARIABLES
# -----------------------------------------------------------------------------
# Population density requires area, which is time-invariant
block_group_areas <- get_acs(
  geography = "block group", variables = "B01003_001",
  state = "IL", county = "Cook", year = 2019, geometry = TRUE
) %>%
  mutate(land_area_sqkm = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  select(GEOID, land_area_sqkm)

# Calculate the final variables
bg_controls <- bg_controls_raw %>%
  left_join(block_group_areas, by = "GEOID") %>%
  mutate(
    homeownership_rate = owner_occupied / total_units,
    population_density = total_population / land_area_sqkm,
    percent_black = black_population / total_population,
    percent_hispanic = hispanic_population / total_population
  ) %>%
  # Keep only the final variables for merging
  select(
    GEOID,
    year,
    homeownership_rate,
    population_density,
    median_income,
    percent_black,
    percent_hispanic,
    avg_household_size # NEW: Keep the household size variable
  )


write_csv(bg_controls, "../output/block_group_controls.csv")
