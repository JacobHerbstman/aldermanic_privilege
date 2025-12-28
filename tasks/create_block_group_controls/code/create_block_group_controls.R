## this code creates a panel of block-group level controls from the ACS
## EXPANDED VERSION: includes rents, home values, education, and more demographics

source("../../setup_environment/code/packages.R")

## set census api key if not already done
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# -----------------------------------------------------------------------------
# 1. DEFINE VARIABLES
# -----------------------------------------------------------------------------
acs_vars <- c(
  # Population and Race/Ethnicity
  total_population = "B01003_001",
  white_population = "B03002_003", # White alone, not Hispanic
  black_population = "B03002_004", # Black alone
  hispanic_population = "B03003_003", # Hispanic/Latino

  # Housing Tenure
  total_units = "B25003_001",
  owner_occupied = "B25003_002",
  renter_occupied = "B25003_003",

  # Income
  median_income = "B19013_001",

  # Average Household Size
  avg_household_size = "B25010_001",

  # Median Gross Rent
  median_rent = "B25064_001",

  # Median Home Value (owner-occupied)
  median_home_value = "B25077_001",

  # Education (Population 25+)
  pop_25_plus = "B15003_001", # Total population 25+
  bach_degree = "B15003_022", # Bachelor's degree
  masters_degree = "B15003_023", # Master's degree
  professional_degree = "B15003_024", # Professional school degree
  doctorate_degree = "B15003_025", # Doctorate degree

  # Age Structure
  median_age = "B01002_001"
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
message("Downloading pre-period ACS data (2014 5-year)...")
pre_period_data <- get_bg_data(2014)
message("Downloading post-period ACS data (2019 5-year)...")
post_period_data <- get_bg_data(2019)


# -----------------------------------------------------------------------------
# 3. CREATE THE FULL 2010-2019 CONTROL PANEL
# -----------------------------------------------------------------------------
# Create a template with every block group for every year in the panel
panel_template <- expand.grid(
  GEOID = unique(c(pre_period_data$GEOID, post_period_data$GEOID)),
  year = 2006:2024,
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
message("Getting block group geometries for area calculation...")
block_group_areas <- get_acs(
  geography = "block group", variables = "B01003_001",
  state = "IL", county = "Cook", year = 2019, geometry = TRUE
) %>%
  mutate(land_area_sqkm = as.numeric(st_area(.)) / 1e6) %>%
  st_drop_geometry() %>%
  select(GEOID, land_area_sqkm)

# Calculate the final variables
message("Calculating derived variables...")
bg_controls <- bg_controls_raw %>%
  left_join(block_group_areas, by = "GEOID") %>%
  mutate(
    # Race/Ethnicity shares
    percent_white = white_population / total_population,
    percent_black = black_population / total_population,
    percent_hispanic = hispanic_population / total_population,

    # Housing
    homeownership_rate = owner_occupied / total_units,

    # Education (share with bachelor's or higher)
    bach_plus = bach_degree + masters_degree + professional_degree + doctorate_degree,
    share_bach_plus = bach_plus / pop_25_plus,

    # Density
    population_density = total_population / land_area_sqkm
  ) %>%
  # Keep only the final variables for merging
  select(
    GEOID,
    year,
    # Race/Ethnicity
    percent_white,
    percent_black,
    percent_hispanic,
    # Housing
    homeownership_rate,
    median_rent,
    median_home_value,
    # Income & Education
    median_income,
    share_bach_plus,
    # Demographics
    avg_household_size,
    median_age,
    # Density
    population_density
  )

message(sprintf("Final dataset: %d rows, %d columns", nrow(bg_controls), ncol(bg_controls)))
message(sprintf("Block groups: %d", n_distinct(bg_controls$GEOID)))
message(sprintf("Years: %s to %s", min(bg_controls$year), max(bg_controls$year)))

write_csv(bg_controls, "../output/block_group_controls.csv")
message("Saved to ../output/block_group_controls.csv")
