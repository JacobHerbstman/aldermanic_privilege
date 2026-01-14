## this code generates a simple alderman restrictiveness score based on pop. density, homeonwer rates, and incomes at the ward level

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

## set census api key
census_api_key(Sys.getenv("CENSUS_API_KEY"))

# Use the 2012-2015 ward map for the pre-treatment period
## bring in old ward boundary data
wards_pre_2015 <- st_read("../input/ward_panel.gpkg") %>% 
  filter(year == 2014) %>% 
  select(-year)

# -----------------------------------------------------------------------------
# STEP 1: Get all required block group data from the ACS in one call
# -----------------------------------------------------------------------------
# We'll use the 2014 5-year ACS as a stable pre-treatment measure.
acs_vars <- c(
  total_units = "B25003_001",      # Total Housing Units
  owner_occupied = "B25003_002",  # Owner-Occupied Housing Units
  median_income = "B19013_001",    # Median Household Income
  total_population = "B01003_001" # Total Population
)

acs_data <- get_acs(
  geography = "block group",
  variables = acs_vars,
  state = "IL",
  county = "Cook",
  year = 2014,
  geometry = TRUE
) %>%
  st_transform(st_crs(wards_pre_2015))

# -----------------------------------------------------------------------------
# STEP 2: Spatially allocate data to wards (in two parts)
# -----------------------------------------------------------------------------

# Part A: Interpolate EXTENSIVE (count) data
extensive_vars <- c("total_units", "owner_occupied", "total_population")

acs_data_extensive_wide <- acs_data %>%
  filter(variable %in% extensive_vars) %>%
  # Select only the necessary columns
  select(GEOID, variable, estimate) %>%
  # Pivot to create separate numeric columns for each variable
  tidyr::pivot_wider(names_from = variable, values_from = estimate)

ward_counts <- st_interpolate_aw(
  select(acs_data_extensive_wide, -GEOID),
  wards_pre_2015,
  extensive = TRUE
) 



# Part B: Interpolate INTENSIVE (median) data
intensive_vars <- c("median_income")

acs_data_intensive_wide <- acs_data %>%
  filter(variable %in% intensive_vars) %>%
  # Select only the necessary columns
  select(GEOID, variable, estimate) %>%
  # Pivot to create separate numeric columns for each variable
  tidyr::pivot_wider(names_from = variable, values_from = estimate)

ward_medians <- acs_data_intensive_wide %>%
  select(-GEOID) %>%
  # THIS IS THE FIX: Remove block groups with no median income data before interpolating
  filter(!is.na(median_income)) %>%
  st_interpolate_aw(
    wards_pre_2015,
    extensive = FALSE
  )

ward_counts <- st_join(ward_counts, wards_pre_2015, join = st_equals) %>%
  # st_join adds all columns, so select only what you need to keep it clean
  select(ward, total_units, owner_occupied, total_population)


# Add ward numbers back to ward_medians
ward_medians <- st_join(ward_medians, wards_pre_2015, join = st_equals) %>%
  select(ward, median_income)


# -----------------------------------------------------------------------------
# STEP 3: Calculate final metrics and join everything together
# -----------------------------------------------------------------------------
ward_demographics <- wards_pre_2015 %>%
  mutate(ward_area_sqkm = as.numeric(st_area(.)) / 1e6) %>% # Get ward area in sq km
  st_drop_geometry() %>%
  left_join(ward_counts, by = "ward") %>%
  left_join(ward_medians, by = "ward") %>%
  mutate(
    homeownership_rate = owner_occupied / total_units,
    population_density = total_population / ward_area_sqkm
  ) %>%
  select(ward, homeownership_rate, median_income, population_density)

# -----------------------------------------------------------------------------
# Do PCA to create a restrictiveness score
# -----------------------------------------------------------------------------
pca_data <- ward_demographics %>%
  select(homeownership_rate, median_income, population_density)

pca_result <- prcomp(pca_data, scale. = TRUE)


ward_demographics$restrictiveness_score_pca <- pca_result$x[, 1]
ward_demographics <- ward_demographics %>% 
  mutate(
    restrictiveness_score = as.numeric(scale(homeownership_rate) + scale(median_income) - scale(population_density))
  ) 

ward_demographics <- ward_demographics %>% 
  select(ward, restrictiveness_score_pca, restrictiveness_score)

write_csv(ward_demographics, "../output/alderman_restrictiveness_score_test.csv")

