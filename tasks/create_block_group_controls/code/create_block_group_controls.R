# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_block_group_controls/code")
# acs_year <- 2014
# geometry_year <- 2019

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(acs_year, geometry_year)
}
if (length(cli_args) != 2) {
  stop("Script requires the ACS year and geometry year.", call. = FALSE)
}

acs_year <- as.integer(cli_args[1])
geometry_year <- as.integer(cli_args[2])
if (any(!is.finite(c(acs_year, geometry_year)))) {
  stop("ACS and geometry years must be integers.", call. = FALSE)
}

if (Sys.getenv("CENSUS_API_KEY") == "") {
  stop("CENSUS_API_KEY not found in the environment.", call. = FALSE)
}
census_api_key(Sys.getenv("CENSUS_API_KEY"))

acs_vars <- c(
  total_population = "B01003_001",
  white_population = "B03002_003",
  black_population = "B03002_004",
  hispanic_population = "B03003_003",
  total_units = "B25003_001",
  owner_occupied = "B25003_002",
  median_income = "B19013_001",
  avg_household_size = "B25010_001",
  median_rent = "B25064_001",
  median_home_value = "B25077_001",
  pop_25_plus = "B15003_001",
  bach_degree = "B15003_022",
  masters_degree = "B15003_023",
  professional_degree = "B15003_024",
  doctorate_degree = "B15003_025",
  median_age = "B01002_001"
)

message(sprintf("Downloading %d ACS 5-year block-group controls...", acs_year))
block_group_controls <- get_acs(
  geography = "block group",
  variables = acs_vars,
  state = "IL",
  county = "Cook",
  year = acs_year,
  survey = "acs5",
  output = "wide"
) %>%
  st_drop_geometry() %>%
  select(GEOID, ends_with("E")) %>%
  rename_with(~ sub("E$", "", .), .cols = everything())

message(sprintf("Downloading %d block-group geometry...", geometry_year))
block_group_geometry <- get_acs(
  geography = "block group",
  variables = "B01003_001",
  state = "IL",
  county = "Cook",
  year = geometry_year,
  geometry = TRUE
) %>%
  select(GEOID, geometry)

if (nrow(block_group_geometry) == 0) {
  stop("No block-group geometries retrieved.", call. = FALSE)
}
block_group_geometry <- block_group_geometry %>%
  filter(!st_is_empty(geometry))
if (
  nrow(block_group_geometry) == 0 ||
    any(is.na(block_group_geometry$GEOID) | block_group_geometry$GEOID == "") ||
    anyDuplicated(block_group_geometry$GEOID) > 0
) {
  stop("Block-group geometry identifiers must be nonmissing and unique.", call. = FALSE)
}

block_group_areas <- block_group_geometry %>%
  mutate(land_area_sqkm = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  select(GEOID, land_area_sqkm)

block_group_controls <- block_group_controls %>%
  left_join(block_group_areas, by = "GEOID", relationship = "many-to-one") %>%
  mutate(
    year = acs_year,
    percent_white = white_population / total_population,
    percent_black = black_population / total_population,
    percent_hispanic = hispanic_population / total_population,
    homeownership_rate = owner_occupied / total_units,
    bach_plus = bach_degree + masters_degree + professional_degree + doctorate_degree,
    share_bach_plus = bach_plus / pop_25_plus,
    population_density = total_population / land_area_sqkm
  ) %>%
  select(
    GEOID,
    year,
    percent_white,
    percent_black,
    percent_hispanic,
    homeownership_rate,
    median_rent,
    median_home_value,
    median_income,
    share_bach_plus,
    avg_household_size,
    median_age,
    population_density
  )

if (anyDuplicated(block_group_controls$GEOID) > 0) {
  stop("Block-group controls must be unique by GEOID.", call. = FALSE)
}

write_csv(block_group_controls, "../output/block_group_controls.csv")
