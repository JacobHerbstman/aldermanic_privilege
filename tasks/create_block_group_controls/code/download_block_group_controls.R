# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_block_group_controls/code")
# acs_year <- 2014
# geometry_year <- 2019

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) {
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
)

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


write_csv(block_group_controls, "../temp/acs_block_group_controls.csv")
st_write(block_group_geometry, "../temp/block_group_geometry.gpkg", delete_dsn = TRUE, quiet = TRUE)
stopifnot(file.rename("../temp/acs_block_group_controls.csv", "../output/acs_block_group_controls_current.csv"),
  file.rename("../temp/block_group_geometry.gpkg", "../output/block_group_geometry_current.gpkg"))

ReportData("../output/acs_block_group_controls_current.csv", "GEOID")
ReportData("../output/block_group_geometry_current.gpkg", "GEOID")
