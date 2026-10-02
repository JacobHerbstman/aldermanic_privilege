# setwd("tasks/create_ward_controls/code")
# start_year <- 2006
# end_year <- 2022
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(start_year, end_year)
stopifnot(length(cli_args) == 2L)
start_year <- as.integer(cli_args[1]); end_year <- as.integer(cli_args[2])
stopifnot(!anyNA(c(start_year, end_year)), start_year <= end_year)
if (!nzchar(Sys.getenv("CENSUS_API_KEY"))) stop("CENSUS_API_KEY is required for acquisition.")
census_api_key(Sys.getenv("CENSUS_API_KEY"))
options(tigris_use_cache = TRUE)

vars_acs <- c(
  tot_pop       = "B01003_001",
  tot_hhs       = "B11001_001",
  tot_units     = "B25003_001",
  owner_occ     = "B25003_002",
  renter_occ    = "B25003_003",
  pop_white     = "B03002_003",
  pop_black     = "B03002_004",
  pop_hisp      = "B03002_012",
  median_income = "B19013_001",
  pop_25plus    = "B15003_001",
  educ_bach     = "B15003_022",
  educ_mast     = "B15003_023",
  educ_prof     = "B15003_024",
  educ_doc      = "B15003_025"
)

# 2000 Decennial (SF3) - Has Econ Data
vars_2000 <- c(
  tot_pop       = "P001001",
  tot_hhs       = "P010001",
  tot_units     = "H007001",
  owner_occ     = "H007002",
  renter_occ    = "H007003",
  pop_white     = "P007003",
  pop_black     = "P007004",
  pop_hisp      = "P007010",
  median_income = "P053001",
  pop_25plus    = "P037001",
  educ_bach_m   = "P037015",
  educ_mast_m   = "P037016",
  educ_prof_m   = "P037017",
  educ_doc_m    = "P037018",
  educ_bach_f   = "P037032",
  educ_mast_f   = "P037033",
  educ_prof_f   = "P037034",
  educ_doc_f    = "P037035"
)

# 2010 Decennial (SF1) - Counts Only (No Econ)
vars_2010_sf1 <- c(
  tot_pop       = "P001001",
  tot_hhs       = "P018001",
  tot_units     = "H004001",
  owner_mortgage = "H004002",
  owner_free_clear = "H004003",
  renter_occ    = "H004004",
  pop_white     = "P005003",
  pop_black     = "P005004",
  pop_hisp      = "P005010"
)

data_2000_raw <- get_decennial(
  geography = "block group", variables = vars_2000,
  state = "IL", county = "Cook", year = 2000, sumfile = "sf3", geometry = TRUE
)
data_2010_sf1_raw <- get_decennial(
  geography = "block group", variables = vars_2010_sf1,
  state = "IL", county = "Cook", year = 2010, geometry = FALSE
)
geo_2010 <- tigris::block_groups(state = "IL", county = "Cook", year = 2010, cb = FALSE) %>%
  st_transform(3435) %>%
  select(GEOID = GEOID10, geometry)
geo_2020 <- tigris::block_groups(state = "IL", county = "Cook", year = 2020, cb = FALSE) %>%
  st_transform(3435) %>%
  select(GEOID, geometry)
acs <- lapply(sort(unique(c(2013L, seq.int(max(2013L, start_year), max(2013L, end_year))))), function(y) {
  get_acs(geography = "block group", variables = vars_acs, state = "IL", county = "Cook",
          year = y, survey = "acs5", geometry = FALSE) %>% mutate(source_year = y)
}) %>% bind_rows()
stopifnot(!anyDuplicated(acs[c("source_year", "GEOID", "variable")]))
st_write(st_transform(data_2000_raw, 3435), "../temp/census_2000.gpkg", delete_dsn = TRUE, quiet = TRUE)
write_csv(data_2010_sf1_raw, "../temp/census_2010.csv")
write_csv(acs, "../temp/census_acs.csv")
st_write(geo_2010, "../temp/census_block_groups.gpkg", layer = "year2010", delete_dsn = TRUE, quiet = TRUE)
st_write(geo_2020, "../temp/census_block_groups.gpkg", layer = "year2020", quiet = TRUE)
stopifnot(file.rename("../temp/census_2000.gpkg", "../output/census_2000_current.gpkg"),
  file.rename("../temp/census_2010.csv", "../output/census_2010_current.csv"),
  file.rename("../temp/census_acs.csv", "../output/census_acs_current.csv"),
  file.rename("../temp/census_block_groups.gpkg", "../output/census_block_groups_current.gpkg"))

ReportData("../output/census_2000_current.gpkg", c("GEOID", "variable"))
ReportData("../output/census_2010_current.csv", c("GEOID", "variable"))
ReportData("../output/census_acs_current.csv", c("source_year", "GEOID", "variable"))
ReportData("../output/census_block_groups_current.gpkg", "GEOID")
