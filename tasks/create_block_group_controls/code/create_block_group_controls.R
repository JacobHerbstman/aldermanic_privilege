# setwd("tasks/create_block_group_controls/code")
# The recorded sources are 2014 ACS five-year block-group counts and 2019 block-group geography.
acs_year <- 2014L

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

block_group_controls <- read_csv("../output/acs_block_group_controls.csv", show_col_types = FALSE,
  col_types = cols(GEOID = col_character(), .default = col_guess())) %>%
  select(GEOID, ends_with("E")) %>%
  rename_with(~ sub("E$", "", .), .cols = everything())
block_group_geometry <- st_read("../output/block_group_geometry.gpkg", quiet = TRUE) %>% rename(geometry = geom)

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

# Preserve the existing geodesic area denominator for these Census controls.
block_group_areas <- block_group_geometry %>%
  mutate(land_area_sqkm = as.numeric(st_area(geometry)) / 1e6) %>%
  st_drop_geometry() %>%
  select(GEOID, land_area_sqkm)

# A 2014 block group whose GEOID has no 2019 polygon keeps its other controls and a missing population density.
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

SaveData(block_group_controls, c("GEOID"), "../output/block_group_controls.csv")
