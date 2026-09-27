# setwd("tasks/estimate_alderman_permit_effects/code")
# Block-year panel for every 2010 census block in Chicago, 2006-2022: permit applications by year of application and
# the alderman representing the block that year.
first_year <- 2006L
last_year <- 2022L
# Blocks follow the 2003 ward map through 2014 and the 2015 map from 2015 on; the 2015 map took effect on May 18,
# 2015, so it was in force for most of that year. Each ward-year's alderman is the one who served it the most days.
first_year_on_2015_map <- 2015L

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

blocks <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE,
  col_types = cols(GEOID10 = col_character(), .default = col_guess())) |>
  transmute(block_id = GEOID10, geometry = the_geom) |>
  distinct(block_id, .keep_all = TRUE) |>
  st_as_sf(wkt = "geometry", crs = 4269) |>
  st_make_valid() |>
  st_transform(3435)

# Permits, excluding cancelled, revoked and suspended ones, placed in the block containing them. Permits outside every
# block have a hand-reviewed decision (tasks/permit_block_hand_adjudications); all current decisions drop them.
permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE, query = paste(
    "SELECT id, permit_type, high_discretion, application_start_date_ym, geom FROM building_permits_clean",
    "WHERE permit_status NOT IN ('CANCELLED', 'REVOKED', 'SUSPENDED') OR permit_status IS NULL")) |>
  mutate(id = as.character(id), year = as.integer(format(as.Date(application_start_date_ym), "%Y"))) |>
  filter(year >= first_year, year <= last_year) |>
  st_transform(3435)
permit_blocks <- st_join(permits, blocks, join = st_within) |> st_drop_geometry()
stopifnot(!anyDuplicated(permit_blocks$id))
decisions <- read_csv("../input/manual_permit_block_assignments.csv", show_col_types = FALSE,
  col_types = cols(.default = col_character())) |>
  filter(block_vintage == "2010")
stopifnot(!anyDuplicated(decisions$id), all(decisions$reviewed_block_id %in% c(NA, blocks$block_id)),
  all(permit_blocks$id[is.na(permit_blocks$block_id)] %in% decisions$id))
permit_blocks <- permit_blocks |>
  left_join(select(decisions, id, reviewed_block_id), by = "id", relationship = "many-to-one") |>
  mutate(block_id = coalesce(block_id, reviewed_block_id)) |>
  filter(!is.na(block_id))
permit_counts <- permit_blocks |>
  summarise(n_high_discretion = sum(high_discretion == 1),
    n_low_discretion_nosigns = sum(high_discretion == 0 & permit_type != "PERMIT - SIGNS"),
    .by = c(block_id, year))

# Each block's ward under the 2003 and 2015 maps (the ward covering most of its area, tasks/create_block_treatment_panel).
block_wards <- read_csv("../input/block_treatment_pre_scores.csv", show_col_types = FALSE,
  col_types = cols(block_id = col_character(), .default = col_guess())) |>
  transmute(block_id, ward_2003_map = ward_origin, ward_2015_map = ward_dest)
stopifnot(!anyDuplicated(block_wards$block_id), setequal(block_wards$block_id, blocks$block_id),
  !anyNA(block_wards$ward_2003_map), !anyNA(block_wards$ward_2015_map))

# Days each alderman served each ward in each year.
terms <- read_csv("../input/chicago_alderman_terms.csv", show_col_types = FALSE)
days_served <- tidyr::expand_grid(terms, year = first_year:last_year) |>
  mutate(days = as.integer(pmin(end_date, as.Date(sprintf("%d-12-31", year))) -
    pmax(start_date, as.Date(sprintf("%d-01-01", year)))) + 1L) |>
  filter(days > 0)
serving <- days_served |>
  slice_max(days, n = 1, with_ties = FALSE, by = c(ward, year)) |>
  select(ward, year, alderman, days_served = days)
stopifnot(nrow(serving) == 50L * length(first_year:last_year))

panel <- tidyr::expand_grid(block_wards, year = first_year:last_year) |>
  mutate(ward = if_else(year < first_year_on_2015_map, ward_2003_map, ward_2015_map)) |>
  left_join(serving, by = c("ward", "year"), relationship = "many-to-one") |>
  left_join(permit_counts, by = c("block_id", "year"), relationship = "one-to-one") |>
  mutate(n_high_discretion = coalesce(n_high_discretion, 0L),
    n_low_discretion_nosigns = coalesce(n_low_discretion_nosigns, 0L))
stopifnot(!anyNA(panel$alderman), sum(panel$n_high_discretion) == sum(permit_counts$n_high_discretion))

SaveData(panel, c("block_id", "year"), "../output/block_alderman_year_panel.parquet")
