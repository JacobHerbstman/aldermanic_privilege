# setwd("tasks/new_construction_analysis_data/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")

boundary_window_ft <- 1500  # buildings this close to a ward-pair boundary enter the analysis

# New residential buildings from the permit-based construction data, near a ward boundary.
buildings <- read_csv("../input/permit_construction.csv", col_types = cols(building_id = "c", permit_number = "c",
    member_permit_numbers = "c", record_ids = "c", ward_pair = "c", construction_date = "D", .default = col_guess())) |>
  filter(within_1500ft) |>
  rename(density_far = far, density_dupac = dupac)
stopifnot(!anyDuplicated(buildings$building_id))

# City permit row ids of every permit behind each building, for scores estimated without them.
permit_ids <- read_csv("../input/construction_permits.csv", col_types = cols(.default = col_character()),
  col_select = c(permit_id, permit_number))
stopifnot(!anyDuplicated(permit_ids$permit_number))
building_permits <- buildings |> filter(!is.na(member_permit_numbers)) |> select(building_id, member_permit_numbers) |>
  separate_longer_delim(member_permit_numbers, "/") |>
  inner_join(permit_ids, by = c("member_permit_numbers" = "permit_number"), relationship = "many-to-one") |>
  group_by(building_id) |> summarise(permit_ids = paste(sort(permit_id), collapse = "/"), .groups = "drop")
buildings <- buildings |> left_join(building_permits, by = "building_id", relationship = "one-to-one")

# Nearest boundary segment of the building's ward pair.
points <- st_as_sf(buildings, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)
segments <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg", eras = sort(unique(buildings$era)))
buildings$segment_id <- assign_points_to_nearest_segments(points, buildings$era, buildings$ward_pair, segments,
  max_distance = units::set_units(boundary_window_ft, "ft"))
assert_event_segment_contract(points, buildings$era, buildings$ward_pair, segments, buildings$segment_id,
  buildings$distance_to_boundary_ft * 0.3048, max_distance_m = boundary_window_ft * 0.3048,
  analysis_window_m = boundary_window_ft * 0.3048, context = "new construction")

# Aldermen serving on each side on the construction date, their uncertainty scores, and ward controls in the
# construction year.
terms <- read_csv("../input/chicago_alderman_terms.csv", show_col_types = FALSE,
  col_types = cols(ward = "i", alderman = "c", start_date = "D", end_date = "D"))
term_overlap <- terms |> arrange(ward, start_date) |> group_by(ward) |> mutate(next_start = lead(start_date)) |> ungroup() |>
  filter(!is.na(next_start), next_start <= end_date)
stopifnot(!anyNA(terms), all(terms$start_date <= terms$end_date), nrow(term_overlap) == 0)
controls <- read_csv("../input/ward_controls_2006_2022.csv", show_col_types = FALSE) |>
  select(ward, year, share_white, share_black, median_hh_income, share_bach_plus, homeownership_rate)
scores <- read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE) |>
  select(alderman, score = uncertainty_index)
stopifnot(!anyDuplicated(controls[c("ward", "year")]), !anyDuplicated(scores$alderman))
buildings <- buildings |>
  left_join(terms, by = join_by(ward, construction_date >= start_date, construction_date <= end_date),
    relationship = "many-to-one") |>
  rename(alderman_own = alderman, own_term_start = start_date) |> select(-end_date) |>
  left_join(terms |> rename(alderman_neighbor = alderman, neighbor_term_start = start_date),
    by = join_by(neighbor_ward == ward, construction_date >= neighbor_term_start, construction_date <= end_date),
    relationship = "many-to-one") |> select(-end_date) |>
  left_join(scores |> rename(alderman_own = alderman, strictness_own = score), by = "alderman_own", relationship = "many-to-one") |>
  left_join(scores |> rename(alderman_neighbor = alderman, strictness_neighbor = score), by = "alderman_neighbor",
    relationship = "many-to-one") |>
  left_join(controls, by = c("ward", "construction_year" = "year"), relationship = "many-to-one") |>
  left_join(controls, by = c("neighbor_ward" = "ward", "construction_year" = "year"), suffix = c("_own", "_neighbor"),
    relationship = "many-to-one") |>
  mutate(alderman_assignment_status = case_when(
      is.na(alderman_own) | is.na(alderman_neighbor) ~ "no_recorded_term_on_construction_date",
      !is.finite(strictness_own) | !is.finite(strictness_neighbor) ~ "serving_alderman_without_score",
      strictness_own == strictness_neighbor ~ "equal_scores",
      TRUE ~ "assigned"),
    signed_distance_m = if_else(alderman_assignment_status == "assigned",
      distance_to_boundary_ft * 0.3048 * sign(strictness_own - strictness_neighbor), NA_real_),
    # Joint service: one ward pair (within a map era) while the same two aldermen serve it, each term being one
    # alderman's continuous tenure in the ward.
    own_term = paste(alderman_own, own_term_start), neighbor_term = paste(alderman_neighbor, neighbor_term_start),
    joint_service = if_else(is.na(alderman_own) | is.na(alderman_neighbor), NA_character_,
      paste(era, ward_pair, pmin(own_term, neighbor_term), pmax(own_term, neighbor_term), sep = ":"))) |>
  select(-own_term_start, -neighbor_term_start, -own_term, -neighbor_term)
for (field in c("share_white_own", "share_black_own", "median_hh_income_own", "share_bach_plus_own", "homeownership_rate_own")) {
  stopifnot(all(is.finite(buildings[[field]])))
}

# Zoning in effect at construction, from the official maps: the current polygon when its last amendment precedes
# construction, and otherwise the latest snapshot before construction (the reconstructed 2006 map through 2012,
# then the 2012, 2014 and 2016 maps). Where the 2006 reconstruction has no polygon, the 2012 map applies if the 2012,
# 2014 and 2016 maps agree.
zone_group <- function(code) {
  code <- str_to_upper(code)
  case_when(
    is.na(code) | str_trim(code) == "" ~ NA_character_,
    str_detect(code, "^RS-?") ~ "Single-Family Residential",
    str_detect(code, "^(RT|RM)-?") ~ "Multi-Family Residential",
    str_detect(code, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(code, "^C-?[1-7]-") ~ "Commercial",
    str_detect(code, "^M-?[1-7]-") ~ "Industrial",
    str_detect(code, "^(DX|DR|DS|DC)-") ~ "Downtown",
    str_starts(code, "PD") ~ "Planned Development",
    str_starts(code, "PMD") ~ "Planned Manufacturing",
    str_starts(code, "POS") ~ "Open Space",
    TRUE ~ "Other")
}
zoned <- st_as_sf(buildings |> select(building_id, x_3435, y_3435), coords = c("x_3435", "y_3435"), crs = 3435) |>
  st_join(st_read("../input/historical_zoning_2006_candidate.gpkg", quiet = TRUE) |>
    transmute(group_2006 = candidate_zone_group_2006) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp", quiet = TRUE) |>
    transmute(group_2012 = zone_group(ZONE_CLASS)) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("/vsizip/../input/zoning_sep2014.zip/Zoning.shp", quiet = TRUE) |>
    transmute(group_2014 = zone_group(ZONE_CLASS)) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp", quiet = TRUE) |>
    transmute(group_2016 = zone_group(ZONE_CLASS)) |> st_transform(3435), largest = TRUE) |>
  st_join(st_read("../input/zoning_sep2025.geojson", quiet = TRUE) |>
    transmute(group_2025 = zone_group(zone_class), ordinance_date = as.Date(ordinance_1)) |> st_transform(3435), largest = TRUE) |>
  st_drop_geometry()
stopifnot(!anyDuplicated(zoned$building_id))
buildings <- buildings |> left_join(zoned, by = "building_id", relationship = "one-to-one") |>
  mutate(preceding_group = case_when(construction_year <= 2012 ~ group_2006, construction_year <= 2014 ~ group_2012,
      construction_year == 2015 ~ group_2014, TRUE ~ group_2016),
    zoning_source = case_when(
      !is.na(group_2025) & coalesce(ordinance_date <= construction_date, FALSE) ~ "current_map_amended_before_construction",
      !is.na(preceding_group) ~ "latest_map_before_construction",
      construction_year <= 2012 & group_2012 == group_2014 & group_2014 == group_2016 ~ "later_maps_agree_2006_missing"),
    zone_group = case_when(zoning_source == "current_map_amended_before_construction" ~ group_2025,
      zoning_source == "latest_map_before_construction" ~ preceding_group,
      zoning_source == "later_maps_agree_2006_missing" ~ group_2012)) |>
  select(-starts_with("group_"), -preceding_group, -ordinance_date)
# Every building with a usable dwelling density needs a zoning group.
stopifnot(!any(buildings$allow_dupac & coalesce(buildings$density_dupac > 0, FALSE) & is.na(buildings$zone_group)))

SaveData(buildings |> arrange(construction_year, building_id), "building_id", "../output/new_construction_analysis_data.csv", na = "")
