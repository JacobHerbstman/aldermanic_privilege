# setwd("tasks/construction_historical_coordinates/code")
# start_year <- 2006
# end_year <- 2022
# minimum_area_sqft <- 1
# maximum_distance_ft <- 1500
# exact_match_ft <- 10
# local_match_ft <- 150
# max_building_gap <- 0.10

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/canonical_geometry_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(start_year, end_year, minimum_area_sqft, maximum_distance_ft, exact_match_ft, local_match_ft, max_building_gap)
if (length(args) != 7L) stop("Expected 7 specification arguments from Makefile.")
start_year <- as.integer(args[1])
end_year <- as.integer(args[2])
minimum_area_sqft <- as.numeric(args[3])
maximum_distance_ft <- as.numeric(args[4])
exact_match_ft <- as.numeric(args[5])
local_match_ft <- as.numeric(args[6])
max_building_gap <- as.numeric(args[7])

# Build historical coordinate requests

if (anyNA(c(start_year, end_year, minimum_area_sqft)) || start_year > end_year || minimum_area_sqft < 0) {
  stop("Invalid historical-coordinate request window or area threshold.")
}

residential <- read_csv(
  "../input/residential_discovery_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), class = col_character(), year_built = col_integer(),
    land_sqft = col_double(), building_sqft = col_double(), num_apartments = col_double(),
    single_v_multi_family = col_character(), type_of_residence = col_character(),
    proration_key_pin = col_character(), pin_proration_rate = col_double(),
    .default = col_skip()
  )
) %>%
  mutate(
    residential_single_family =
      (!is.na(single_v_multi_family) &
        str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    unitscount = if_else(
      residential_single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  ) %>%
  transmute(
    pin,
    construction_year = as.integer(year_built),
    arealotsf = as.numeric(land_sqft),
    areabuilding = as.numeric(building_sqft),
    unitscount,
    source = "residential_improvements",
    source_class = class,
    project_key = paste0("residential_", coalesce(na_if(proration_key_pin, ""), pin)),
    coordinate_weight = if_else(is.finite(pin_proration_rate) & pin_proration_rate > 0, pin_proration_rate, 1)
  )

commercial <- read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), pins = col_character(), yearbuilt = col_integer(),
    landsf = col_double(), bldgsf = col_double(), tot_units = col_double(),
    .default = col_skip()
  )
) %>%
  transmute(
    pin,
    construction_year = as.integer(yearbuilt),
    arealotsf = as.numeric(landsf),
    areabuilding = as.numeric(bldgsf),
    unitscount = as.numeric(tot_units),
    source = "commercial_valuation",
    source_class = NA_character_,
    project_key = paste0("commercial_", str_replace_all(coalesce(pins, pin), "[^0-9]", "")),
    coordinate_weight = 1
  )

buildings <- bind_rows(residential, commercial) %>%
  group_by(pin) %>%
  arrange(desc(unitscount), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  filter(
    construction_year >= start_year,
    construction_year <= end_year,
    arealotsf > minimum_area_sqft,
    areabuilding > minimum_area_sqft,
    unitscount > 0
  )

if (anyNA(buildings$pin) || any(!str_detect(buildings$pin, "^[0-9]{14}$")) || anyDuplicated(buildings$pin) > 0) {
  stop("Eligible construction rows are not unique by original building PIN.", call. = FALSE)
}
if (any(buildings$source_class == "299", na.rm = TRUE)) {
  stop("The original building source unexpectedly contains class-299 rows.", call. = FALSE)
}

current_parcels <- read_csv(
  "../input/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), class = col_character(), subdivision_id = col_character(),
    longitude = col_double(), latitude = col_double(),
    centroid_x_crs_3435 = col_double(), centroid_y_crs_3435 = col_double(),
    .default = col_skip()
  )
) %>%
  transmute(
    pin,
    current_parcel_class = class,
    current_subdivision_id = subdivision_id,
    current_longitude = longitude,
    current_latitude = latitude,
    current_coordinates_complete =
      is.finite(longitude) & is.finite(latitude)
  )

if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel universe is not unique by PIN.", call. = FALSE)
}

buildings %>%
  left_join(current_parcels, by = "pin", relationship = "many-to-one") %>%
  mutate(
    current_pin_present = pin %in% current_parcels$pin,
    current_coordinates_complete = coalesce(current_coordinates_complete, FALSE),
    multifamily = unitscount > 1
  ) %>%
  arrange(pin) %>%
  SaveData(key = "pin", outfile = "../output/density_historical_building_universe.csv")

# Build historical coordinates

if (!is.finite(maximum_distance_ft) || maximum_distance_ft <= 0) {
  stop("The maximum boundary distance must be positive.")
}

buildings <- readr::read_csv(
  "../output/density_historical_building_universe.csv",
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess()),
  show_col_types = FALSE
)
historical <- readr::read_csv(
  "../input/density_historical_parcel_records.csv",
  col_types = readr::cols(pin = readr::col_character(), parcel_class = readr::col_character(), .default = readr::col_guess()),
  show_col_types = FALSE
)
address_matches <- readr::read_csv(
  "../input/historical_address_matches.csv",
  col_types = readr::cols(pin = readr::col_character(), matched_pin = readr::col_character(), .default = readr::col_guess()),
  show_col_types = FALSE
)
current <- readr::read_csv(
  "../input/parcel_universe_2025_city.csv",
  col_types = readr::cols(pin = readr::col_character(), longitude = readr::col_double(), latitude = readr::col_double(), .default = readr::col_skip()),
  show_col_types = FALSE
)
if (anyDuplicated(buildings$pin) || anyDuplicated(historical[c("pin", "year")]) ||
    anyDuplicated(address_matches$pin) || anyDuplicated(current$pin)) {
  stop("Historical coordinate inputs must have unique building, PIN-year, and address-decision keys.")
}

parcel_coordinates <- buildings |>
  dplyr::anti_join(
    current |>
      dplyr::filter(is.finite(longitude), is.finite(latitude)),
    by = "pin"
  ) |>
  dplyr::inner_join(historical, by = c("pin", "construction_year" = "year"), relationship = "one-to-one") |>
  dplyr::filter(is.finite(longitude), is.finite(latitude)) |>
  dplyr::transmute(pin, construction_year, longitude, latitude, coordinate_source = "historical_parcel")

if (nrow(dplyr::anti_join(address_matches, buildings, by = c("pin", "construction_year"))) > 0L ||
    any(address_matches$pin %in% parcel_coordinates$pin)) {
  stop("An address decision is outside the building universe or overlaps an exact historical coordinate.")
}
address_coordinates <- address_matches |>
  dplyr::left_join(current, by = c("matched_pin" = "pin"), relationship = "many-to-one") |>
  dplyr::transmute(pin, construction_year, longitude, latitude, coordinate_source = "historical_address")
if (any(!is.finite(address_coordinates$longitude) | !is.finite(address_coordinates$latitude))) {
  stop("A reviewed address match lacks coordinates for its source PIN.")
}

coordinates <- dplyr::bind_rows(parcel_coordinates, address_coordinates) |>
  dplyr::mutate(
    construction_date = as.Date(sprintf("%d-06-15", construction_year)),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )
points <- sf::st_as_sf(coordinates, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  sf::st_transform(3435)
ward_maps <- load_canonical_ward_maps(
  sf::st_read("../input/ward_panel.gpkg", quiet = TRUE),
  eras = sort(unique(coordinates$era))
)
boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = sort(unique(coordinates$era)))
assignments <- assign_points_to_boundaries(points, coordinates$era, ward_maps, boundaries, chunk_n = 2000L)

coordinates <- coordinates |>
  dplyr::mutate(distance_to_boundary_ft = assignments$dist_ft) |>
  dplyr::filter(is.finite(distance_to_boundary_ft), distance_to_boundary_ft <= maximum_distance_ft) |>
  dplyr::select(pin, construction_year, longitude, latitude, coordinate_source) |>
  dplyr::arrange(pin)
if (anyNA(coordinates$pin) || anyDuplicated(coordinates$pin)) {
  stop("Recovered coordinates must be unique by original building PIN.")
}
SaveData(coordinates, character(), "../output/density_historical_coordinate_candidates.csv")

# Build historical project screen

if (any(!is.finite(c(exact_match_ft, local_match_ft, max_building_gap))) ||
    exact_match_ft < 0 || local_match_ft <= exact_match_ft || max_building_gap < 0) {
  stop("Invalid historical project matching thresholds.")
}

buildings <- readr::read_csv(
  "../output/density_historical_building_universe.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(), project_key = readr::col_character(),
    current_subdivision_id = readr::col_character(), .default = readr::col_guess()
  )
)
historical <- readr::read_csv(
  "../input/density_historical_parcel_records.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(), subdivision_id = readr::col_character(),
    .default = readr::col_guess()
  )
)
if (anyNA(buildings$pin) || anyDuplicated(buildings$pin) ||
    anyNA(buildings$project_key) || anyDuplicated(historical[c("pin", "year")])) {
  stop("Discovery buildings and historical PIN-year records must have unique keys.")
}

# Screen missing locations against the current-only universe. Adding recovered
# locations before this comparison would make them match themselves.
lineage <- buildings |>
  dplyr::filter(!current_coordinates_complete) |>
  dplyr::left_join(
    historical |>
      dplyr::select(pin, year, centroid_x_crs_3435, centroid_y_crs_3435, subdivision_id),
    by = c("pin", "construction_year" = "year"),
    relationship = "one-to-one"
  ) |>
  dplyr::arrange(project_key, pin) |>
  dplyr::group_by(project_key) |>
  dplyr::summarise(
    source = dplyr::first(source),
    member_pins = paste(sort(unique(pin)), collapse = ";"),
    member_pin_count = dplyr::n_distinct(pin),
    reported_construction_year = min(construction_year),
    distinct_construction_years = dplyr::n_distinct(construction_year),
    distinct_unit_counts = dplyr::n_distinct(unitscount),
    distinct_building_areas = dplyr::n_distinct(areabuilding),
    unitscount = max(unitscount),
    areabuilding = max(areabuilding),
    exact_coordinate_pin_count = sum(is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)),
    historical_x = {
      located <- is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)
      if (any(located)) weighted.mean(centroid_x_crs_3435[located], coordinate_weight[located]) else NA_real_
    },
    historical_y = {
      located <- is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435)
      if (any(located)) weighted.mean(centroid_y_crs_3435[located], coordinate_weight[located]) else NA_real_
    },
    historical_subdivision_id = {
      values <- sort(unique(stats::na.omit(subdivision_id)))
      if (length(values) == 1L) values else NA_character_
    },
    .groups = "drop"
  ) |>
  dplyr::mutate(
    historical_pin_prefix_8 = substr(member_pins, 1, 8),
    source_group_conflict = distinct_construction_years > 1 |
      distinct_unit_counts > 1 | distinct_building_areas > 1,
    lineage_row = dplyr::row_number()
  )

current <- buildings |>
  dplyr::filter(current_coordinates_complete) |>
  sf::st_as_sf(coords = c("current_longitude", "current_latitude"), crs = 4326) |>
  sf::st_transform(3435)
current_xy <- sf::st_coordinates(current)
current <- current |>
  sf::st_drop_geometry() |>
  dplyr::mutate(current_x = current_xy[, "X"], current_y = current_xy[, "Y"]) |>
  dplyr::arrange(project_key, pin) |>
  dplyr::group_by(project_key) |>
  dplyr::summarise(
    current_member_pins = paste(sort(unique(pin)), collapse = ";"),
    current_construction_year = min(construction_year),
    current_unitscount = max(unitscount),
    current_areabuilding = max(areabuilding),
    current_x = weighted.mean(current_x, coordinate_weight),
    current_y = weighted.mean(current_y, coordinate_weight),
    current_subdivision_id = {
      values <- sort(unique(stats::na.omit(current_subdivision_id)))
      if (length(values) == 1L) values else NA_character_
    },
    current_source_group_conflict = dplyr::n_distinct(construction_year) > 1 |
      dplyr::n_distinct(unitscount) > 1 | dplyr::n_distinct(areabuilding) > 1,
    .groups = "drop"
  ) |>
  dplyr::rename(current_project_key = project_key) |>
  dplyr::mutate(current_pin_prefix_8 = substr(current_member_pins, 1, 8))

same_year_matches <- list()
for (construction_year_i in sort(unique(lineage$reported_construction_year))) {
  historical_rows <- which(
    lineage$reported_construction_year == construction_year_i &
      is.finite(lineage$historical_x) & is.finite(lineage$historical_y)
  )
  current_rows <- which(current$current_construction_year == construction_year_i)
  if (length(historical_rows) == 0L || length(current_rows) == 0L) next
  nearest <- nabor::knn(
    data = as.matrix(current[current_rows, c("current_x", "current_y")]),
    query = as.matrix(lineage[historical_rows, c("historical_x", "historical_y")]),
    k = 1
  )
  same_year_matches[[as.character(construction_year_i)]] <-
    current[current_rows[nearest$nn.idx[, 1]], ] |>
    dplyr::transmute(
      lineage_row = historical_rows,
      same_year_current_member_pins = current_member_pins,
      same_year_current_unitscount = current_unitscount,
      same_year_current_areabuilding = current_areabuilding,
      same_year_current_source_group_conflict = current_source_group_conflict,
      same_year_current_subdivision_id = current_subdivision_id,
      same_year_current_pin_prefix_8 = current_pin_prefix_8,
      same_year_project_distance_ft = nearest$nn.dists[, 1]
    )
}
lineage <- lineage |>
  dplyr::left_join(dplyr::bind_rows(same_year_matches), by = "lineage_row", relationship = "one-to-one")

coordinate_rows <- which(is.finite(lineage$historical_x) & is.finite(lineage$historical_y))
nearest <- nabor::knn(
  data = as.matrix(current[c("current_x", "current_y")]),
  query = as.matrix(lineage[coordinate_rows, c("historical_x", "historical_y")]),
  k = 1
)
nearest_projects <- current[nearest$nn.idx[, 1], ] |>
  dplyr::transmute(
    lineage_row = coordinate_rows,
    nearest_current_project_pins = current_member_pins,
    nearest_current_project_year = current_construction_year,
    nearest_current_project_units = current_unitscount,
    nearest_current_project_building_area = current_areabuilding,
    nearest_current_project_distance_ft = nearest$nn.dists[, 1]
  )

lineage <- lineage |>
  dplyr::left_join(nearest_projects, by = "lineage_row", relationship = "one-to-one") |>
  dplyr::mutate(
    same_year_unit_gap = abs(unitscount - same_year_current_unitscount),
    same_year_building_gap = abs(areabuilding - same_year_current_areabuilding) /
      pmax(areabuilding, same_year_current_areabuilding),
    same_year_same_prefix_8 = historical_pin_prefix_8 == same_year_current_pin_prefix_8,
    same_year_same_subdivision = !is.na(historical_subdivision_id) &
      historical_subdivision_id != "" & historical_subdivision_id == same_year_current_subdivision_id,
    successor_match_reason = dplyr::case_when(
      same_year_current_source_group_conflict ~ NA_character_,
      same_year_project_distance_ft <= 1 ~ "same_year_exact_centroid",
      same_year_project_distance_ft <= exact_match_ft & same_year_unit_gap == 0 &
        same_year_building_gap <= max_building_gap ~ "same_year_exact_location_and_attributes",
      same_year_project_distance_ft <= 50 & same_year_unit_gap == 0 &
        same_year_building_gap <= max_building_gap &
        (same_year_same_prefix_8 | same_year_same_subdivision) ~ "same_year_local_identity_and_attributes",
      same_year_project_distance_ft <= local_match_ft & same_year_building_gap <= 0.02 &
        (same_year_same_prefix_8 | same_year_same_subdivision) ~ "same_year_local_identity_and_building_area",
      TRUE ~ NA_character_
    ),
    high_confidence_successor = !is.na(successor_match_reason),
    probable_successor = !high_confidence_successor & !same_year_current_source_group_conflict &
      same_year_project_distance_ft <= local_match_ft & same_year_building_gap <= max_building_gap &
      (same_year_unit_gap == 0 | same_year_same_prefix_8 | same_year_same_subdivision),
    nearby_different_year_project = nearest_current_project_distance_ft <= 50 &
      abs(reported_construction_year - nearest_current_project_year) <= 2,
    lineage_status = dplyr::case_when(
      exact_coordinate_pin_count == 0 ~ "unresolved_no_exact_historical_coordinate",
      source_group_conflict ~ "unresolved_source_group_conflict",
      high_confidence_successor ~ "duplicate_high_confidence",
      probable_successor ~ "duplicate_probable",
      same_year_project_distance_ft <= local_match_ft ~ "unresolved_nearby_same_year_project",
      nearby_different_year_project ~ "unresolved_nearby_different_year_project",
      TRUE ~ "candidate_unique_historical_project"
    ),
    recommended_action = dplyr::case_when(
      lineage_status == "duplicate_high_confidence" ~ "exclude_as_already_represented",
      lineage_status == "candidate_unique_historical_project" ~ "candidate_for_recovery",
      TRUE ~ "hold_out_pending_resolution"
    )
  ) |>
  dplyr::select(-lineage_row) |>
  dplyr::arrange(project_key)
SaveData(lineage, character(), "../output/density_project_lineage.csv")
