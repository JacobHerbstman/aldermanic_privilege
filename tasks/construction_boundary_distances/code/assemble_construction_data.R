# setwd("tasks/construction_boundary_distances/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")


# Build preferred new construction ledger

residential_projects <- readr::read_csv(
  "../output/preferred_residential_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

commercial_projects <- readr::read_csv(
  "../output/preferred_commercial_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    source_project_ids = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

residential_components <- readr::read_csv(
  "../output/preferred_residential_project_components_final.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

commercial_components <- readr::read_csv(
  "../output/preferred_commercial_project_component_locations.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)

residential_boundary <- readr::read_csv(
  "../output/preferred_residential_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

commercial_boundary <- readr::read_csv(
  "../output/preferred_commercial_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
)

additional_decisions <- readr::read_csv(
  "../input/residential_additional_candidate_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    candidate_project_id = readr::col_character(),
    replacement_project_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

stopifnot(!anyDuplicated(residential_projects$project_id),
  !anyDuplicated(commercial_projects$project_id))

# Final geography may withhold an observation, but may not change its measurements.
projects <- readr::read_csv("../input/new_construction_measurements.csv",
  col_types = readr::cols(component_pins = "c", class_values = "c", .default = readr::col_guess()))
located <- bind_rows(
  residential_projects %>% transmute(project_id, construction_year, dwelling_units, building_sqft, land_sqft,
    allow_far, allow_dupac, geometry_source = project_geometry_source,
    geometry_evidence = coalesce(location_reference_row_ids, geometry_source_project_ids),
    x_3435, y_3435, location_resolved),
  commercial_projects %>% transmute(project_id, construction_year, dwelling_units, building_sqft, land_sqft,
    allow_far, allow_dupac, geometry_source = project_geometry_source,
    geometry_evidence = project_geometry_evidence, x_3435, y_3435, location_resolved))
stopifnot(!anyDuplicated(located$project_id), setequal(projects$project_id, located$project_id))
located <- located[match(projects$project_id, located$project_id), ]
for (field in c("construction_year", "dwelling_units", "building_sqft", "land_sqft")) {
  stopifnot(isTRUE(all.equal(projects[[field]], located[[field]], check.attributes = FALSE)))
}
stopifnot(all(!located$allow_far | projects$allow_far), all(!located$allow_dupac | projects$allow_dupac))
projects <- projects %>% select(-allow_far, -allow_dupac) %>%
  left_join(located %>% select(-construction_year, -dwelling_units, -building_sqft, -land_sqft),
    by = "project_id", relationship = "one-to-one") %>%
  mutate(far = if_else(allow_far, far, NA_real_), dupac = if_else(allow_dupac, dupac, NA_real_)) %>%
  select(project_id, source_family, source_project_ids, source_addresses, component_pins, project_kind,
    construction_year, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac,
    membership_source, year_source, units_source, building_source, land_source, evidence_ids,
    decision_reason, confidence, decision_source, decision_action, geometry_source, geometry_evidence,
    x_3435, y_3435, location_resolved, far, dupac, class_values, external_multifamily,
    multifamily_source) %>% arrange(source_family, project_id)

components <- bind_rows(
  residential_components %>%
    left_join(residential_projects %>% select(project_id, project_kind),
      by = "project_id", relationship = "many-to-one") %>%
    transmute(
      project_id,
      source_family = "residential",
      project_kind,
      component_pin
    ),
  commercial_components %>%
    distinct(project_id, component_pin) %>%
    transmute(
      project_id,
      source_family = "commercial",
      project_kind = "commercial_assessor_project",
      component_pin
    )
) %>%
  arrange(source_family, project_id, component_pin)

boundary <- bind_rows(
  residential_boundary %>% mutate(source_family = "residential"),
  commercial_boundary %>% mutate(source_family = "commercial")
) %>%
  select(source_family, everything()) %>%
  arrange(source_family, project_id)

if (anyDuplicated(projects$project_id) > 0 ||
    anyDuplicated(components$component_pin) > 0 ||
    anyDuplicated(components[c("project_id", "component_pin")]) > 0 ||
    anyDuplicated(boundary$project_id) > 0 ||
    !setequal(projects$project_id, components$project_id) ||
    !setequal(projects$project_id, boundary$project_id)) {
  stop("The combined project, component, and boundary ledgers do not reconcile.", call. = FALSE)
}

component_membership <- components %>%
  group_by(project_id) %>%
  summarise(
    reconstructed_component_pins = paste(sort(unique(component_pin)), collapse = "/"),
    .groups = "drop"
  )

project_membership <- projects %>%
  transmute(
    project_id,
    recorded_component_pins = vapply(
      strsplit(component_pins, "/", fixed = TRUE),
      function(x) paste(sort(unique(x)), collapse = "/"),
      character(1)
    )
  ) %>%
  left_join(component_membership, by = "project_id", relationship = "one-to-one")

if (any(project_membership$recorded_component_pins !=
    project_membership$reconstructed_component_pins)) {
  stop("A combined project component list does not match the component ledger.", call. = FALSE)
}

boundary_contract <- boundary %>%
  select(
    project_id,
    boundary_allow_far = allow_far,
    boundary_allow_dupac = allow_dupac
  ) %>%
  inner_join(
    projects %>% select(project_id, allow_far, allow_dupac),
    by = "project_id",
    relationship = "one-to-one"
  )

if (any(boundary_contract$boundary_allow_far != boundary_contract$allow_far) ||
    any(boundary_contract$boundary_allow_dupac != boundary_contract$allow_dupac) ||
    any(boundary$location_resolved & !is.finite(boundary$distance_to_boundary_ft)) ||
    any(boundary$location_resolved & is.na(boundary$ward)) ||
    any(boundary$location_resolved & is.na(boundary$ward_pair))) {
  stop("The combined boundary file violates the project ledger contract.", call. = FALSE)
}

cross_family_replacements <- additional_decisions %>%
  filter(decision == "replace_by_commercial")

if (nrow(cross_family_replacements) != 1 ||
    any(cross_family_replacements$candidate_project_id %in% projects$project_id) ||
    any(!cross_family_replacements$replacement_project_ids %in% projects$project_id) ||
    any(projects$source_family[
      match(cross_family_replacements$replacement_project_ids, projects$project_id)
    ] != "commercial")) {
  stop("The documented residential-to-commercial replacement is not enforced.", call. = FALSE)
}

required_text_columns <- c(
  "project_id",
  "source_family",
  "source_project_ids",
  "component_pins",
  "project_kind",
  "membership_source",
  "year_source",
  "units_source",
  "land_source",
  "decision_reason",
  "decision_source",
  "decision_action"
)

field_failures <- c(
  construction_year = sum(!between(projects$construction_year, 2006L, 2022L)),
  land_area = sum((projects$allow_far | projects$allow_dupac) &
    (!is.finite(projects$land_sqft) | projects$land_sqft <= 0)),
  coordinates = sum(projects$location_resolved &
    (!is.finite(projects$x_3435) | !is.finite(projects$y_3435))),
  unresolved_eligibility = sum(!projects$location_resolved & (projects$allow_far | projects$allow_dupac)),
  location_provenance = sum(projects$location_resolved &
    (is.na(projects$geometry_source) | is.na(projects$geometry_evidence))),
  far_flag = sum(
    projects$allow_far &
      (!is.finite(projects$building_sqft) |
        projects$building_sqft <= 0 |
        projects$land_sqft <= 0)
  ),
  dupac_flag = sum(
    projects$allow_dupac &
      (!is.finite(projects$dwelling_units) |
        projects$dwelling_units <= 0 |
        projects$land_sqft <= 0)
  ),
  missing_provenance = sum(is.na(projects[required_text_columns])),
  blank_provenance = sum(projects[required_text_columns] == "", na.rm = TRUE)
)

if (any(field_failures > 0)) {
  stop(
    "The combined preferred project ledger violates its field contract: ",
    paste(
      names(field_failures)[field_failures > 0],
      field_failures[field_failures > 0],
      sep = "=",
      collapse = ", "
    ),
    call. = FALSE
  )
}

residential_centroids <- sf::st_read(
  "../output/preferred_residential_project_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  mutate(source_family = "residential")
names(residential_centroids)[
  names(residential_centroids) == attr(residential_centroids, "sf_column")
] <- "geometry"
sf::st_geometry(residential_centroids) <- "geometry"

commercial_centroids <- sf::st_read(
  "../output/preferred_commercial_project_centroids.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  mutate(source_family = "commercial")
names(commercial_centroids)[
  names(commercial_centroids) == attr(commercial_centroids, "sf_column")
] <- "geometry"
sf::st_geometry(commercial_centroids) <- "geometry"

centroids <- rbind(
  residential_centroids %>% select(project_id, source_family, geometry),
  commercial_centroids %>% select(project_id, source_family, geometry)
) %>%
  arrange(source_family, project_id)

if (sf::st_crs(centroids)$epsg != 3435 ||
    nrow(centroids) != sum(projects$location_resolved) ||
    anyDuplicated(centroids$project_id) > 0 ||
    !setequal(centroids$project_id, projects$project_id[projects$location_resolved]) ||
    any(sf::st_is_empty(centroids)) ||
    any(!sf::st_is_valid(centroids))) {
  stop("The combined centroid file is incomplete or invalid.", call. = FALSE)
}

boundary_counts <- boundary %>%
  group_by(source_family) %>%
  summarise(
    n_within_1500ft = sum(within_1500ft, na.rm = TRUE),
    n_within_500ft = sum(within_500ft, na.rm = TRUE),
    n_far_within_500ft = sum(within_500ft & allow_far, na.rm = TRUE),
    n_dupac_within_500ft = sum(within_500ft & allow_dupac, na.rm = TRUE),
    .groups = "drop"
  )

if (any(boundary_counts$n_far_within_500ft > boundary_counts$n_within_500ft) ||
    any(boundary_counts$n_dupac_within_500ft > boundary_counts$n_within_500ft) ||
    any(boundary_counts$n_within_500ft > boundary_counts$n_within_1500ft)) {
  stop("Combined boundary eligibility counts exceed their geographic samples.", call. = FALSE)
}

SaveData(projects, c("project_id"), "../output/preferred_new_construction_project_ledger.csv")
SaveData(components, c("project_id", "component_pin"), "../output/preferred_new_construction_project_components.csv")
SaveData(boundary, c("project_id"), "../output/preferred_new_construction_boundary_scope.csv")
SaveData(centroids, c("project_id"), "../output/preferred_new_construction_project_centroids.gpkg", delete_dsn = TRUE, quiet = TRUE)

# Build preferred construction zoning

library(sf)

zone_group <- function(zone_code) {
  zone_code <- stringr::str_to_upper(as.character(zone_code))
  dplyr::case_when(
    is.na(zone_code) | stringr::str_trim(zone_code) == "" ~ NA_character_,
    stringr::str_detect(zone_code, "^RS-?") ~ "Single-Family Residential",
    stringr::str_detect(zone_code, "^(RT|RM)-?") ~ "Multi-Family Residential",
    stringr::str_detect(zone_code, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    stringr::str_detect(zone_code, "^C-?[1-7]-") ~ "Commercial",
    stringr::str_detect(zone_code, "^M-?[1-7]-") ~ "Industrial",
    stringr::str_detect(zone_code, "^(DX|DR|DS|DC)-") ~ "Downtown",
    stringr::str_starts(zone_code, "PD") ~ "Planned Development",
    stringr::str_starts(zone_code, "PMD") ~ "Planned Manufacturing",
    stringr::str_starts(zone_code, "POS") ~ "Open Space",
    TRUE ~ "Other"
  )
}

projects <- readr::read_csv(
  "../output/preferred_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(within_1500ft) |>
  dplyr::select(
    project_id,
    source_family,
    construction_year,
    construction_date,
    within_500ft
  )

components <- readr::read_csv(
  "../output/preferred_new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

points <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    projects,
    by = c("project_id", "source_family"),
    relationship = "one-to-one"
  )

if (sf::st_crs(points)$epsg != 3435) {
  stop("Preferred project centroids must use EPSG:3435.", call. = FALSE)
}
if (anyDuplicated(points$project_id)) {
  stop("Preferred project centroids are not unique by project.", call. = FALSE)
}

validated <- readr::read_csv(
  "../input/historical_zoning_project_construction_year.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character())
) |>
  dplyr::transmute(
    component_pin = pin,
    validated_year = as.integer(construction_year),
    validated_group = construction_zone_group,
    validated_status = construction_zoning_status,
    longitude,
    latitude
  )

if (anyDuplicated(validated$component_pin)) {
  stop("Validated construction-year zoning is not unique by PIN.", call. = FALSE)
}

component_matches <- components |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    validated,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    exact_year = construction_year == validated_year,
    adjacent_year = abs(construction_year - validated_year) <= 1
  ) |>
  dplyr::summarise(
    component_count = dplyr::n(),
    matched_component_count = sum(!is.na(validated_group)),
    exact_component_count = sum(exact_year, na.rm = TRUE),
    adjacent_component_count = sum(adjacent_year, na.rm = TRUE),
    exact_groups = paste(sort(unique(validated_group[exact_year])), collapse = ";"),
    adjacent_groups = paste(sort(unique(validated_group[adjacent_year])), collapse = ";"),
    exact_group_count = dplyr::n_distinct(validated_group[exact_year], na.rm = TRUE),
    adjacent_group_count = dplyr::n_distinct(validated_group[adjacent_year], na.rm = TRUE),
    adjacent_year_min = if (any(adjacent_year, na.rm = TRUE)) min(validated_year[adjacent_year %in% TRUE]) else NA_integer_,
    adjacent_year_max = if (any(adjacent_year, na.rm = TRUE)) max(validated_year[adjacent_year %in% TRUE]) else NA_integer_,
    .by = project_id
  )

zoning_2006 <- sf::st_read(
  "../input/historical_zoning_2006_candidate.gpkg",
  quiet = TRUE
) |>
  dplyr::select(zone_group_2006 = candidate_zone_group_2006)
zoning_2012 <- sf::st_read(
  "/vsizip/../input/zoning_nov2012.zip/Zoning_nov2012.shp",
  quiet = TRUE
) |>
  dplyr::select(zone_code_2012 = ZONE_CLASS)
zoning_2014 <- sf::st_read(
  "/vsizip/../input/zoning_sep2014.zip/Zoning.shp",
  quiet = TRUE
) |>
  dplyr::select(zone_code_2014 = ZONE_CLASS)
zoning_2016 <- sf::st_read(
  "/vsizip/../input/zoning_jan2016.zip/zoning_2016_01.shp",
  quiet = TRUE
) |>
  dplyr::select(zone_code_2016 = ZONE_CLASS)
zoning_2025 <- sf::st_read(
  "../input/zoning_sep2025.geojson",
  quiet = TRUE
) |>
  dplyr::select(
    zone_code_2025 = zone_class,
    ordinance_number_2025 = ordinance,
    ordinance_date_2025 = ordinance_1,
    clerk_document_2025 = clerk_docn
  )

for (object_name in c(
  "zoning_2006",
  "zoning_2012",
  "zoning_2014",
  "zoning_2016",
  "zoning_2025"
)) {
  object <- get(object_name)
  if (sf::st_crs(object) != sf::st_crs(points)) {
    object <- sf::st_transform(object, sf::st_crs(points))
  }
  assign(object_name, object)
}

project_count <- nrow(points)
points <- sf::st_join(points, zoning_2006, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2012, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2014, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2016, left = TRUE, largest = TRUE)
points <- sf::st_join(points, zoning_2025, left = TRUE, largest = TRUE)
if (nrow(points) != project_count) {
  stop("Historical zoning joins changed the preferred-project row count.", call. = FALSE)
}

points <- points |>
  dplyr::mutate(
    zone_group_2012 = zone_group(zone_code_2012),
    zone_group_2014 = zone_group(zone_code_2014),
    zone_group_2016 = zone_group(zone_code_2016),
    zone_group_2025 = zone_group(zone_code_2025),
    construction_date = as.Date(construction_date),
    ordinance_date_2025 = as.Date(ordinance_date_2025),
    current_last_event_preconstruction = !is.na(ordinance_date_2025) &
      ordinance_date_2025 <= construction_date,
    preceding_snapshot_group = dplyr::case_when(
      construction_year <= 2012 ~ zone_group_2006,
      construction_year <= 2014 ~ zone_group_2012,
      construction_year == 2015 ~ zone_group_2014,
      construction_year >= 2016 ~ zone_group_2016,
      TRUE ~ NA_character_
    ),
    early_missing_2006_fallback = dplyr::if_else(
      construction_year <= 2012 &
        is.na(preceding_snapshot_group) &
        zone_group_2012 == zone_group_2014 &
        zone_group_2014 == zone_group_2016 &
        (is.na(ordinance_date_2025) | ordinance_date_2025 > construction_date),
      zone_group_2012,
      NA_character_
    )
  ) |>
  dplyr::left_join(
    component_matches,
    by = "project_id",
    relationship = "one-to-one"
  )

validated_points <- sf::st_as_sf(
  validated,
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) |>
  sf::st_transform(3435)
nearest_index <- sf::st_nearest_feature(points, validated_points)
points$nearest_validated_pin <- validated_points$component_pin[nearest_index]
points$nearest_validated_year <- validated_points$validated_year[nearest_index]
points$nearest_validated_group <- validated_points$validated_group[nearest_index]
points$nearest_validated_distance_ft <- as.numeric(sf::st_distance(
  points,
  validated_points[nearest_index, ],
  by_element = TRUE
))

points <- points |>
  dplyr::mutate(
    stable_interval_group = dplyr::case_when(
      construction_year == 2006 ~ zone_group_2006,
      construction_year <= 2012 & zone_group_2006 == zone_group_2012 ~ zone_group_2006,
      construction_year <= 2014 & zone_group_2012 == zone_group_2014 ~ zone_group_2012,
      construction_year == 2015 & zone_group_2014 == zone_group_2016 ~ zone_group_2014,
      construction_year >= 2016 & zone_group_2016 == zone_group_2025 ~ zone_group_2016,
      TRUE ~ NA_character_
    ),
    # Do not carry zoning across a recorded amendment between the two dates.
    adjacent_event_between_dates = dplyr::coalesce(
      ordinance_date_2025 > pmin(construction_date, as.Date(ifelse(is.na(adjacent_year_min), NA_character_, paste0(adjacent_year_min, "-06-15")))) &
        ordinance_date_2025 <= pmax(construction_date, as.Date(ifelse(is.na(adjacent_year_max), NA_character_, paste0(adjacent_year_max, "-06-15")))), FALSE),
    coincident_event_between_dates = dplyr::coalesce(
      ordinance_date_2025 > pmin(construction_date, as.Date(paste0(nearest_validated_year, "-06-15"))) &
        ordinance_date_2025 <= pmax(construction_date, as.Date(paste0(nearest_validated_year, "-06-15"))), FALSE),
    coincident_validated_group = dplyr::if_else(
      nearest_validated_distance_ft <= 1 &
        abs(construction_year - nearest_validated_year) <= 1 & !coincident_event_between_dates,
      nearest_validated_group,
      NA_character_
    ),
    construction_zone_group = dplyr::case_when(
      exact_group_count == 1 ~ exact_groups,
      exact_group_count == 0 & adjacent_group_count == 1 & !adjacent_event_between_dates ~ adjacent_groups,
      !is.na(coincident_validated_group) ~ coincident_validated_group,
      !is.na(stable_interval_group) ~ stable_interval_group,
      current_last_event_preconstruction ~ zone_group_2025,
      !is.na(preceding_snapshot_group) ~ preceding_snapshot_group,
      !is.na(early_missing_2006_fallback) ~ early_missing_2006_fallback,
      TRUE ~ NA_character_
    ),
    zoning_assignment_source = dplyr::case_when(
      exact_group_count == 1 ~ "validated_component_exact_year",
      exact_group_count == 0 & adjacent_group_count == 1 & !adjacent_event_between_dates ~
        "validated_component_adjacent_year",
      !is.na(coincident_validated_group) ~ "coincident_validated_project",
      !is.na(stable_interval_group) ~ "stable_official_snapshot_interval",
      current_last_event_preconstruction ~
        "current_polygon_last_event_preconstruction",
      !is.na(preceding_snapshot_group) ~ "preceding_official_snapshot",
      !is.na(early_missing_2006_fallback) ~
        "stable_later_snapshots_missing_2006_polygon",
      TRUE ~ "unresolved_snapshot_change"
    )
  )

# Apply the recorded zoning decisions once, at the zoning producer. A reviewed
# construction year must match; an old decision cannot override a different year.
year_decisions <- readr::read_csv("../input/corrected_year_zoning_decisions.csv",
  show_col_types = FALSE) |>
  dplyr::transmute(project_id, construction_year,
    reviewed_zone_group = construction_zone_group,
    reviewed_source = paste0("recorded_corrected_year:", decision_source),
    zoning_decision_note = decision_note)
stopifnot(!anyDuplicated(year_decisions[c("project_id", "construction_year")]))
points <- points |>
  dplyr::left_join(year_decisions, by = c("project_id", "construction_year"),
    relationship = "one-to-one") |>
  dplyr::mutate(construction_zone_group = dplyr::coalesce(reviewed_zone_group, construction_zone_group),
    zoning_assignment_source = dplyr::coalesce(reviewed_source, zoning_assignment_source))

flat <- points |>
  sf::st_drop_geometry() |>
  dplyr::select(
    project_id,
    source_family,
    construction_year,
    within_500ft,
    construction_zone_group,
    zoning_assignment_source,
    zoning_decision_note,
    adjacent_event_between_dates,
    coincident_event_between_dates,
    component_count,
    matched_component_count,
    exact_component_count,
    adjacent_component_count,
    exact_groups,
    adjacent_groups,
    zone_group_2006,
    zone_group_2012,
    zone_group_2014,
    zone_group_2016,
    zone_group_2025,
    ordinance_number_2025,
    ordinance_date_2025,
    clerk_document_2025,
    nearest_validated_pin,
    nearest_validated_year,
    nearest_validated_group,
    nearest_validated_distance_ft
  )

if (nrow(flat) != nrow(projects) || anyDuplicated(flat$project_id)) {
  stop("Preferred construction-zoning output is not one row per project.", call. = FALSE)
}

SaveData(flat, c("project_id"), "../output/preferred_new_construction_zoning.csv", na = "")
