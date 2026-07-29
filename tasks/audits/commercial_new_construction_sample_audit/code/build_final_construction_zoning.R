# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

library(sf)

zone_group <- function(zone_code) {
  zone_code <- stringr::str_to_upper(as.character(zone_code))
  dplyr::case_when(
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

scope <- readr::read_csv(
  "../output/final_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(within_1500ft)

if (anyDuplicated(scope$project_id)) {
  stop("Final boundary scope is not unique by project.", call. = FALSE)
}

ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

recovered <- scope |>
  dplyr::filter(ledger_action == "add_recovered_project") |>
  dplyr::left_join(
    ledger |>
      dplyr::select(
        project_id,
        source_addresses
      ),
    by = "project_id",
    relationship = "one-to-one"
  )

if (nrow(recovered) != 35 || anyDuplicated(recovered$project_id)) {
  stop("Recovered-project scope is incomplete or duplicated.", call. = FALSE)
}

recovered_points <- sf::st_as_sf(
  recovered,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

zoning_2006 <- sf::st_read(
  "../adjudication/historical_zoning_2006_candidate.gpkg",
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
    ordinance_date_2025 = ordinance_1
  )

for (object_name in c(
  "zoning_2006",
  "zoning_2012",
  "zoning_2014",
  "zoning_2016",
  "zoning_2025"
)) {
  object <- get(object_name)
  if (sf::st_crs(object) != sf::st_crs(recovered_points)) {
    object <- sf::st_transform(object, sf::st_crs(recovered_points))
  }
  assign(object_name, object)
}

recovered_count <- nrow(recovered_points)
recovered_points <- sf::st_join(
  recovered_points,
  zoning_2006,
  left = TRUE,
  largest = TRUE
)
recovered_points <- sf::st_join(
  recovered_points,
  zoning_2012,
  left = TRUE,
  largest = TRUE
)
recovered_points <- sf::st_join(
  recovered_points,
  zoning_2014,
  left = TRUE,
  largest = TRUE
)
recovered_points <- sf::st_join(
  recovered_points,
  zoning_2016,
  left = TRUE,
  largest = TRUE
)
recovered_points <- sf::st_join(
  recovered_points,
  zoning_2025,
  left = TRUE,
  largest = TRUE
)

if (nrow(recovered_points) != recovered_count) {
  stop("Historical zoning joins changed the recovered-project count.", call. = FALSE)
}

validated <- readr::read_csv(
  "../adjudication/historical_zoning_project_construction_year.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character())
) |>
  dplyr::transmute(
    component_pin = pin,
    validated_year = as.integer(construction_year),
    validated_group = construction_zone_group
  )

if (anyDuplicated(validated$component_pin)) {
  stop("Validated zoning is not unique by component PIN.", call. = FALSE)
}

validated_matches <- recovered |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::rename(component_pin = component_pins) |>
  dplyr::left_join(
    validated,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::summarise(
    exact_group_count = dplyr::n_distinct(
      validated_group[construction_year == validated_year],
      na.rm = TRUE
    ),
    exact_groups = paste(
      sort(unique(validated_group[construction_year == validated_year])),
      collapse = ";"
    ),
    .by = project_id
  )

overrides <- readr::read_csv(
  "../adjudication/recovered_project_zoning_overrides.csv",
  show_col_types = FALSE
)

if (anyDuplicated(overrides$project_id)) {
  stop("Recovered zoning overrides are not unique by project.", call. = FALSE)
}
if (!all(overrides$project_id %in% recovered$project_id)) {
  stop("A recovered zoning override does not identify a recovered project.", call. = FALSE)
}

recovered_zoning <- recovered_points |>
  dplyr::mutate(
    zone_group_2012 = zone_group(zone_code_2012),
    zone_group_2014 = zone_group(zone_code_2014),
    zone_group_2016 = zone_group(zone_code_2016),
    zone_group_2025 = zone_group(zone_code_2025),
    construction_date = as.Date(sprintf("%d-06-15", construction_year)),
    ordinance_date_2025 = as.Date(ordinance_date_2025),
    stable_interval_group = dplyr::case_when(
      construction_year == 2006 ~ zone_group_2006,
      construction_year <= 2012 &
        zone_group_2006 == zone_group_2012 ~ zone_group_2006,
      construction_year <= 2014 &
        zone_group_2012 == zone_group_2014 ~ zone_group_2012,
      construction_year == 2015 &
        zone_group_2014 == zone_group_2016 ~ zone_group_2014,
      construction_year >= 2016 &
        zone_group_2016 == zone_group_2025 ~ zone_group_2016,
      TRUE ~ NA_character_
    ),
    preceding_snapshot_group = dplyr::case_when(
      construction_year <= 2012 ~ zone_group_2006,
      construction_year <= 2014 ~ zone_group_2012,
      construction_year == 2015 ~ zone_group_2014,
      construction_year >= 2016 ~ zone_group_2016,
      TRUE ~ NA_character_
    ),
    current_last_event_preconstruction = !is.na(ordinance_date_2025) &
      ordinance_date_2025 <= construction_date
  ) |>
  dplyr::left_join(
    validated_matches,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    overrides,
    by = "project_id",
    relationship = "one-to-one",
    suffix = c("", "_override")
  ) |>
  dplyr::mutate(
    construction_zone_group = dplyr::case_when(
      !is.na(construction_zone_group) ~ construction_zone_group,
      exact_group_count == 1 ~ exact_groups,
      !is.na(stable_interval_group) ~ stable_interval_group,
      current_last_event_preconstruction ~ zone_group_2025,
      !is.na(preceding_snapshot_group) ~ preceding_snapshot_group,
      TRUE ~ NA_character_
    ),
    zoning_assignment_source = dplyr::case_when(
      !is.na(decision_source) ~ decision_source,
      exact_group_count == 1 ~ "validated_component_exact_year",
      !is.na(stable_interval_group) ~ "stable_official_snapshot_interval",
      current_last_event_preconstruction ~
        "current_polygon_last_event_preconstruction",
      !is.na(preceding_snapshot_group) ~ "preceding_official_snapshot",
      TRUE ~ "unresolved_snapshot_change"
    )
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    project_id,
    source_family,
    construction_year,
    within_500ft,
    construction_zone_group,
    zoning_assignment_source,
    decision_note,
    zone_group_2006,
    zone_group_2012,
    zone_group_2014,
    zone_group_2016,
    zone_group_2025
  )

existing_zoning <- readr::read_csv(
  "../output/preferred_new_construction_zoning.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    ordinance_date_2025 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::semi_join(
    scope |>
      dplyr::filter(ledger_action == "retain_existing"),
    by = "project_id"
  ) |>
  dplyr::mutate(decision_note = NA_character_)

final_zoning <- dplyr::bind_rows(
  existing_zoning,
  recovered_zoning
) |>
  dplyr::semi_join(scope, by = "project_id") |>
  dplyr::arrange(project_id)

if (
  nrow(final_zoning) != nrow(scope) ||
    anyDuplicated(final_zoning$project_id) ||
    any(is.na(final_zoning$construction_zone_group))
) {
  stop("Final zoning is incomplete or duplicated.", call. = FALSE)
}

readr::write_csv(
  recovered_zoning,
  "../output/final_recovered_project_zoning.csv",
  na = ""
)
readr::write_csv(
  final_zoning,
  "../output/final_new_construction_zoning.csv",
  na = ""
)
readr::write_csv(
  final_zoning |>
    dplyr::count(
      within_500ft,
      source_family,
      zoning_assignment_source,
      name = "projects"
    ) |>
    dplyr::arrange(
      dplyr::desc(within_500ft),
      source_family,
      zoning_assignment_source
    ),
  "../output/final_new_construction_zoning_summary.csv",
  na = ""
)
