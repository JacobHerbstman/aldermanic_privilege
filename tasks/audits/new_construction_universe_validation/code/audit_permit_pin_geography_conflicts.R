# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

collapse_values <- function(x) {
  values <- sort(unique(x[!is.na(x) & x != ""]))
  if (length(values) == 0) NA_character_ else paste(values, collapse = " | ")
}

normalize_address <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_replace(
      "\\s+(AVENUE|AVE|STREET|ST|ROAD|RD|BOULEVARD|BLVD|PLACE|PL|COURT|CT|DRIVE|DR|PARKWAY|PKWY|TERRACE|TER)(\\s+(UNIT|APT|SUITE)\\s*[A-Z0-9-]+)?$",
      ""
    ) |>
    stringr::str_replace("\\bCHICAGO\\b.*$", "") |>
    stringr::str_squish()
}

projects <- readr::read_csv(
  "../output/project_evidence_inventory.csv",
  show_col_types = FALSE
)

analysis_projects <- readr::read_csv(
  "../input/multicard_external_reviewed_model_input.csv",
  show_col_types = FALSE,
  col_select = "project_id",
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_skip()
  )
) |>
  dplyr::select(project_id)

aliases <- dplyr::bind_rows(
  projects |>
    dplyr::transmute(project_id, alias = project_id),
  projects |>
    dplyr::select(project_id, source_project_ids) |>
    tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
    dplyr::transmute(project_id, alias = source_project_ids),
  projects |>
    dplyr::select(project_id, source_family, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      project_id,
      alias = paste0(source_family, "_", component_pins)
    ),
  projects |>
    dplyr::filter(source_family == "residential") |>
    dplyr::select(project_id, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      project_id,
      alias = paste0("residential_multicard_", component_pins)
    )
) |>
  dplyr::filter(!is.na(alias), alias != "") |>
  dplyr::distinct() |>
  dplyr::add_count(alias, name = "alias_project_count") |>
  dplyr::filter(alias_project_count == 1L) |>
  dplyr::select(
    final_project_id = project_id,
    alias
  ) |>
  dplyr::semi_join(
    analysis_projects,
    by = c("final_project_id" = "project_id")
  )

project_addresses <- dplyr::bind_rows(
  projects |>
    dplyr::select(project_id, source_addresses) |>
    tidyr::separate_longer_delim(source_addresses, delim = "/") |>
    dplyr::transmute(
      project_id,
      project_address = source_addresses
    ),
  projects |>
    dplyr::select(project_id, current_property_addresses) |>
    tidyr::separate_longer_delim(current_property_addresses, delim = "/") |>
    dplyr::transmute(
      project_id,
      project_address = current_property_addresses
    )
) |>
  dplyr::mutate(normalized_project_address = normalize_address(project_address)) |>
  dplyr::filter(normalized_project_address != "") |>
  dplyr::distinct(project_id, normalized_project_address)

spatial_matches <- readr::read_csv(
  "../input/new_construction_spatial_permit_matches.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
) |>
  dplyr::inner_join(
    aliases,
    by = c("project_id" = "alias"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    project_id = final_project_id,
    normalized_permit_address = normalize_address(permit_address)
  ) |>
  dplyr::select(-final_project_id)

exact_matches <- readr::read_csv(
  "../input/new_construction_exact_permit_matches.csv",
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
) |>
  dplyr::inner_join(
    aliases,
    by = c("project_id" = "alias"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    project_id = final_project_id,
    normalized_permit_address = normalize_address(permit_address)
  ) |>
  dplyr::select(-final_project_id) |>
  dplyr::distinct(
    permit_number,
    exact_project_id = project_id,
    normalized_permit_address
  )

polygon_matches <- spatial_matches |>
  dplyr::filter(
    spatial_match_method %in% c(
      "inside_project_polygon",
      "exact_pin_and_project_polygon"
    )
  ) |>
  dplyr::distinct(
    permit_number,
    polygon_project_id = project_id,
    normalized_permit_address
  )

polygon_matches_by_permit <- polygon_matches |>
  dplyr::group_by(permit_number, normalized_permit_address) |>
  tidyr::nest(polygon_matches = polygon_project_id) |>
  dplyr::ungroup()

conflicts <- exact_matches |>
  dplyr::inner_join(
    polygon_matches_by_permit,
    by = c("permit_number", "normalized_permit_address"),
    relationship = "many-to-one"
  ) |>
  tidyr::unnest(polygon_matches) |>
  dplyr::filter(exact_project_id != polygon_project_id) |>
  dplyr::distinct() |>
  dplyr::left_join(
    project_addresses |>
      dplyr::rename(
        exact_project_id = project_id,
        exact_project_address = normalized_project_address
      ) |>
      dplyr::mutate(exact_address_match = TRUE),
    by = c(
      "exact_project_id",
      "normalized_permit_address" = "exact_project_address"
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    project_addresses |>
      dplyr::rename(
        polygon_project_id = project_id,
        polygon_project_address = normalized_project_address
      ) |>
      dplyr::mutate(polygon_address_match = TRUE),
    by = c(
      "polygon_project_id",
      "normalized_permit_address" = "polygon_project_address"
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    exact_address_match = dplyr::coalesce(exact_address_match, FALSE),
    polygon_address_match = dplyr::coalesce(polygon_address_match, FALSE),
    conflict_rule = dplyr::case_when(
      polygon_address_match & !exact_address_match ~
        "prefer_polygon_project_address_and_geometry_agree",
      exact_address_match & !polygon_address_match ~
        "prefer_exact_pin_project_address_agrees",
      exact_address_match & polygon_address_match ~
        "shared_site_address_requires_review",
      TRUE ~
        "permit_point_inside_different_project_requires_review"
    )
  ) |>
  dplyr::left_join(
    spatial_matches |>
      dplyr::group_by(permit_number) |>
      dplyr::summarise(
        permit_address = dplyr::first(permit_address),
        permit_application_year = dplyr::first(application_year),
        work_description = dplyr::first(work_description),
        .groups = "drop"
      ),
    by = "permit_number",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        exact_project_id = project_id,
        exact_construction_year = construction_year,
        exact_dwelling_units = dwelling_units,
        exact_positive_new_building = positive_new_building_permit,
        exact_within_500ft = within_500ft
      ),
    by = "exact_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        polygon_project_id = project_id,
        polygon_construction_year = construction_year,
        polygon_dwelling_units = dwelling_units,
        polygon_positive_new_building = positive_new_building_permit,
        polygon_within_500ft = within_500ft
      ),
    by = "polygon_project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    permit_supports_new_building = stringr::str_detect(
      stringr::str_to_upper(dplyr::coalesce(work_description, "")),
      paste0(
        "\\bNEW CONSTRUCTION\\b|",
        "\\bCONSTRUCTION OF (?:A |AN )?NEW\\b|",
        "\\bCONSTRUCT(?:ION)? (?:A |AN )?NEW\\b|",
        "\\bERECT (?:A |AN )?NEW\\b|",
        "\\bERECT .*\\b(BUILDING|RESIDENCE|HOUSE|DWELLING|TOWNHOUSE)\\b|",
        "\\bPROPOSED\\b.*\\b(BUILDING|RESIDENCE|DWELLING)\\b"
      )
    ),
    permit_dwelling_units = as.numeric(
      stringr::str_match(
        stringr::str_to_upper(dplyr::coalesce(work_description, "")),
        "([0-9]+)\\s*(?:DWELLING\\s+UNITS?|D\\.?\\s*U\\.?)"
      )[, 2]
    ),
    substantive_conflict = dplyr::case_when(
      permit_supports_new_building &
        is.finite(permit_dwelling_units) &
        permit_dwelling_units > 1 &
        dplyr::coalesce(polygon_dwelling_units <= 1, FALSE) ~
        "polygon_project_requires_permit_unit_recovery",
      permit_supports_new_building &
        !dplyr::coalesce(polygon_positive_new_building, FALSE) ~
        "polygon_project_requires_new_building_evidence",
      permit_supports_new_building ~
        "new_building_permit_but_no_project_value_change",
      TRUE ~
        "permit_does_not_establish_new_building"
    )
  ) |>
  dplyr::arrange(conflict_rule, permit_number, exact_project_id, polygon_project_id)

summary <- conflicts |>
  dplyr::count(
    substantive_conflict,
    conflict_rule,
    name = "project_permit_conflicts"
  ) |>
  dplyr::arrange(substantive_conflict, conflict_rule)

readr::write_csv(
  conflicts,
  "../output/permit_pin_geography_conflicts.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/permit_pin_geography_conflict_summary.csv",
  na = ""
)
