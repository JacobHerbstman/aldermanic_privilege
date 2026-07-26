# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

collapse_values <- function(x) {
  values <- sort(unique(x[!is.na(x) & x != ""]))
  if (length(values) == 0) NA_character_ else paste(values, collapse = " | ")
}

finite_min <- function(x) {
  values <- x[is.finite(x)]
  if (length(values) == 0) NA_real_ else min(values)
}

finite_max <- function(x) {
  values <- x[is.finite(x)]
  if (length(values) == 0) NA_real_ else max(values)
}

ledger <- readr::read_csv(
  "../input/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE
)
density <- readr::read_csv(
  "../input/final_density_model_input.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    ward_pair,
    distance_to_boundary_ft,
    within_500ft,
    within_1500ft
  )
residential <- readr::read_csv(
  "../input/preferred_residential_project_ledger.csv",
  show_col_types = FALSE
) |>
  dplyr::select(project_id, class_values)
addresses <- readr::read_csv(
  "../input/preferred_project_duplicate_dispositions.csv",
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    current_property_addresses,
    current_address_count,
    direct_permit_address_count,
    address_count,
    addresses
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        current_address_count,
        direct_permit_address_count,
        address_count
      ),
      readr::parse_number
    )
  )
residential_history_source <- readr::read_csv(
  "../input/residential_project_candidate_inventory.csv",
  col_types = readr::cols(
    class = readr::col_character(),
    .default = readr::col_guess()
  ),
  show_col_types = FALSE
) |>
  dplyr::select(
    pin,
    source_years,
    source_building_areas,
    source_unit_counts
  ) |>
  dplyr::distinct()

residential_history_years <- residential_history_source |>
  dplyr::select(pin, source_years) |>
  tidyr::separate_longer_delim(source_years, delim = "/") |>
  dplyr::mutate(
    history_year = readr::parse_number(source_years)
  ) |>
  dplyr::filter(is.finite(history_year)) |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    minimum_history_year = min(history_year),
    maximum_history_year = max(history_year),
    distinct_history_years = dplyr::n_distinct(history_year),
    history_year_values = collapse_values(as.character(history_year)),
    .groups = "drop"
  )

residential_history_building_areas <- residential_history_source |>
  dplyr::select(pin, source_building_areas) |>
  tidyr::separate_longer_delim(source_building_areas, delim = "/") |>
  dplyr::mutate(
    history_building_area = readr::parse_number(source_building_areas)
  ) |>
  dplyr::filter(is.finite(history_building_area)) |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    distinct_history_building_areas =
      dplyr::n_distinct(history_building_area),
    history_building_area_values = collapse_values(
      as.character(history_building_area)
    ),
    .groups = "drop"
  )

residential_history_units <- residential_history_source |>
  dplyr::select(pin, source_unit_counts) |>
  tidyr::separate_longer_delim(source_unit_counts, delim = "/") |>
  dplyr::mutate(
    history_units = readr::parse_number(source_unit_counts)
  ) |>
  dplyr::filter(is.finite(history_units)) |>
  dplyr::group_by(pin) |>
  dplyr::summarise(
    distinct_history_unit_counts = dplyr::n_distinct(history_units),
    history_unit_count_values = collapse_values(as.character(history_units)),
    .groups = "drop"
  )

residential_history <- residential_history_years |>
  dplyr::full_join(
    residential_history_building_areas,
    by = "pin",
    relationship = "one-to-one"
  ) |>
  dplyr::full_join(
    residential_history_units,
    by = "pin",
    relationship = "one-to-one"
  )

project_history <- ledger |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::rename(pin = component_pins) |>
  dplyr::left_join(
    residential_history,
    by = "pin",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    minimum_history_year = finite_min(minimum_history_year),
    maximum_history_year = finite_max(maximum_history_year),
    distinct_history_years = sum(distinct_history_years, na.rm = TRUE),
    history_year_values = collapse_values(history_year_values),
    distinct_history_building_areas = sum(
      distinct_history_building_areas,
      na.rm = TRUE
    ),
    history_building_area_values = collapse_values(
      history_building_area_values
    ),
    distinct_history_unit_counts = sum(
      distinct_history_unit_counts,
      na.rm = TRUE
    ),
    history_unit_count_values = collapse_values(history_unit_count_values),
    .groups = "drop"
  )

component_crosswalk <- ledger |>
  dplyr::select(final_project_id = project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::rename(component_pin = component_pins) |>
  dplyr::distinct()

if (anyDuplicated(component_crosswalk$component_pin)) {
  stop("A component PIN maps to more than one final project.", call. = FALSE)
}

project_aliases <- dplyr::bind_rows(
  ledger |>
    dplyr::transmute(final_project_id = project_id, alias = project_id),
  ledger |>
    dplyr::select(final_project_id = project_id, source_project_ids) |>
    tidyr::separate_longer_delim(source_project_ids, delim = "/") |>
    dplyr::transmute(final_project_id, alias = source_project_ids),
  ledger |>
    dplyr::select(
      final_project_id = project_id,
      source_family,
      component_pins
    ) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      final_project_id,
      alias = paste0(source_family, "_", component_pins)
    ),
  ledger |>
    dplyr::filter(source_family == "residential") |>
    dplyr::select(final_project_id = project_id, component_pins) |>
    tidyr::separate_longer_delim(component_pins, delim = "/") |>
    dplyr::transmute(
      final_project_id,
      alias = paste0("residential_multicard_", component_pins)
    )
) |>
  dplyr::filter(!is.na(alias), alias != "") |>
  dplyr::distinct()

unique_project_aliases <- project_aliases |>
  dplyr::add_count(alias, name = "alias_project_count") |>
  dplyr::filter(alias_project_count == 1L) |>
  dplyr::select(-alias_project_count)

exact_permits <- readr::read_csv(
  "../input/new_construction_exact_permit_matches.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(source_project_id = project_id) |>
  dplyr::inner_join(
    component_crosswalk,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::rename(project_id = final_project_id) |>
  dplyr::mutate(
    permit_number = as.character(permit_number),
    work_description = stringr::str_squish(
      stringr::str_to_upper(dplyr::coalesce(work_description, ""))
    ),
    explicit_new_building_raw = stringr::str_detect(
      work_description,
      paste0(
        "NEW CONSTRUCTION|",
        "CONSTRUCTION OF .*NEW|",
        "CONSTRUCT(?:ION)? NEW|",
        "ERECT (?:A )?NEW|",
        "NEW (?:SINGLE|[0-9]+[ -]?STORY)|",
        "ERECT .* (?:BUILDING|RESIDENCE|HOUSE|DWELLING|TOWNHOUSE)|",
        "FULL BUILDING PERMIT"
      )
    ),
    addition_to_existing_scope = stringr::str_detect(
      work_description,
      paste0(
        "CONSTRUCT NEW ADDITION|",
        "NEW .* CONSTRUCTION OVER EXISTING|",
        "UNIT ADDITION .* EXISTING"
      )
    ),
    explicit_new_building =
      explicit_new_building_raw &
      !addition_to_existing_scope &
      permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING"),
    explicit_existing_work = stringr::str_detect(
      work_description,
      paste0(
        "\\bEXISTING\\b.*\\b(DECONVERSION|CONVERSION|ADDITION|ALTERATION|",
        "MODIFICATION|RENOVATION|REHAB)\\b|",
        "\\b(DECONVERSION|CONVERSION|ADDITION|ALTERATION|MODIFICATION|",
        "RENOVATION|REHAB)\\b.*\\bEXISTING\\b"
      )
    ),
    existing_only_work = explicit_existing_work & !explicit_new_building
  )
spatial_permits_raw <- readr::read_csv(
  "../input/new_construction_spatial_permit_matches.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(source_project_id = project_id)

spatial_permits <- dplyr::bind_rows(
  spatial_permits_raw |>
    dplyr::filter(source_project_id %in% ledger$project_id) |>
    dplyr::rename(project_id = source_project_id),
  spatial_permits_raw |>
    dplyr::filter(!source_project_id %in% ledger$project_id) |>
    dplyr::inner_join(
      unique_project_aliases,
      by = c("source_project_id" = "alias"),
      relationship = "many-to-one"
    ) |>
    dplyr::rename(project_id = final_project_id)
) |>
  dplyr::mutate(
    permit_number = as.character(permit_number),
    work_description = stringr::str_squish(
      stringr::str_to_upper(dplyr::coalesce(work_description, ""))
    ),
    strong_spatial_match =
      exact_pin_match |
      spatial_match_method %in% c(
        "inside_project_polygon",
        "exact_pin_and_project_polygon"
      ) |
      polygon_distance_ft <= 5,
    explicit_new_building_raw = stringr::str_detect(
      work_description,
      paste0(
        "NEW CONSTRUCTION|",
        "CONSTRUCTION OF .*NEW|",
        "CONSTRUCT(?:ION)? NEW|",
        "ERECT (?:A )?NEW|",
        "NEW (?:SINGLE|[0-9]+[ -]?STORY)|",
        "ERECT .* (?:BUILDING|RESIDENCE|HOUSE|DWELLING|TOWNHOUSE)|",
        "FULL BUILDING PERMIT"
      )
    ),
    addition_to_existing_scope = stringr::str_detect(
      work_description,
      paste0(
        "CONSTRUCT NEW ADDITION|",
        "NEW .* CONSTRUCTION OVER EXISTING|",
        "UNIT ADDITION .* EXISTING"
      )
    ),
    explicit_new_building =
      explicit_new_building_raw &
      !addition_to_existing_scope &
      permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING"),
    explicit_existing_work = stringr::str_detect(
      work_description,
      paste0(
        "\\bEXISTING\\b.*\\b(DECONVERSION|CONVERSION|ADDITION|ALTERATION|",
        "MODIFICATION|RENOVATION|REHAB)\\b|",
        "\\b(DECONVERSION|CONVERSION|ADDITION|ALTERATION|MODIFICATION|",
        "RENOVATION|REHAB)\\b.*\\bEXISTING\\b"
      )
    ),
    existing_only_work = explicit_existing_work & !explicit_new_building
  ) |>
  dplyr::filter(strong_spatial_match)
permit_units <- readr::read_csv(
  "../input/new_construction_permit_unit_mentions.csv",
  show_col_types = FALSE
) |>
  dplyr::rename(source_project_id = project_id) |>
  dplyr::inner_join(
    component_crosswalk,
    by = "component_pin",
    relationship = "many-to-one"
  ) |>
  dplyr::rename(project_id = final_project_id)
reviews <- readr::read_csv(
  "../input/multicard_external_review_queue.csv",
  show_col_types = FALSE
) |>
  dplyr::select(
    project_id,
    review_status,
    external_structure_class,
    multifamily_disposition,
    external_building_count,
    external_unit_count,
    external_building_sqft,
    reviewer_notes
  )

historical_permit_evidence <- readr::read_csv(
  "../output/historical_permit_project_evidence.csv",
  show_col_types = FALSE
)

for (
  x in list(
    ledger,
    density,
    residential,
    addresses,
    historical_permit_evidence,
    reviews
  )
) {
  if (anyDuplicated(x$project_id)) {
    stop("A project-level input is not uniquely keyed.", call. = FALSE)
  }
}

permit_project_counts <- exact_permits |>
  dplyr::distinct(permit_number, project_id) |>
  dplyr::group_by(permit_number) |>
  dplyr::summarise(
    exact_permit_project_count = dplyr::n(),
    exact_permit_project_ids = collapse_values(project_id),
    .groups = "drop"
  )

project_permit_summary <- exact_permits |>
  dplyr::left_join(
    permit_project_counts,
    by = "permit_number",
    relationship = "many-to-one"
  ) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    exact_permit_count = dplyr::n_distinct(permit_number),
    exact_permit_numbers = collapse_values(permit_number),
    exact_permit_statuses = collapse_values(permit_status),
    exact_permit_addresses = collapse_values(permit_address),
    exact_permit_descriptions = collapse_values(work_description),
    exact_positive_new_building = any(explicit_new_building),
    exact_negative_existing_work = any(existing_only_work),
    maximum_exact_permit_project_count = max(exact_permit_project_count),
    shared_exact_permit_project_ids = collapse_values(
      exact_permit_project_ids[exact_permit_project_count > 1]
    ),
    .groups = "drop"
  )

project_spatial_permit_summary <- spatial_permits |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    strong_spatial_permit_count = dplyr::n_distinct(permit_number),
    strong_spatial_permit_numbers = collapse_values(permit_number),
    strong_spatial_permit_addresses = collapse_values(permit_address),
    strong_spatial_permit_descriptions = collapse_values(work_description),
    strong_spatial_positive_new_building = any(explicit_new_building),
    strong_spatial_negative_existing_work = any(existing_only_work),
    .groups = "drop"
  )

project_permit_units <- permit_units |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    permit_unit_mention_count = dplyr::n(),
    permit_unit_min = min(unit_count, na.rm = TRUE),
    permit_unit_max = max(unit_count, na.rm = TRUE),
    permit_unit_values = collapse_values(as.character(unit_count)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    permit_unit_min = dplyr::if_else(
      is.infinite(permit_unit_min),
      NA_real_,
      permit_unit_min
    ),
    permit_unit_max = dplyr::if_else(
      is.infinite(permit_unit_max),
      NA_real_,
      permit_unit_max
    )
  )

evidence <- ledger |>
  dplyr::left_join(density, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(
    residential,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    addresses,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    project_history,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    project_permit_summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    project_spatial_permit_summary,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    historical_permit_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    project_permit_units,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(reviews, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(
    within_500ft = dplyr::coalesce(within_500ft, FALSE),
    within_1500ft = dplyr::coalesce(within_1500ft, FALSE),
    exact_permit_count = dplyr::coalesce(exact_permit_count, 0L),
    maximum_exact_permit_project_count = dplyr::coalesce(
      maximum_exact_permit_project_count,
      0L
    ),
    exact_positive_new_building = dplyr::coalesce(
      exact_positive_new_building,
      FALSE
    ),
    exact_negative_existing_work = dplyr::coalesce(
      exact_negative_existing_work,
      FALSE
    ),
    strong_spatial_permit_count = dplyr::coalesce(
      strong_spatial_permit_count,
      0L
    ),
    strong_spatial_positive_new_building = dplyr::coalesce(
      strong_spatial_positive_new_building,
      FALSE
    ),
    strong_spatial_negative_existing_work = dplyr::coalesce(
      strong_spatial_negative_existing_work,
      FALSE
    ),
    exact_pin_permit_records = dplyr::coalesce(
      exact_pin_permit_records,
      0L
    ),
    exact_pin_positive_new_building = dplyr::coalesce(
      exact_pin_positive_new_building,
      FALSE
    ),
    exact_pin_negative_existing_building = dplyr::coalesce(
      exact_pin_negative_existing_building,
      FALSE
    ),
    exact_pin_broad_negative_existing_work = dplyr::coalesce(
      exact_pin_broad_negative_existing_work,
      FALSE
    ),
    exact_pin_post_construction_existing_work = dplyr::coalesce(
      exact_pin_post_construction_existing_work,
      FALSE
    ),
    positive_new_building_permit =
      exact_positive_new_building |
      strong_spatial_positive_new_building |
      exact_pin_positive_new_building,
    assessor_year_recode_risk =
      source_family == "residential" &
      is.finite(minimum_history_year) &
      minimum_history_year < 2006 &
      maximum_history_year >= construction_year &
      distinct_history_years > 1,
    assessor_year_only_recode =
      assessor_year_recode_risk &
      distinct_history_building_areas <= 1 &
      distinct_history_unit_counts <= 1,
    assessor_physical_change =
      assessor_year_recode_risk &
      (
        distinct_history_building_areas > 1 |
        distinct_history_unit_counts > 1
      ),
    externally_reviewed = !is.na(review_status),
    missing_units = !is.finite(dwelling_units),
    missing_building_sqft = !is.finite(building_sqft),
    current_multifamily = dplyr::coalesce(dwelling_units > 1, FALSE),
    building_sqft_per_unit = building_sqft / dwelling_units,
    density_dupac = 43560 * dwelling_units / land_sqft,
    class_211_212 = stringr::str_detect(
      dplyr::coalesce(class_values, ""),
      "(^|/)(211|212)($|/)"
    ),
    class_278_295 = stringr::str_detect(
      dplyr::coalesce(class_values, ""),
      "(^|/)(278|295)($|/)"
    ),
    class_297 = stringr::str_detect(
      dplyr::coalesce(class_values, ""),
      "(^|/)297($|/)"
    ),
    implausibly_small_sqft_per_unit =
      current_multifamily &
      is.finite(building_sqft_per_unit) &
      building_sqft_per_unit < 400,
    development_unit_count_risk =
      source_family == "residential" &
      class_278_295 &
      current_multifamily &
      is.finite(building_sqft_per_unit) &
      building_sqft_per_unit < 500,
    apartment_class_false_negative_risk = dplyr::coalesce(
      source_family == "residential" &
      class_211_212 &
      dwelling_units <= 1,
      FALSE
    ),
    permit_unit_conflict = dplyr::coalesce(
      is.finite(permit_unit_min) &
      is.finite(permit_unit_max) &
      (dwelling_units < permit_unit_min | dwelling_units > permit_unit_max),
      FALSE
    ),
    shared_exact_permit_risk = maximum_exact_permit_project_count > 1,
    external_classification_conflict = dplyr::case_when(
      multifamily_disposition == "include" ~ !current_multifamily,
      multifamily_disposition == "exclude" ~ current_multifamily,
      multifamily_disposition == "suppress" ~ TRUE,
      TRUE ~ FALSE
    ),
    eligibility_signal = dplyr::case_when(
      positive_new_building_permit ~
        "permit_supports_new_building",
      source_family == "residential" &
        (exact_negative_existing_work |
          strong_spatial_negative_existing_work |
          exact_pin_negative_existing_building) ~
        "exact_permit_describes_work_on_existing_building",
      source_family == "commercial" &
        (exact_negative_existing_work |
          strong_spatial_negative_existing_work |
          exact_pin_negative_existing_building) ~
        "commercial_exact_permits_only_show_existing_buildout",
      source_family == "residential" &
        exact_pin_broad_negative_existing_work ~
        "broad_exact_permit_existing_work_review",
      assessor_year_only_recode ~
        "assessor_year_changes_without_physical_change",
      assessor_physical_change ~
        "assessor_history_shows_physical_change_without_permit",
      exact_permit_count > 0 ~ "exact_permit_without_decisive_scope",
      TRUE ~ "no_exact_permit"
    ),
    classification_signal = dplyr::case_when(
      multifamily_disposition == "include" ~
        "external_review_multifamily",
      multifamily_disposition == "exclude" ~
        "external_review_not_multifamily",
      multifamily_disposition == "suppress" ~
        "external_review_suppress",
      source_family == "commercial" ~
        "commercial_multifamily_source",
      apartment_class_false_negative_risk ~
        "apartment_class_with_zero_or_one_reported_unit",
      development_unit_count_risk ~
        "development_level_unit_count_risk",
      class_211_212 & current_multifamily ~
        "apartment_class_multifamily",
      current_multifamily ~
        "other_residential_multifamily_candidate",
      TRUE ~ "single_unit_candidate"
    ),
    requires_initial_review =
      development_unit_count_risk |
      apartment_class_false_negative_risk |
      (allow_dupac & missing_units) |
      (allow_far & missing_building_sqft) |
      permit_unit_conflict |
      shared_exact_permit_risk |
      external_classification_conflict |
      eligibility_signal %in% c(
        "exact_permit_describes_work_on_existing_building",
        "commercial_exact_permits_only_show_existing_buildout",
        "assessor_year_changes_without_physical_change",
        "assessor_history_shows_physical_change_without_permit",
        "broad_exact_permit_existing_work_review",
        "mixed_exact_permit_scope"
      ),
    review_priority = dplyr::case_when(
      eligibility_signal ==
        "exact_permit_describes_work_on_existing_building" ~ 1L,
      eligibility_signal ==
        "commercial_exact_permits_only_show_existing_buildout" ~ 2L,
      eligibility_signal ==
        "assessor_year_changes_without_physical_change" ~ 2L,
      eligibility_signal ==
        "assessor_history_shows_physical_change_without_permit" ~ 3L,
      eligibility_signal ==
        "broad_exact_permit_existing_work_review" ~ 3L,
      development_unit_count_risk ~ 2L,
      apartment_class_false_negative_risk ~ 3L,
      allow_dupac & missing_units ~ 4L,
      allow_far & missing_building_sqft ~ 4L,
      external_classification_conflict ~ 4L,
      permit_unit_conflict ~ 5L,
      shared_exact_permit_risk ~ 6L,
      eligibility_signal == "mixed_exact_permit_scope" ~ 7L,
      TRUE ~ NA_integer_
    ),
    review_reasons = paste0(
      dplyr::if_else(
        eligibility_signal ==
          "exact_permit_describes_work_on_existing_building",
        "existing_building_scope;",
        ""
      ),
      dplyr::if_else(
        eligibility_signal ==
          "commercial_exact_permits_only_show_existing_buildout",
        "commercial_existing_buildout_only;",
        ""
      ),
      dplyr::if_else(
        eligibility_signal ==
          "assessor_year_changes_without_physical_change",
        "assessor_year_only_recode;",
        ""
      ),
      dplyr::if_else(
        eligibility_signal ==
          "assessor_history_shows_physical_change_without_permit",
        "assessor_physical_change_without_permit;",
        ""
      ),
      dplyr::if_else(
        eligibility_signal ==
          "broad_exact_permit_existing_work_review",
        "broad_existing_work_permit;",
        ""
      ),
      dplyr::if_else(
        development_unit_count_risk,
        "development_unit_count;",
        ""
      ),
      dplyr::if_else(
        apartment_class_false_negative_risk,
        "apartment_class_false_negative;",
        ""
      ),
      dplyr::if_else(
        allow_dupac & missing_units,
        "missing_units_for_dupac;",
        ""
      ),
      dplyr::if_else(
        allow_far & missing_building_sqft,
        "missing_building_sqft_for_far;",
        ""
      ),
      dplyr::if_else(
        external_classification_conflict,
        "external_classification_conflict;",
        ""
      ),
      dplyr::if_else(permit_unit_conflict, "permit_unit_conflict;", ""),
      dplyr::if_else(shared_exact_permit_risk, "shared_exact_permit;", ""),
      dplyr::if_else(
        eligibility_signal == "mixed_exact_permit_scope",
        "mixed_permit_scope;",
        ""
      )
    )
  ) |>
  dplyr::arrange(project_id)

if (
  nrow(evidence) != nrow(ledger) ||
    anyDuplicated(evidence$project_id) ||
    any(!is.finite(evidence$land_sqft)) ||
    any(evidence$allow_dupac & !is.finite(evidence$dwelling_units)) ||
    any(evidence$allow_far & !is.finite(evidence$building_sqft))
) {
  stop("The project evidence inventory failed validation.", call. = FALSE)
}

flag_summary <- evidence |>
  dplyr::summarise(
    projects = dplyr::n(),
    projects_within_1500ft = sum(within_1500ft),
    projects_within_500ft = sum(within_500ft),
    multifamily_within_500ft = sum(current_multifamily & within_500ft),
    externally_reviewed = sum(externally_reviewed),
    development_unit_count_risk_total = sum(development_unit_count_risk),
    development_unit_count_risk_500ft = sum(
      development_unit_count_risk & within_500ft
    ),
    apartment_class_false_negative_risk_total = sum(
      apartment_class_false_negative_risk
    ),
    apartment_class_false_negative_risk_500ft = sum(
      apartment_class_false_negative_risk & within_500ft
    ),
    exact_existing_building_scope = sum(
      eligibility_signal ==
        "exact_permit_describes_work_on_existing_building"
    ),
    exact_existing_building_scope_500ft = sum(
      eligibility_signal ==
        "exact_permit_describes_work_on_existing_building" &
        within_500ft
    ),
    exact_existing_building_scope_multifamily_500ft = sum(
      eligibility_signal ==
        "exact_permit_describes_work_on_existing_building" &
        current_multifamily &
        within_500ft
    ),
    assessor_year_recode_risk_total = sum(
      assessor_year_recode_risk
    ),
    assessor_year_recode_risk_500ft = sum(
      assessor_year_recode_risk & within_500ft
    ),
    assessor_year_recode_risk_multifamily_500ft = sum(
      assessor_year_recode_risk &
        current_multifamily &
        within_500ft
    ),
    assessor_year_only_recode_500ft = sum(
      assessor_year_only_recode & within_500ft
    ),
    assessor_year_only_recode_multifamily_500ft = sum(
      assessor_year_only_recode &
        current_multifamily &
        within_500ft
    ),
    assessor_physical_change_500ft = sum(
      assessor_physical_change & within_500ft
    ),
    assessor_physical_change_multifamily_500ft = sum(
      assessor_physical_change &
        current_multifamily &
        within_500ft
    ),
    shared_exact_permit_risk_total = sum(shared_exact_permit_risk),
    shared_exact_permit_risk_500ft = sum(
      shared_exact_permit_risk & within_500ft
    ),
    initial_review_queue = sum(requires_initial_review),
    initial_review_queue_500ft = sum(requires_initial_review & within_500ft)
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "metric",
    values_to = "value"
  )

known_ids <- c(
  "residential_13254280100000",
  "residential_17061140060000",
  "residential_multicard_17341211170000",
  "residential_17341211360000",
  "residential_17341211370000",
  "residential_17341211380000",
  "residential_17341211390000",
  "residential_17341211400000"
)

readr::write_csv(
  evidence,
  "../output/project_evidence_inventory.csv",
  na = ""
)
readr::write_csv(
  flag_summary,
  "../output/project_evidence_summary.csv",
  na = ""
)
readr::write_csv(
  evidence |>
    dplyr::filter(requires_initial_review) |>
    dplyr::arrange(review_priority, within_500ft, project_id),
  "../output/initial_review_queue.csv",
  na = ""
)
readr::write_csv(
  evidence |>
    dplyr::filter(project_id %in% known_ids),
  "../output/known_case_validation.csv",
  na = ""
)
