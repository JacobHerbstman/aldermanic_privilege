# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

parse_apartments <- function(x) {
  value <- stringr::str_to_lower(
    stringr::str_squish(as.character(x))
  )
  dplyr::case_when(
    is.na(value) | value == "" ~ NA_real_,
    value %in% c("none", "zero") ~ 0,
    value == "one" ~ 1,
    value == "two" ~ 2,
    value == "three" ~ 3,
    value == "four" ~ 4,
    value == "five" ~ 5,
    value == "six" ~ 6,
    TRUE ~ suppressWarnings(as.numeric(
      stringr::str_replace_all(value, "[^0-9.-]", "")
    ))
  )
}

projects <- readr::read_csv(
  "../output/permit_rule_coverage.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    source_family == "residential",
    assessor_year_recode_risk
  ) |>
  dplyr::select(
    project_id,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    within_500ft,
    within_1500ft,
    current_multifamily
  )

requests <- projects |>
  dplyr::select(
    project_id,
    component_pins,
    construction_year
  ) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::transmute(
    project_id,
    component_pin = component_pins,
    construction_year
  ) |>
  dplyr::filter(stringr::str_length(component_pin) == 14L) |>
  dplyr::distinct()

project_component_counts <- requests |>
  dplyr::count(
    project_id,
    name = "requested_component_pins"
  )

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(
  DBI::dbDisconnect(connection, shutdown = TRUE),
  add = TRUE
)

DBI::dbWriteTable(
  connection,
  "requested_pins",
  requests,
  temporary = TRUE,
  overwrite = TRUE
)

history <- DBI::dbGetQuery(
  connection,
  paste(
    "SELECT",
    "r.project_id,",
    "r.component_pin,",
    "r.construction_year,",
    "TRY_CAST(h.year AS INTEGER) AS tax_year,",
    "TRY_CAST(h.card AS INTEGER) AS card_num,",
    "CAST(h.class AS VARCHAR) AS assessor_class,",
    "TRY_CAST(h.char_yrblt AS INTEGER) AS reported_year_built,",
    "TRY_CAST(h.char_bldg_sf AS DOUBLE) AS building_sqft,",
    "CAST(h.char_apts AS VARCHAR) AS apartments_text,",
    "CAST(h.char_type_resd AS VARCHAR) AS residence_type,",
    "CAST(h.char_use AS VARCHAR) AS residence_use,",
    "CAST(h.row_id AS VARCHAR) AS row_id",
    "FROM read_csv_auto(",
    "'../input/residential_improvement_characteristics_full.csv',",
    "header = true, all_varchar = true, ignore_errors = true",
    ") h",
    "INNER JOIN requested_pins r ON h.pin = r.component_pin",
    "WHERE TRY_CAST(h.char_yrblt AS INTEGER) < 2006",
    "AND TRY_CAST(h.year AS INTEGER) < r.construction_year"
  )
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    parsed_apartments = parse_apartments(apartments_text),
    single_family_record =
      stringr::str_detect(
        dplyr::coalesce(residence_use, ""),
        stringr::regex("^single", ignore_case = TRUE)
      ) |
      residence_type %in% c(
        "1 Story",
        "1.5 Story",
        "2 Story",
        "3 Story +",
        "Split Level"
      ),
    dwelling_units = dplyr::case_when(
      is.finite(parsed_apartments) &
        parsed_apartments > 0 ~ parsed_apartments,
      single_family_record ~ 1,
      TRUE ~ NA_real_
    )
  ) |>
  dplyr::arrange(
    project_id,
    component_pin,
    card_num,
    tax_year,
    row_id
  ) |>
  dplyr::group_by(
    project_id,
    component_pin,
    card_num,
    tax_year
  ) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

snapshots <- history |>
  dplyr::filter(
    is.finite(tax_year),
    is.finite(building_sqft),
    building_sqft > 0
  ) |>
  dplyr::group_by(project_id, tax_year) |>
  dplyr::summarise(
    observed_component_pins = dplyr::n_distinct(component_pin),
    observed_cards = dplyr::n_distinct(
      paste(component_pin, card_num)
    ),
    reported_year_values = paste(
      sort(unique(reported_year_built)),
      collapse = "/"
    ),
    presample_building_sqft = sum(building_sqft),
    presample_dwelling_units = dplyr::if_else(
      all(is.na(dwelling_units)),
      NA_real_,
      sum(dwelling_units, na.rm = TRUE)
    ),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    project_component_counts,
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    projects |>
      dplyr::select(
        project_id,
        current_building_sqft = building_sqft,
        current_dwelling_units = dwelling_units
      ),
    by = "project_id",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    complete_component_coverage =
      observed_component_pins == requested_component_pins,
    building_area_ratio =
      presample_building_sqft / current_building_sqft,
    dwelling_unit_ratio =
      presample_dwelling_units / current_dwelling_units,
    building_area_matches =
      is.finite(building_area_ratio) &
      dplyr::between(building_area_ratio, 0.9, 1.1),
    dwelling_units_match =
      is.finite(dwelling_unit_ratio) &
      dplyr::between(dwelling_unit_ratio, 0.9, 1.1),
    complete_physical_comparison =
      complete_component_coverage &
      is.finite(presample_building_sqft) &
      is.finite(presample_dwelling_units),
    comparison_distance = abs(log(building_area_ratio)) +
      abs(log(dwelling_unit_ratio))
  )

best_snapshot <- snapshots |>
  dplyr::filter(complete_physical_comparison) |>
  dplyr::group_by(project_id) |>
  dplyr::arrange(
    comparison_distance,
    tax_year,
    .by_group = TRUE
  ) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(
    project_id,
    presample_tax_year = tax_year,
    presample_reported_years = reported_year_values,
    presample_component_pins = observed_component_pins,
    presample_cards = observed_cards,
    presample_building_sqft,
    presample_dwelling_units,
    presample_building_area_ratio = building_area_ratio,
    presample_dwelling_unit_ratio = dwelling_unit_ratio,
    presample_building_area_matches = building_area_matches,
    presample_dwelling_units_match = dwelling_units_match
  )

evidence <- projects |>
  dplyr::left_join(
    best_snapshot,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    presample_structure_status = dplyr::case_when(
      presample_building_area_matches &
        presample_dwelling_units_match ~
        "presample_structure_matches_current",
      is.finite(presample_building_sqft) &
        (
          !presample_building_area_matches |
            !presample_dwelling_units_match
        ) ~ "presample_structure_differs_from_current",
      TRUE ~ "presample_physical_fields_incomplete"
    ),
    presample_same_structure_signal =
      presample_structure_status ==
      "presample_structure_matches_current",
    presample_replacement_signal =
      presample_structure_status ==
      "presample_structure_differs_from_current"
  )

summary <- evidence |>
  dplyr::filter(within_1500ft) |>
  dplyr::count(
    current_multifamily,
    within_500ft,
    presample_structure_status,
    name = "projects"
  ) |>
  dplyr::arrange(
    dplyr::desc(current_multifamily),
    presample_structure_status,
    within_500ft
  )

readr::write_csv(
  evidence,
  "../output/presample_assessor_structure_evidence.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/presample_assessor_structure_summary.csv",
  na = ""
)
