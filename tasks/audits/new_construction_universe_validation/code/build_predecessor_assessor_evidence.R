# setwd("tasks/audits/new_construction_universe_validation/code")

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
  show_col_types = FALSE
) |>
  dplyr::filter(source_family == "residential") |>
  dplyr::select(
    project_id,
    construction_year,
    dwelling_units,
    building_sqft,
    within_500ft,
    within_1500ft,
    current_multifamily
  )

predecessors <- readr::read_csv(
  "../input/preferred_historical_predecessor_resolution.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    predecessor_pin14 = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(
    source_family == "residential",
    !is.na(predecessor_pin14),
    predecessor_status %in% c(
      "unique_predecessor_polygon",
      "equivalent_predecessor_geometry"
    )
  ) |>
  dplyr::select(
    project_id,
    component_pin,
    predecessor_pin14,
    predecessor_status
  ) |>
  dplyr::distinct()

requests <- projects |>
  dplyr::inner_join(
    predecessors,
    by = "project_id",
    relationship = "one-to-many"
  ) |>
  dplyr::select(
    project_id,
    predecessor_pin14,
    construction_year
  ) |>
  dplyr::distinct()

if (anyDuplicated(requests[c("project_id", "predecessor_pin14")])) {
  stop("Predecessor assessor requests are not unique.")
}

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(
  DBI::dbDisconnect(connection, shutdown = TRUE),
  add = TRUE
)

DBI::dbWriteTable(
  connection,
  "requested_predecessors",
  requests,
  temporary = TRUE,
  overwrite = TRUE
)

history <- DBI::dbGetQuery(
  connection,
  paste(
    "SELECT",
    "r.project_id,",
    "r.construction_year,",
    "h.pin,",
    "TRY_CAST(h.year AS INTEGER) AS tax_year,",
    "CAST(h.card AS VARCHAR) AS card,",
    "CAST(h.class AS VARCHAR) AS class,",
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
    "INNER JOIN requested_predecessors r",
    "ON h.pin = r.predecessor_pin14",
    "WHERE TRY_CAST(h.year AS INTEGER) < r.construction_year"
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
  )

latest_cards <- history |>
  dplyr::filter(
    is.finite(tax_year),
    is.finite(building_sqft),
    building_sqft > 0
  ) |>
  dplyr::group_by(project_id, pin) |>
  dplyr::filter(tax_year == max(tax_year)) |>
  dplyr::distinct(project_id, pin, tax_year, card, .keep_all = TRUE) |>
  dplyr::ungroup()

predecessor_evidence <- latest_cards |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    predecessor_pins_with_structures = dplyr::n_distinct(pin),
    predecessor_latest_tax_year = max(tax_year),
    predecessor_reported_years = paste(
      sort(unique(reported_year_built[is.finite(reported_year_built)])),
      collapse = "/"
    ),
    predecessor_building_sqft = sum(building_sqft, na.rm = TRUE),
    predecessor_dwelling_units = sum(
      dplyr::if_else(
        is.finite(dwelling_units) & dwelling_units > 0,
        dwelling_units,
        1
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

evidence <- projects |>
  dplyr::left_join(
    predecessors |>
      dplyr::group_by(project_id) |>
      dplyr::summarise(
        resolved_predecessor_pins = dplyr::n_distinct(predecessor_pin14),
        predecessor_statuses = paste(
          sort(unique(predecessor_status)),
          collapse = "/"
        ),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    predecessor_evidence,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    has_predecessor_structure =
      dplyr::coalesce(predecessor_pins_with_structures > 0, FALSE),
    predecessor_area_ratio = predecessor_building_sqft / building_sqft,
    predecessor_unit_ratio = predecessor_dwelling_units / dwelling_units,
    predecessor_matches_current_area =
      is.finite(predecessor_area_ratio) &
      dplyr::between(predecessor_area_ratio, 0.9, 1.1),
    predecessor_matches_current_units =
      is.finite(predecessor_unit_ratio) &
      dplyr::between(predecessor_unit_ratio, 0.9, 1.1),
    predecessor_same_structure_signal =
      has_predecessor_structure &
      predecessor_matches_current_area &
      predecessor_matches_current_units,
    predecessor_replacement_signal =
      has_predecessor_structure &
      !predecessor_same_structure_signal &
      (
        !predecessor_matches_current_area |
          !predecessor_matches_current_units
      ),
    predecessor_evidence_status = dplyr::case_when(
      predecessor_same_structure_signal ~
        "predecessor_matches_current_structure",
      predecessor_replacement_signal ~
        "predecessor_differs_from_current_structure",
      resolved_predecessor_pins > 0 & !has_predecessor_structure ~
        "resolved_predecessor_without_assessor_structure",
      TRUE ~ "no_resolved_predecessor_evidence"
    )
  )

summary <- evidence |>
  dplyr::filter(within_1500ft) |>
  dplyr::count(
    current_multifamily,
    predecessor_evidence_status,
    within_500ft,
    name = "projects"
  ) |>
  dplyr::arrange(
    dplyr::desc(current_multifamily),
    predecessor_evidence_status,
    within_500ft
  )

readr::write_csv(
  evidence,
  "../output/predecessor_assessor_evidence.csv",
  na = ""
)
readr::write_csv(
  summary,
  "../output/predecessor_assessor_evidence_summary.csv",
  na = ""
)
