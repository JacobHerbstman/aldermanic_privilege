# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/project_verification_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    construction_year = readr::col_double(),
    .default = readr::col_skip()
  )
)

project_pins <- projects |>
  dplyr::select(project_id, construction_year, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::transmute(
    project_id,
    selected_construction_year = construction_year,
    component_pin = stringr::str_replace_all(component_pins, "[^0-9]", "")
  ) |>
  dplyr::filter(nchar(component_pin) == 14L) |>
  dplyr::distinct()

if (
  nrow(projects) != 795L ||
    anyDuplicated(projects$project_id) ||
    anyDuplicated(project_pins[c("project_id", "component_pin")])
) {
  stop("The Assessor-history review scope failed validation.")
}

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

DBI::dbWriteTable(
  connection,
  "project_pins",
  project_pins,
  temporary = TRUE,
  overwrite = TRUE
)

history <- DBI::dbGetQuery(
  connection,
  "
  WITH assessor AS (
    SELECT
      regexp_replace(trim(pin), '[^0-9]', '', 'g') AS component_pin,
      try_cast(regexp_replace(year, '[^0-9.-]', '', 'g') AS INTEGER)
        AS tax_year,
      try_cast(regexp_replace(card, '[^0-9.-]', '', 'g') AS INTEGER)
        AS card_num,
      try_cast(regexp_replace(char_yrblt, '[^0-9.-]', '', 'g') AS INTEGER)
        AS reported_year_built,
      try_cast(regexp_replace(char_bldg_sf, '[^0-9.-]', '', 'g') AS DOUBLE)
        AS building_sqft,
      try_cast(regexp_replace(char_land_sf, '[^0-9.-]', '', 'g') AS DOUBLE)
        AS land_sqft,
      trim(char_apts) AS apartments,
      trim(char_use) AS building_use,
      trim(char_type_resd) AS residence_type,
      trim(class) AS assessor_class
    FROM read_csv(
      '../input/residential_improvement_characteristics_full.csv',
      all_varchar = true,
      header = true,
      ignore_errors = false,
      max_line_size = 10000000
    )
    WHERE try_cast(
      regexp_replace(township_code, '[^0-9.-]', '', 'g') AS INTEGER
    ) IN (70, 71, 72, 73, 74, 75, 76, 77)
  )
  SELECT
    p.project_id,
    p.selected_construction_year,
    p.component_pin,
    a.tax_year,
    a.card_num,
    a.reported_year_built,
    a.building_sqft,
    a.land_sqft,
    a.apartments,
    a.building_use,
    a.residence_type,
    a.assessor_class
  FROM project_pins p
  INNER JOIN assessor a USING (component_pin)
  ORDER BY p.project_id, a.tax_year, a.card_num
  "
) |>
  tibble::as_tibble()

if (anyDuplicated(history[c("project_id", "component_pin", "tax_year", "card_num")])) {
  stop("Assessor-history rows are not unique by project, PIN, tax year, and card.")
}

history_summary <- history |>
  dplyr::group_by(project_id, selected_construction_year) |>
  dplyr::summarise(
    assessor_rows = dplyr::n(),
    first_tax_year = min(tax_year, na.rm = TRUE),
    last_tax_year = max(tax_year, na.rm = TRUE),
    reported_year_values = paste(
      sort(unique(reported_year_built[is.finite(reported_year_built)])),
      collapse = "/"
    ),
    selected_year_first_reported = suppressWarnings(min(
      tax_year[reported_year_built == selected_construction_year],
      na.rm = TRUE
    )),
    selected_year_last_reported = suppressWarnings(max(
      tax_year[reported_year_built == selected_construction_year],
      na.rm = TRUE
    )),
    building_sqft_values = paste(
      sort(unique(building_sqft[is.finite(building_sqft)])),
      collapse = "/"
    ),
    apartment_values = paste(
      sort(unique(apartments[!is.na(apartments) & apartments != ""])),
      collapse = "/"
    ),
    construction_year_revised =
      dplyr::n_distinct(reported_year_built[is.finite(reported_year_built)]) > 1L,
    physical_fields_revised =
      dplyr::n_distinct(building_sqft[is.finite(building_sqft)]) > 1L |
      dplyr::n_distinct(apartments[!is.na(apartments) & apartments != ""]) > 1L,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    selected_year_first_reported = dplyr::if_else(
      is.infinite(selected_year_first_reported),
      NA_real_,
      selected_year_first_reported
    ),
    selected_year_last_reported = dplyr::if_else(
      is.infinite(selected_year_last_reported),
      NA_real_,
      selected_year_last_reported
    )
  ) |>
  dplyr::right_join(
    projects |>
      dplyr::transmute(
        project_id,
        selected_construction_year = construction_year
      ),
    by = c("project_id", "selected_construction_year"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    assessor_history_available = !is.na(assessor_rows)
  )

if (
  nrow(history_summary) != 795L ||
    anyDuplicated(history_summary$project_id)
) {
  stop("Assessor-history summary does not cover all 795 projects.")
}

readr::write_csv(
  history,
  "../output/assessor_history_review.csv",
  na = ""
)
readr::write_csv(
  history_summary,
  "../output/assessor_history_review_summary.csv",
  na = ""
)
