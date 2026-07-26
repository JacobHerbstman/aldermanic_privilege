# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_pin <- function(x) {
  value <- str_replace_all(str_squish(as.character(x)), "[^0-9]", "")
  if_else(value == "", NA_character_, value)
}

review_projects <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    all_lineage_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(review_projects$project_id) > 0) {
  stop("Residential review projects are not unique.", call. = FALSE)
}

project_pins <- review_projects %>%
  transmute(
    project_id,
    pin = coalesce(all_lineage_pins, component_pins)
  ) %>%
  tidyr::separate_longer_delim(pin, delim = "/") %>%
  mutate(pin = normalize_pin(pin)) %>%
  filter(!is.na(pin)) %>%
  distinct(project_id, pin)

requested_pins <- project_pins %>%
  distinct(pin)

if (anyDuplicated(project_pins$pin) > 0) {
  stop("A residential review PIN belongs to more than one candidate project.", call. = FALSE)
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbWriteTable(con, "requested_pins", requested_pins, overwrite = TRUE)

history <- DBI::dbGetQuery(con, "
  WITH source AS (
    SELECT
      regexp_replace(trim(pin), '[^0-9]', '', 'g') AS pin,
      try_cast(regexp_replace(cast(year AS VARCHAR), '[^0-9.-]', '', 'g') AS INTEGER) AS tax_year,
      try_cast(regexp_replace(cast(card AS VARCHAR), '[^0-9.-]', '', 'g') AS INTEGER) AS card_num,
      trim(class) AS class,
      try_cast(regexp_replace(cast(char_yrblt AS VARCHAR), '[^0-9.-]', '', 'g') AS INTEGER) AS year_built,
      trim(char_apts) AS apartments_text,
      try_cast(regexp_replace(cast(char_bldg_sf AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS building_sqft,
      try_cast(regexp_replace(cast(char_land_sf AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS land_sqft,
      trim(char_type_resd) AS type_of_residence,
      trim(char_use) AS single_v_multi_family,
      regexp_replace(trim(tieback_key_pin), '[^0-9]', '', 'g') AS tieback_group,
      try_cast(regexp_replace(cast(tieback_proration_rate AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS pin_proration_rate,
      try_cast(regexp_replace(cast(card_proration_rate AS VARCHAR), '[^0-9.-]', '', 'g') AS DOUBLE) AS card_proration_rate,
      trim(row_id) AS row_id
    FROM read_csv(
      '../input/residential_improvement_characteristics_full.csv',
      all_varchar = true,
      header = true,
      ignore_errors = false,
      max_line_size = 10000000
    )
    WHERE try_cast(regexp_replace(cast(township_code AS VARCHAR), '[^0-9.-]', '', 'g') AS INTEGER)
      IN (70, 71, 72, 73, 74, 75, 76, 77)
  )
  SELECT source.*
  FROM source
  INNER JOIN requested_pins USING (pin)
  ORDER BY pin, card_num, tax_year, row_id
") %>%
  as_tibble()

if (anyDuplicated(history$row_id) > 0) {
  stop("Residential review history row IDs are not unique.", call. = FALSE)
}

history <- project_pins %>%
  inner_join(history, by = "pin", relationship = "one-to-many") %>%
  arrange(project_id, pin, card_num, tax_year, row_id)

missing_pins <- requested_pins %>%
  anti_join(history %>% distinct(pin), by = "pin")

summary <- tibble::tibble(
  metric = c(
    "review_projects",
    "requested_component_pins",
    "component_pins_with_history",
    "component_pins_without_history",
    "assessor_history_rows"
  ),
  value = c(
    nrow(review_projects),
    nrow(requested_pins),
    n_distinct(history$pin),
    nrow(missing_pins),
    nrow(history)
  )
)

readr::write_csv(history, "../output/residential_review_assessor_history.csv")
readr::write_csv(missing_pins, "../output/residential_review_assessor_missing_pins.csv")
readr::write_csv(summary, "../output/residential_review_assessor_history_summary.csv")
