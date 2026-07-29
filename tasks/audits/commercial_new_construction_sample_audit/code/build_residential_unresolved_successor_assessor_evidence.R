# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

parse_numeric <- function(x) {
  suppressWarnings(as.numeric(str_replace_all(as.character(x), "[^0-9.-]", "")))
}

parse_units <- function(x, single_family, residence_type) {
  text <- str_to_lower(str_squish(x))
  units <- case_when(
    is.na(text) | text == "" ~ NA_real_,
    text %in% c("none", "zero") ~ 0,
    text == "one" ~ 1,
    text == "two" ~ 2,
    text == "three" ~ 3,
    text == "four" ~ 4,
    text == "five" ~ 5,
    text == "six" ~ 6,
    TRUE ~ suppressWarnings(as.numeric(str_replace_all(text, "[^0-9.-]", "")))
  )

  case_when(
    is.finite(units) & units > 0 ~ units,
    str_detect(single_family, regex("^single", ignore_case = TRUE)) |
      residence_type %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level") ~ 1,
    TRUE ~ NA_real_
  )
}

candidates <- readr::read_csv(
  "../output/residential_unresolved_successor_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    class != "299",
    exact_address_match |
      (is.finite(minimum_point_distance_ft) & minimum_point_distance_ft <= 50)
  ) %>%
  distinct(project_id, pin, .keep_all = TRUE)

if (anyDuplicated(candidates[c("project_id", "pin")]) > 0) {
  stop("Non-condo successor candidates are not unique by project and PIN.", call. = FALSE)
}

requested_pins <- candidates %>%
  distinct(pin)

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

duckdb::duckdb_register(connection, "requested_pins", requested_pins)

history <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT trim(r.pin) AS pin, trim(r.year) AS tax_year, trim(r.card) AS card_num, ",
    "trim(r.class) AS property_class, trim(r.char_yrblt) AS year_built, ",
    "trim(r.char_bldg_sf) AS building_sqft, trim(r.char_land_sf) AS land_sqft, ",
    "trim(r.char_apts) AS apartments_text, trim(r.char_type_resd) AS residence_type, ",
    "trim(r.char_use) AS residence_use, trim(r.row_id) AS row_id ",
    "FROM read_csv('../input/residential_improvement_characteristics_full.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) r ",
    "JOIN requested_pins p ON trim(r.pin) = p.pin ",
    "WHERE try_cast(regexp_replace(trim(r.year), '[^0-9.-]', '', 'g') AS INTEGER) <= 2025"
  )
) %>%
  as_tibble() %>%
  mutate(
    tax_year = as.integer(parse_numeric(tax_year)),
    card_num = as.integer(parse_numeric(card_num)),
    year_built = as.integer(parse_numeric(year_built)),
    building_sqft = parse_numeric(building_sqft),
    land_sqft = parse_numeric(land_sqft),
    dwelling_units = parse_units(apartments_text, residence_use, residence_type)
  )

if (anyDuplicated(history[c("pin", "tax_year", "card_num")]) > 0) {
  stop("Successor assessor history is not unique by PIN, year, and card.", call. = FALSE)
}

history_by_pin <- history %>%
  tidyr::nest(assessor_rows = -pin)

project_history <- candidates %>%
  left_join(history_by_pin, by = "pin", relationship = "many-to-one") %>%
  tidyr::unnest(assessor_rows, keep_empty = TRUE) %>%
  arrange(project_id, pin, tax_year, card_num)

project_summary <- candidates %>%
  distinct(project_id) %>%
  left_join(
    project_history %>%
      group_by(project_id) %>%
      summarise(
        candidate_pins = n_distinct(pin),
        candidate_pins_with_assessor_history = n_distinct(pin[!is.na(row_id)]),
        assessor_history_rows = sum(!is.na(row_id)),
        reported_years = paste(sort(unique(year_built[is.finite(year_built)])), collapse = "/"),
        latest_tax_year = {
          finite_years <- tax_year[is.finite(tax_year)]
          if (length(finite_years) > 0) max(finite_years) else NA_integer_
        },
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(across(c(candidate_pins, candidate_pins_with_assessor_history, assessor_history_rows), ~ replace_na(.x, 0))) %>%
  arrange(project_id)

summary <- tibble::tibble(
  metric = c(
    "projects_with_noncondo_successor_candidates",
    "distinct_noncondo_successor_pins",
    "successor_pins_with_assessor_history",
    "assessor_history_rows",
    "projects_with_assessor_history"
  ),
  value = c(
    n_distinct(candidates$project_id),
    nrow(requested_pins),
    n_distinct(history$pin),
    nrow(history),
    sum(project_summary$candidate_pins_with_assessor_history > 0)
  )
)

readr::write_csv(
  project_history,
  "../output/residential_unresolved_successor_assessor_history.csv"
)
readr::write_csv(
  project_summary,
  "../output/residential_unresolved_successor_assessor_projects.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_successor_assessor_summary.csv"
)
