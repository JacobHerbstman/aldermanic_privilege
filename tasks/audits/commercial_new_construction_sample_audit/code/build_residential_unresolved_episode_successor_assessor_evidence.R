# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

links <- readr::read_csv(
  "../output/residential_unresolved_episode_successor_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    episode_id = readr::col_character(),
    project_id = readr::col_character(),
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(inside_episode_polygon | distance_to_episode_polygon_ft <= 25) %>%
  distinct(episode_id, pin, .keep_all = TRUE)

if (anyDuplicated(links[c("episode_id", "pin")]) > 0) {
  stop("Episode-successor parcel links are not unique.", call. = FALSE)
}

requested_pins <- links %>%
  distinct(pin)

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
duckdb::duckdb_register(connection, "requested_pins", requested_pins)

assessor_history <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT trim(r.pin) AS pin, try_cast(r.year AS INTEGER) AS tax_year, ",
    "trim(r.card) AS card_num, trim(r.class) AS property_class, ",
    "try_cast(r.char_yrblt AS DOUBLE) AS year_built, ",
    "try_cast(r.char_bldg_sf AS DOUBLE) AS building_sqft, ",
    "try_cast(r.char_land_sf AS DOUBLE) AS land_sqft, ",
    "trim(r.char_apts) AS apartments_text, trim(r.char_type_resd) AS residence_type, ",
    "trim(r.char_use) AS residence_use, trim(r.row_id) AS row_id ",
    "FROM read_csv('../input/residential_improvement_characteristics_full.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) r ",
    "JOIN requested_pins p ON trim(r.pin) = p.pin"
  )
) %>%
  as_tibble() %>%
  mutate(
    apartment_value = suppressWarnings(readr::parse_number(apartments_text)),
    dwelling_units = case_when(
      property_class %in% c("211", "212") &
        is.finite(apartment_value) & apartment_value > 0 ~ apartment_value,
      property_class %in% c("202", "203", "204", "205", "206", "207", "208", "209", "210", "234", "278", "295") ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  arrange(pin, tax_year, card_num, row_id)

if (anyDuplicated(assessor_history$row_id) > 0) {
  stop("Episode-successor assessor evidence contains duplicate source row IDs.", call. = FALSE)
}

parcel_addresses <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT trim(a.pin) AS pin, string_agg(DISTINCT trim(a.prop_address_full), '/') AS parcel_addresses ",
    "FROM read_csv('../input/parcel_addresses_2025_chicago.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) a ",
    "JOIN requested_pins p ON trim(a.pin) = p.pin ",
    "GROUP BY trim(a.pin)"
  )
) %>%
  as_tibble()

history_by_pin <- assessor_history %>%
  group_by(pin) %>%
  summarise(assessor_rows = list(pick(everything())), .groups = "drop")

evidence <- links %>%
  left_join(parcel_addresses, by = "pin", relationship = "many-to-one") %>%
  left_join(history_by_pin, by = "pin", relationship = "many-to-one") %>%
  tidyr::unnest(assessor_rows, keep_empty = TRUE) %>%
  arrange(project_id, target_year, pin, tax_year, card_num, row_id)

latest_evidence <- evidence %>%
  filter(!is.na(tax_year), tax_year <= 2025) %>%
  group_by(episode_id, pin, card_num) %>%
  filter(tax_year == max(tax_year)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  arrange(project_id, target_year, pin, card_num)

summary <- tibble::tibble(
  metric = c(
    "episode_successor_links",
    "distinct_successor_pins",
    "successor_pins_with_assessor_history",
    "assessor_history_rows",
    "latest_episode_pin_card_rows"
  ),
  value = c(
    nrow(links),
    nrow(requested_pins),
    n_distinct(assessor_history$pin),
    nrow(assessor_history),
    nrow(latest_evidence)
  )
)

readr::write_csv(
  evidence,
  "../output/residential_unresolved_episode_successor_assessor_history.csv"
)
readr::write_csv(
  latest_evidence,
  "../output/residential_unresolved_episode_successor_assessor_latest.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_episode_successor_assessor_summary.csv"
)
