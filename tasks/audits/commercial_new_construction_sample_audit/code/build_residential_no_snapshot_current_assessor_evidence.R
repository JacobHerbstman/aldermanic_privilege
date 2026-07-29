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

review <- readr::read_csv(
  "../output/residential_tieback_no_snapshot_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  select(source_project_id, candidate_year)

links <- readr::read_csv(
  "../output/residential_review_current_parcel_links.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(project_id %in% review$source_project_id, class != "299") %>%
  transmute(source_project_id = project_id, pin, current_class = class)

if (anyDuplicated(review$source_project_id) > 0 ||
    anyDuplicated(links[c("source_project_id", "pin")]) > 0) {
  stop("Current-assessor evidence inputs violate their declared keys.", call. = FALSE)
}

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

pin_sql <- paste(DBI::dbQuoteString(connection, unique(links$pin)), collapse = ", ")
query <- paste0(
  "SELECT trim(pin) AS pin, trim(year) AS tax_year, trim(card) AS card_num, ",
  "trim(class) AS class, trim(char_yrblt) AS year_built, ",
  "trim(char_bldg_sf) AS building_sqft, trim(char_land_sf) AS land_sqft, ",
  "trim(char_apts) AS apartments_text, trim(char_type_resd) AS type_of_residence, ",
  "trim(char_use) AS single_v_multi_family, trim(row_id) AS row_id ",
  "FROM read_csv('../input/residential_improvement_characteristics_full.csv', ",
  "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) ",
  "WHERE trim(pin) IN (", pin_sql, ") ",
  "AND try_cast(regexp_replace(trim(year), '[^0-9.-]', '', 'g') AS INTEGER) <= 2025"
)

history <- DBI::dbGetQuery(connection, query) %>%
  as_tibble() %>%
  mutate(
    tax_year = as.integer(parse_numeric(tax_year)),
    card_num = as.integer(parse_numeric(card_num)),
    year_built = as.integer(parse_numeric(year_built)),
    building_sqft = parse_numeric(building_sqft),
    land_sqft = parse_numeric(land_sqft),
    dwelling_units = parse_units(apartments_text, single_v_multi_family, type_of_residence)
  ) %>%
  inner_join(links, by = "pin", relationship = "many-to-one")

if (anyDuplicated(history[c("source_project_id", "pin", "tax_year", "card_num")]) > 0) {
  stop("Current assessor history is not unique by project-PIN-year-card.", call. = FALSE)
}

selected_years <- history %>%
  group_by(source_project_id) %>%
  summarise(
    selected_tax_year = if_else(
      any(tax_year <= 2022),
      max(tax_year[tax_year <= 2022]),
      max(tax_year)
    ),
    .groups = "drop"
  )

selected_rows <- history %>%
  inner_join(
    selected_years,
    by = c("source_project_id", "tax_year" = "selected_tax_year"),
    relationship = "many-to-one"
  ) %>%
  arrange(source_project_id, pin, card_num)

physical_cards <- selected_rows %>%
  mutate(
    card_signature = paste(
      card_num,
      coalesce(as.character(building_sqft), "missing"),
      coalesce(as.character(dwelling_units), "missing"),
      coalesce(type_of_residence, "missing"),
      coalesce(single_v_multi_family, "missing"),
      sep = "|"
    )
  ) %>%
  group_by(source_project_id, card_signature) %>%
  summarise(
    source_pins = paste(sort(unique(pin)), collapse = "/"),
    card_num = first(card_num),
    card_reported_years = paste(sort(unique(year_built[is.finite(year_built)])), collapse = "/"),
    building_sqft = first(building_sqft),
    dwelling_units = first(dwelling_units),
    .groups = "drop"
  )

summary <- physical_cards %>%
  group_by(source_project_id) %>%
  summarise(
    physical_cards = n(),
    reported_years = paste(sort(unique(unlist(str_split(card_reported_years, fixed("/"))))), collapse = "/"),
    building_sqft_sum = sum(building_sqft, na.rm = TRUE),
    dwelling_units_sum = sum(dwelling_units, na.rm = TRUE),
    card_evidence = paste0(
      "card", card_num,
      " pins=", source_pins,
      "; years=", coalesce(card_reported_years, "missing"),
      "; building_sqft=", coalesce(as.character(building_sqft), "missing"),
      "; units=", coalesce(as.character(dwelling_units), "missing"),
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  left_join(
    selected_rows %>%
      distinct(source_project_id, pin, land_sqft) %>%
      group_by(source_project_id) %>%
      summarise(
        current_pins = n_distinct(pin),
        current_land_sqft = sum(land_sqft, na.rm = TRUE),
        pins_with_land = n_distinct(pin[is.finite(land_sqft) & land_sqft > 0]),
        .groups = "drop"
      ),
    by = "source_project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(selected_years, by = "source_project_id", relationship = "one-to-one") %>%
  left_join(review, by = "source_project_id", relationship = "one-to-one") %>%
  arrange(source_project_id)

readr::write_csv(
  selected_rows,
  "../output/residential_tieback_no_snapshot_current_assessor_rows.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_no_snapshot_current_assessor_summary.csv"
)
