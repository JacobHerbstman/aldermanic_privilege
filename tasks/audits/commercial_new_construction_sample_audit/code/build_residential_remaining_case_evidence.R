# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    source_row_ids = readr::col_character(),
    .default = readr::col_guess()
  )
)

handled_projects <- bind_rows(
  readr::read_csv(
    "../output/residential_class297_source_disposition.csv",
    show_col_types = FALSE,
    col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
  ) %>% select(project_id = source_project_id),
  readr::read_csv(
    "../output/residential_overlap_resolution.csv",
    show_col_types = FALSE,
    col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
  ) %>% select(project_id = source_project_id),
  readr::read_csv(
    "../output/residential_tieback_episode_resolution.csv",
    show_col_types = FALSE,
    col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
  ) %>% select(project_id = source_project_id),
  readr::read_csv(
    "../output/residential_tieback_no_snapshot_resolution.csv",
    show_col_types = FALSE,
    col_types = readr::cols(source_project_id = readr::col_character(), .default = readr::col_guess())
  ) %>% select(project_id = source_project_id)
) %>%
  distinct(project_id)

review <- review %>%
  anti_join(handled_projects, by = "project_id") %>%
  filter(project_kind %in% c("same_pin_multiple_cards", "single_pin_single_card"))

if (nrow(review) != 7 ||
    sum(review$project_kind == "same_pin_multiple_cards") != 4 ||
    sum(review$project_kind == "single_pin_single_card") != 3 ||
    anyDuplicated(review$project_id) > 0) {
  stop("The remaining residential review scope is not the expected four multicard and three ordinary projects.", call. = FALSE)
}

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
  semi_join(review, by = "project_id")

addresses <- readr::read_csv(
  "../input/parcel_addresses_2025_chicago.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_character())
) %>%
  select(pin, prop_address_full) %>%
  inner_join(links %>% select(project_id, pin), by = "pin", relationship = "many-to-one") %>%
  mutate(
    normalized_address = prop_address_full %>%
      str_to_upper() %>%
      str_squish() %>%
      str_remove(" [0-9]+$")
  ) %>%
  distinct(project_id, pin, prop_address_full, normalized_address)

address_keys <- addresses %>% distinct(project_id, normalized_address)

if (anyDuplicated(addresses[c("project_id", "pin")]) > 0 ||
    anyDuplicated(address_keys$normalized_address) > 0) {
  stop("Current parcel addresses are missing a unique project assignment.", call. = FALSE)
}

noncondo_links <- links %>%
  filter(!class %in% c("100", "299")) %>%
  select(project_id, pin)

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

pin_sql <- paste(
  DBI::dbQuoteString(connection, unique(noncondo_links$pin)),
  collapse = ", "
)

current_rows <- DBI::dbGetQuery(
  connection,
  paste0(
    "SELECT trim(pin) AS pin, trim(year) AS tax_year, trim(card) AS card_num, ",
    "trim(class) AS property_class, trim(char_yrblt) AS year_built, ",
    "trim(char_bldg_sf) AS building_sqft, trim(char_land_sf) AS land_sqft, ",
    "trim(char_apts) AS apartments_text, trim(char_type_resd) AS residence_type, ",
    "trim(char_use) AS residence_use, trim(row_id) AS row_id ",
    "FROM read_csv('../input/residential_improvement_characteristics_full.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) ",
    "WHERE trim(pin) IN (", pin_sql, ") ",
    "AND try_cast(regexp_replace(trim(year), '[^0-9.-]', '', 'g') AS INTEGER) <= 2025"
  )
) %>%
  as_tibble() %>%
  mutate(
    across(c(tax_year, card_num, year_built, building_sqft, land_sqft),
           ~ suppressWarnings(as.numeric(str_replace_all(.x, "[^0-9.-]", "")))),
    apartment_value = suppressWarnings(as.numeric(str_replace_all(apartments_text, "[^0-9.-]", ""))),
    dwelling_units = case_when(
      is.finite(apartment_value) & apartment_value > 0 ~ apartment_value,
      str_to_lower(apartments_text) == "one" ~ 1,
      str_to_lower(apartments_text) == "two" ~ 2,
      str_to_lower(apartments_text) == "three" ~ 3,
      str_detect(residence_use, regex("^single", ignore_case = TRUE)) ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  inner_join(noncondo_links, by = "pin", relationship = "many-to-one")

selected_current_rows <- current_rows %>%
  group_by(project_id, pin) %>%
  filter(tax_year == max(tax_year)) %>%
  ungroup()

if (anyDuplicated(selected_current_rows[c("project_id", "pin", "card_num")]) > 0 ||
    any(!noncondo_links$pin %in% selected_current_rows$pin)) {
  stop("Current successor assessor rows are missing or nonunique.", call. = FALSE)
}

current_summary <- selected_current_rows %>%
  group_by(project_id) %>%
  summarise(
    current_noncondo_pins = n_distinct(pin),
    current_assessor_years = paste(sort(unique(year_built)), collapse = "/"),
    current_assessor_units = sum(dwelling_units),
    current_assessor_building_sqft = sum(building_sqft),
    current_assessor_land_sqft = sum(land_sqft),
    current_assessor_rows = paste(sort(row_id), collapse = "/"),
    .groups = "drop"
  )

condo_rows <- readr::read_csv(
  "../output/residential_successor_condo_evidence.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    year = readr::col_double(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(
    links %>% filter(class == "299") %>% select(project_id, pin10) %>% distinct(),
    by = "pin10",
    relationship = "many-to-one"
  ) %>%
  filter(year <= 2022, !is_parking_space, !is_common_area) %>%
  group_by(project_id, pin10) %>%
  filter(year == max(year)) %>%
  ungroup()

condo_summary <- condo_rows %>%
  group_by(project_id) %>%
  summarise(
    successor_condo_bases = paste(sort(unique(pin10)), collapse = "/"),
    successor_condo_units = n_distinct(pin),
    successor_condo_building_values = n_distinct(char_building_sf),
    successor_condo_building_sqft = max(char_building_sf),
    successor_condo_land_values = n_distinct(char_land_sf),
    successor_condo_land_sqft = max(char_land_sf),
    successor_condo_rows = paste(sort(row_id), collapse = "/"),
    .groups = "drop"
  )

geometry <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  semi_join(review, by = "project_id") %>%
  sf::st_drop_geometry() %>%
  select(project_id, project_polygon_valid, project_land_area_sqft)

permits <- sf::st_read(
  "../input/building_permits_clean.gpkg",
  quiet = TRUE
) %>%
  sf::st_drop_geometry() %>%
  as_tibble() %>%
  mutate(
    normalized_address = str_squish(str_to_upper(paste(street_number, street_direction, street_name)))
  ) %>%
  inner_join(
    address_keys,
    by = "normalized_address",
    relationship = "many-to-one"
  ) %>%
  inner_join(
    review %>% select(project_id, construction_year),
    by = "project_id",
    relationship = "many-to-one"
  ) %>%
  filter(
    year(application_start_date) >= construction_year - 5,
    year(application_start_date) <= construction_year + 5,
    str_detect(permit_type, regex("NEW CONSTRUCTION", ignore_case = TRUE))
  ) %>%
  distinct(project_id, permit, .keep_all = TRUE) %>%
  arrange(project_id, application_start_date, permit)

permit_summary <- permits %>%
  group_by(project_id) %>%
  summarise(
    address_new_construction_permits = paste0(
      permit,
      " [", coalesce(permit_status, "missing status"), "] ",
      application_start_date,
      "; ", normalized_address,
      "; ", str_squish(work_description),
      collapse = " || "
    ),
    .groups = "drop"
  )

spatial_permits <- readr::read_csv(
  "../output/new_construction_spatial_permit_matches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  semi_join(review, by = "project_id") %>%
  arrange(project_id, application_date, permit_number) %>%
  group_by(project_id) %>%
  summarise(
    spatial_permit_evidence = paste0(
      permit_number,
      " [", coalesce(permit_status, "missing status"), "] ",
      application_date,
      "; ", permit_address,
      "; ", str_squish(work_description),
      collapse = " || "
    ),
    .groups = "drop"
  )

evidence <- review %>%
  select(
    source_project_id = project_id,
    project_kind,
    component_pins,
    construction_year,
    dwelling_units,
    building_sqft,
    land_sqft,
    source_row_ids,
    distance_to_boundary_ft,
    component_history_evidence,
    multicard_evidence,
    permit_chain_evidence,
    permit_unit_evidence,
    city_footprint_evidence
  ) %>%
  left_join(
    addresses %>%
      group_by(project_id) %>%
      summarise(
        current_addresses = paste(sort(unique(prop_address_full)), collapse = "/"),
        .groups = "drop"
      ),
    by = c("source_project_id" = "project_id"),
    relationship = "one-to-one"
  ) %>%
  left_join(current_summary, by = c("source_project_id" = "project_id"), relationship = "one-to-one") %>%
  left_join(condo_summary, by = c("source_project_id" = "project_id"), relationship = "one-to-one") %>%
  left_join(geometry, by = c("source_project_id" = "project_id"), relationship = "one-to-one") %>%
  left_join(permit_summary, by = c("source_project_id" = "project_id"), relationship = "one-to-one") %>%
  left_join(spatial_permits, by = c("source_project_id" = "project_id"), relationship = "one-to-one") %>%
  arrange(source_project_id)

if (nrow(evidence) != 7 ||
    anyDuplicated(evidence$source_project_id) > 0 ||
    any(!evidence$project_polygon_valid) ||
    any(!is.finite(evidence$project_land_area_sqft))) {
  stop("Remaining-case evidence is incomplete or nonunique.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "remaining_projects",
    "multicard_projects",
    "ordinary_projects",
    "projects_with_current_noncondo_evidence",
    "projects_with_successor_condo_evidence",
    "projects_with_address_permit_evidence",
    "projects_with_spatial_permit_evidence",
    "projects_with_valid_construction_year_geometry"
  ),
  value = c(
    nrow(evidence),
    sum(evidence$project_kind == "same_pin_multiple_cards"),
    sum(evidence$project_kind == "single_pin_single_card"),
    sum(!is.na(evidence$current_noncondo_pins)),
    sum(!is.na(evidence$successor_condo_bases)),
    sum(!is.na(evidence$address_new_construction_permits)),
    sum(!is.na(evidence$spatial_permit_evidence)),
    sum(evidence$project_polygon_valid)
  )
)

readr::write_csv(evidence, "../output/residential_remaining_case_evidence.csv")
readr::write_csv(selected_current_rows, "../output/residential_remaining_current_assessor_rows.csv")
readr::write_csv(permits, "../output/residential_remaining_address_permits.csv")
readr::write_csv(summary, "../output/residential_remaining_case_evidence_summary.csv")
