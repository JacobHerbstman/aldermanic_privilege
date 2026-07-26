# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

invisible(DBI::dbExecute(con, "
CREATE TEMP TABLE production_residential AS
SELECT trim(pin) AS pin
FROM read_csv(
  '../input/residential_cross_section.csv',
  all_varchar = true,
  header = true
);
"))

history <- DBI::dbGetQuery(con, "
WITH source AS (
  SELECT
    trim(r.pin) AS pin,
    try_cast(numeric_text(r.year) AS INTEGER) AS tax_year,
    try_cast(numeric_text(r.card) AS INTEGER) AS card_num,
    try_cast(numeric_text(r.char_yrblt) AS INTEGER) AS year_built,
    try_cast(numeric_text(r.char_bldg_sf) AS DOUBLE) AS building_sqft,
    try_cast(numeric_text(r.char_land_sf) AS DOUBLE) AS land_sqft,
    CASE
      WHEN lower(trim(r.char_apts)) IN ('none', 'zero') THEN 0
      WHEN lower(trim(r.char_apts)) = 'one' THEN 1
      WHEN lower(trim(r.char_apts)) = 'two' THEN 2
      WHEN lower(trim(r.char_apts)) = 'three' THEN 3
      WHEN lower(trim(r.char_apts)) = 'four' THEN 4
      WHEN lower(trim(r.char_apts)) = 'five' THEN 5
      WHEN lower(trim(r.char_apts)) = 'six' THEN 6
      ELSE try_cast(numeric_text(r.char_apts) AS INTEGER)
    END AS apartments,
    trim(r.char_use) AS building_use,
    trim(r.char_type_resd) AS residence_type,
    trim(r.row_id) AS row_id
  FROM read_csv(
    '../input/residential_improvement_characteristics_full.csv',
    all_varchar = true,
    header = true,
    ignore_errors = true,
    max_line_size = 10000000
  ) r
  INNER JOIN production_residential p ON trim(r.pin) = p.pin
  WHERE try_cast(numeric_text(r.township_code) AS INTEGER) IN (70, 71, 72, 73, 74, 75, 76, 77)
    AND try_cast(numeric_text(r.card) AS INTEGER) IS NOT NULL
),
deduplicated AS (
  SELECT *,
    row_number() OVER (
      PARTITION BY pin, card_num, tax_year
      ORDER BY row_id DESC
    ) AS row_priority
  FROM source
)
SELECT * EXCLUDE (row_priority)
FROM deduplicated
WHERE row_priority = 1
ORDER BY pin, card_num, tax_year, row_id;
") %>%
  as_tibble() %>%
  group_by(pin) %>%
  mutate(cards_in_history = n_distinct(card_num)) %>%
  ungroup()

production <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), class = readr::col_character(), .default = readr::col_guess())
) %>%
  transmute(
    pin,
    production_tax_year = as.integer(tax_year),
    production_card_num = as.integer(card_num),
    production_yearbuilt = as.integer(year_built),
    production_bldgsf = as.numeric(building_sqft),
    production_landsf = as.numeric(land_sqft),
    production_units = as.numeric(num_apartments),
    production_class = class
  )

latest_endpoint <- history %>%
  filter(cards_in_history == 1) %>%
  mutate(
    endpoint_priority = case_when(
      tax_year <= 2022 ~ 1L,
      tax_year <= 2025 ~ 2L,
      TRUE ~ 3L
    )
  ) %>%
  arrange(
    pin, endpoint_priority, desc(tax_year), desc(building_sqft),
    desc(year_built), desc(row_id)
  ) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    pin,
    endpoint_tax_year = tax_year,
    endpoint_yearbuilt = year_built,
    endpoint_bldgsf = building_sqft,
    endpoint_landsf = land_sqft,
    endpoint_units = apartments,
    endpoint_building_use = building_use,
    endpoint_residence_type = residence_type
  )

analysis <- readr::read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), segment_id = readr::col_character(), .default = readr::col_guess())
) %>%
  transmute(
    pin,
    in_500ft_source_sample =
      arealotsf > 1 &
      areabuilding > 1 &
      unitscount > 0 &
      construction_year >= 2006 &
      construction_year <= 2022 &
      dist_to_boundary_m <= 152.4,
    in_main_model_inputs =
      in_500ft_source_sample &
      !is.na(ward_pair) &
      is.finite(signed_distance_m) &
      !is.na(construction_zone_group) &
      !is.na(segment_id) &
      segment_id != "" &
      is.finite(density_far) & density_far > 0 &
      is.finite(density_dupac) & density_dupac > 0 &
      is.finite(strictness_own) &
      is.finite(strictness_neighbor) &
      if_all(
        c(
          share_white_own, share_black_own, median_hh_income_own,
          share_bach_plus_own, homeownership_rate_own
        ),
        is.finite
      )
  )

permits <- sf::st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  sf::st_drop_geometry() %>%
  transmute(
    permit_id = as.character(id),
    permit_pin = as.character(pin),
    permit_type,
    permit_issued = as.integer(permit_issued),
    permit_status,
    application_date = as.Date(application_start_date)
  ) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    permit_issued == 1,
    permit_status == "COMPLETE",
    !is.na(permit_pin),
    !is.na(application_date)
  ) %>%
  separate_rows(permit_pin, sep = "\\s*\\|\\s*") %>%
  mutate(pin10 = str_replace_all(permit_pin, "[^0-9]", "")) %>%
  filter(str_detect(pin10, "^[0-9]{10}$")) %>%
  distinct(permit_id, pin10, .keep_all = TRUE) %>%
  mutate(permit_application_year = lubridate::year(application_date))

pin10_cardinality <- history %>%
  filter(cards_in_history == 1) %>%
  distinct(pin) %>%
  mutate(pin10 = str_sub(pin, 1, 10)) %>%
  count(pin10, name = "production_pins_under_pin10")

permit_summary <- permits %>%
  count(pin10, name = "new_construction_permits_under_pin10")

unique_permit <- permits %>%
  group_by(pin10) %>%
  filter(n_distinct(permit_id) == 1) %>%
  ungroup() %>%
  distinct(pin10, permit_application_year)

cases <- production %>%
  inner_join(latest_endpoint, by = "pin", relationship = "one-to-one") %>%
  left_join(analysis, by = "pin", relationship = "one-to-one") %>%
  mutate(pin10 = str_sub(pin, 1, 10)) %>%
  left_join(pin10_cardinality, by = "pin10", relationship = "many-to-one") %>%
  left_join(permit_summary, by = "pin10", relationship = "many-to-one") %>%
  left_join(unique_permit, by = "pin10", relationship = "many-to-one") %>%
  mutate(
    in_500ft_source_sample = replace_na(in_500ft_source_sample, FALSE),
    in_main_model_inputs = replace_na(in_main_model_inputs, FALSE),
    endpoint_pre1999_disagreement =
      production_yearbuilt >= 1999 & endpoint_yearbuilt < 1999,
    later_pre1999_report_after_selected_modern_report =
      endpoint_pre1999_disagreement & endpoint_tax_year > production_tax_year,
    selected_modern_report_after_pre1999_endpoint =
      endpoint_pre1999_disagreement & production_tax_year > endpoint_tax_year,
    same_building_size =
      (is.na(production_bldgsf) & is.na(endpoint_bldgsf)) |
      (!is.na(production_bldgsf) & !is.na(endpoint_bldgsf) &
        abs(log(production_bldgsf / endpoint_bldgsf)) <= log(1.1)),
    same_land_size =
      (is.na(production_landsf) & is.na(endpoint_landsf)) |
      (!is.na(production_landsf) & !is.na(endpoint_landsf) &
        abs(log(production_landsf / endpoint_landsf)) <= log(1.1)),
    same_unit_count =
      (is.na(production_units) & is.na(endpoint_units)) |
      (!is.na(production_units) & !is.na(endpoint_units) & production_units == endpoint_units),
    stable_physical_fields = same_building_size & same_land_size & same_unit_count,
    unique_new_construction_permit =
      production_pins_under_pin10 == 1 &
      new_construction_permits_under_pin10 == 1,
    production_year_permit_supported =
      unique_new_construction_permit &
      between(production_yearbuilt - permit_application_year, 0, 3)
  )

summary <- bind_rows(
  tibble(metric = "production_single_card_residential_pins", value = nrow(cases)),
  tibble(metric = "production_single_card_2006_2022_pins", value = sum(between(cases$production_yearbuilt, 2006, 2022))),
  tibble(metric = "production_rows_with_pre1999_endpoint_disagreement", value = sum(cases$endpoint_pre1999_disagreement)),
  tibble(metric = "production_rows_with_later_pre1999_report", value = sum(cases$later_pre1999_report_after_selected_modern_report)),
  tibble(metric = "production_rows_with_modern_report_after_pre1999_endpoint", value = sum(cases$selected_modern_report_after_pre1999_endpoint)),
  tibble(metric = "main_model_rows_with_pre1999_endpoint_disagreement", value = sum(cases$in_main_model_inputs & cases$endpoint_pre1999_disagreement)),
  tibble(metric = "main_model_rows_with_later_pre1999_report", value = sum(cases$in_main_model_inputs & cases$later_pre1999_report_after_selected_modern_report)),
  tibble(metric = "main_model_later_pre1999_reports_with_stable_physical_fields", value = sum(cases$in_main_model_inputs & cases$later_pre1999_report_after_selected_modern_report & cases$stable_physical_fields, na.rm = TRUE)),
  tibble(metric = "main_model_later_pre1999_reports_with_permit_support", value = sum(cases$in_main_model_inputs & cases$later_pre1999_report_after_selected_modern_report & cases$production_year_permit_supported, na.rm = TRUE)),
  tibble(metric = "main_model_rows_with_modern_report_after_pre1999_endpoint", value = sum(cases$in_main_model_inputs & cases$selected_modern_report_after_pre1999_endpoint)),
  tibble(metric = "main_model_modern_after_endpoint_with_permit_support", value = sum(cases$in_main_model_inputs & cases$selected_modern_report_after_pre1999_endpoint & cases$production_year_permit_supported, na.rm = TRUE))
)

readr::write_csv(summary, "../output/residential_latest_report_summary.csv")
readr::write_csv(
  cases %>%
    filter(endpoint_pre1999_disagreement) %>%
    arrange(desc(in_main_model_inputs), pin),
  "../output/residential_latest_report_cases.csv"
)
