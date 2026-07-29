# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_construction_year_audit/code")

source("../../../setup_environment/code/packages.R")

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

invisible(dbExecute(con, "
CREATE TEMP TABLE assessor_history AS
SELECT
  trim(pin) AS pin,
  substr(trim(pin), 1, 10) AS pin10,
  try_cast(numeric_text(year) AS INTEGER) AS tax_year,
  try_cast(numeric_text(card) AS INTEGER) AS card_num,
  trim(class) AS property_class,
  try_cast(numeric_text(char_yrblt) AS INTEGER) AS year_built,
  try_cast(numeric_text(char_bldg_sf) AS DOUBLE) AS building_sqft,
  try_cast(numeric_text(char_land_sf) AS DOUBLE) AS land_sqft,
  try_cast(numeric_text(char_beds) AS DOUBLE) AS bedrooms,
  try_cast(numeric_text(char_rooms) AS DOUBLE) AS rooms,
  try_cast(numeric_text(char_fbath) AS DOUBLE) AS full_baths,
  CASE
    WHEN lower(trim(char_apts)) IN ('none', 'zero') THEN 0
    WHEN lower(trim(char_apts)) = 'one' THEN 1
    WHEN lower(trim(char_apts)) = 'two' THEN 2
    WHEN lower(trim(char_apts)) = 'three' THEN 3
    WHEN lower(trim(char_apts)) = 'four' THEN 4
    WHEN lower(trim(char_apts)) = 'five' THEN 5
    WHEN lower(trim(char_apts)) = 'six' THEN 6
    ELSE try_cast(numeric_text(char_apts) AS INTEGER)
  END AS apartments,
  trim(char_use) AS building_use,
  trim(char_type_resd) AS residence_type,
  trim(row_id) AS row_id
FROM read_csv(
  '../input/residential_improvement_characteristics_full.csv',
  all_varchar = true,
  header = true,
  ignore_errors = true,
  max_line_size = 10000000
)
WHERE try_cast(numeric_text(township_code) AS INTEGER) IN (70, 71, 72, 73, 74, 75, 76, 77)
  AND trim(pin) IS NOT NULL
  AND trim(pin) != ''
  AND try_cast(numeric_text(card) AS INTEGER) IS NOT NULL;
"))

source_summary <- dbGetQuery(con, "
SELECT
  count(*) AS rows,
  count(DISTINCT pin) AS pins,
  count(DISTINCT (pin, card_num)) AS pin_cards,
  min(tax_year) AS first_tax_year,
  max(tax_year) AS last_tax_year,
  count(*) FILTER (WHERE year_built >= 1999) AS post_1999_rows
FROM assessor_history;
") %>% as_tibble()

duplicate_keys <- dbGetQuery(con, "
SELECT count(*) AS duplicate_pin_card_tax_year_keys
FROM (
  SELECT pin, card_num, tax_year
  FROM assessor_history
  GROUP BY pin, card_num, tax_year
  HAVING count(*) > 1
);
") %>% pull(duplicate_pin_card_tax_year_keys)

history <- dbGetQuery(con, "
WITH candidate_cards AS (
  SELECT pin, card_num
  FROM assessor_history
  GROUP BY pin, card_num
  HAVING max(year_built) >= 1999
)
SELECT h.*
FROM assessor_history h
INNER JOIN candidate_cards c USING (pin, card_num)
WHERE h.year_built IS NOT NULL
ORDER BY h.pin, h.card_num, h.tax_year, h.row_id;
") %>%
  as_tibble() %>%
  mutate(
    pin = as.character(pin),
    pin10 = as.character(pin10),
    card_num = as.integer(card_num),
    tax_year = as.integer(tax_year),
    year_built = as.integer(year_built)
  ) %>%
  arrange(pin, card_num, tax_year, row_id) %>%
  group_by(pin, card_num, tax_year) %>%
  slice_tail(n = 1) %>%
  ungroup()

if (anyDuplicated(history[c("pin", "card_num", "tax_year")]) > 0) {
  stop("Assessor history is not unique by PIN-card-tax year after deterministic row selection.", call. = FALSE)
}

card_counts <- history %>%
  distinct(pin, card_num) %>%
  count(pin, name = "cards_in_history")

history <- history %>%
  left_join(card_counts, by = "pin", relationship = "many-to-one")

year_counts <- history %>%
  filter(year_built >= 1999) %>%
  group_by(pin, card_num, year_built) %>%
  summarise(
    reports = n(),
    first_tax_year_for_value = min(tax_year, na.rm = TRUE),
    last_tax_year_for_value = max(tax_year, na.rm = TRUE),
    .groups = "drop"
  )

card_summary <- history %>%
  group_by(pin, pin10, card_num, cards_in_history) %>%
  summarise(
    reports = n(),
    first_tax_year = min(tax_year, na.rm = TRUE),
    last_tax_year = max(tax_year, na.rm = TRUE),
    distinct_years_all = n_distinct(year_built),
    distinct_years_post_1999 = n_distinct(year_built[year_built >= 1999]),
    has_pre_1999_report = any(year_built < 1999),
    first_post_1999_tax_year = min(tax_year[year_built >= 1999], na.rm = TRUE),
    minimum_post_1999_year = min(year_built[year_built >= 1999], na.rm = TRUE),
    maximum_post_1999_year = max(year_built[year_built >= 1999], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    first_report_lag_minimum = first_post_1999_tax_year - minimum_post_1999_year,
    revised_post_1999 = distinct_years_post_1999 > 1
  )

select_minimum <- history %>%
  filter(year_built >= 1999) %>%
  arrange(pin, card_num, year_built, tax_year, desc(building_sqft), row_id) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(selection_rule = "minimum_post_1999")

select_latest_all <- history %>%
  filter(year_built >= 1999) %>%
  arrange(pin, card_num, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(selection_rule = "latest_report_all")

select_latest_2022 <- history %>%
  filter(year_built >= 1999, tax_year <= 2022) %>%
  arrange(pin, card_num, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(selection_rule = "latest_report_through_2022")

select_latest_2025 <- history %>%
  filter(year_built >= 1999, tax_year <= 2025) %>%
  arrange(pin, card_num, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(selection_rule = "latest_report_through_2025")

select_sample_end <- bind_rows(
  select_latest_2022,
  select_latest_2025 %>%
    anti_join(select_latest_2022, by = c("pin", "card_num")),
  select_latest_all %>%
    anti_join(select_latest_2025, by = c("pin", "card_num"))
) %>%
  mutate(selection_rule = "latest_report_at_sample_end")

modal_years <- year_counts %>%
  arrange(pin, card_num, desc(reports), desc(last_tax_year_for_value), desc(year_built)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup()

select_modal <- history %>%
  inner_join(
    modal_years %>% select(pin, card_num, year_built),
    by = c("pin", "card_num", "year_built"),
    relationship = "many-to-one"
  ) %>%
  arrange(pin, card_num, desc(tax_year), desc(building_sqft), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(selection_rule = "modal_post_1999")

repeated_years <- year_counts %>%
  filter(reports >= 2) %>%
  arrange(pin, card_num, desc(last_tax_year_for_value), desc(reports), desc(year_built)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup()

select_latest_repeated <- history %>%
  inner_join(
    repeated_years %>% select(pin, card_num, year_built),
    by = c("pin", "card_num", "year_built"),
    relationship = "many-to-one"
  ) %>%
  arrange(pin, card_num, desc(tax_year), desc(building_sqft), desc(row_id)) %>%
  group_by(pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(selection_rule = "latest_repeated_post_1999")

no_repeated_year <- anti_join(
  select_latest_all %>% select(pin, card_num),
  select_latest_repeated %>% select(pin, card_num),
  by = c("pin", "card_num")
)
select_latest_repeated <- bind_rows(
  select_latest_repeated,
  select_latest_all %>%
    semi_join(no_repeated_year, by = c("pin", "card_num")) %>%
    mutate(selection_rule = "latest_repeated_post_1999")
)

selected_rows <- bind_rows(
  select_minimum,
  select_latest_2022,
  select_latest_2025,
  select_sample_end,
  select_latest_all,
  select_modal,
  select_latest_repeated
) %>%
  left_join(
    card_summary,
    by = c("pin", "pin10", "card_num", "cards_in_history"),
    relationship = "many-to-one"
  ) %>%
  transmute(
    selection_rule,
    pin,
    pin10,
    card_num,
    cards_in_history,
    selected_tax_year = tax_year,
    construction_year = year_built,
    property_class,
    building_sqft,
    land_sqft,
    apartments,
    building_use,
    residence_type,
    reports,
    first_tax_year,
    last_tax_year,
    distinct_years_all,
    distinct_years_post_1999,
    has_pre_1999_report,
    first_post_1999_tax_year,
    minimum_post_1999_year,
    maximum_post_1999_year,
    first_report_lag_minimum,
    revised_post_1999
  )

expected_rules <- c(
  "minimum_post_1999",
  "latest_report_through_2022",
  "latest_report_through_2025",
  "latest_report_at_sample_end",
  "latest_report_all",
  "modal_post_1999",
  "latest_repeated_post_1999"
)
rule_counts <- selected_rows %>% count(selection_rule)
if (!setequal(rule_counts$selection_rule, expected_rules)) {
  stop("Construction-year rule output is incomplete.", call. = FALSE)
}
if (anyDuplicated(selected_rows[c("selection_rule", "pin", "card_num")]) > 0) {
  stop("Construction-year selections are not unique by rule and PIN-card.", call. = FALSE)
}

production <- read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    production_card_num = as.integer(card_num),
    production_year = as.integer(year_built),
    production_tax_year = as.integer(tax_year),
    production_building_sqft = as.numeric(building_sqft),
    production_land_sqft = as.numeric(land_sqft)
  )

production_single_2022 <- history %>%
  filter(cards_in_history == 1, year_built >= 1999, tax_year <= 2022) %>%
  arrange(pin, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup()

production_single_2025 <- history %>%
  filter(cards_in_history == 1, year_built >= 1999, tax_year <= 2025) %>%
  anti_join(production_single_2022 %>% select(pin), by = "pin") %>%
  arrange(pin, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup()

production_single_later <- history %>%
  filter(cards_in_history == 1, year_built >= 1999) %>%
  anti_join(bind_rows(production_single_2022, production_single_2025) %>% select(pin), by = "pin") %>%
  arrange(pin, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup()

production_multicard <- history %>%
  filter(cards_in_history > 1, year_built >= 1999) %>%
  group_by(pin) %>%
  slice_min(order_by = year_built, with_ties = TRUE) %>%
  slice_min(order_by = tax_year, with_ties = TRUE) %>%
  slice_max(order_by = building_sqft, with_ties = FALSE) %>%
  ungroup()

production_check <- bind_rows(
  production_single_2022,
  production_single_2025,
  production_single_later,
  production_multicard
) %>%
  transmute(
    pin,
    reproduced_card_num = card_num,
    reproduced_year = year_built,
    reproduced_tax_year = tax_year,
    reproduced_building_sqft = building_sqft,
    reproduced_land_sqft = land_sqft
  ) %>%
  full_join(production, by = "pin", relationship = "one-to-one") %>%
  mutate(
    exact_year_match = production_year == reproduced_year,
    exact_card_match = production_card_num == reproduced_card_num,
    exact_tax_year_match = production_tax_year == reproduced_tax_year
  )

production_mismatch_summary <- production_check %>%
  summarise(
    rows = n(),
    missing_production = sum(is.na(production_year)),
    missing_reproduction = sum(is.na(reproduced_year)),
    year_mismatches = sum(!exact_year_match, na.rm = TRUE),
    card_mismatches = sum(!exact_card_match, na.rm = TRUE),
    tax_year_mismatches = sum(!exact_tax_year_match, na.rm = TRUE)
  )
if (production_mismatch_summary$missing_production > 0 ||
    production_mismatch_summary$missing_reproduction > 0 ||
    production_mismatch_summary$year_mismatches > 0 ||
    production_mismatch_summary$tax_year_mismatches > 0) {
  stop(
    paste(
      "Audit did not reproduce the production PIN-level year selection:",
      paste(names(production_mismatch_summary), production_mismatch_summary, collapse = ", ")
    ),
    call. = FALSE
  )
}

tax_year_history <- history %>%
  arrange(pin, card_num, tax_year, row_id) %>%
  group_by(pin, card_num) %>%
  mutate(
    previous_year_built = lag(year_built),
    previous_building_sqft = lag(building_sqft),
    previous_land_sqft = lag(land_sqft),
    previous_apartments = lag(apartments),
    previous_building_use = lag(building_use),
    previous_residence_type = lag(residence_type),
    year_changed = !is.na(previous_year_built) & year_built != previous_year_built,
    sqft_ratio = building_sqft / previous_building_sqft,
    absolute_log_sqft_change = abs(log(sqft_ratio)),
    apartments_changed = apartments != previous_apartments,
    use_changed = building_use != previous_building_use,
    residence_type_changed = residence_type != previous_residence_type
  ) %>%
  ungroup()

revision_events <- tax_year_history %>%
  filter(year_changed) %>%
  mutate(
    transition_type = case_when(
      previous_year_built < 1999 & year_built >= 1999 ~ "pre_1999_to_post_1999",
      previous_year_built >= 1999 & year_built >= 1999 ~ "post_1999_revision",
      previous_year_built >= 1999 & year_built < 1999 ~ "post_1999_to_pre_1999",
      TRUE ~ "other"
    ),
    large_characteristic_change =
      (!is.na(absolute_log_sqft_change) & absolute_log_sqft_change >= log(1.25)) |
      replace_na(apartments_changed, FALSE) |
      replace_na(use_changed, FALSE) |
      replace_na(residence_type_changed, FALSE)
  ) %>%
  select(
    pin, pin10, card_num, cards_in_history, tax_year,
    previous_year_built, year_built, transition_type,
    previous_building_sqft, building_sqft, sqft_ratio,
    previous_land_sqft, land_sqft,
    previous_apartments, apartments,
    previous_building_use, building_use,
    previous_residence_type, residence_type,
    large_characteristic_change
  )

permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(
    permit_id = as.character(id),
    pin10 = str_pad(str_extract(as.character(pin), "[0-9]{10}"), 10, pad = "0"),
    permit_type,
    application_date = as.Date(application_start_date),
    issue_date = as.Date(issue_date),
    permit_issued = as.integer(permit_issued),
    permit_status,
    permit_address = str_squish(paste(street_number, street_direction, street_name)),
    work_description
  ) %>%
  filter(
    permit_type == "PERMIT - NEW CONSTRUCTION",
    permit_issued == 1,
    permit_status == "COMPLETE",
    !is.na(pin10),
    str_detect(pin10, "^[0-9]{10}$"),
    !is.na(application_date)
  ) %>%
  mutate(application_year = year(application_date))

pin10_cardinality <- card_summary %>%
  distinct(pin10, pin) %>%
  count(pin10, name = "candidate_pins_under_pin10")
permit_counts <- permits %>%
  count(pin10, name = "new_construction_permits_under_pin10")

validation_base <- selected_rows %>%
  filter(cards_in_history == 1) %>%
  left_join(pin10_cardinality, by = "pin10", relationship = "many-to-one") %>%
  left_join(permit_counts, by = "pin10", relationship = "many-to-one") %>%
  mutate(
    new_construction_permits_under_pin10 = replace_na(new_construction_permits_under_pin10, 0L),
    unique_pin10_permit_match = candidate_pins_under_pin10 == 1 &
      new_construction_permits_under_pin10 == 1
  ) %>%
  left_join(
    permits %>%
      group_by(pin10) %>%
      filter(n() == 1) %>%
      ungroup() %>%
      select(
        pin10, permit_id, permit_application_date = application_date,
        permit_application_year = application_year,
        permit_issue_date = issue_date,
        permit_issued,
        permit_status,
        permit_address,
        work_description
      ),
    by = "pin10",
    relationship = "many-to-one"
  ) %>%
  mutate(
    build_minus_permit_year = construction_year - permit_application_year,
    absolute_permit_gap = abs(build_minus_permit_year),
    plausible_permit_timing = between(build_minus_permit_year, 0, 3),
    in_paper_window = between(construction_year, 2006, 2022),
    selected_year_after_tax_record = construction_year > selected_tax_year
  )

rule_validation <- validation_base %>%
  group_by(selection_rule) %>%
  summarise(
    single_card_pin_cards = n(),
    in_paper_window = sum(in_paper_window, na.rm = TRUE),
    revised_post_1999 = sum(revised_post_1999, na.rm = TRUE),
    unique_new_construction_permit_matches = sum(unique_pin10_permit_match, na.rm = TRUE),
    median_build_minus_permit_year = median(build_minus_permit_year[unique_pin10_permit_match], na.rm = TRUE),
    median_absolute_permit_gap = median(absolute_permit_gap[unique_pin10_permit_match], na.rm = TRUE),
    share_permit_zero_to_three_years_before_build = mean(plausible_permit_timing[unique_pin10_permit_match], na.rm = TRUE),
    selected_year_after_tax_record = sum(selected_year_after_tax_record, na.rm = TRUE),
    .groups = "drop"
  )

rule_validation_strata <- bind_rows(
  validation_base %>% mutate(validation_stratum = "all_single_card"),
  validation_base %>%
    filter(revised_post_1999) %>%
    mutate(validation_stratum = "revised_post_1999")
) %>%
  group_by(validation_stratum, selection_rule) %>%
  summarise(
    pin_cards = n(),
    in_paper_window = sum(in_paper_window, na.rm = TRUE),
    unique_new_construction_permit_matches = sum(unique_pin10_permit_match, na.rm = TRUE),
    median_build_minus_permit_year = median(build_minus_permit_year[unique_pin10_permit_match], na.rm = TRUE),
    median_absolute_permit_gap = median(absolute_permit_gap[unique_pin10_permit_match], na.rm = TRUE),
    share_permit_zero_to_three_years_before_build = mean(plausible_permit_timing[unique_pin10_permit_match], na.rm = TRUE),
    .groups = "drop"
  )

minimum_permit_matches <- validation_base %>%
  filter(selection_rule == "minimum_post_1999", unique_pin10_permit_match) %>%
  select(
    pin, card_num, revised_post_1999, permit_application_year,
    minimum_year = construction_year,
    minimum_absolute_gap = absolute_permit_gap,
    minimum_plausible_timing = plausible_permit_timing
  )

permit_comparison_rows <- validation_base %>%
  filter(selection_rule != "minimum_post_1999", unique_pin10_permit_match) %>%
  select(
    pin, card_num, alternative_rule = selection_rule,
    alternative_year = construction_year,
    alternative_absolute_gap = absolute_permit_gap,
    alternative_plausible_timing = plausible_permit_timing
  ) %>%
  inner_join(
    minimum_permit_matches,
    by = c("pin", "card_num"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    gap_comparison = case_when(
      alternative_absolute_gap < minimum_absolute_gap ~ "alternative_closer",
      alternative_absolute_gap > minimum_absolute_gap ~ "minimum_closer",
      TRUE ~ "equal_gap"
    )
  )

permit_comparison <- bind_rows(
  permit_comparison_rows %>% mutate(validation_stratum = "all_unique_permit_matches"),
  permit_comparison_rows %>%
    filter(revised_post_1999) %>%
    mutate(validation_stratum = "revised_post_1999")
) %>%
  group_by(validation_stratum, alternative_rule) %>%
  summarise(
    pin_cards = n(),
    same_selected_year = sum(alternative_year == minimum_year),
    alternative_closer = sum(gap_comparison == "alternative_closer"),
    minimum_closer = sum(gap_comparison == "minimum_closer"),
    equal_gap = sum(gap_comparison == "equal_gap"),
    minimum_median_absolute_gap = median(minimum_absolute_gap, na.rm = TRUE),
    alternative_median_absolute_gap = median(alternative_absolute_gap, na.rm = TRUE),
    minimum_plausible_timing_share = mean(minimum_plausible_timing, na.rm = TRUE),
    alternative_plausible_timing_share = mean(alternative_plausible_timing, na.rm = TRUE),
    .groups = "drop"
  )

rule_wide <- selected_rows %>%
  select(pin, pin10, card_num, cards_in_history, selection_rule, construction_year, selected_tax_year) %>%
  pivot_wider(
    names_from = selection_rule,
    values_from = c(construction_year, selected_tax_year),
    names_glue = "{selection_rule}_{.value}"
  ) %>%
  left_join(card_summary, by = c("pin", "pin10", "card_num", "cards_in_history"), relationship = "one-to-one")

revision_cases <- rule_wide %>%
  filter(distinct_years_post_1999 > 1 | has_pre_1999_report) %>%
  left_join(
    validation_base %>%
      filter(selection_rule == "minimum_post_1999") %>%
      select(
        pin, card_num, unique_pin10_permit_match, permit_id,
        permit_application_date, permit_application_year,
        permit_issue_date, permit_issued, permit_status,
        permit_address, work_description
      ),
    by = c("pin", "card_num"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    latest_minus_minimum = latest_report_all_construction_year - minimum_post_1999_construction_year,
    latest_2025_minus_minimum = latest_report_through_2025_construction_year - minimum_post_1999_construction_year,
    modal_minus_minimum = modal_post_1999_construction_year - minimum_post_1999_construction_year,
    minimum_in_paper_window = between(minimum_post_1999_construction_year, 2006, 2022),
    latest_in_paper_window = between(latest_report_all_construction_year, 2006, 2022),
    latest_2025_in_paper_window = between(latest_report_through_2025_construction_year, 2006, 2022),
    modal_in_paper_window = between(modal_post_1999_construction_year, 2006, 2022)
  )

unique_permit_lookup <- permits %>%
  group_by(pin10) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  select(
    pin10, permit_id, permit_application_date = application_date,
    permit_application_year = application_year,
    permit_issue_date = issue_date,
    permit_issued,
    permit_status,
    permit_address,
    work_description
  )

episode_candidates <- revision_events %>%
  filter(
    large_characteristic_change,
    transition_type %in% c("pre_1999_to_post_1999", "post_1999_revision")
  ) %>%
  left_join(pin10_cardinality, by = "pin10", relationship = "many-to-one") %>%
  left_join(permit_counts, by = "pin10", relationship = "many-to-one") %>%
  mutate(new_construction_permits_under_pin10 = replace_na(new_construction_permits_under_pin10, 0L)) %>%
  left_join(unique_permit_lookup, by = "pin10", relationship = "many-to-one") %>%
  mutate(
    unique_pin10_permit_match = candidate_pins_under_pin10 == 1 & new_construction_permits_under_pin10 == 1,
    build_minus_permit_year = year_built - permit_application_year,
    permit_supports_new_year = unique_pin10_permit_match & between(build_minus_permit_year, 0, 3),
    permit_postdates_previous_reported_year =
      permit_supports_new_year & permit_application_year > previous_year_built,
    candidate_type = if_else(
      transition_type == "pre_1999_to_post_1999",
      "old_building_to_new_building",
      "possible_second_post_1999_building"
    )
  ) %>%
  arrange(candidate_type, pin, card_num, tax_year)

review_cases <- revision_cases %>%
  mutate(
    review_priority = case_when(
      minimum_in_paper_window != latest_in_paper_window ~ 1L,
      unique_pin10_permit_match &
        abs(minimum_post_1999_construction_year - permit_application_year) !=
          abs(latest_report_all_construction_year - permit_application_year) ~ 2L,
      abs(latest_minus_minimum) >= 5 ~ 3L,
      TRUE ~ 4L
    ),
    minimum_permit_gap = minimum_post_1999_construction_year - permit_application_year,
    latest_permit_gap = latest_report_all_construction_year - permit_application_year
  ) %>%
  arrange(review_priority, desc(abs(latest_minus_minimum)), pin, card_num) %>%
  group_by(review_priority) %>%
  slice_head(n = 30) %>%
  ungroup()

history_summary <- bind_rows(
  source_summary %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(section = "raw_chicago_history"),
  tibble(section = "raw_chicago_history", metric = "duplicate_pin_card_tax_year_keys", value = duplicate_keys),
  card_summary %>%
    summarise(
      candidate_pin_cards = n(),
      candidate_pins = n_distinct(pin),
      single_card_pins = n_distinct(pin[cards_in_history == 1]),
      multicard_pins = n_distinct(pin[cards_in_history > 1]),
      pin_cards_with_pre_1999_history = sum(has_pre_1999_report),
      pin_cards_revised_among_post_1999_years = sum(revised_post_1999),
      pin_cards_minimum_in_2006_2022 = sum(between(minimum_post_1999_year, 2006, 2022)),
      median_first_report_lag_minimum = median(first_report_lag_minimum, na.rm = TRUE),
      p90_first_report_lag_minimum = quantile(first_report_lag_minimum, 0.9, na.rm = TRUE)
    ) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
    mutate(section = "candidate_card_histories"),
  revision_events %>%
    count(transition_type, name = "value") %>%
    transmute(section = "year_revision_events", metric = transition_type, value),
  revision_events %>%
    group_by(transition_type) %>%
    summarise(value = mean(large_characteristic_change, na.rm = TRUE), .groups = "drop") %>%
    transmute(section = "share_with_large_characteristic_change", metric = transition_type, value),
  rule_validation %>%
    select(selection_rule, in_paper_window) %>%
    transmute(section = "rule_sample_membership", metric = selection_rule, value = in_paper_window),
  tibble(
    section = "production_reproduction",
    metric = c("production_rows", "exact_year_matches", "exact_card_matches", "exact_tax_year_matches"),
    value = c(
      nrow(production_check),
      sum(production_check$exact_year_match),
      sum(production_check$exact_card_match),
      sum(production_check$exact_tax_year_match)
    )
  )
) %>%
  select(section, metric, value)

write_csv(history_summary, "../output/construction_year_history_summary.csv")
write_csv(revision_cases, "../output/construction_year_revision_cases.csv")
write_csv(revision_events, "../output/construction_year_revision_events.csv")
write_csv(
  selected_rows %>%
    select(
      selection_rule, pin, card_num, cards_in_history,
      selected_tax_year, construction_year,
      building_sqft, land_sqft, apartments, building_use, residence_type
    ),
  "../output/construction_year_rule_selections.csv"
)
write_csv(rule_validation, "../output/construction_year_rule_validation.csv")
write_csv(rule_validation_strata, "../output/construction_year_rule_validation_strata.csv")
write_csv(permit_comparison, "../output/construction_year_permit_comparison.csv")
write_csv(episode_candidates, "../output/construction_year_episode_candidates.csv")
write_csv(review_cases, "../output/construction_year_review_cases.csv")
