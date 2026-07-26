# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

candidates <- readr::read_csv(
  "../output/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(between(construction_year, 2006L, 2022L)) %>%
  select(project_id, construction_year, current_within_1500ft)

components <- readr::read_csv(
  "../output/preferred_commercial_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  inner_join(candidates, by = "project_id", relationship = "many-to-one") %>%
  distinct(project_id, component_pin, .keep_all = TRUE)

if (anyDuplicated(components[c("project_id", "component_pin")]) > 0) {
  stop("Commercial project-component links are not unique.", call. = FALSE)
}

con <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

DBI::dbWriteTable(
  con,
  "commercial_components",
  components %>% select(project_id, component_pin, construction_year),
  overwrite = TRUE
)

invisible(DBI::dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

residential_history <- DBI::dbGetQuery(con, "
WITH source AS (
  SELECT
    regexp_replace(trim(pin), '[^0-9]', '', 'g') AS component_pin,
    try_cast(numeric_text(year) AS INTEGER) AS tax_year,
    try_cast(numeric_text(card) AS INTEGER) AS card_num,
    trim(class) AS class,
    try_cast(numeric_text(char_yrblt) AS INTEGER) AS year_built,
    try_cast(numeric_text(char_bldg_sf) AS DOUBLE) AS building_sqft,
    try_cast(numeric_text(char_land_sf) AS DOUBLE) AS land_sqft,
    try_cast(numeric_text(char_apts) AS DOUBLE) AS num_apartments,
    trim(char_use) AS use_description,
    trim(row_id) AS row_id
  FROM read_csv(
    '../input/residential_improvement_characteristics_full.csv',
    all_varchar = true,
    header = true,
    ignore_errors = false,
    max_line_size = 10000000
  )
  WHERE try_cast(numeric_text(township_code) AS INTEGER)
        IN (70, 71, 72, 73, 74, 75, 76, 77)
)
SELECT
  commercial_components.project_id,
  commercial_components.component_pin,
  commercial_components.construction_year,
  source.tax_year,
  source.card_num,
  source.class,
  source.year_built,
  source.building_sqft,
  source.land_sqft,
  source.num_apartments,
  source.use_description,
  source.row_id
FROM commercial_components
INNER JOIN source USING (component_pin)
") %>%
  arrange(project_id, component_pin, card_num, tax_year, row_id) %>%
  group_by(project_id, component_pin, card_num, tax_year) %>%
  slice_tail(n = 1) %>%
  ungroup()

latest_before <- residential_history %>%
  filter(tax_year < construction_year) %>%
  arrange(project_id, component_pin, card_num, desc(tax_year), desc(row_id)) %>%
  group_by(project_id, component_pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    project_id,
    component_pin,
    card_num,
    before_tax_year = tax_year,
    before_year_built = year_built,
    before_building_sqft = building_sqft,
    before_apartments = num_apartments
  )

earliest_after <- residential_history %>%
  filter(tax_year >= construction_year) %>%
  arrange(project_id, component_pin, card_num, tax_year, row_id) %>%
  group_by(project_id, component_pin, card_num) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    project_id,
    component_pin,
    card_num,
    after_tax_year = tax_year,
    after_year_built = year_built,
    after_building_sqft = building_sqft,
    after_apartments = num_apartments
  )

card_transitions <- full_join(
  latest_before,
  earliest_after,
  by = c("project_id", "component_pin", "card_num"),
  relationship = "one-to-one"
) %>%
  left_join(
    components,
    by = c("project_id", "component_pin"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    same_building_area =
      is.finite(before_building_sqft) & before_building_sqft > 0 &
      is.finite(after_building_sqft) & after_building_sqft > 0 &
      abs(after_building_sqft - before_building_sqft) /
        pmax(after_building_sqft, before_building_sqft) <= 0.02,
    old_structure_before =
      is.finite(before_year_built) & before_year_built <= construction_year - 5L,
    old_structure_after =
      is.finite(after_year_built) & after_year_built <= construction_year - 5L,
    year_recode_without_physical_change =
      same_building_area & old_structure_before &
      is.finite(after_year_built) & after_year_built >= construction_year - 3L,
    old_structure_persists = same_building_area & old_structure_before & old_structure_after
  ) %>%
  arrange(desc(current_within_1500ft), project_id, component_pin, card_num)

project_evidence <- components %>%
  group_by(project_id, construction_year, current_within_1500ft) %>%
  summarise(component_count = n_distinct(component_pin), .groups = "drop") %>%
  left_join(
    residential_history %>%
      group_by(project_id) %>%
      summarise(
        residential_history_components = n_distinct(component_pin),
        residential_history_cards = n_distinct(paste(component_pin, card_num)),
        residential_first_tax_year = min(tax_year, na.rm = TRUE),
        residential_last_tax_year = max(tax_year, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    card_transitions %>%
      group_by(project_id) %>%
      summarise(
        cards_observed_before = sum(is.finite(before_tax_year)),
        cards_observed_after = sum(is.finite(after_tax_year)),
        cards_same_area_across_year = sum(same_building_area, na.rm = TRUE),
        cards_with_year_recode = sum(year_recode_without_physical_change, na.rm = TRUE),
        cards_with_old_structure_persisting = sum(old_structure_persists, na.rm = TRUE),
        transition_evidence = paste(
          paste0(
            component_pin, " card ", card_num, ": ",
            before_tax_year, "/", before_year_built, "/", before_building_sqft,
            " -> ", after_tax_year, "/", after_year_built, "/", after_building_sqft
          ),
          collapse = " | "
        ),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    across(c(residential_history_components:cards_with_old_structure_persisting),
           ~ coalesce(.x, 0)),
    residential_history_screen = case_when(
      cards_with_year_recode > 0 ~ "review_year_recode_without_physical_change",
      cards_with_old_structure_persisting > 0 ~ "review_old_structure_persists",
      residential_history_components > 0 ~ "residential_history_no_mechanical_recode",
      TRUE ~ "no_residential_component_history"
    )
  ) %>%
  arrange(desc(current_within_1500ft), project_id)

if (anyDuplicated(project_evidence$project_id) > 0) {
  stop("Commercial residential-history evidence is not unique by project.", call. = FALSE)
}

readr::write_csv(
  residential_history,
  "../output/commercial_residential_component_history.csv"
)
readr::write_csv(
  card_transitions,
  "../output/commercial_residential_card_transitions.csv"
)
readr::write_csv(
  project_evidence,
  "../output/commercial_residential_history_evidence.csv"
)
readr::write_csv(
  bind_rows(
    project_evidence %>%
      count(residential_history_screen, name = "value") %>%
      transmute(section = "all", metric = residential_history_screen, value),
    project_evidence %>%
      filter(current_within_1500ft) %>%
      count(residential_history_screen, name = "value") %>%
      transmute(section = "within_1500ft", metric = residential_history_screen, value)
  ),
  "../output/commercial_residential_history_summary.csv"
)
