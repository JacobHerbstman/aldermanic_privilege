# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_cleaning/code")
# minimum_construction_year <- 1999
# preferred_assessment_year <- 2022
# fallback_assessment_year <- 2025

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) {
  args <- c(minimum_construction_year, preferred_assessment_year, fallback_assessment_year)
}
if (length(args) != 3L) {
  stop("Expected minimum construction year, preferred assessment year, and fallback assessment year.")
}
minimum_construction_year <- as.integer(args[1])
preferred_assessment_year <- as.integer(args[2])
fallback_assessment_year <- as.integer(args[3])
if (anyNA(c(minimum_construction_year, preferred_assessment_year, fallback_assessment_year)) ||
    minimum_construction_year > preferred_assessment_year ||
    preferred_assessment_year > fallback_assessment_year) {
  stop("Construction and assessment-year cutoffs must be ordered integers.")
}

con <- DBI::dbConnect(duckdb::duckdb())

data <- DBI::dbGetQuery(con, sprintf("
WITH history AS (
  SELECT * FROM read_parquet('../input/residential_assessor_history.parquet')
), candidate_cards AS (
  SELECT pin, card_num FROM history
  GROUP BY pin, card_num HAVING max(year_built) >= %d
)
SELECT r.* EXCLUDE (apartments_text, source_row_order)
FROM history r INNER JOIN candidate_cards USING (pin, card_num)
ORDER BY source_row_order
", minimum_construction_year)) %>%
  arrange(pin, card_num, tax_year, row_id) %>%
  group_by(pin, card_num, tax_year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  group_by(pin) %>%
  mutate(cards_in_history = n_distinct(card_num)) %>%
  ungroup()

# Prefer the latest report through 2022; use the 2025 window, then later
# reports, only for PINs absent from the earlier window.
single_card <- data %>%
  filter(cards_in_history == 1, year_built >= minimum_construction_year) %>%
  mutate(report_priority = case_when(
    tax_year <= preferred_assessment_year ~ 1L,
    tax_year <= fallback_assessment_year ~ 2L,
    TRUE ~ 3L
  )) %>%
  arrange(pin, report_priority, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) %>%
  group_by(pin) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-report_priority)

multicard <- data %>%
  filter(cards_in_history > 1, year_built >= minimum_construction_year) %>%
  group_by(pin) %>%
  slice_min(order_by = year_built, with_ties = TRUE) %>%
  slice_min(order_by = tax_year, with_ties = TRUE) %>%
  slice_max(order_by = building_sqft, with_ties = FALSE) %>%
  ungroup()

cross_section_buildings <- bind_rows(
  single_card,
  multicard
) %>%
  select(-cards_in_history) %>%
  arrange(pin)

if (any(cross_section_buildings$year_built < minimum_construction_year, na.rm = TRUE)) {
  stop("New-construction residential cross-section contains buildings before the minimum construction year.", call. = FALSE)
}

if (anyDuplicated(cross_section_buildings$pin) > 0) {
  stop("Residential new-construction cross-section is not unique by PIN.", call. = FALSE)
}

dbDisconnect(con, shutdown = TRUE)

write_csv(cross_section_buildings, "../output/residential_cross_section.csv")
