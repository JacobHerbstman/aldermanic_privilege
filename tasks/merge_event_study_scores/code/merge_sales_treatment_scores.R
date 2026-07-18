# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_event_study_scores/code")

source("../../setup_environment/code/packages.R")

sales_scores_raw <- read_csv("../input/aldermen_uncertainty_scores_through2022.csv", show_col_types = FALSE)
if (!"uncertainty_index" %in% names(sales_scores_raw)) {
  stop("Through-2022 score input must include uncertainty_index.", call. = FALSE)
}

sales_scores <- sales_scores_raw %>%
  select(alderman, score = uncertainty_index) %>%
  filter(!is.na(alderman))
if (anyDuplicated(sales_scores$alderman) > 0) {
  stop("Through-2022 score input must be unique by alderman.", call. = FALSE)
}
if (any(!is.finite(sales_scores$score))) {
  stop("Through-2022 score input contains non-finite scores.", call. = FALSE)
}

sales_pre <- read_csv(
  "../input/sales_pre_scores.csv",
  col_types = cols(pin = col_character(), .default = col_guess()),
  show_col_types = FALSE
)
if (!all(c("alderman_own", "alderman_neighbor", "dist_m") %in% names(sales_pre))) {
  stop("Sales pre-score input must include alderman_own, alderman_neighbor, and dist_m.", call. = FALSE)
}
sales_pre <- sales_pre %>%
  mutate(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin)
  )
if (any(nchar(sales_pre$pin) != 14L)) {
  stop("Sales input contains an invalid full PIN.", call. = FALSE)
}

sales <- sales_pre %>%
  left_join(sales_scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_own = score) %>%
  left_join(sales_scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_dist_m = dist_m * sign,
    dist_ft = dist_m / 0.3048,
    signed_dist = signed_dist_m / 0.3048
  )

if (nrow(sales) != nrow(sales_pre)) {
  stop("Row-count mismatch after sales score merge.", call. = FALSE)
}

sales <- sales %>%
  filter(!is.na(sign)) %>%
  select(
    any_of(c("row_id", "sale_document_num")),
    pin, year, sale_date, sale_price,
    any_of(c(
      "sale_price_nominal",
      "sale_price_real_2022_raw",
      "sale_price_cpi_chi_all_items",
      "sale_price_deflator_to_2022"
    )),
    class,
    latitude, longitude, any_of(c("coordinate_source", "coordinate_year")),
    ward, neighbor_ward, ward_pair_id, any_of("segment_id"),
    dist_m, signed_dist_m, sign,
    alderman_own, alderman_neighbor,
    strictness_own, strictness_neighbor
  )

write_csv(sales, "../output/sales_with_ward_distances_through2022.csv")

treatment_scores_raw <- read_csv("../input/aldermen_uncertainty_scores_through202604.csv", show_col_types = FALSE)
if (!"uncertainty_index" %in% names(treatment_scores_raw)) {
  stop("Through-202604 score input must include uncertainty_index.", call. = FALSE)
}

treatment_scores <- treatment_scores_raw %>%
  select(alderman, score = uncertainty_index) %>%
  filter(!is.na(alderman))
if (anyDuplicated(treatment_scores$alderman) > 0) {
  stop("Through-202604 score input must be unique by alderman.", call. = FALSE)
}
if (any(!is.finite(treatment_scores$score))) {
  stop("Through-202604 score input contains non-finite scores.", call. = FALSE)
}

treat_pre <- read_csv("../input/block_treatment_pre_scores.csv", show_col_types = FALSE)
if (anyDuplicated(paste(treat_pre$cohort, treat_pre$block_id, sep = "\r")) > 0) {
  stop("Block treatment pre-score input must be unique by cohort-block.", call. = FALSE)
}

alderman_lookup_raw <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(
    month_date = as.Date(paste("01", month), format = "%d %b %Y"),
    month_key = format(month_date, "%Y-%m")
  ) %>%
  select(month_key, ward, alderman)
alderman_lookup_counts <- alderman_lookup_raw %>%
  count(month_key, ward, name = "n_lookup_matches")
if (any(alderman_lookup_counts$n_lookup_matches > 1L)) {
  stop("Alderman lookup must be unique by month-ward.", call. = FALSE)
}
alderman_lookup <- alderman_lookup_raw %>%
  distinct()

treat_panel_full <- treat_pre %>%
  mutate(cohort = as.character(cohort)) %>%
  mutate(
    score_month_origin = case_when(
      cohort == "2015" ~ "2014-06",
      cohort == "2023" ~ "2023-04",
      TRUE ~ NA_character_
    ),
    score_month_dest = case_when(
      cohort == "2015" ~ "2015-06",
      cohort == "2023" ~ "2023-06",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(alderman_lookup_counts %>% rename(ward_origin = ward, origin_lookup_matches = n_lookup_matches),
    by = c("score_month_origin" = "month_key", "ward_origin"),
    relationship = "many-to-one"
  ) %>%
  left_join(alderman_lookup_counts %>% rename(ward_dest = ward, dest_lookup_matches = n_lookup_matches),
    by = c("score_month_dest" = "month_key", "ward_dest"),
    relationship = "many-to-one"
  ) %>%
  left_join(alderman_lookup %>% rename(ward_origin = ward, alderman_origin = alderman),
    by = c("score_month_origin" = "month_key", "ward_origin"),
    relationship = "many-to-one"
  ) %>%
  left_join(alderman_lookup %>% rename(ward_dest = ward, alderman_dest = alderman),
    by = c("score_month_dest" = "month_key", "ward_dest"),
    relationship = "many-to-one"
  ) %>%
  left_join(treatment_scores %>% rename(alderman_origin = alderman, strictness_origin = score),
    by = "alderman_origin",
    relationship = "many-to-one"
  ) %>%
  left_join(treatment_scores %>% rename(alderman_dest = alderman, strictness_dest = score),
    by = "alderman_dest",
    relationship = "many-to-one"
  ) %>%
  mutate(
    strictness_change = strictness_dest - strictness_origin,
    switch_type = case_when(
      is.na(strictness_change) ~ "No Data",
      strictness_change > 0 ~ "Moved to Stricter",
      strictness_change < 0 ~ "Moved to More Lenient",
      TRUE ~ "No Change"
    )
  )

if (nrow(treat_panel_full) != nrow(treat_pre)) {
  stop("Row-count mismatch after block treatment score merge.", call. = FALSE)
}
if (anyDuplicated(paste(treat_panel_full$cohort, treat_panel_full$block_id, sep = "\r")) > 0) {
  stop("Merged block treatment panel must be unique by cohort-block.", call. = FALSE)
}
if (any(treat_panel_full$valid & (is.na(treat_panel_full$alderman_origin) | treat_panel_full$alderman_origin == ""), na.rm = TRUE) ||
    any(treat_panel_full$valid & (is.na(treat_panel_full$alderman_dest) | treat_panel_full$alderman_dest == ""), na.rm = TRUE)) {
  stop("Valid block treatment rows have missing alderman lookup values.", call. = FALSE)
}
if (any(treat_panel_full$valid & is.na(treat_panel_full$strictness_origin), na.rm = TRUE) ||
    any(treat_panel_full$valid & is.na(treat_panel_full$strictness_dest), na.rm = TRUE)) {
  stop("Valid block treatment rows have missing alderman scores.", call. = FALSE)
}
if (any(treat_panel_full$valid & is.na(treat_panel_full$strictness_change), na.rm = TRUE)) {
  stop("Valid block treatment rows have missing strictness changes.", call. = FALSE)
}

treat_panel <- treat_panel_full %>%
  select(
    block_id, block_vintage, ward_origin, ward_dest, switched,
    any_of(c(
      "ward_origin_share", "ward_dest_share", "ward_origin_n_wards",
      "ward_dest_n_wards", "min_assignment_share",
      "has_complete_ward_assignment"
    )),
    score_month_origin, score_month_dest, alderman_origin, alderman_dest,
    strictness_origin, strictness_dest, strictness_change, switch_type,
    ward_had_turnover, valid, cohort
  )

write_csv(treat_panel, "../output/block_treatment_panel_through202604.csv")
