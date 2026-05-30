# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_event_study_scores/code")
# mode <- "sales_treatment"
# score_column <- "uncertainty_index"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(mode, score_column)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <mode> <score_column>.", call. = FALSE)
}

mode <- cli_args[1]
score_column <- cli_args[2]

if (!mode %in% c("sales_treatment", "rent")) {
  stop("FATAL: mode must be 'sales_treatment' or 'rent'.", call. = FALSE)
}

scores_raw <- read_csv("../input/aldermen_uncertainty_scores.csv", show_col_types = FALSE)
if (!score_column %in% names(scores_raw)) {
  stop(sprintf(
    "Score column '%s' not found. Available: %s",
    score_column, paste(names(scores_raw), collapse = ", ")
  ), call. = FALSE)
}

scores <- scores_raw %>%
  select(alderman, score = all_of(score_column)) %>%
  filter(!is.na(alderman))
if (anyDuplicated(scores$alderman) > 0) {
  stop("Score input must be unique by alderman.", call. = FALSE)
}
if (any(!is.finite(scores$score))) {
  stop("Score input contains non-finite scores.", call. = FALSE)
}

merge_border_scores <- function(df, dist_col = "dist_m", signed_dist_col = "signed_dist_m") {
  if (!"alderman_own" %in% names(df) || !"alderman_neighbor" %in% names(df)) {
    stop("Expected columns alderman_own and alderman_neighbor are missing.", call. = FALSE)
  }
  if (!dist_col %in% names(df)) {
    stop(sprintf("Expected distance column '%s' is missing.", dist_col), call. = FALSE)
  }

  out <- df %>%
    left_join(scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_own = score) %>%
    left_join(scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_neighbor = score) %>%
    mutate(
      sign = case_when(
        strictness_own > strictness_neighbor ~ 1,
        strictness_own < strictness_neighbor ~ -1,
        TRUE ~ NA_real_
      ),
      "{signed_dist_col}" := .data[[dist_col]] * sign,
      dist_ft = .data[[dist_col]] / 0.3048
    )

  if (signed_dist_col == "signed_dist_m") {
    out <- out %>%
      mutate(signed_dist = signed_dist_m / 0.3048)
  }

  if (nrow(out) != nrow(df)) {
    stop(sprintf(
      "Row-count mismatch after score merge: input=%d output=%d",
      nrow(df), nrow(out)
    ), call. = FALSE)
  }

  filter(out, !is.na(sign))
}

if (mode == "sales_treatment") {
  sales_pre <- read_csv("../input/sales_pre_scores.csv", show_col_types = FALSE)
  if (!"dist_m" %in% names(sales_pre)) {
    stop("Sales pre-score input must include meter-native dist_m.", call. = FALSE)
  }
  sales <- merge_border_scores(sales_pre, "dist_m", "signed_dist_m") %>%
    select(
      pin, year, sale_date, sale_price,
      any_of(c(
        "sale_price_nominal",
        "sale_price_real_2022_raw",
        "sale_price_cpi_chi_all_items",
        "sale_price_deflator_to_2022"
      )),
      class,
      latitude, longitude, ward, neighbor_ward, ward_pair_id, any_of("segment_id"),
      dist_m, signed_dist_m, sign,
      alderman_own, alderman_neighbor,
      strictness_own, strictness_neighbor
  )
  write_csv(sales, "../output/sales_with_ward_distances.csv")

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
    left_join(scores %>% rename(alderman_origin = alderman, strictness_origin = score),
      by = "alderman_origin",
      relationship = "many-to-one"
    ) %>%
    left_join(scores %>% rename(alderman_dest = alderman, strictness_dest = score),
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

  write_csv(treat_panel, "../output/block_treatment_panel.csv")
}

if (mode == "rent") {
  rent_pre <- read_parquet("../input/rent_pre_scores_full.parquet") %>% as_tibble()
  if (!"dist_m" %in% names(rent_pre)) {
    stop("Rent pre-score input must include meter-native dist_m.", call. = FALSE)
  }
  rent <- merge_border_scores(rent_pre, "dist_m", "signed_dist_m")
  if (!"rent_panel_id" %in% names(rent)) {
    stop("Rent score merge output must include rent_panel_id.", call. = FALSE)
  }
  if (any(is.na(rent$rent_panel_id) | rent$rent_panel_id == "")) {
    stop("Rent score merge output contains missing rent_panel_id values.", call. = FALSE)
  }
  if (anyDuplicated(rent$rent_panel_id) > 0) {
    stop("Rent score merge output must be unique by rent_panel_id.", call. = FALSE)
  }
  n_signed_dist_sign_mismatch <- sum(
    is.finite(rent$signed_dist) & rent$signed_dist != 0 &
      sign(rent$signed_dist) != rent$sign,
    na.rm = TRUE
  )
  if (n_signed_dist_sign_mismatch > 0) {
    stop("Rental signed-distance sign does not agree with strictness sign.", call. = FALSE)
  }
  write_parquet(rent, "../output/rent_with_ward_distances_full.parquet")
}
