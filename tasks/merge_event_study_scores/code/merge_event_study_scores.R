source("../../setup_environment/code/packages.R")


# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_event_study_scores/code")
# mode <- "sales_treatment"
# score_column <- "uncertainty_index"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(mode, score_column)
}

mode <- cli_args[1]
if (!mode %in% c("sales_treatment", "rent", "all")) {
  stop("FATAL: mode must be 'sales_treatment', 'rent', or 'all'.", call. = FALSE)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <mode> <score_column>.", call. = FALSE)
}
score_column <- cli_args[2]

cat("=== Merge Event Study Scores ===\n")
cat("Mode:", mode, "\n")
cat("Score column:", score_column, "\n")

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
      "{signed_dist_col}" := .data[[dist_col]] * sign
    )

  if (nrow(out) != nrow(df)) {
    stop(sprintf(
      "Row-count mismatch after score merge: input=%d output=%d",
      nrow(df), nrow(out)
    ), call. = FALSE)
  }

  n_in <- nrow(out)
  n_missing_own   <- sum(is.na(out$strictness_own))
  n_missing_nbr   <- sum(is.na(out$strictness_neighbor))
  n_tied          <- sum(!is.na(out$strictness_own) & !is.na(out$strictness_neighbor) &
                           out$strictness_own == out$strictness_neighbor)
  n_dropped       <- sum(is.na(out$sign))
  cat(sprintf(
    "  Score merge: %d in | missing own score: %d | missing neighbor score: %d | tied (equal scores): %d | total dropped: %d (%.1f%%)\n",
    n_in, n_missing_own, n_missing_nbr, n_tied, n_dropped, 100 * n_dropped / n_in
  ))

  out <- filter(out, !is.na(sign))
  out
}

if (mode %in% c("sales_treatment", "all")) {
  cat("\nMerging scores into sales pre-scores...\n")
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
  cat("Sales output rows:", nrow(sales), "\n")

  cat("\nMerging scores into block treatment pre-scores...\n")
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

  summarize_lookup_role <- function(df, role) {
    score_month_col <- paste0("score_month_", role)
    ward_col <- paste0("ward_", role)
    alderman_col <- paste0("alderman_", role)
    score_col <- paste0("strictness_", role)
    lookup_col <- paste0(role, "_lookup_matches")

    df %>%
      summarise(
        role = role,
        score_month = paste(sort(unique(.data[[score_month_col]])), collapse = "|"),
        n_rows = n(),
        n_valid = sum(valid, na.rm = TRUE),
        n_unique_wards = n_distinct(.data[[ward_col]], na.rm = TRUE),
        n_missing_alderman_lookup = sum(is.na(.data[[alderman_col]]) | .data[[alderman_col]] == ""),
        n_valid_missing_alderman_lookup = sum(valid & (is.na(.data[[alderman_col]]) | .data[[alderman_col]] == ""), na.rm = TRUE),
        n_missing_score = sum(is.na(.data[[score_col]])),
        n_valid_missing_score = sum(valid & is.na(.data[[score_col]]), na.rm = TRUE),
        n_duplicate_ward_month_matches = sum(coalesce(.data[[lookup_col]], 0L) > 1L, na.rm = TRUE),
        .by = cohort
      ) %>%
      arrange(cohort)
  }

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

  score_lookup_diagnostics <- bind_rows(
    summarize_lookup_role(treat_panel_full, "origin"),
    summarize_lookup_role(treat_panel_full, "dest")
  ) %>%
    arrange(cohort, role)
  if (any(score_lookup_diagnostics$n_duplicate_ward_month_matches > 0L)) {
    stop("Block treatment score lookup found duplicate ward-month matches.", call. = FALSE)
  }
  if (any(score_lookup_diagnostics$n_valid_missing_alderman_lookup > 0L)) {
    stop("Valid block treatment rows have missing alderman lookup values.", call. = FALSE)
  }
  if (any(score_lookup_diagnostics$n_valid_missing_score > 0L)) {
    stop("Valid block treatment rows have missing alderman scores.", call. = FALSE)
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
  cat("Block treatment output rows:", nrow(treat_panel), "\n")

  score_diagnostics <- treat_panel %>%
    summarise(
      n_rows = n(),
      n_valid = sum(valid, na.rm = TRUE),
      n_switched = sum(switched, na.rm = TRUE),
      n_missing_origin_alderman = sum(is.na(alderman_origin) | alderman_origin == ""),
      n_missing_dest_alderman = sum(is.na(alderman_dest) | alderman_dest == ""),
      n_missing_origin_score = sum(is.na(strictness_origin)),
      n_missing_dest_score = sum(is.na(strictness_dest)),
      n_missing_strictness_change = sum(is.na(strictness_change)),
      n_valid_missing_strictness_change = sum(valid & is.na(strictness_change), na.rm = TRUE),
      n_unique_origin_aldermen = n_distinct(alderman_origin, na.rm = TRUE),
      n_unique_dest_aldermen = n_distinct(alderman_dest, na.rm = TRUE),
      .by = cohort
    ) %>%
    arrange(cohort)
  if (any(score_diagnostics$n_valid_missing_strictness_change > 0L)) {
    stop("Valid block treatment rows have missing strictness changes.", call. = FALSE)
  }
  write_csv(score_diagnostics, "../output/block_treatment_score_diagnostics.csv")
  write_csv(score_lookup_diagnostics, "../output/block_treatment_score_lookup_diagnostics.csv")
  cat("Block treatment score diagnostics rows:", nrow(score_diagnostics), "\n")
  cat("Block treatment score lookup diagnostics rows:", nrow(score_lookup_diagnostics), "\n")
}

if (mode %in% c("rent", "all")) {
  cat("\nMerging scores into rent pre-scores...\n")
  rent_pre <- read_parquet("../input/rent_pre_scores_full.parquet") %>% as_tibble()
  if (!"dist_m" %in% names(rent_pre)) {
    stop("Rent pre-score input must include meter-native dist_m.", call. = FALSE)
  }
  rent <- merge_border_scores(rent_pre, "dist_m", "signed_dist_m")
  write_parquet(rent, "../output/rent_with_ward_distances_full.parquet")
  cat("Rent output rows:", nrow(rent), "\n")
}

cat("\n=== Merge complete ===\n")
if (mode %in% c("sales_treatment", "all")) {
  cat("Sales output: ../output/sales_with_ward_distances.csv\n")
  cat("Block treatment output: ../output/block_treatment_panel.csv\n")
}
if (mode %in% c("rent", "all")) {
  cat("Rent output: ../output/rent_with_ward_distances_full.parquet\n")
}
