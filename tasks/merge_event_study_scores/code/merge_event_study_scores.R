source("../../setup_environment/code/packages.R")


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_event_study_scores/code")
# mode <- "all"
# score_file <- "../input/aldermen_uncertainty_scores.csv"
# score_column <- "uncertainty_index"
# sales_input <- "../input/sales_pre_scores.csv"
# rent_input <- "../input/rent_pre_scores_full.parquet"
# treatment_input <- "../input/block_treatment_pre_scores.csv"
# alderman_panel <- "../input/chicago_alderman_panel.csv"
# sales_output <- "../output/sales_with_ward_distances.csv"
# rent_output <- "../output/rent_with_ward_distances_full.parquet"
# treatment_output <- "../output/block_treatment_panel.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(mode, score_file, score_column, sales_input, rent_input, treatment_input, alderman_panel, sales_output, rent_output, treatment_output)
}

mode <- cli_args[1]
if (!mode %in% c("sales_treatment", "rent", "all")) {
  stop("FATAL: mode must be 'sales_treatment', 'rent', or 'all'.", call. = FALSE)
}

if (mode == "sales_treatment") {
  if (length(cli_args) != 8) {
    stop("FATAL: sales_treatment mode requires 8 args: sales_treatment <score_file> <score_column> <sales_input> <treatment_input> <alderman_panel> <sales_output> <treatment_output>", call. = FALSE)
  }
  score_file <- cli_args[2]
  score_column <- cli_args[3]
  sales_input <- cli_args[4]
  treatment_input <- cli_args[5]
  alderman_panel <- cli_args[6]
  sales_output <- cli_args[7]
  treatment_output <- cli_args[8]
} else if (mode == "rent") {
  if (length(cli_args) != 5) {
    stop("FATAL: rent mode requires 5 args: rent <score_file> <score_column> <rent_input> <rent_output>", call. = FALSE)
  }
  score_file <- cli_args[2]
  score_column <- cli_args[3]
  rent_input <- cli_args[4]
  rent_output <- cli_args[5]
} else {
  if (length(cli_args) != 10) {
    stop("FATAL: all mode requires 10 args: all <score_file> <score_column> <sales_input> <rent_input> <treatment_input> <alderman_panel> <sales_output> <rent_output> <treatment_output>", call. = FALSE)
  }
  score_file <- cli_args[2]
  score_column <- cli_args[3]
  sales_input <- cli_args[4]
  rent_input <- cli_args[5]
  treatment_input <- cli_args[6]
  alderman_panel <- cli_args[7]
  sales_output <- cli_args[8]
  rent_output <- cli_args[9]
  treatment_output <- cli_args[10]
}

cat("=== Merge Event Study Scores ===\n")
cat("Mode:", mode, "\n")
cat("Score file:", score_file, "\n")
cat("Score column:", score_column, "\n")

scores_raw <- read_csv(score_file, show_col_types = FALSE)
if (!score_column %in% names(scores_raw)) {
  stop(sprintf(
    "Score column '%s' not found. Available: %s",
    score_column, paste(names(scores_raw), collapse = ", ")
  ), call. = FALSE)
}

scores <- scores_raw %>%
  select(alderman, score = all_of(score_column)) %>%
  distinct()

merge_border_scores <- function(df, dist_col = "dist_m", signed_dist_col = "signed_dist_m") {
  if (!"alderman_own" %in% names(df) || !"alderman_neighbor" %in% names(df)) {
    stop("Expected columns alderman_own and alderman_neighbor are missing.", call. = FALSE)
  }
  if (!dist_col %in% names(df)) {
    stop(sprintf("Expected distance column '%s' is missing.", dist_col), call. = FALSE)
  }

  out <- df %>%
    left_join(scores, by = c("alderman_own" = "alderman")) %>%
    rename(strictness_own = score) %>%
    left_join(scores, by = c("alderman_neighbor" = "alderman")) %>%
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
  sales_pre <- read_csv(sales_input, show_col_types = FALSE)
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
  write_csv(sales, sales_output)
  cat("Sales output rows:", nrow(sales), "\n")

  cat("\nMerging scores into block treatment pre-scores...\n")
  treat_pre <- read_csv(treatment_input, show_col_types = FALSE)

  alderman_lookup <- read_csv(alderman_panel, show_col_types = FALSE) %>%
    mutate(
      month_date = as.Date(paste("01", month), format = "%d %b %Y"),
      year = as.integer(format(month_date, "%Y"))
    ) %>%
    filter(as.integer(format(month_date, "%m")) == 6) %>%
    select(year, ward, alderman) %>%
    distinct()

  treat_panel <- treat_pre %>%
    mutate(cohort = as.character(cohort)) %>%
    mutate(score_year = case_when(
      cohort == "2015" ~ 2014L,
      cohort == "2023" ~ 2022L,
      TRUE ~ NA_integer_
    )) %>%
    left_join(alderman_lookup %>% rename(ward_origin = ward, alderman_origin = alderman),
      by = c("score_year" = "year", "ward_origin")
    ) %>%
    left_join(alderman_lookup %>% rename(ward_dest = ward, alderman_dest = alderman),
      by = c("score_year" = "year", "ward_dest")
    ) %>%
    left_join(scores %>% rename(alderman_origin = alderman, strictness_origin = score),
      by = "alderman_origin"
    ) %>%
    left_join(scores %>% rename(alderman_dest = alderman, strictness_dest = score),
      by = "alderman_dest"
    ) %>%
    mutate(
      strictness_change = strictness_dest - strictness_origin,
      switch_type = case_when(
        is.na(strictness_change) ~ "No Data",
        strictness_change > 0 ~ "Moved to Stricter",
        strictness_change < 0 ~ "Moved to More Lenient",
        TRUE ~ "No Change"
      )
    ) %>%
    select(
      block_id, block_vintage, ward_origin, ward_dest, switched,
      any_of(c(
        "ward_origin_share", "ward_dest_share", "ward_origin_n_wards",
        "ward_dest_n_wards", "min_assignment_share"
      )),
      strictness_origin, strictness_dest, strictness_change, switch_type,
      ward_had_turnover, valid, cohort
    )

  write_csv(treat_panel, treatment_output)
  cat("Block treatment output rows:", nrow(treat_panel), "\n")
}

if (mode %in% c("rent", "all")) {
  cat("\nMerging scores into rent pre-scores...\n")
  rent_pre <- read_parquet(rent_input) %>% as_tibble()
  if (!"dist_m" %in% names(rent_pre)) {
    stop("Rent pre-score input must include meter-native dist_m.", call. = FALSE)
  }
  rent <- merge_border_scores(rent_pre, "dist_m", "signed_dist_m")
  write_parquet(rent, rent_output)
  cat("Rent output rows:", nrow(rent), "\n")
}

cat("\n=== Merge complete ===\n")
if (mode %in% c("sales_treatment", "all")) {
  cat("Sales output:", sales_output, "\n")
  cat("Block treatment output:", treatment_output, "\n")
}
if (mode %in% c("rent", "all")) {
  cat("Rent output:", rent_output, "\n")
}
