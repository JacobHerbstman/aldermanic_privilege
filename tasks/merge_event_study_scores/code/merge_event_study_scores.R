source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/merge_event_study_scores/code")
# score_file <- "../input/aldermen_uncertainty_scores.csv"
# score_column <- "uncertainty_index"
# sales_input <- "../input/sales_pre_scores.csv"
# rent_input <- "../input/rent_pre_scores_full.parquet"
# treatment_input <- "../input/block_treatment_pre_scores.csv"
# alderman_panel <- "../input/chicago_alderman_panel.csv"
# sales_output <- "../output/sales_with_ward_distances.csv"
# rent_output <- "../output/rent_with_ward_distances_full.parquet"
# treatment_output <- "../output/block_treatment_panel.csv"
# Rscript merge_event_study_scores.R "../input/aldermen_uncertainty_scores.csv" "uncertainty_index" "../input/sales_pre_scores.csv" "../input/rent_pre_scores_full.parquet" "../input/block_treatment_pre_scores.csv" "../input/chicago_alderman_panel.csv" "../output/sales_with_ward_distances.csv" "../output/rent_with_ward_distances_full.parquet" "../output/block_treatment_panel.csv"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 9) {
  score_file <- cli_args[1]
  score_column <- cli_args[2]
  sales_input <- cli_args[3]
  rent_input <- cli_args[4]
  treatment_input <- cli_args[5]
  alderman_panel <- cli_args[6]
  sales_output <- cli_args[7]
  rent_output <- cli_args[8]
  treatment_output <- cli_args[9]
} else {
  if (!exists("score_file") || !exists("score_column") || !exists("sales_input") || !exists("rent_input") || !exists("treatment_input") || !exists("alderman_panel") || !exists("sales_output") || !exists("rent_output") || !exists("treatment_output")) {
    stop("FATAL: Script requires 9 args: <score_file> <score_column> <sales_input> <rent_input> <treatment_input> <alderman_panel> <sales_output> <rent_output> <treatment_output>", call. = FALSE)
  }
}

cat("=== Merge Event Study Scores ===\n")
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

merge_border_scores <- function(df, dist_col = "dist_ft") {
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
      signed_dist = .data[[dist_col]] * sign
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

cat("\nMerging scores into sales pre-scores...\n")
sales_pre <- read_csv(sales_input, show_col_types = FALSE)
sales <- merge_border_scores(sales_pre, "dist_ft") %>%
  select(
    pin, year, sale_date, sale_price,
    any_of(c(
      "sale_price_nominal",
      "sale_price_real_2022_raw",
      "sale_price_cpi_chi_ex_shelter",
      "sale_price_deflator_to_2022"
    )),
    class,
    latitude, longitude, ward, neighbor_ward, ward_pair_id, any_of("segment_id"),
    dist_ft, signed_dist, sign,
    alderman_own, alderman_neighbor,
    strictness_own, strictness_neighbor
  )
write_csv(sales, sales_output)
cat("Sales output rows:", nrow(sales), "\n")

cat("\nMerging scores into rent pre-scores...\n")
rent_pre <- read_parquet(rent_input) %>% as_tibble()
rent <- merge_border_scores(rent_pre, "dist_ft")
write_parquet(rent, rent_output)
cat("Rent output rows:", nrow(rent), "\n")

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
    strictness_origin, strictness_dest, strictness_change, switch_type,
    ward_had_turnover, valid, cohort
  )

write_csv(treat_panel, treatment_output)
cat("Block treatment output rows:", nrow(treat_panel), "\n")

cat("\n=== Merge complete ===\n")
cat("Sales output:", sales_output, "\n")
cat("Rent output:", rent_output, "\n")
cat("Block treatment output:", treatment_output, "\n")
