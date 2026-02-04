source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--score_file", type = "character",
    default = "../input/aldermen_uncertainty_scores.csv"
  ),
  make_option("--score_column", type = "character", default = "uncertainty_index"),
  make_option("--sales_input", type = "character", default = "../input/sales_pre_scores.csv"),
  make_option("--rent_input", type = "character", default = "../input/rent_pre_scores_full.parquet"),
  make_option("--treatment_input", type = "character", default = "../input/block_treatment_pre_scores.csv"),
  make_option("--alderman_panel", type = "character", default = "../input/chicago_alderman_panel.csv"),
  make_option("--sales_output", type = "character", default = "../output/sales_with_ward_distances.csv"),
  make_option("--rent_output", type = "character", default = "../output/rent_with_ward_distances_full.parquet"),
  make_option("--treatment_output", type = "character", default = "../output/block_treatment_panel.csv")
)
opt <- parse_args(OptionParser(option_list = option_list))

cat("=== Merge Event Study Scores ===\n")
cat("Score file:", opt$score_file, "\n")
cat("Score column:", opt$score_column, "\n")

scores_raw <- read_csv(opt$score_file, show_col_types = FALSE)
if (!opt$score_column %in% names(scores_raw)) {
  stop(sprintf(
    "Score column '%s' not found. Available: %s",
    opt$score_column, paste(names(scores_raw), collapse = ", ")
  ), call. = FALSE)
}

scores <- scores_raw %>%
  select(alderman, score = all_of(opt$score_column)) %>%
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
    ) %>%
    filter(!is.na(sign))

  out
}

cat("\nMerging scores into sales pre-scores...\n")
sales_pre <- read_csv(opt$sales_input, show_col_types = FALSE)
sales <- merge_border_scores(sales_pre, "dist_ft") %>%
  select(
    pin, year, sale_date, sale_price, class,
    latitude, longitude, ward, neighbor_ward, ward_pair_id,
    dist_ft, signed_dist, sign,
    alderman_own, alderman_neighbor,
    strictness_own, strictness_neighbor
  )
write_csv(sales, opt$sales_output)
cat("Sales output rows:", nrow(sales), "\n")

cat("\nMerging scores into rent pre-scores...\n")
rent_pre <- read_parquet(opt$rent_input) %>% as_tibble()
rent <- merge_border_scores(rent_pre, "dist_ft")
write_parquet(rent, opt$rent_output)
cat("Rent output rows:", nrow(rent), "\n")

cat("\nMerging scores into block treatment pre-scores...\n")
treat_pre <- read_csv(opt$treatment_input, show_col_types = FALSE)

alderman_lookup <- read_csv(opt$alderman_panel, show_col_types = FALSE) %>%
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

write_csv(treat_panel, opt$treatment_output)
cat("Block treatment output rows:", nrow(treat_panel), "\n")

cat("\n=== Merge complete ===\n")
cat("Sales output:", opt$sales_output, "\n")
cat("Rent output:", opt$rent_output, "\n")
cat("Block treatment output:", opt$treatment_output, "\n")
