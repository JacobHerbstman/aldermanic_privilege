source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# min_strictness_diff_pctile <- 0
# output_tex <- NA
# output_csv <- NA
# Rscript listing_units_fe_table.R "../input/rent_with_ward_distances.parquet" 1000 "pre_2021" "all" 0 NA NA
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 7) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[5]))
  output_tex <- cli_args[6]
  output_csv <- cli_args[7]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("min_strictness_diff_pctile") || !exists("output_tex") || !exists("output_csv")) {
    stop("FATAL: Script requires 7 args: <input> <bw_ft> <window> <sample_filter> <min_strictness_diff_pctile> <output_tex> <output_csv>", call. = FALSE)
  }
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

apply_window <- function(df, w) {
  if (w == "full") return(df)
  if (w == "pre_2021") return(df %>% filter(year <= 2020))
  if (w == "pre_2023") return(df %>% filter(year <= 2022))
  if (w == "pre_covid") return(df %>% filter(year <= 2019))
  df
}

message(sprintf("=== Listing Units FE Table | bw=%d | window=%s | sample=%s | pctile=%d ===",
                bw_ft, window, sample_filter, min_strictness_diff_pctile))

dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    listing_id = as.character(id),
    listing_fallback = paste(
      round(latitude, 5),
      round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    ),
    listing_key = if_else(!is.na(listing_id) & listing_id != "", listing_id, listing_fallback)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist), !is.na(strictness_own),
    !is.na(latitude), !is.na(longitude), !is.na(listing_key),
    abs(signed_dist) <= bw_ft
  ) %>%
  apply_window(window)

if (sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

if (min_strictness_diff_pctile > 0) {
  pair_diffs <- dat %>%
    group_by(ward_pair) %>%
    summarise(diff = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, min_strictness_diff_pctile / 100, na.rm = TRUE)
  keep_pairs <- pair_diffs %>% filter(diff >= cutoff) %>% pull(ward_pair)
  dat <- dat %>% filter(ward_pair %in% keep_pairs)
  message(sprintf("  After p%d filter (cutoff=%.3f): %d obs, %d pairs",
                  min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$ward_pair)))
}

strictness_sd <- sd(dat$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero/invalid SD in this sample.", call. = FALSE)
}

side_cells <- dat %>%
  mutate(
    right = as.integer(signed_dist >= 0),
    strictness_std = strictness_own / strictness_sd
  ) %>%
  distinct(ward_pair, right, year_month, listing_key, strictness_own, strictness_std) %>%
  group_by(ward_pair, right, year_month) %>%
  summarise(
    n_units = n(),
    strictness_own = mean(strictness_own, na.rm = TRUE),
    strictness_std = mean(strictness_std, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(log_n = log(n_units))

stopifnot(nrow(side_cells) > 0, n_distinct(side_cells$ward_pair) >= 2)

m <- feols(log_n ~ strictness_std | ward_pair^year_month, data = side_cells, cluster = ~ward_pair)
ct <- coeftable(m)

pair_month_sides <- side_cells %>%
  count(ward_pair, year_month, name = "n_sides")

out <- tibble(
  estimate = ct["strictness_std", "Estimate"],
  std_error = ct["strictness_std", "Std. Error"],
  p_value = ct["strictness_std", "Pr(>|t|)"],
  implied_pct_change = 100 * (exp(ct["strictness_std", "Estimate"]) - 1),
  n_obs = nobs(m),
  ward_pairs = n_distinct(side_cells$ward_pair),
  pair_month_cells = nrow(pair_month_sides),
  share_single_sided_pair_month = mean(pair_month_sides$n_sides == 1),
  dep_var_mean = mean(side_cells$log_n, na.rm = TRUE),
  mean_units_per_cell = mean(side_cells$n_units, na.rm = TRUE),
  strictness_sd = strictness_sd,
  bandwidth_ft = bw_ft,
  window = window,
  sample_filter = sample_filter,
  min_strictness_diff_pctile = min_strictness_diff_pctile
)

write_csv(out, output_csv)

coef_str <- sprintf("%.4f%s", out$estimate, stars(out$p_value))
se_str <- sprintf("(%.4f)", out$std_error)

tex <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lc}",
  "  \\toprule",
  "  & Log(Distinct Listed Units) \\\\",
  "  \\midrule",
  sprintf("  Uncertainty Index (1 SD) & %s \\\\", coef_str),
  sprintf("  & %s \\\\", se_str),
  "  \\\\",
  sprintf("  Implied \\%% change (1 SD) & %.2f\\%% \\\\", out$implied_pct_change),
  sprintf("  Observations & %s \\\\", format(out$n_obs, big.mark = ",")),
  sprintf("  Ward Pairs & %s \\\\", format(out$ward_pairs, big.mark = ",")),
  sprintf("  Pair-Month Cells & %s \\\\", format(out$pair_month_cells, big.mark = ",")),
  sprintf("  Single-Sided Pair-Month Share & %.1f\\%% \\\\", 100 * out$share_single_sided_pair_month),
  sprintf("  Dep. Var. Mean & %.3f \\\\", out$dep_var_mean),
  sprintf("  Mean Units per Cell & %.2f \\\\", out$mean_units_per_cell),
  "  Ward-Pair $\\times$ Year-Month FE & $\\checkmark$ \\\\",
  "  \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)
writeLines(tex, output_tex)

message(sprintf("  b=%.4f (SE %.4f, p=%.3f), implied=%.2f%%",
                out$estimate, out$std_error, out$p_value, out$implied_pct_change))
message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))