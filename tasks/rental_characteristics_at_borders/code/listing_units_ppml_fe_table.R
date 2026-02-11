source("../../setup_environment/code/packages.R")
library(optparse)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--bw_ft", type = "integer", default = 1000),
  make_option("--window", type = "character", default = "pre_2021"),
  make_option("--sample_filter", type = "character", default = "all"),
  make_option("--min_strictness_diff_pctile", type = "integer", default = 0),
  make_option("--output_tex", type = "character"),
  make_option("--output_csv", type = "character")
)
opt <- parse_args(OptionParser(option_list = option_list))

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

message(sprintf("=== Listing Units PPML FE Table | bw=%d | window=%s | sample=%s | pctile=%d ===",
                opt$bw_ft, opt$window, opt$sample_filter, opt$min_strictness_diff_pctile))

dat <- read_parquet(opt$input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    right = as.integer(signed_dist >= 0),
    listing_id = as.character(id),
    listing_fallback = paste(
      round(latitude, 5),
      round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    ),
    listing_key = if_else(!is.na(listing_id) & listing_id != "", listing_id, listing_fallback),
    strict_more = pmax(strictness_own, strictness_neighbor),
    strict_less = pmin(strictness_own, strictness_neighbor)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
    !is.na(latitude), !is.na(longitude), !is.na(listing_key),
    abs(signed_dist) <= opt$bw_ft
  ) %>%
  apply_window(opt$window)

if (opt$sample_filter == "multifamily_only") {
  dat <- dat %>% filter(building_type_clean == "multi_family")
}

if (opt$min_strictness_diff_pctile > 0) {
  pair_diffs <- dat %>%
    group_by(ward_pair) %>%
    summarise(diff = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE), .groups = "drop")
  cutoff <- quantile(pair_diffs$diff, opt$min_strictness_diff_pctile / 100, na.rm = TRUE)
  keep_pairs <- pair_diffs %>% filter(diff >= cutoff) %>% pull(ward_pair)
  dat <- dat %>% filter(ward_pair %in% keep_pairs)
  message(sprintf("  After p%d filter (cutoff=%.3f): %d obs, %d pairs",
                  opt$min_strictness_diff_pctile, cutoff, nrow(dat), n_distinct(dat$ward_pair)))
}

pair_month_map <- dat %>%
  group_by(ward_pair, year_month) %>%
  summarise(
    strict_more = max(strict_more, na.rm = TRUE),
    strict_less = min(strict_less, na.rm = TRUE),
    .groups = "drop"
  )

side_template <- bind_rows(
  pair_month_map %>% transmute(ward_pair, year_month, right = 0L, strictness_own = strict_less),
  pair_month_map %>% transmute(ward_pair, year_month, right = 1L, strictness_own = strict_more)
)

side_counts <- dat %>%
  distinct(ward_pair, right, year_month, listing_key) %>%
  count(ward_pair, right, year_month, name = "n_units")

panel <- side_template %>%
  left_join(side_counts, by = c("ward_pair", "right", "year_month")) %>%
  mutate(n_units = as.integer(coalesce(n_units, 0L)))

stopifnot(nrow(panel) > 0, n_distinct(panel$ward_pair) >= 2)

strictness_sd <- sd(panel$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero/invalid SD in this sample.", call. = FALSE)
}

panel <- panel %>%
  mutate(strictness_std = strictness_own / strictness_sd)

m <- fepois(n_units ~ strictness_std | ward_pair^year_month, data = panel, cluster = ~ward_pair)
ct <- coeftable(m)
p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
if (length(p_col) == 0) {
  stop("Could not find p-value column in coeftable output.", call. = FALSE)
}

pair_month_obs <- side_counts %>%
  count(ward_pair, year_month, name = "n_sides_obs")

pair_month_diag <- pair_month_map %>%
  left_join(pair_month_obs, by = c("ward_pair", "year_month")) %>%
  mutate(n_sides_obs = coalesce(n_sides_obs, 0L))

out <- tibble(
  estimate = ct["strictness_std", "Estimate"],
  std_error = ct["strictness_std", "Std. Error"],
  p_value = ct["strictness_std", p_col[1]],
  implied_pct_change = 100 * (exp(ct["strictness_std", "Estimate"]) - 1),
  n_obs = nobs(m),
  ward_pairs = n_distinct(panel$ward_pair),
  pair_month_cells = nrow(pair_month_map),
  share_single_sided_pair_month = mean(pair_month_diag$n_sides_obs == 1),
  share_zero_cells = mean(panel$n_units == 0),
  mean_units_per_cell = mean(panel$n_units, na.rm = TRUE),
  strictness_sd = strictness_sd,
  bandwidth_ft = opt$bw_ft,
  window = opt$window,
  sample_filter = opt$sample_filter,
  min_strictness_diff_pctile = opt$min_strictness_diff_pctile
)

write_csv(out, opt$output_csv)

coef_str <- sprintf("%.4f%s", out$estimate, stars(out$p_value))
se_str <- sprintf("(%.4f)", out$std_error)

tex <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lc}",
  "  \\toprule",
  "  & Distinct Listed Units (PPML) \\\\",
  "  \\midrule",
  sprintf("  Uncertainty Index (1 SD) & %s \\\\", coef_str),
  sprintf("  & %s \\\\", se_str),
  "  \\\\",
  sprintf("  Implied \\%% change (1 SD) & %.2f\\%% \\\\", out$implied_pct_change),
  sprintf("  Observations & %s \\\\", format(out$n_obs, big.mark = ",")),
  sprintf("  Ward Pairs & %s \\\\", format(out$ward_pairs, big.mark = ",")),
  sprintf("  Pair-Month Cells & %s \\\\", format(out$pair_month_cells, big.mark = ",")),
  sprintf("  Single-Sided Pair-Month Share & %.1f\\%% \\\\", 100 * out$share_single_sided_pair_month),
  sprintf("  Zero-Count Cell Share & %.1f\\%% \\\\", 100 * out$share_zero_cells),
  sprintf("  Mean Units per Cell & %.2f \\\\", out$mean_units_per_cell),
  "  Estimator & PPML \\\\",
  "  Ward-Pair $\\times$ Year-Month FE & $\\checkmark$ \\\\",
  "  \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)
writeLines(tex, opt$output_tex)

message(sprintf("  b=%.4f (SE %.4f, p=%.3f), implied=%.2f%%",
                out$estimate, out$std_error, out$p_value, out$implied_pct_change))
message(sprintf("Saved: %s", opt$output_tex))
message(sprintf("Saved: %s", opt$output_csv))
