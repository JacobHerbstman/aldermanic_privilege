source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# input <- "../input/rent_with_ward_distances.parquet"
# bw_ft <- 1000
# window <- "pre_2021"
# sample_filter <- "all"
# min_strictness_diff_pctile <- 0
# unit_def <- "unit_proxy"
# output_tex <- NA
# output_csv <- NA
# Rscript listing_units_ppml_fe_table.R "../input/rent_with_ward_distances.parquet" 1000 "pre_2021" "all" 0 "unit_proxy" NA NA
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 8) {
  input <- cli_args[1]
  bw_ft <- suppressWarnings(as.integer(cli_args[2]))
  window <- cli_args[3]
  sample_filter <- cli_args[4]
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[5]))
  unit_def <- cli_args[6]
  output_tex <- cli_args[7]
  output_csv <- cli_args[8]
} else {
  if (!exists("input") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("min_strictness_diff_pctile") || !exists("unit_def") || !exists("output_tex") || !exists("output_csv")) {
    stop("FATAL: Script requires 8 args: <input> <bw_ft> <window> <sample_filter> <min_strictness_diff_pctile> <unit_def> <output_tex> <output_csv>", call. = FALSE)
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

message(sprintf("=== Listing Units PPML FE Table | bw=%d | window=%s | sample=%s | pctile=%d | unit_def=%s ===",
                bw_ft, window, sample_filter, min_strictness_diff_pctile, unit_def))

dat <- read_parquet(input) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
    right = as.integer(signed_dist >= 0),
    listing_id = as.character(id),
    loc_key = paste(round(latitude, 5), round(longitude, 5)),
    unit_proxy_key = paste(
      round(latitude, 5),
      round(longitude, 5),
      coalesce(as.character(beds), "NA"),
      coalesce(as.character(baths), "NA"),
      coalesce(as.character(sqft), "NA"),
      sep = "_"
    ),
    listing_key = if_else(!is.na(listing_id) & listing_id != "", listing_id, unit_proxy_key),
    strict_more = pmax(strictness_own, strictness_neighbor),
    strict_less = pmin(strictness_own, strictness_neighbor)
  ) %>%
  filter(
    !is.na(file_date), !is.na(ward_pair), !is.na(signed_dist),
    !is.na(strictness_own), !is.na(strictness_neighbor),
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

dat <- dat %>%
  mutate(unit_key = case_when(
    unit_def == "id" ~ listing_key,
    unit_def == "loc_key" ~ loc_key,
    unit_def == "unit_proxy" ~ unit_proxy_key,
    TRUE ~ unit_proxy_key
  )) %>%
  filter(!is.na(unit_key), unit_key != "")

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
  distinct(ward_pair, right, year_month, unit_key) %>%
  count(ward_pair, right, year_month, name = "n_units")

panel <- side_template %>%
  left_join(side_counts, by = c("ward_pair", "right", "year_month")) %>%
  mutate(n_units = as.integer(coalesce(n_units, 0L)))

stopifnot(nrow(panel) > 0, n_distinct(panel$ward_pair) >= 2)

strictness_sd <- sd(panel$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero/invalid SD in this sample.", call. = FALSE)
}

within_pair_gap <- panel %>%
  group_by(ward_pair, year_month) %>%
  summarise(gap = max(strictness_own) - min(strictness_own), .groups = "drop")
mean_within_pair_gap <- mean(within_pair_gap$gap, na.rm = TRUE)
within_pair_sd <- sd(within_pair_gap$gap, na.rm = TRUE)
message(sprintf("  Panel SD: %.4f | Mean within-pair gap: %.4f | SD of within-pair gap: %.4f",
                strictness_sd, mean_within_pair_gap, within_pair_sd))

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
  mean_within_pair_gap = mean_within_pair_gap,
  unit_def = unit_def,
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
  "  & Distinct Listed Units (PPML) \\\\",
  "  \\midrule",
  sprintf("  Uncertainty Index (1 SD) & %s \\\\", coef_str),
  sprintf("  & %s \\\\", se_str),
  "  \\\\",
  sprintf("  Implied \\%% change (1 SD) & %.2f\\%% \\\\", out$implied_pct_change),
  sprintf("  Observations & %s \\\\", format(out$n_obs, big.mark = ",")),
  sprintf("  Ward Pairs & %s \\\\", format(out$ward_pairs, big.mark = ",")),
  sprintf("  Pair-Month Cells & %s \\\\", format(out$pair_month_cells, big.mark = ",")),
  sprintf("  Mean Units per Cell & %.2f \\\\", out$mean_units_per_cell),
  sprintf("  Mean Within-Pair Gap & %.3f \\\\", out$mean_within_pair_gap),
  "  Estimator & PPML \\\\",
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