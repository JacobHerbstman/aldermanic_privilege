# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# bw_ft <- 500
# window <- "pre_2023"
# sample_filter <- "all"
# unit_def <- "unit_proxy"
# min_strictness_diff_pctile <- 0
# bins_per_side <- 8
# cluster_level <- "ward_pair"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bw_ft, window, sample_filter, unit_def, min_strictness_diff_pctile, bins_per_side, cluster_level)
}

if (length(cli_args) == 7) {
  bw_ft <- suppressWarnings(as.integer(cli_args[1]))
  window <- cli_args[2]
  sample_filter <- cli_args[3]
  unit_def <- cli_args[4]
  min_strictness_diff_pctile <- suppressWarnings(as.integer(cli_args[5]))
  bins_per_side <- suppressWarnings(as.integer(cli_args[6]))
  cluster_level <- tolower(cli_args[7])
} else {
  stop(
    paste(
      "FATAL: Script requires 7 args:",
      "<bw_ft> <window> <sample_filter> <unit_def>",
      "<min_strictness_diff_pctile> <bins_per_side> <cluster_level>"
    ),
    call. = FALSE
  )
}

if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("cluster_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("bw_ft must be a positive integer.", call. = FALSE)
}
if (!is.finite(bins_per_side) || bins_per_side <= 0) {
  stop("bins_per_side must be a positive integer.", call. = FALSE)
}

input_panel <- sprintf(
  "../output/listing_units_side_panel_bw%d_%s_%s_pct%d_%s_all.parquet",
  bw_ft, window, sample_filter, min_strictness_diff_pctile, unit_def
)
output_stem <- sprintf(
  "../output/listing_units_ppml_fe_table_bw%d_%s_%s_pct%d_clust_%s",
  bw_ft, window, sample_filter, min_strictness_diff_pctile, cluster_level
)
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair
cluster_label <- if (cluster_level == "segment") "Segment" else "Ward Pair"

panel <- read_parquet(input_panel) |>
  as_tibble()

required_cols <- c("segment_id", "year_month", "right", "strictness_own", "n_units")
if (cluster_level == "ward_pair") {
  required_cols <- c(required_cols, "ward_pair")
}
missing_cols <- setdiff(required_cols, names(panel))
if (length(missing_cols) > 0) {
  stop(sprintf("Cached side panel missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

strictness_sd <- sd(panel$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd) || strictness_sd <= 0) {
  stop("strictness_own has zero or invalid SD in cached panel.", call. = FALSE)
}

panel <- panel |>
  mutate(strictness_std = strictness_own / strictness_sd)

m <- fepois(n_units ~ strictness_std | segment_id^year_month, data = panel, cluster = cluster_formula)
ct <- coeftable(m)
p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)
if (length(p_col) == 0) {
  stop("Could not find p-value column in coeftable output.", call. = FALSE)
}

pair_month_diag <- panel |>
  group_by(segment_id, year_month) |>
  summarise(n_sides_obs = sum(n_units > 0), .groups = "drop")

within_pair_gap <- panel |>
  group_by(segment_id, year_month) |>
  summarise(gap = max(strictness_own) - min(strictness_own), .groups = "drop")

out <- tibble(
  estimate = ct["strictness_std", "Estimate"],
  std_error = ct["strictness_std", "Std. Error"],
  p_value = ct["strictness_std", p_col[1]],
  implied_pct_change = 100 * (exp(ct["strictness_std", "Estimate"]) - 1),
  n_obs = nobs(m),
  segments = n_distinct(panel$segment_id),
  ward_pairs = if ("ward_pair" %in% names(panel)) n_distinct(panel$ward_pair) else NA_integer_,
  pair_month_cells = nrow(pair_month_diag),
  share_single_sided_pair_month = mean(pair_month_diag$n_sides_obs == 1),
  share_zero_cells = mean(panel$n_units == 0),
  mean_units_per_cell = mean(panel$n_units, na.rm = TRUE),
  strictness_sd = strictness_sd,
  mean_within_pair_gap = mean(within_pair_gap$gap, na.rm = TRUE),
  input_panel = input_panel,
  cluster_level = cluster_level
)

write_csv(out, paste0(output_stem, ".csv"))

star_text <- case_when(
  !is.finite(out$p_value) ~ "",
  out$p_value < 0.01 ~ "***",
  out$p_value < 0.05 ~ "**",
  out$p_value < 0.1 ~ "*",
  TRUE ~ ""
)
coef_str <- sprintf("%.4f%s", out$estimate, star_text)
se_str <- sprintf("(%.4f)", out$std_error)

tex <- c(
  "\\begingroup",
  "\\centering",
  "\\tiny",
  "\\begin{tabular}{@{}lc@{}}",
  "  \\toprule",
  "  & Distinct Units \\\\",
  "  \\midrule",
  sprintf("  Stringency (1 SD) & %s \\\\", coef_str),
  sprintf("  & %s \\\\", se_str),
  "  \\\\",
  "  Segment $\\times$ Year-Month FE & $\\checkmark$ \\\\",
  sprintf("  N & %s \\\\", format(out$n_obs, big.mark = ",")),
  sprintf("  Dep. Var. Mean & %.2f \\\\", out$mean_units_per_cell),
  sprintf("  Ward Pairs & %s \\\\", format(out$ward_pairs, big.mark = ",")),
  "  \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)
writeLines(tex, paste0(output_stem, ".tex"))

message(sprintf(
  "Cached PPML: b=%.4f (SE %.4f, p=%.3f), implied=%.2f%%",
  out$estimate, out$std_error, out$p_value, out$implied_pct_change
))
message(sprintf("Saved: %s.tex", output_stem))
message(sprintf("Saved: %s.csv", output_stem))
