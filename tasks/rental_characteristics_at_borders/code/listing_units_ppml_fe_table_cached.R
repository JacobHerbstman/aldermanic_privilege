source("../../setup_environment/code/packages.R")

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_characteristics_at_borders/code")
# Rscript listing_units_ppml_fe_table_cached.R ../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 3) {
  input_panel <- cli_args[1]
  output_tex <- cli_args[2]
  output_csv <- cli_args[3]
} else {
  stop("FATAL: Script requires 3 args: <input_panel> <output_tex> <output_csv>", call. = FALSE)
}

cluster_level <- tolower(Sys.getenv("CLUSTER_LEVEL", "segment"))
if (!cluster_level %in% c("segment", "ward_pair")) {
  stop("CLUSTER_LEVEL must be one of: segment, ward_pair", call. = FALSE)
}
cluster_formula <- if (cluster_level == "segment") ~segment_id else ~ward_pair
cluster_label <- if (cluster_level == "segment") "Segment" else "Ward Pair"

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

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

write_csv(out, output_csv)

coef_str <- sprintf("%.4f%s", out$estimate, stars(out$p_value))
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
writeLines(tex, output_tex)

message(sprintf(
  "Cached PPML: b=%.4f (SE %.4f, p=%.3f), implied=%.2f%%",
  out$estimate, out$std_error, out$p_value, out$implied_pct_change
))
message(sprintf("Saved: %s", output_tex))
message(sprintf("Saved: %s", output_csv))
