source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_corner_clean_table/code")
# baseline_summary_path <- "../output/fe_summary_100m_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv"
# corner_clean_summary_path <- "../output/fe_summary_100m_all_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv"
# ambiguity_summary_path <- "../input/boundary_ambiguity_by_bw.csv"
# output_tex <- "../output/fe_table_100m_all_corner_clean_compare.tex"
# bandwidth_m <- 100
# sample_filter <- "all"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    baseline_summary_path,
    corner_clean_summary_path,
    ambiguity_summary_path,
    output_tex,
    bandwidth_m,
    sample_filter
  )
}

if (length(args) != 6) {
  stop(
    "FATAL: Script requires args: <baseline_summary_path> <corner_clean_summary_path> <ambiguity_summary_path> <output_tex> <bandwidth_m> <sample_filter>",
    call. = FALSE
  )
}

baseline_summary_path <- args[1]
corner_clean_summary_path <- args[2]
ambiguity_summary_path <- args[3]
output_tex <- args[4]
bandwidth_m <- as.numeric(args[5])
sample_filter <- args[6]
bandwidth_ft <- round(bandwidth_m / 0.3048)

if (!is.finite(bandwidth_m) || bandwidth_m <= 0) {
  stop("bandwidth_m must be a positive number.", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily")) {
  stop("sample_filter must be one of: all, multifamily.", call. = FALSE)
}

baseline_summary <- read_csv(baseline_summary_path, show_col_types = FALSE)
corner_clean_summary <- read_csv(corner_clean_summary_path, show_col_types = FALSE)
ambiguity_summary <- read_csv(ambiguity_summary_path, show_col_types = FALSE)

panel_order <- c("log(density_far)", "log(density_dupac)")

stars <- function(p_value) {
  if (!is.finite(p_value)) {
    return("")
  }
  if (p_value <= 0.01) {
    return("$^{***}$")
  }
  if (p_value <= 0.05) {
    return("$^{**}$")
  }
  if (p_value <= 0.10) {
    return("$^{*}$")
  }
  ""
}

baseline_panel <- baseline_summary %>%
  filter(yvar %in% panel_order) %>%
  mutate(order = match(yvar, panel_order)) %>%
  arrange(order)

corner_clean_panel <- corner_clean_summary %>%
  filter(yvar %in% panel_order) %>%
  mutate(order = match(yvar, panel_order)) %>%
  arrange(order)

if (nrow(baseline_panel) != 2 || nrow(corner_clean_panel) != 2) {
  stop("Expected exactly two outcomes in each summary file.", call. = FALSE)
}

if (!all(baseline_panel$yvar == panel_order) || !all(corner_clean_panel$yvar == panel_order)) {
  stop("Summary rows are not aligned to FAR and DUPAC in the expected order.", call. = FALSE)
}

ambiguity_row <- ambiguity_summary %>%
  filter(sample_filter == .env$sample_filter, bw_ft == .env$bandwidth_ft)

if (nrow(ambiguity_row) != 1) {
  stop("Expected one ambiguity row for the requested sample and bandwidth.", call. = FALSE)
}

model_drop_n <- baseline_panel$n_obs - corner_clean_panel$n_obs
if (length(unique(model_drop_n)) != 1) {
  stop("Corner-clean model drops differ across outcomes.", call. = FALSE)
}

if (!identical(as.numeric(model_drop_n[[1]]), as.numeric(ambiguity_row$n_ambiguous[[1]]))) {
  stop("Corner-clean model drop count does not match ambiguity summary at requested bandwidth.", call. = FALSE)
}

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "   \\toprule",
  "                    & \\multicolumn{2}{c}{Baseline} & \\multicolumn{2}{c}{Corner-Clean}\\\\",
  "                    & ln(FAR) & ln(DUPAC) & ln(FAR) & ln(DUPAC)\\\\",
  "                    & (1) & (2) & (3) & (4)\\\\",
  "   \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "   Stringency Index",
  sprintf(
    "                    & %s%s & %s%s & %s%s & %s%s\\\\",
    sprintf("%.2f", baseline_panel$estimate[[1]]),
    stars(baseline_panel$p_value[[1]]),
    sprintf("%.2f", baseline_panel$estimate[[2]]),
    stars(baseline_panel$p_value[[2]]),
    sprintf("%.2f", corner_clean_panel$estimate[[1]]),
    stars(corner_clean_panel$p_value[[1]]),
    sprintf("%.2f", corner_clean_panel$estimate[[2]]),
    stars(corner_clean_panel$p_value[[2]])
  ),
  sprintf(
    "                    & (%s) & (%s) & (%s) & (%s)\\\\",
    sprintf("%.2f", baseline_panel$se[[1]]),
    sprintf("%.2f", baseline_panel$se[[2]]),
    sprintf("%.2f", corner_clean_panel$se[[1]]),
    sprintf("%.2f", corner_clean_panel$se[[2]])
  ),
  "    \\\\",
  "   Zoning Group FE  & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",
  "   Segment FE       & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",
  "   Year FE          & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$\\\\",
  sprintf(
    "   N                & %s & %s & %s & %s\\\\",
    format(baseline_panel$n_obs[[1]], big.mark = ","),
    format(baseline_panel$n_obs[[2]], big.mark = ","),
    format(corner_clean_panel$n_obs[[1]], big.mark = ","),
    format(corner_clean_panel$n_obs[[2]], big.mark = ",")
  ),
  sprintf(
    "   Dep. Var. Mean   & %s & %s & %s & %s\\\\",
    sprintf("%.2f", baseline_panel$depvar_mean[[1]]),
    sprintf("%.2f", baseline_panel$depvar_mean[[2]]),
    sprintf("%.2f", corner_clean_panel$depvar_mean[[1]]),
    sprintf("%.2f", corner_clean_panel$depvar_mean[[2]])
  ),
  sprintf(
    "   Ward Pairs       & %s & %s & %s & %s\\\\",
    format(baseline_panel$n_ward_pairs[[1]], big.mark = ","),
    format(baseline_panel$n_ward_pairs[[2]], big.mark = ","),
    format(corner_clean_panel$n_ward_pairs[[1]], big.mark = ","),
    format(corner_clean_panel$n_ward_pairs[[2]], big.mark = ",")
  ),
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup",
  ""
)

writeLines(table_lines, output_tex)
