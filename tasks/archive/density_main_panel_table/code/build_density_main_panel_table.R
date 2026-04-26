source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/density_main_panel_table/code")
# all_summary_path <- "../input/fe_summary_bw250_all_zonegroup_segment_year_additive_clust_ward_pair.csv"
# multifamily_summary_path <- "../input/fe_summary_bw250_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv"
# output_tex <- "../output/fe_table_bw250_density_main_panel.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(all_summary_path, multifamily_summary_path, output_tex)
}

if (length(args) != 3) {
  stop(
    "FATAL: Script requires args: <all_summary_path> <multifamily_summary_path> <output_tex>",
    call. = FALSE
  )
}

all_summary_path <- args[1]
multifamily_summary_path <- args[2]
output_tex <- args[3]

all_summary <- read_csv(all_summary_path, show_col_types = FALSE)
multifamily_summary <- read_csv(multifamily_summary_path, show_col_types = FALSE)

panel_order <- c("log(density_far)", "log(density_dupac)")
panel_labels <- c("ln(FAR)", "ln(DUPAC)")

stars <- function(p_value) {
  if (!is.finite(p_value)) return("")
  if (p_value <= 0.01) return("$^{***}$")
  if (p_value <= 0.05) return("$^{**}$")
  if (p_value <= 0.10) return("$^{*}$")
  ""
}

all_panel <- all_summary %>%
  filter(yvar %in% panel_order) %>%
  mutate(order = match(yvar, panel_order)) %>%
  arrange(order)

multifamily_panel <- multifamily_summary %>%
  filter(yvar %in% panel_order) %>%
  mutate(order = match(yvar, panel_order)) %>%
  arrange(order)

if (nrow(all_panel) != 2 || nrow(multifamily_panel) != 2) {
  stop("Expected exactly two outcomes in each summary file.", call. = FALSE)
}

if (!all(all_panel$yvar == panel_order) || !all(multifamily_panel$yvar == panel_order)) {
  stop("Summary rows are not aligned to FAR and DUPAC in the expected order.", call. = FALSE)
}

build_panel_lines <- function(panel_title, panel_df) {
  c(
    sprintf("   \\multicolumn{3}{l}{\\textit{%s}} \\\\", panel_title),
    sprintf(
      "   Stringency Index & %s%s & %s%s\\\\",
      sprintf("%.2f", panel_df$estimate[1]),
      stars(panel_df$p_value[1]),
      sprintf("%.2f", panel_df$estimate[2]),
      stars(panel_df$p_value[2])
    ),
    sprintf(
      "   & (%s) & (%s)\\\\",
      sprintf("%.2f", panel_df$se[1]),
      sprintf("%.2f", panel_df$se[2])
    ),
    "   \\\\",
    "   Zoning Group FE & $\\checkmark$ & $\\checkmark$\\\\",
    "   Segment FE & $\\checkmark$ & $\\checkmark$\\\\",
    "   Year FE & $\\checkmark$ & $\\checkmark$\\\\",
    sprintf(
      "   N & %s & %s\\\\",
      format(panel_df$n_obs[1], big.mark = ","),
      format(panel_df$n_obs[2], big.mark = ",")
    ),
    sprintf(
      "   Dep. Var. Mean & %s & %s\\\\",
      sprintf("%.2f", panel_df$depvar_mean[1]),
      sprintf("%.2f", panel_df$depvar_mean[2])
    ),
    sprintf(
      "   Ward Pairs & %s & %s\\\\",
      format(panel_df$n_ward_pairs[1], big.mark = ","),
      format(panel_df$n_ward_pairs[2], big.mark = ",")
    )
  )
}

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcc}",
  "   \\toprule",
  sprintf("                    & %s & %s \\\\", panel_labels[1], panel_labels[2]),
  "                    & (1) & (2)\\\\",
  "   \\midrule",
  build_panel_lines("Panel A. All Construction", all_panel),
  "   \\addlinespace",
  build_panel_lines("Panel B. Multifamily", multifamily_panel),
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup",
  ""
)

writeLines(table_lines, output_tex)
