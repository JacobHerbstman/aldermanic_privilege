source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/spatial_rd_fe/code")
# far_multifamily_csv <- "../output/rdrobust_fe_log_density_far_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv"
# dupac_multifamily_csv <- "../output/rdrobust_fe_log_density_dupac_bw500_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv"
# far_all_csv <- "../output/rdrobust_fe_log_density_far_bw500_all_zonegroup_segment_year_additive_clust_ward_pair.csv"
# dupac_all_csv <- "../output/rdrobust_fe_log_density_dupac_bw500_all_zonegroup_segment_year_additive_clust_ward_pair.csv"
# output_csv <- "../output/rdrobust_fe_summary_bw500_zonegroup_segment_year_additive_clust_ward_pair.csv"
# output_tex <- "../output/rdrobust_fe_summary_bw500_zonegroup_segment_year_additive_clust_ward_pair.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    far_multifamily_csv,
    dupac_multifamily_csv,
    far_all_csv,
    dupac_all_csv,
    output_csv,
    output_tex
  )
}

if (length(args) != 6) {
  stop(
    paste(
      "FATAL: Script requires args:",
      "<far_multifamily_csv> <dupac_multifamily_csv> <far_all_csv> <dupac_all_csv> <output_csv> <output_tex>"
    ),
    call. = FALSE
  )
}

far_multifamily_csv <- args[1]
dupac_multifamily_csv <- args[2]
far_all_csv <- args[3]
dupac_all_csv <- args[4]
output_csv <- args[5]
output_tex <- args[6]

inputs <- c(far_multifamily_csv, dupac_multifamily_csv, far_all_csv, dupac_all_csv)
missing_inputs <- inputs[!file.exists(inputs)]
if (length(missing_inputs) > 0) {
  stop(sprintf("Missing rdrobust diagnostic inputs: %s", paste(missing_inputs, collapse = ", ")), call. = FALSE)
}

summary_df <- bind_rows(lapply(inputs, read_csv, show_col_types = FALSE)) %>%
  mutate(
    outcome_label = case_when(
      yvar == "density_far" & use_log ~ "ln(FAR)",
      yvar == "density_dupac" & use_log ~ "ln(DUPAC)",
      yvar == "density_far" ~ "FAR",
      yvar == "density_dupac" ~ "DUPAC",
      TRUE ~ yvar
    ),
    sample_label = case_when(
      sample_filter == "multifamily" ~ "Multifamily",
      sample_filter == "all" ~ "All construction",
      TRUE ~ sample_filter
    ),
    method_label = case_when(
      method == "residualized_outcome" ~ "Residualized outcome",
      method == "covariate_matrix_fe" ~ "FE dummies in covs",
      TRUE ~ method
    )
  )

write_csv(summary_df, output_csv)

stars <- function(p_value) {
  if (!is.finite(p_value)) return("")
  if (p_value <= 0.01) return("$^{***}$")
  if (p_value <= 0.05) return("$^{**}$")
  if (p_value <= 0.10) return("$^{*}$")
  ""
}

fmt_cell <- function(df, sample, method_id, outcome) {
  row <- df %>%
    filter(sample_label == sample, method == method_id, outcome_label == outcome) %>%
    slice(1)

  if (nrow(row) == 0 || row$status == "error" || !is.finite(row$robust_estimate)) {
    return("")
  }

  sprintf(
    "%s%s (%s)",
    sprintf("%.3f", row$robust_estimate),
    stars(row$robust_p),
    sprintf("%.3f", row$robust_se)
  )
}

status_note <- summary_df %>%
  filter(status != "ok", note != "") %>%
  distinct(method_label, note) %>%
  mutate(note = paste(method_label, note, sep = ": ")) %>%
  pull(note)
status_note <- gsub("\\.+$", "", status_note)

note_line <- paste(
  "\\textit{Notes:}",
  "Cells report the rdrobust bias-corrected estimate with robust standard error in parentheses.",
  "The residualized-outcome specification first residualizes the logged outcome on the same FE set and controls used in the main spatial RD FE figures.",
  "The covariate-matrix specification passes the controls and explicit FE dummies to rdrobust through covs=.",
  "All specifications use a 500-foot bandwidth, triangular kernel, and ward-pair clustering."
)
if (length(status_note) > 0) {
  note_line <- paste(
    note_line,
    sprintf("rdrobust notes: %s.", paste(status_note, collapse = "; "))
  )
}

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{llcc}",
  "   \\toprule",
  "   Sample & Specification & ln(FAR) & ln(DUPAC)\\\\",
  "   \\midrule",
  sprintf(
    "   Multifamily & Residualized outcome & %s & %s\\\\",
    fmt_cell(summary_df, "Multifamily", "residualized_outcome", "ln(FAR)"),
    fmt_cell(summary_df, "Multifamily", "residualized_outcome", "ln(DUPAC)")
  ),
  sprintf(
    "                & FE dummies in covs & %s & %s\\\\",
    fmt_cell(summary_df, "Multifamily", "covariate_matrix_fe", "ln(FAR)"),
    fmt_cell(summary_df, "Multifamily", "covariate_matrix_fe", "ln(DUPAC)")
  ),
  "   \\addlinespace",
  sprintf(
    "   All construction & Residualized outcome & %s & %s\\\\",
    fmt_cell(summary_df, "All construction", "residualized_outcome", "ln(FAR)"),
    fmt_cell(summary_df, "All construction", "residualized_outcome", "ln(DUPAC)")
  ),
  sprintf(
    "                    & FE dummies in covs & %s & %s\\\\",
    fmt_cell(summary_df, "All construction", "covariate_matrix_fe", "ln(FAR)"),
    fmt_cell(summary_df, "All construction", "covariate_matrix_fe", "ln(DUPAC)")
  ),
  "   \\bottomrule",
  "\\end{tabular}",
  sprintf("\\par\\smallskip\\parbox{0.95\\linewidth}{\\footnotesize %s}", note_line),
  "\\par\\endgroup",
  ""
)

writeLines(table_lines, output_tex)

cat("Saved:\n")
cat(" -", output_csv, "\n")
cat(" -", output_tex, "\n")
