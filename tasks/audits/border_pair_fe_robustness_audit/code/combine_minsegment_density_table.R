# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/border_pair_fe_robustness_audit/code")
# bandwidth_label <- "500ft"
# min_segment_ft <- 500

source("../../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth_label, min_segment_ft)
}

if (length(args) != 2) {
  stop("FATAL: Script requires args: <bandwidth_label> <min_segment_ft>", call. = FALSE)
}

bandwidth_label <- args[1]
min_segment_ft <- as.integer(args[2])
if (!is.finite(min_segment_ft)) {
  stop("min_segment_ft must be numeric.", call. = FALSE)
}

summary_tag <- sprintf("zonegroup_segment_year_additive_clust_ward_pair_minsegment%sft", min_segment_ft)
all_summary_csv <- sprintf("../output/fe_summary_%s_all_%s.csv", bandwidth_label, summary_tag)
multifamily_summary_csv <- sprintf("../output/fe_summary_%s_multifamily_%s.csv", bandwidth_label, summary_tag)
output_tex <- sprintf("../output/fe_table_%s_all_multifamily_%s.tex", bandwidth_label, summary_tag)

summaries <- bind_rows(
  read_csv(all_summary_csv, show_col_types = FALSE) %>%
    mutate(sample_label = "All Construction"),
  read_csv(multifamily_summary_csv, show_col_types = FALSE) %>%
    mutate(sample_label = "Multifamily")
) %>%
  mutate(
    outcome_short = case_when(
      outcome_label == "ln(FAR)" ~ "FAR",
      outcome_label == "ln(DUPAC)" ~ "DUPAC",
      TRUE ~ outcome_label
    )
  )

expected_rows <- tidyr::expand_grid(
  sample_label = c("All Construction", "Multifamily"),
  yvar = c("log(density_far)", "log(density_dupac)")
)

summary_keys <- summaries %>%
  count(sample_label, yvar, name = "n")

missing_rows <- anti_join(
  expected_rows,
  summary_keys,
  by = c("sample_label", "yvar")
)

duplicate_rows <- summary_keys %>% filter(n > 1)

if (nrow(missing_rows) > 0) {
  stop("Missing expected density FE summary rows.", call. = FALSE)
}
if (nrow(duplicate_rows) > 0) {
  stop("Duplicate density FE summary rows.", call. = FALSE)
}

ordered <- summaries %>%
  mutate(
    sample_order = match(sample_label, c("All Construction", "Multifamily")),
    outcome_order = match(outcome_short, c("FAR", "DUPAC")),
    star_text = case_when(
      !is.finite(p_value) ~ "",
      p_value <= 0.01 ~ "***",
      p_value <= 0.05 ~ "**",
      p_value <= 0.1 ~ "*",
      TRUE ~ ""
    ),
    estimate_text = if_else(
      star_text == "",
      sprintf("%.3f", estimate),
      sprintf("%.3f$^{%s}$", estimate, star_text)
    ),
    se_text = sprintf("(%.3f)", se)
  ) %>%
  arrange(sample_order, outcome_order)

if (nrow(ordered) != 4) {
  stop("Expected exactly four rows in the combined density FE table.", call. = FALSE)
}

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "   \\toprule",
  "                    & \\multicolumn{2}{c}{All Construction} & \\multicolumn{2}{c}{Multifamily} \\\\",
  "                    & ln(FAR)       & ln(DUPAC)      & ln(FAR)       & ln(DUPAC) \\\\",
  "                    & (1)           & (2)            & (3)           & (4) \\\\",
  "   \\midrule",
  paste0(
    "   Stringency Index & ",
    paste(ordered$estimate_text, collapse = " & "),
    " \\\\"
  ),
  paste0(
    "                    & ",
    paste(ordered$se_text, collapse = " & "),
    " \\\\"
  ),
  "    \\\\",
  "   Zoning Group FE  & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
  "   Segment FE       & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
  "   Year FE          & $\\checkmark$  & $\\checkmark$   & $\\checkmark$  & $\\checkmark$ \\\\",
  paste0("   N                & ", paste(trimws(format(ordered$n_obs, big.mark = ",")), collapse = " & "), " \\\\"),
  paste0("   Dep. Var. Mean   & ", paste(sprintf("%.2f", ordered$depvar_mean), collapse = " & "), " \\\\"),
  paste0("   Ward Pairs       & ", paste(trimws(format(ordered$n_ward_pairs, big.mark = ",")), collapse = " & "), " \\\\"),
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(table_lines, output_tex)
