# --- Interactive Test Block ---
# setwd("tasks/border_pair_FE_regressions/code")
# bandwidth_label <- "500ft"
# fe_spec <- "zonegroup_segment_year_additive"
# prune_sample <- "all"
# cluster_level <- "ward_pair"

source("../../setup_environment/code/packages.R", local = new.env(parent = globalenv()))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth_label, fe_spec, prune_sample, cluster_level)
}

if (!(length(args) %in% c(3, 4))) {
  stop("FATAL: Script requires args: <bandwidth_label> <fe_spec> <prune_sample> <cluster_level>", call. = FALSE)
}

if (grepl("\\.csv$", args[1])) {
  if (length(args) != 3) {
    stop("Path-based calls require args: <all_summary_csv> <multifamily_summary_csv> <output_tex>", call. = FALSE)
  }
  all_summary_csv <- args[1]
  multifamily_summary_csv <- args[2]
  output_tex <- args[3]
} else {
  if (length(args) != 4) {
    stop("Production calls require args: <bandwidth_label> <fe_spec> <prune_sample> <cluster_level>", call. = FALSE)
  }
  bandwidth_label <- args[1]
  fe_spec <- args[2]
  prune_sample <- tolower(args[3])
  cluster_level <- args[4]
  if (!prune_sample %in% c("all", "pruned")) {
    stop("prune_sample must be one of: all, pruned", call. = FALSE)
  }
  if (!cluster_level %in% c("ward_pair", "segment")) {
    stop("cluster_level must be one of: ward_pair, segment", call. = FALSE)
  }

  prune_suffix <- if (prune_sample == "pruned") "_pruned" else ""
  all_summary_csv <- sprintf(
    "../temp/fe_summary_%s_all_%s_clust_%s%s.csv",
    bandwidth_label,
    fe_spec,
    cluster_level,
    prune_suffix
  )
  multifamily_summary_csv <- sprintf(
    "../temp/fe_summary_%s_multifamily_%s_clust_%s%s.csv",
    bandwidth_label,
    fe_spec,
    cluster_level,
    prune_suffix
  )
  output_tex <- sprintf(
    "../output/fe_table_%s_all_multifamily_%s_clust_%s%s.tex",
    bandwidth_label,
    fe_spec,
    cluster_level,
    prune_suffix
  )
}

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
