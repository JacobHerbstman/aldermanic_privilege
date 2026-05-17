source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_regressions/code")
# all_summary_csv <- "../output/fe_summary_500ft_all_zonegroup_segment_year_additive_clust_ward_pair.csv"
# multifamily_summary_csv <- "../output/fe_summary_500ft_multifamily_zonegroup_segment_year_additive_clust_ward_pair.csv"
# output_tex <- "../output/fe_table_500ft_all_multifamily_zonegroup_segment_year_additive_clust_ward_pair.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(all_summary_csv, multifamily_summary_csv, output_tex)
}

if (length(args) != 3) {
  stop("FATAL: Script requires args: <all_summary_csv> <multifamily_summary_csv> <output_tex>", call. = FALSE)
}

all_summary_csv <- args[1]
multifamily_summary_csv <- args[2]
output_tex <- args[3]

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p <= 0.01) return("***")
  if (p <= 0.05) return("**")
  if (p <= 0.1) return("*")
  ""
}

format_coef <- function(x, p) {
  star_text <- stars(p)
  if (!nzchar(star_text)) {
    return(sprintf("%.3f", x))
  }
  sprintf("%.3f$^{%s}$", x, star_text)
}

format_se <- function(x) {
  sprintf("(%.3f)", x)
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
    outcome_order = match(outcome_short, c("FAR", "DUPAC"))
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
    paste(mapply(format_coef, ordered$estimate, ordered$p_value), collapse = " & "),
    " \\\\"
  ),
  paste0(
    "                    & ",
    paste(vapply(ordered$se, format_se, character(1)), collapse = " & "),
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
