# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

results <- readr::read_csv(
  "../output/provisional_validated_density_results.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    variant == "income_added_back",
    sample_rule == "common_density",
    treatment == "binary",
    cluster_level == "ward_pair"
  ) |>
  dplyr::mutate(
    column = dplyr::case_when(
      sample == "all" & outcome == "density_far" ~ 1L,
      sample == "all" & outcome == "density_dupac" ~ 2L,
      sample == "multifamily" & outcome == "density_far" ~ 3L,
      sample == "multifamily" & outcome == "density_dupac" ~ 4L
    )
  ) |>
  dplyr::arrange(column)

if (
  nrow(results) != 4L ||
    any(is.na(results$column)) ||
    !identical(results$column, 1:4)
) {
  stop("Could not identify the four binary density estimates.")
}

stars <- dplyr::case_when(
  results$p_value <= 0.01 ~ "***",
  results$p_value <= 0.05 ~ "**",
  results$p_value <= 0.10 ~ "*",
  TRUE ~ ""
)
estimate_text <- paste0(sprintf("%.3f", results$estimate), stars)
se_text <- paste0("(", sprintf("%.3f", results$se), ")")
n_text <- trimws(format(results$n_obs, big.mark = ","))
pair_text <- trimws(format(results$n_clusters, big.mark = ","))

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "   \\toprule",
  "                    & \\multicolumn{2}{c}{All Construction} & \\multicolumn{2}{c}{Multifamily} \\\\",
  "                    \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "                    & ln(FAR) & ln(DUPAC) & ln(FAR) & ln(DUPAC) \\\\",
  "                    & (1) & (2) & (3) & (4) \\\\",
  "   \\midrule",
  paste0(
    "   More-Stringent Side & ",
    paste(estimate_text, collapse = " & "),
    " \\\\"
  ),
  paste0(
    "                    & ",
    paste(se_text, collapse = " & "),
    " \\\\"
  ),
  paste0("   N                & ", paste(n_text, collapse = " & "), " \\\\"),
  paste0(
    "   Ward Pairs       & ",
    paste(pair_text, collapse = " & "),
    " \\\\"
  ),
  "   \\midrule",
  "   Pair-Average Score & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "   Zoning Group FE  & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "   Segment FE       & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "   Year FE          & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(
  table_lines,
  "../output/binary_density_main_table_income_added_back.tex"
)
