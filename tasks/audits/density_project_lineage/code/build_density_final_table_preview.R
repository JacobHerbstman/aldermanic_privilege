# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")

results <- read_csv(
  "../output/density_bandwidth_fe_robustness.csv",
  show_col_types = FALSE
) %>%
  filter(
    bandwidth == "500ft",
    fixed_effect_spec == "main",
    model_status == "estimated"
  ) %>%
  mutate(
    column = case_when(
      construction_sample == "all" & outcome == "density_far" ~ 1L,
      construction_sample == "all" & outcome == "density_dupac" ~ 2L,
      construction_sample == "multifamily" & outcome == "density_far" ~ 3L,
      construction_sample == "multifamily" & outcome == "density_dupac" ~ 4L
    )
  ) %>%
  arrange(treatment, column)

if (nrow(results) != 8 || any(is.na(results$column))) {
  stop("Final density table does not contain eight ordered models.", call. = FALSE)
}

format_estimate <- function(estimate, p_value) {
  stars <- case_when(
    p_value <= 0.01 ~ "$^{***}$",
    p_value <= 0.05 ~ "$^{**}$",
    p_value <= 0.10 ~ "$^{*}$",
    TRUE ~ ""
  )
  paste0(sprintf("%.3f", estimate), stars)
}

continuous <- results %>% filter(treatment == "continuous")
binary <- results %>% filter(treatment == "binary")

lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lcccc}",
  "   \\toprule",
  "                    & \\multicolumn{2}{c}{All Construction} & \\multicolumn{2}{c}{Multifamily} \\\\",
  "                    & ln(FAR)       & ln(DUPAC)      & ln(FAR)       & ln(DUPAC) \\\\",
  "                    & (1)           & (2)            & (3)           & (4) \\\\",
  "   \\midrule",
  "   \\multicolumn{5}{l}{\\textit{Panel A: Continuous Stringency Specification}} \\\\",
  paste0(
    "   Stringency Index (1 SD) & ",
    paste(format_estimate(continuous$estimate, continuous$p_value), collapse = " & "),
    " \\\\"
  ),
  paste0(
    "                    & ",
    paste(sprintf("(%.3f)", continuous$se), collapse = " & "),
    " \\\\"
  ),
  paste0("   N                & ", paste(format(continuous$n, big.mark = ","), collapse = " & "), " \\\\"),
  paste0("   Dep. Var. Mean   & ", paste(sprintf("%.2f", continuous$dep_var_mean), collapse = " & "), " \\\\"),
  paste0("   Ward Pairs       & ", paste(continuous$ward_pairs, collapse = " & "), " \\\\"),
  "   \\addlinespace",
  "   \\multicolumn{5}{l}{\\textit{Panel B: Binary Boundary Specification}} \\\\",
  paste0(
    "   More-Stringent Side & ",
    paste(format_estimate(binary$estimate, binary$p_value), collapse = " & "),
    " \\\\"
  ),
  paste0(
    "                    & ",
    paste(sprintf("(%.3f)", binary$se), collapse = " & "),
    " \\\\"
  ),
  paste0("   N                & ", paste(format(binary$n, big.mark = ","), collapse = " & "), " \\\\"),
  paste0("   Dep. Var. Mean   & ", paste(sprintf("%.2f", binary$dep_var_mean), collapse = " & "), " \\\\"),
  paste0("   Ward Pairs       & ", paste(binary$ward_pairs, collapse = " & "), " \\\\"),
  "   \\midrule",
  "   Zoning Group FE  & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "   Segment FE       & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "   Year FE          & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ & $\\checkmark$ \\\\",
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(lines, "../output/density_final_regression_table_preview.tex")
