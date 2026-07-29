# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_covariate_balance/code")

source("../../../setup_environment/code/packages.R")

balance <- read_csv("../output/density_covariate_balance.csv", show_col_types = FALSE)
joint_tests <- read_csv("../output/density_covariate_joint_tests.csv", show_col_types = FALSE)
sur_tests <- read_csv("../output/density_balance_sur_tests.csv", show_col_types = FALSE)

format_number <- function(x) {
  ifelse(is.finite(x), sprintf("%.3f", x), "")
}

format_p <- function(x) {
  ifelse(x < 0.001, "$<$.001", sprintf("%.3f", x))
}

format_estimate <- function(estimate, standard_error) {
  sprintf("%.3f (%.3f)", estimate, standard_error)
}

lines <- c(
  "\\documentclass[11pt]{article}",
  "\\usepackage[margin=0.55in]{geometry}",
  "\\usepackage{booktabs}",
  "\\usepackage{pdflscape}",
  "\\usepackage{array}",
  "\\setlength{\\parindent}{0pt}",
  "\\begin{document}",
  "\\begin{landscape}",
  "\\section*{Density Covariate Balance Audit}",
  paste0(
    "Each row reports unadjusted means by side, followed by coefficients from the current 500-foot density design. ",
    "The binary column uses the more-stringent-side indicator; the continuous column uses the locally centered score difference. ",
    "Both models include the pair-average score, separate distance slopes, boundary-segment fixed effects, and construction-year fixed effects. ",
    "Coefficients and standard errors are in outcome standard deviations. Standard errors are clustered by ward pair."
  )
)

for (sample_name in c("All construction", "Multifamily")) {
  sample_rows <- balance %>% filter(sample == sample_name)
  lines <- c(
    lines,
    sprintf("\\subsection*{%s}", sample_name),
    "\\footnotesize",
    "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}p{2.35in}rrrrrr}",
    "\\toprule",
    "Characteristic & Lenient mean & Stringent mean & Binary difference & $p$ & Continuous coefficient & $p$ \\\\",
    "\\midrule"
  )

  for (group_name in unique(sample_rows$covariate_group)) {
    group_rows <- sample_rows %>% filter(covariate_group == group_name)
    lines <- c(lines, sprintf("\\multicolumn{7}{l}{\\textit{%s}} \\\\", group_name))
    for (i in seq_len(nrow(group_rows))) {
      row_i <- group_rows[i, ]
      lines <- c(
        lines,
        sprintf(
          "%s & %s & %s & %s & %s & %s & %s \\\\",
          row_i$label,
          format_number(row_i$lenient_mean),
          format_number(row_i$stringent_mean),
          format_estimate(row_i$binary_estimate_sd, row_i$binary_se),
          format_p(row_i$binary_p_value),
          format_estimate(row_i$continuous_estimate_sd, row_i$continuous_se),
          format_p(row_i$continuous_p_value)
        )
      )
    }
    lines <- c(lines, "\\addlinespace")
  }

  sample_joint <- joint_tests %>% filter(sample == sample_name)
  lines <- c(
    lines,
    "\\bottomrule",
    "\\end{tabular*}",
    "\\normalsize",
    "\\vspace{0.35em}",
    "\\begin{tabular}{lrrr}",
    "\\toprule",
    "Joint test & Binary $p$ & Continuous $p$ & Observations \\\\",
    "\\midrule"
  )
  for (group_name in unique(sample_joint$covariate_group)) {
    binary_row <- sample_joint %>% filter(covariate_group == group_name, treatment == "side")
    continuous_row <- sample_joint %>% filter(covariate_group == group_name, treatment == "continuous_score_difference")
    lines <- c(
      lines,
      sprintf(
        "%s & %s & %s & %s \\\\",
        group_name,
        format_p(binary_row$p_value),
        format_p(continuous_row$p_value),
        format(binary_row$observations, big.mark = ",", scientific = FALSE)
      )
    )
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}", "\\clearpage")
}

sur_table <- sur_tests %>%
  select(
    sample, covariate_group, treatment, covariance_method,
    p_value, positive_definite
  ) %>%
  mutate(
    display = if_else(
      positive_definite,
      if_else(p_value < 0.001, "$<$.001", sprintf("%.3f", p_value)),
      "not PD"
    )
  ) %>%
  select(-p_value, -positive_definite) %>%
  pivot_wider(names_from = covariance_method, values_from = display) %>%
  left_join(
    joint_tests %>%
      transmute(
        sample,
        covariate_group,
        treatment,
        predictive_test = if_else(p_value < 0.001, "$<$.001", sprintf("%.3f", p_value))
      ),
    by = c("sample", "covariate_group", "treatment"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    sample = if_else(sample == "All construction", "All", "Multifamily"),
    covariate_group = if_else(
      covariate_group == "Density model controls",
      "Ward controls",
      "Block-group"
    ),
    treatment = if_else(treatment == "side", "Binary", "Continuous")
  )

lines <- c(
  lines,
  "\\section*{Joint Balance Tests}",
  paste0(
    "Entries are joint $p$-values. The predictive test regresses treatment on all covariates. ",
    "The remaining columns jointly test the treatment coefficients across the covariate equations. ",
    "Ward-pair, endpoint, and dyadic tests use finite-cluster $F$ reference distributions. ",
    "Spatial-HAC tests use Bartlett kernels and asymptotic $\\chi^2$ reference distributions. ",
    "A covariance matrix marked `not PD' cannot support a valid full-system Wald test."
  ),
  "\\vspace{0.5em}",
  "\\footnotesize",
  "\\begin{tabular*}{\\linewidth}{@{\\extracolsep{\\fill}}lllrrrrrrrr}",
  "\\toprule",
  "Sample & Covariates & Treatment & Predictive & Ward pair & Endpoint & Dyadic & 1,000ft & 2,640ft & 5,280ft \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(sur_table))) {
  row_i <- sur_table[i, ]
  lines <- c(
    lines,
    sprintf(
      "%s & %s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
      row_i$sample,
      row_i$covariate_group,
      row_i$treatment,
      row_i$predictive_test,
      row_i$ward_pair_clustered,
      row_i$directional_endpoint_clustered,
      row_i$dyadic_shared_ward,
      row_i$spatial_bartlett_1000ft,
      row_i$spatial_bartlett_2640ft,
      row_i$spatial_bartlett_5280ft
    )
  )
}
lines <- c(
  lines,
  "\\bottomrule",
  "\\end{tabular*}",
  "\\normalsize",
  "\\clearpage"
)

lines <- c(
  lines,
  "\\end{landscape}",
  "\\end{document}"
)

writeLines(lines, "../output/density_covariate_balance_report.tex")
