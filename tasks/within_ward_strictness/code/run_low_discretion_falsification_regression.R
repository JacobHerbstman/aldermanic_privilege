# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/within_ward_strictness/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"

source("../../setup_environment/code/packages.R")
library(fixest)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(spec)
}
if (length(args) != 1) {
  stop("FATAL: Script requires 1 arg: <uncertainty_spec>", call. = FALSE)
}

spec <- args[1]

score_df <- read_csv(
  sprintf("../input/alderman_uncertainty_index_%s.csv", spec),
  show_col_types = FALSE
) %>%
  transmute(alderman, uncertainty_index)
if (anyDuplicated(score_df$alderman) > 0) {
  stop("Scores must be unique by alderman.", call. = FALSE)
}

wide_df <- read_csv(
  sprintf("../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_%s.csv", spec),
  show_col_types = FALSE
)
if (anyDuplicated(wide_df$alderman) > 0) {
  stop("Residualized high-low input must be unique by alderman.", call. = FALSE)
}

analysis_df <- score_df %>%
  inner_join(wide_df, by = "alderman", relationship = "one-to-one") %>%
  filter(
    is.finite(uncertainty_index),
    is.finite(mean_resid_low),
    is.finite(mean_log_low),
    is.finite(mean_days_low),
    is.finite(n_low),
    n_low > 0
  )

model <- feols(
  mean_resid_low ~ uncertainty_index,
  data = analysis_df,
  se = "hetero",
  warn = FALSE
)

coef_table <- coeftable(model)
p_value <- unname(coef_table["uncertainty_index", grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]])
estimate <- unname(coef_table["uncertainty_index", "Estimate"])
std_error <- unname(coef_table["uncertainty_index", "Std. Error"])

estimate_display <- if (is.finite(estimate)) formatC(estimate, digits = 3, format = "f") else ""
se_display <- if (is.finite(std_error)) formatC(std_error, digits = 3, format = "f") else ""
stars_display <- case_when(
  is.na(p_value) ~ "",
  p_value < 0.01 ~ "***",
  p_value < 0.05 ~ "**",
  p_value < 0.10 ~ "*",
  TRUE ~ ""
)

writeLines(
  c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "Outcome & Coef. on stringency & SE \\\\",
    "\\midrule",
    sprintf(
      "Mean residual low-discretion log days & %s & %s \\\\",
      paste0(estimate_display, stars_display),
      paste0("(", se_display, ")")
    ),
    "\\midrule",
    sprintf("N aldermen & %s &  \\\\", format(nobs(model), big.mark = ",")),
    "\\bottomrule",
    "\\end{tabular}",
    paste0(
      "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Regresses the alderman-level mean residualized low-discretion log processing time on the main standardized stringency score. ",
      "Standard errors are heteroskedasticity-robust. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
    ),
    "\\endgroup"
  ),
  sprintf("../output/low_discretion_falsification_compact_uncertainty_%s.tex", spec)
)
