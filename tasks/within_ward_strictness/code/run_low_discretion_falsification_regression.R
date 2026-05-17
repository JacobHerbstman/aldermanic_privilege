source("../../setup_environment/code/packages.R")
library(fixest)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/within_ward_strictness/code")
# score_input <- "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# residualized_wide_input <- "../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_summary_csv <- "../output/low_discretion_falsification_summary_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_tex <- "../output/low_discretion_falsification_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex"
# output_compact_tex <- "../output/low_discretion_falsification_compact_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(score_input, residualized_wide_input, output_summary_csv, output_tex, output_compact_tex)
}

if (!(length(args) %in% c(4, 5))) {
  stop(
    "FATAL: Script requires 4 or 5 args: <score_input> <residualized_wide_input> <output_summary_csv> <output_tex> [output_compact_tex]",
    call. = FALSE
  )
}

score_input <- args[1]
residualized_wide_input <- args[2]
output_summary_csv <- args[3]
output_tex <- args[4]
output_compact_tex <- if (length(args) >= 5) args[5] else NA_character_

fmt_num <- function(x, digits = 3) {
  ifelse(is.finite(x), formatC(x, digits = digits, format = "f"), "")
}

stars <- function(p) {
  case_when(
    is.na(p) ~ "",
    p < 0.01 ~ "***",
    p < 0.05 ~ "**",
    p < 0.10 ~ "*",
    TRUE ~ ""
  )
}

score_df <- read_csv(score_input, show_col_types = FALSE) %>%
  transmute(alderman, uncertainty_index)
if (anyDuplicated(score_df$alderman) > 0) {
  stop("Scores must be unique by alderman.", call. = FALSE)
}

wide_df <- read_csv(residualized_wide_input, show_col_types = FALSE)
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

model_specs <- tribble(
  ~model_id, ~outcome, ~weights_var, ~outcome_label,
  "residual_unweighted", "mean_resid_low", NA_character_, "Mean residual low-discretion log days",
  "residual_weighted", "mean_resid_low", "n_low", "Mean residual low-discretion log days",
  "raw_log_unweighted", "mean_log_low", NA_character_, "Mean low-discretion log days",
  "raw_days_unweighted", "mean_days_low", NA_character_, "Mean low-discretion days"
)

results <- bind_rows(lapply(seq_len(nrow(model_specs)), function(i) {
  spec_row <- model_specs[i, ]
  model <- if (is.na(spec_row$weights_var)) {
    feols(
      as.formula(sprintf("%s ~ uncertainty_index", spec_row$outcome)),
      data = analysis_df,
      se = "hetero",
      warn = FALSE
    )
  } else {
    feols(
      as.formula(sprintf("%s ~ uncertainty_index", spec_row$outcome)),
      data = analysis_df,
      weights = as.formula(paste0("~", spec_row$weights_var)),
      se = "hetero",
      warn = FALSE
    )
  }

  coef_table <- coeftable(model)
  tibble(
    model_id = spec_row$model_id,
    outcome = spec_row$outcome_label,
    weighting = if_else(is.na(spec_row$weights_var), "None", spec_row$weights_var),
    estimate = unname(coef_table["uncertainty_index", "Estimate"]),
    se = unname(coef_table["uncertainty_index", "Std. Error"]),
    p_value = unname(coef_table["uncertainty_index", grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]]),
    n_aldermen = nobs(model),
    r2 = tryCatch(as.numeric(r2(model, type = "r2")), error = function(e) NA_real_)
  )
}))

write_csv(results, output_summary_csv)

tex_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  "Outcome & Coef. on stringency & SE & $p$-value \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(results))) {
  row_i <- results[i, ]
  outcome_label <- row_i$outcome[[1]]
  if (row_i$weighting[[1]] != "None") {
    outcome_label <- paste0(outcome_label, " (weighted)")
  }
  tex_lines <- c(
    tex_lines,
    sprintf(
      "%s & %s & %s & %s \\\\",
      outcome_label,
      paste0(fmt_num(row_i$estimate[[1]]), stars(row_i$p_value[[1]])),
      paste0("(", fmt_num(row_i$se[[1]]), ")"),
      fmt_num(row_i$p_value[[1]])
    )
  )
}

tex_lines <- c(
  tex_lines,
  "\\midrule",
  sprintf("N aldermen & %s &  &  \\\\", format(results$n_aldermen[[1]], big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}",
  paste0(
    "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Regresses alderman-level low-discretion processing outcomes on the main residualized alderman stringency index.",
    " The first row is the direct placebo-style falsification using mean residualized log low-discretion processing time.",
    " Remaining rows report unweighted raw-log and raw-day versions for comparison.}"
  ),
  "\\endgroup"
)

writeLines(tex_lines, output_tex)

if (!is.na(output_compact_tex)) {
  compact_row <- results %>%
    filter(model_id == "residual_unweighted") %>%
    slice(1)

  compact_lines <- c(
    "\\begingroup",
    "\\centering",
    "\\begin{tabular}{lcc}",
    "\\toprule",
    "Outcome & Coef. on stringency & SE \\\\",
    "\\midrule",
    sprintf(
      "%s & %s & %s \\\\",
      compact_row$outcome[[1]],
      paste0(fmt_num(compact_row$estimate[[1]]), stars(compact_row$p_value[[1]])),
      paste0("(", fmt_num(compact_row$se[[1]]), ")")
    ),
    "\\midrule",
    sprintf("N aldermen & %s &  \\\\", format(compact_row$n_aldermen[[1]], big.mark = ",")),
    "\\bottomrule",
    "\\end{tabular}",
    paste0(
      "\\par\\vspace{0.5em}\\parbox{0.9\\linewidth}{\\footnotesize Notes: Regresses the alderman-level mean residualized low-discretion log processing time on the main standardized stringency score. ",
      "Standard errors are heteroskedasticity-robust. * $p<0.10$, ** $p<0.05$, *** $p<0.01$.}"
    ),
    "\\endgroup"
  )

  writeLines(compact_lines, output_compact_tex)
}
