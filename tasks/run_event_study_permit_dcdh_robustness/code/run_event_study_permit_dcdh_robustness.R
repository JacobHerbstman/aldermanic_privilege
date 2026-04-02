
# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit_dcdh_robustness/code")
# bandwidth <- 1000
# weighting <- "uniform"
# bootstrap_reps <- 0
# output_pdf_non_normalized <- "../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_non_normalized_uniform_1000ft_wardpair.pdf"
# output_pdf_normalized <- "../output/event_study_dcdh_yearly_cohort_2015_high_discretion_issue_normalized_uniform_1000ft_wardpair.pdf"
# output_csv <- "../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_uniform_1000ft.csv"
# output_tex <- "../output/summary_dcdh_yearly_cohort_2015_high_discretion_issue_uniform_1000ft.tex"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidth, weighting, bootstrap_reps, output_pdf_non_normalized, output_pdf_normalized, output_csv, output_tex)
}

if (length(args) < 7) {
  stop(
    "FATAL: Script requires args: <bandwidth> <weighting> <bootstrap_reps> <output_pdf_non_normalized> <output_pdf_normalized> <output_csv> <output_tex>",
    call. = FALSE
  )
}

bandwidth <- as.numeric(args[1])
weighting <- args[2]
bootstrap_reps <- as.integer(args[3])
output_pdf_non_normalized <- args[4]
output_pdf_normalized <- args[5]
output_csv <- args[6]
output_tex <- args[7]

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("weighting must be one of: uniform, triangular", call. = FALSE)
}
if (is.na(bootstrap_reps) || bootstrap_reps < 0) {
  stop("bootstrap_reps must be a nonnegative integer.", call. = FALSE)
}

Sys.setenv(RGL_USE_NULL = TRUE)
options(rgl.useNULL = TRUE)

library(arrow)
library(dplyr)
library(DIDmultiplegtDYN)
library(ggplot2)
library(polars)
library(readr)
library(tibble)

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    !is.na(block_id), block_id != "",
    !is.na(ward_pair_id), ward_pair_id != "",
    !is.na(strictness_origin),
    !is.na(strictness_dest),
    !is.na(n_high_discretion_issue),
    dist_ft <= bandwidth,
    relative_year >= -5, relative_year <= 5
  ) %>%
  mutate(
    weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1,
    strictness_level = if_else(year < 2015L, strictness_origin, strictness_dest),
    group = as.integer(factor(block_id)),
    ward_pair = as.integer(factor(ward_pair_id))
  ) %>%
  transmute(
    outcome = n_high_discretion_issue,
    group,
    time = year,
    treatment = strictness_level,
    ward_pair,
    weight
  ) %>%
  arrange(group, time)

run_spec <- function(normalized_flag) {
  spec_args <- list(
    df = as.data.frame(data),
    outcome = "outcome",
    group = "group",
    time = "time",
    treatment = "treatment",
    effects = 5,
    placebo = 4,
    continuous = 1,
    trends_nonparam = "ward_pair",
    cluster = "group",
    graph_off = TRUE
  )
  if (normalized_flag) {
    spec_args$normalized <- TRUE
  }
  if (weighting == "triangular") {
    spec_args$weight <- "weight"
  }
  if (bootstrap_reps > 0) {
    spec_args$bootstrap <- list(as.numeric(bootstrap_reps), as.numeric(12345))
  }
  do.call(did_multiplegt_dyn, spec_args)
}

tidy_results <- function(res, spec_label) {
  effects <- as_tibble(as.data.frame(res$results$Effects), rownames = "term") %>%
    mutate(type = "effect", spec = spec_label)

  placebos <- as_tibble(as.data.frame(res$results$Placebos), rownames = "term") %>%
    mutate(type = "placebo", spec = spec_label)

  ate <- as_tibble(as.data.frame(res$results$ATE), rownames = "term") %>%
    mutate(type = "average_total_effect", spec = spec_label)

  bind_rows(effects, placebos, ate) %>%
    mutate(
      term = trimws(term),
      p_value = 2 * pnorm(-abs(Estimate / SE)),
      bandwidth_ft = bandwidth,
      weighting = weighting,
      bootstrap_reps = bootstrap_reps,
      joint_effects_p = res$results$p_jointeffects,
      joint_placebos_p = res$results$p_jointplacebo,
      delta_D_avg_total = res$results$delta_D_avg_total
    )
}

res_non_normalized <- run_spec(FALSE)
res_normalized <- run_spec(TRUE)

plot_non_normalized <- res_non_normalized$plot +
  labs(
    title = "High-Discretion Permits: DIDmultiplegtDYN",
    subtitle = sprintf("2015 cohort, non-normalized effects, %s weights, %dft", weighting, as.integer(bandwidth)),
    x = "Effect / Placebo Horizon",
    y = "Estimated Effect"
  ) +
  theme_minimal(base_size = 12)

plot_normalized <- res_normalized$plot +
  labs(
    title = "High-Discretion Permits: DIDmultiplegtDYN",
    subtitle = sprintf("2015 cohort, normalized effects, %s weights, %dft", weighting, as.integer(bandwidth)),
    x = "Effect / Placebo Horizon",
    y = "Estimated Effect"
  ) +
  theme_minimal(base_size = 12)

ggsave(output_pdf_non_normalized, plot_non_normalized, width = 8, height = 5)
ggsave(output_pdf_normalized, plot_normalized, width = 8, height = 5)

results_all <- bind_rows(
  tidy_results(res_non_normalized, "Non-normalized"),
  tidy_results(res_normalized, "Normalized")
)

write_csv(results_all, output_csv)

get_stat <- function(df, spec_label, term_name, col_name) {
  value <- df %>%
    filter(spec == spec_label, term == term_name) %>%
    pull(.data[[col_name]])
  if (length(value) == 0) NA_real_ else value[[1]]
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

ate_non <- results_all %>% filter(spec == "Non-normalized", type == "average_total_effect")
ate_norm <- results_all %>% filter(spec == "Normalized", type == "average_total_effect")

table_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  "\\small",
  "\\caption{DIDmultiplegtDYN Robustness for High-Discretion Permits}",
  "\\label{tab:permit_dcdh_robustness_2015}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & Non-normalized & Normalized \\\\",
  "\\midrule",
  sprintf(
    "Effect 1 & %.4f%s & %.4f%s \\\\",
    get_stat(results_all, "Non-normalized", "Effect_1", "Estimate"),
    stars(get_stat(results_all, "Non-normalized", "Effect_1", "p_value")),
    get_stat(results_all, "Normalized", "Effect_1", "Estimate"),
    stars(get_stat(results_all, "Normalized", "Effect_1", "p_value"))
  ),
  sprintf(
    " & (%.4f) & (%.4f) \\\\",
    get_stat(results_all, "Non-normalized", "Effect_1", "SE"),
    get_stat(results_all, "Normalized", "Effect_1", "SE")
  ),
  sprintf(
    "Effect 5 & %.4f%s & %.4f%s \\\\",
    get_stat(results_all, "Non-normalized", "Effect_5", "Estimate"),
    stars(get_stat(results_all, "Non-normalized", "Effect_5", "p_value")),
    get_stat(results_all, "Normalized", "Effect_5", "Estimate"),
    stars(get_stat(results_all, "Normalized", "Effect_5", "p_value"))
  ),
  sprintf(
    " & (%.4f) & (%.4f) \\\\",
    get_stat(results_all, "Non-normalized", "Effect_5", "SE"),
    get_stat(results_all, "Normalized", "Effect_5", "SE")
  ),
  sprintf(
    "Average total effect & %.4f & %.4f \\\\",
    ate_non$Estimate[[1]],
    ate_norm$Estimate[[1]]
  ),
  sprintf(
    " & (%.4f) & (%.4f) \\\\",
    ate_non$SE[[1]],
    ate_norm$SE[[1]]
  ),
  sprintf(
    "Joint post-effects $p$ & %.3f & %.3f \\\\",
    unique(ate_non$joint_effects_p)[[1]],
    unique(ate_norm$joint_effects_p)[[1]]
  ),
  sprintf(
    "Joint placebo $p$ & %.3f & %.3f \\\\",
    unique(ate_non$joint_placebos_p)[[1]],
    unique(ate_norm$joint_placebos_p)[[1]]
  ),
  sprintf(
    "Average cumulative dose & %.2f & %.2f \\\\",
    unique(ate_non$delta_D_avg_total)[[1]],
    unique(ate_norm$delta_D_avg_total)[[1]]
  ),
  sprintf(
    "Switchers & %s & %s \\\\",
    format(ate_non$Switchers[[1]], big.mark = ","),
    format(ate_norm$Switchers[[1]], big.mark = ",")
  ),
  "\\midrule",
  "Outcome & \\multicolumn{2}{c}{Issued high-discretion permits} \\\\",
  "Sample & \\multicolumn{2}{c}{2015 implementation cohort} \\\\",
  sprintf("Bandwidth & \\multicolumn{2}{c}{%d ft} \\\\", as.integer(bandwidth)),
  sprintf("Weights & \\multicolumn{2}{c}{%s} \\\\", tools::toTitleCase(weighting)),
  "Estimator & \\multicolumn{2}{c}{DIDmultiplegtDYN with continuous baseline treatment} \\\\",
  "Comparison set & \\multicolumn{2}{c}{Within ward-pair, using switchers and stayers} \\\\",
  if (bootstrap_reps > 0) {
    sprintf("Inference & \\multicolumn{2}{c}{Cluster bootstrap (%d reps) at block level} \\\\", bootstrap_reps)
  } else {
    "Inference & \\multicolumn{2}{c}{Analytical SEs clustered at block level} \\\\"
  },
  "\\bottomrule",
  "\\end{tabular}",
  sprintf(
    "\\begin{minipage}{0.92\\linewidth}\\footnotesize Notes: This robustness uses the \\texttt{DIDmultiplegtDYN} package on the same 2015 high-discretion permit sample as the main event study: observations within %d feet of the border and relative years $-5$ through $5$. Treatment is the block's alderman strictness level path over time, with pre-2015 periods set to the origin ward's strictness and post-2015 periods set to the destination ward's strictness. The \\texttt{trends\\_nonparam = ward\\_pair} option restricts comparisons to switchers and stayers within the same border pair. Effect horizons are package-based dynamic DID horizons rather than the PPML event-study coefficients from the main specification.\\end{minipage}",
    as.integer(bandwidth)
  ),
  "\\end{table}"
)

writeLines(table_lines, output_tex)

message(sprintf(
  "DIDmultiplegtDYN non-normalized ATE: %.4f (SE %.4f), joint post p = %.3f, joint placebo p = %.3f",
  ate_non$Estimate[[1]],
  ate_non$SE[[1]],
  unique(ate_non$joint_effects_p)[[1]],
  unique(ate_non$joint_placebos_p)[[1]]
))
message(sprintf(
  "DIDmultiplegtDYN normalized Effect 1: %.4f (SE %.4f), Effect 5: %.4f (SE %.4f)",
  get_stat(results_all, "Normalized", "Effect_1", "Estimate"),
  get_stat(results_all, "Normalized", "Effect_1", "SE"),
  get_stat(results_all, "Normalized", "Effect_5", "Estimate"),
  get_stat(results_all, "Normalized", "Effect_5", "SE")
))
