# setwd("tasks/audits/permit_event_study_audit/code")
# bandwidth_label <- "500ft"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0L) {
  cli_args <- c(bandwidth_label)
}
if (length(cli_args) != 1L) {
  stop("Expected one bandwidth label.")
}
bandwidth_label <- cli_args[1]

read_results <- function(outcome) {
  readr::read_csv(
    sprintf(
      paste0(
        "../output/binary_event_study_pooled_%s_",
        "income_added_back_%s.csv"
      ),
      outcome,
      bandwidth_label
    ),
    show_col_types = FALSE
  ) |>
    dplyr::select(
      specification,
      estimate_log,
      se,
      p_value,
      n_obs,
      symmetry_p_value
    )
}

high <- read_results("high_discretion")
low <- read_results("low_discretion_nosigns")

specifications <- c(
  "signed_binary_constrained",
  "joint_stricter_vs_unchanged",
  "joint_lenient_vs_unchanged",
  "joint_aggregate_contrast"
)

if (
  !all(specifications %in% high$specification) ||
    !all(specifications %in% low$specification)
) {
  stop("The binary appendix table is missing pooled estimates.")
}

format_estimate <- function(data, specification) {
  row <- dplyr::filter(
    data,
    .data$specification == .env$specification
  )
  stars <- dplyr::case_when(
    row$p_value <= 0.01 ~ "***",
    row$p_value <= 0.05 ~ "**",
    row$p_value <= 0.10 ~ "*",
    TRUE ~ ""
  )
  sprintf("%.3f%s", row$estimate_log, stars)
}

format_se <- function(data, specification) {
  row <- dplyr::filter(
    data,
    .data$specification == .env$specification
  )
  sprintf("(%.3f)", row$se)
}

format_symmetry_p <- function(data) {
  row <- dplyr::filter(
    data,
    .data$specification == "joint_aggregate_contrast"
  )
  sprintf("%.3f", row$symmetry_p_value)
}

table_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & High-Discretion & Low-Discretion \\\\",
  "\\midrule",
  sprintf(
    "Signed-direction effect & %s & %s \\\\",
    format_estimate(high, specifications[1]),
    format_estimate(low, specifications[1])
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se(high, specifications[1]),
    format_se(low, specifications[1])
  ),
  "\\addlinespace",
  "\\multicolumn{3}{l}{\\textit{Unconstrained assignment effects}} \\\\",
  sprintf(
    "Assigned toward more stringent & %s & %s \\\\",
    format_estimate(high, specifications[2]),
    format_estimate(low, specifications[2])
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se(high, specifications[2]),
    format_se(low, specifications[2])
  ),
  sprintf(
    "Assigned toward more lenient & %s & %s \\\\",
    format_estimate(high, specifications[3]),
    format_estimate(low, specifications[3])
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se(high, specifications[3]),
    format_se(low, specifications[3])
  ),
  sprintf(
    "Directional contrast & %s & %s \\\\",
    format_estimate(high, specifications[4]),
    format_estimate(low, specifications[4])
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se(high, specifications[4]),
    format_se(low, specifications[4])
  ),
  sprintf(
    "Symmetry test $p$-value & %s & %s \\\\",
    format_symmetry_p(high),
    format_symmetry_p(low)
  ),
  "\\midrule",
  sprintf(
    "N & %s & %s \\\\",
    format(high$n_obs[1], big.mark = ","),
    format(low$n_obs[1], big.mark = ",")
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(
  table_lines,
  sprintf(
    paste0(
      "../output/binary_event_study_appendix_",
      "income_added_back_%s.tex"
    ),
    bandwidth_label
  )
)
