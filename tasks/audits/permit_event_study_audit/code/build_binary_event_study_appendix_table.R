# setwd("tasks/audits/permit_event_study_audit/code")
# bandwidth_label <- "500ft"
# sample_rule <- "all"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0L) {
  cli_args <- c(bandwidth_label, sample_rule)
}
if (!length(cli_args) %in% c(1L, 2L)) {
  stop("Expected a bandwidth label and optional sample rule.")
}
bandwidth_label <- cli_args[1]
sample_rule <- if (length(cli_args) == 2L) cli_args[2] else "all"
if (!sample_rule %in% c("all", "stable")) {
  stop("Sample rule must be all or stable.")
}
sample_suffix <- if (sample_rule == "stable") "_stable" else ""

read_results <- function(outcome) {
  readr::read_csv(
    sprintf(
      paste0(
        "../output/binary_event_study_pooled_%s_",
        "income_added_back_%s%s.csv"
      ),
      outcome,
      bandwidth_label,
      sample_suffix
    ),
    show_col_types = FALSE
  ) |>
    dplyr::select(
      specification,
      estimate_log,
      se,
      p_value,
      n_obs,
      ward_pair_clusters,
      control_blocks,
      stricter_blocks,
      lenient_blocks,
      symmetry_p_value,
      pretrend_p_value
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

format_pretrend_p <- function(data, specification) {
  row <- dplyr::filter(
    data,
    .data$specification == .env$specification
  )
  sprintf("%.3f", row$pretrend_p_value)
}

comparison_lines <- character()
if (sample_rule == "stable") {
  comparisons <- readr::read_csv(
    sprintf(
      "../output/stable_binary_outcome_comparison_%s.csv",
      bandwidth_label
    ),
    show_col_types = FALSE
  )
  required_comparisons <- c(
    "signed_direction",
    "stricter",
    "lenient",
    "directional_contrast"
  )
  if (!all(required_comparisons %in% comparisons$specification)) {
    stop("The stable outcome comparison is incomplete.")
  }
  comparison_p <- function(specification) {
    comparisons |>
      dplyr::filter(
        .data$specification == .env$specification
      ) |>
      dplyr::pull(p_value) |>
      sprintf(fmt = "%.3f")
  }
  comparison_lines <- c(
    "\\addlinespace",
    paste0(
      "\\multicolumn{3}{l}{",
      "\\textit{High- versus low-discretion tests}} \\\\"
    ),
    sprintf(
      "Constrained effects equal, $p$-value & \\multicolumn{2}{c}{%s} \\\\",
      comparison_p("signed_direction")
    ),
    sprintf(
      "More-stringent effects equal, $p$-value & \\multicolumn{2}{c}{%s} \\\\",
      comparison_p("stricter")
    ),
    sprintf(
      "More-lenient effects equal, $p$-value & \\multicolumn{2}{c}{%s} \\\\",
      comparison_p("lenient")
    ),
    sprintf(
      "Directional contrasts equal, $p$-value & \\multicolumn{2}{c}{%s} \\\\",
      comparison_p("directional_contrast")
    )
  )
}

table_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & High-Discretion & Low-Discretion \\\\",
  "\\midrule",
  sprintf(
    "Constrained signed-direction effect & %s & %s \\\\",
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
    "One-half directional contrast & %s & %s \\\\",
    format_estimate(high, specifications[4]),
    format_estimate(low, specifications[4])
  ),
  sprintf(
    " & %s & %s \\\\",
    format_se(high, specifications[4]),
    format_se(low, specifications[4])
  ),
  sprintf(
    "Symmetry test $p$-value (unconstrained) & %s & %s \\\\",
    format_symmetry_p(high),
    format_symmetry_p(low)
  ),
  sprintf(
    "Pre-trend test $p$-value (constrained) & %s & %s \\\\",
    format_pretrend_p(high, specifications[1]),
    format_pretrend_p(low, specifications[1])
  ),
  comparison_lines,
  "\\midrule",
  "Block fixed effects & Yes & Yes \\\\",
  "Ward-pair $\\times$ year fixed effects & Yes & Yes \\\\",
  "Pre-period permit volume $\\times$ year & Yes & Yes \\\\",
  "No pre-period permits $\\times$ post & Yes & Yes \\\\",
  "\\midrule",
  sprintf(
    "Observations & %s & %s \\\\",
    format(high$n_obs[1], big.mark = ","),
    format(low$n_obs[1], big.mark = ",")
  ),
  sprintf(
    "Ward pairs & %s & %s \\\\",
    high$ward_pair_clusters[1],
    low$ward_pair_clusters[1]
  ),
  sprintf(
    "Unchanged blocks in design sample & %s & %s \\\\",
    format(high$control_blocks[1], big.mark = ","),
    format(low$control_blocks[1], big.mark = ",")
  ),
  sprintf(
    "More-stringent blocks in design sample & %s & %s \\\\",
    format(high$stricter_blocks[1], big.mark = ","),
    format(low$stricter_blocks[1], big.mark = ",")
  ),
  sprintf(
    "More-lenient blocks in design sample & %s & %s \\\\",
    format(high$lenient_blocks[1], big.mark = ","),
    format(low$lenient_blocks[1], big.mark = ",")
  ),
  "\\bottomrule",
  "\\end{tabular}"
)

writeLines(
  table_lines,
  sprintf(
    paste0(
      "../output/binary_event_study_appendix_",
      "income_added_back_%s%s.tex"
    ),
    bandwidth_label,
    sample_suffix
  )
)
