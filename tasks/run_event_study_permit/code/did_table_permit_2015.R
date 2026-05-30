# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth, bandwidth_label)
}

if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <bandwidth> <bandwidth_label>.", call. = FALSE)
}

bandwidth <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

outcome_var <- "n_high_discretion_issue"

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(block_id),
    block_id != "",
    !is.na(.data[[outcome_var]]),
    dist_m <= bandwidth,
    relative_year >= -5,
    relative_year <= 5
  )

if (sum(
  is.na(data$strictness_origin) |
    is.na(data$strictness_dest) |
    is.na(data$strictness_change)
) > 0L) {
  stop("Requested permit DID regression sample has missing score values.", call. = FALSE)
}

data <- data %>%
  mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

model <- fepois(
  as.formula(sprintf("%s ~ post_treat | block_id + ward_pair_id^year", outcome_var)),
  data = data,
  cluster = ~block_id,
  notes = FALSE
)

coef_table <- coeftable(model)
estimate <- coef(model)[["post_treat"]]
std_error <- se(model)[["post_treat"]]
p_value <- coef_table["post_treat", grep("^Pr\\(", colnames(coef_table), value = TRUE)[1]]
dep_var_mean <- mean(data[[outcome_var]], na.rm = TRUE)
fe_pairs <- n_distinct(data$ward_pair_id)
stars <- if (!is.finite(p_value)) {
  ""
} else if (p_value < 0.01) {
  "***"
} else if (p_value < 0.05) {
  "**"
} else if (p_value < 0.1) {
  "*"
} else {
  ""
}

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\small",
  "\\begin{tabular}{lc}",
  "\\toprule",
  " & 2015 \\\\",
  "\\midrule",
  sprintf("Post $\\times$ Stringency $\\Delta$ & %.4f%s \\\\", estimate, stars),
  sprintf(" & (%.4f) \\\\", std_error),
  "\\\\",
  "Block FE & $\\checkmark$ \\\\",
  "Border-Pair $\\times$ Year FE & $\\checkmark$ \\\\",
  "Pre-Period Permit Controls $\\times$ Year & -- \\\\",
  sprintf("N & %s \\\\", format(nobs(model), big.mark = ",")),
  sprintf("Dep. Var. Mean & %.2f \\\\", dep_var_mean),
  sprintf("Ward Pairs & %s \\\\", format(fe_pairs, big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(
  table_lines,
  sprintf(
    "../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_%s_noctrl_geo_wardpair.tex",
    bandwidth_label
  )
)
