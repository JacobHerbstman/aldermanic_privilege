# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"
# min_period <- -5
# max_period <- 5

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(bandwidth, bandwidth_label, min_period, max_period)
}
if (length(cli_args) != 4) {
  stop("Script requires bandwidth, bandwidth label, minimum period, and maximum period.", call. = FALSE)
}

bandwidth <- as.numeric(cli_args[1])
bandwidth_label <- cli_args[2]
min_period <- as.integer(cli_args[3])
max_period <- as.integer(cli_args[4])

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (!is.finite(min_period) || !is.finite(max_period) || min_period >= max_period) {
  stop("min_period and max_period must define an increasing event window.", call. = FALSE)
}

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    !is.na(ward_pair_id),
    ward_pair_id != "",
    !is.na(block_id),
    block_id != "",
    !is.na(n_high_discretion_issue),
    dist_m <= bandwidth,
    relative_year >= min_period,
    relative_year <= max_period
  ) %>%
  mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

model <- fepois(
  n_high_discretion_issue ~ post_treat | block_id + ward_pair_id^year,
  data = data,
  cluster = ~block_id,
  notes = FALSE
)

estimate <- coef(model)[["post_treat"]]
std_error <- se(model)[["post_treat"]]
p_value <- 2 * pnorm(-abs(estimate / std_error))
stars <- case_when(
  p_value < 0.01 ~ "***",
  p_value < 0.05 ~ "**",
  p_value < 0.10 ~ "*",
  TRUE ~ ""
)

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
  sprintf("Dep. Var. Mean & %.2f \\\\", mean(data$n_high_discretion_issue)),
  sprintf("Ward Pairs & %s \\\\", format(n_distinct(data$ward_pair_id), big.mark = ",")),
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
