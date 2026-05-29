# --- Interactive Test Block ---
# setwd("tasks/run_event_study_permit/code")
# outcome_family <- "high_discretion"
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"

source("../../setup_environment/code/packages.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(outcome_family, bandwidth, bandwidth_label)
}

if (length(args) != 3) {
  stop("FATAL: Script requires args: <outcome_family> <bandwidth> <bandwidth_label>", call. = FALSE)
}

outcome_family <- args[1]
bandwidth <- as.numeric(args[2])
bandwidth_label <- args[3]

if (!outcome_family %in% c("new_construction", "new_construction_demolition", "low_discretion_nosigns", "high_discretion", "unit_increase")) {
  stop("outcome_family must be one of: new_construction, new_construction_demolition, low_discretion_nosigns, high_discretion, unit_increase", call. = FALSE)
}
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}

weighting <- "uniform"

outcome_catalog <- tibble(
  outcome_family = c("new_construction", "new_construction_demolition", "low_discretion_nosigns", "high_discretion", "unit_increase"),
  outcome_var = c(
    "n_new_construction_issue",
    "n_new_construction_demolition_issue",
    "n_low_discretion_nosigns_issue",
    "n_high_discretion_issue",
    "n_unit_increase_issue"
  )
)

outcome_row <- outcome_catalog %>%
  filter(.data$outcome_family == .env$outcome_family)
if (nrow(outcome_row) != 1) {
  stop("Failed to resolve permit DID outcome.", call. = FALSE)
}
outcome_var <- outcome_row$outcome_var[[1]]
output_tex <- sprintf(
  "../output/did_table_permit_2015_%s_issue_ppml_%s_%s_noctrl_geo_wardpair.tex",
  outcome_family,
  weighting,
  bandwidth_label
)

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

missing_score_rows <- sum(
  is.na(data$strictness_origin) |
    is.na(data$strictness_dest) |
    is.na(data$strictness_change)
)
if (missing_score_rows > 0L) {
  stop("Requested permit DID regression sample has missing score values.", call. = FALSE)
}

data <- data %>%
  mutate(
    weight = 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

model <- fepois(
  as.formula(sprintf("%s ~ post_treat | block_id + ward_pair_id^year", outcome_var)),
  data = data,
  weights = ~weight,
  cluster = ~block_id
)

estimate <- coef(model)[["post_treat"]]
std_error <- se(model)[["post_treat"]]
p_value <- coeftable(model)["post_treat", grep("^Pr\\(", colnames(coeftable(model)), value = TRUE)[1]]
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

writeLines(table_lines, output_tex)
