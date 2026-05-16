source("../../setup_environment/code/packages.R")
source("../../_lib/permit_event_study_sample_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# outcome_family <- "high_discretion"
# bandwidth <- 300
# weighting <- "uniform"
# cluster_level <- "block"
# output_tex <- "../output/did_table_permit_2015_high_discretion_issue_ppml_uniform_300m_noctrl_geo_wardpair.tex"
# control_spec <- "none"
# sample_restriction <- "none"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(outcome_family, bandwidth, weighting, cluster_level, output_tex, control_spec, sample_restriction)
}

if (length(args) >= 5) {
  outcome_family <- args[1]
  bandwidth <- as.numeric(args[2])
  weighting <- args[3]
  cluster_level <- args[4]
  output_tex <- args[5]
  control_spec <- if (length(args) >= 6) args[6] else "none"
  sample_restriction <- if (length(args) >= 7) args[7] else "none"
} else if (length(args) >= 4) {
  outcome_family <- "high_discretion"
  bandwidth <- as.numeric(args[1])
  weighting <- args[2]
  cluster_level <- args[3]
  output_tex <- args[4]
  control_spec <- if (length(args) >= 5) args[5] else "none"
  sample_restriction <- if (length(args) >= 6) args[6] else "none"
} else {
  stop("FATAL: Script requires args: <outcome_family> <bandwidth> <weighting> <cluster_level> <output_tex> [<control_spec>] [<sample_restriction>]", call. = FALSE)
}

if (!outcome_family %in% c("new_construction", "new_construction_demolition", "low_discretion_nosigns", "high_discretion", "unit_increase")) {
  stop("outcome_family must be one of: new_construction, new_construction_demolition, low_discretion_nosigns, high_discretion, unit_increase", call. = FALSE)
}

if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("weighting must be one of: uniform, triangular", call. = FALSE)
}
if (!cluster_level %in% c("block", "ward_pair")) {
  stop("cluster_level must be one of: block, ward_pair", call. = FALSE)
}
if (!control_spec %in% c("none", "pre_high_level")) {
  stop("control_spec must be one of: none, pre_high_level", call. = FALSE)
}

sample_restriction_info <- get_permit_sample_restriction_info(sample_restriction)
min_segment_length_raw <- Sys.getenv("MIN_SEGMENT_LENGTH_FT", "")
min_segment_length_ft <- if (nzchar(min_segment_length_raw)) suppressWarnings(as.numeric(min_segment_length_raw)) else NA_real_
if (!is.na(min_segment_length_ft) && (!is.finite(min_segment_length_ft) || min_segment_length_ft < 0)) {
  stop("MIN_SEGMENT_LENGTH_FT must be a nonnegative number when supplied.", call. = FALSE)
}

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
  filter(outcome_family == !!outcome_family)
if (nrow(outcome_row) != 1) {
  stop("Failed to resolve permit DID outcome.", call. = FALSE)
}

outcome_var <- outcome_row$outcome_var[[1]]

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    !is.na(ward_pair_id), ward_pair_id != "",
    !is.na(block_id), block_id != "",
    !is.na(strictness_change),
    !is.na(.data[[outcome_var]]),
    dist_m <= bandwidth,
    relative_year >= -5, relative_year <= 5
  ) %>%
  mutate(
    weight = if (weighting == "triangular") pmax(0, 1 - dist_m / bandwidth) else 1,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

segment_length_input_n <- NA_integer_
segment_length_drop_n <- NA_integer_
segment_length_missing_n <- NA_integer_
segment_length_short_n <- NA_integer_
if (is.finite(min_segment_length_ft)) {
  missing_segment_cols <- setdiff(c("segment_id_cohort", "segment_length_ft_cohort"), names(data))
  if (length(missing_segment_cols) > 0) {
    stop(sprintf(
      "MIN_SEGMENT_LENGTH_FT requires missing panel columns: %s",
      paste(missing_segment_cols, collapse = ", ")
    ), call. = FALSE)
  }
  segment_length_input_n <- nrow(data)
  segment_length_missing_n <- sum(is.na(data$segment_length_ft_cohort))
  segment_length_short_n <- sum(!is.na(data$segment_length_ft_cohort) & data$segment_length_ft_cohort < min_segment_length_ft)
  data <- data %>%
    filter(!is.na(segment_id_cohort), segment_id_cohort != "") %>%
    filter(!is.na(segment_length_ft_cohort), segment_length_ft_cohort >= min_segment_length_ft)
  segment_length_drop_n <- segment_length_input_n - nrow(data)
}

sample_restriction_result <- apply_permit_bg_sample_restriction(
  df = data,
  block_var = "block_id",
  pair_var = "ward_pair_id",
  sample_restriction = sample_restriction,
  block_id_var = "block_id",
  cohort_var = "cohort",
  treat_var = "treat"
)
data <- sample_restriction_result$data

control_terms <- character(0)
control_note <- "None"
if (control_spec == "pre_high_level") {
  pre_high_controls <- data %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(
      pre_high_discretion_issue = sum(n_high_discretion_issue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(no_pre_high_discretion = as.integer(pre_high_discretion_issue == 0L))

  if (anyDuplicated(pre_high_controls$block_id) > 0) {
    stop("Pre-period high-discretion controls are not unique by block_id.", call. = FALSE)
  }

  data <- data %>%
    left_join(pre_high_controls, by = "block_id", relationship = "many-to-one") %>%
    mutate(
      pre_high_discretion_issue = replace_na(pre_high_discretion_issue, 0),
      no_pre_high_discretion = replace_na(no_pre_high_discretion, 1L)
    )

  control_terms <- c(
    "pre_high_discretion_issue:factor(year)",
    "no_pre_high_discretion:factor(year)"
  )
  control_note <- "Pre-period high-discretion permit count and no-pre-period high-discretion indicator $\\times$ year"
}

rhs_terms <- c("post_treat", control_terms)
model <- fepois(
  as.formula(sprintf("%s ~ %s | block_id + ward_pair_id^year", outcome_var, paste(rhs_terms, collapse = " + "))),
  data = data,
  weights = ~weight,
  cluster = if (cluster_level == "block") ~block_id else ~ward_pair_id
)

estimate <- coef(model)[["post_treat"]]
std_error <- se(model)[["post_treat"]]
p_value <- coeftable(model)["post_treat", grep("^Pr\\(", colnames(coeftable(model)), value = TRUE)[1]]
effect_pct <- 100 * (exp(estimate) - 1)
dep_var_mean <- mean(data[[outcome_var]], na.rm = TRUE)
ward_pairs <- n_distinct(data$ward_pair_id)
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

setFixest_dict(c(post_treat = "Post $\\times$ Stringency $\\Delta$"))
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
  sprintf(
    "Pre-Period Permit Controls $\\times$ Year & %s \\\\",
    if (control_spec == "pre_high_level") "$\\checkmark$" else "--"
  ),
  sprintf("N & %s \\\\", format(nobs(model), big.mark = ",")),
  sprintf("Dep. Var. Mean & %.2f \\\\", dep_var_mean),
  sprintf("Ward Pairs & %s \\\\", format(ward_pairs, big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(table_lines, output_tex)

message(sprintf(
  "Permit DID | sample=%s | min_segment_ft=%s | beta = %.4f%s | effect = %.2f%% | se = %.4f | p = %.3f | N = %s | segment_rows_dropped = %s",
  sample_restriction_info$label,
  if (is.finite(min_segment_length_ft)) sprintf("%.1f", min_segment_length_ft) else "none",
  estimate,
  stars,
  effect_pct,
  std_error,
  p_value,
  format(nobs(model), big.mark = ","),
  if (is.finite(min_segment_length_ft)) format(segment_length_drop_n, big.mark = ",") else "0"
))
