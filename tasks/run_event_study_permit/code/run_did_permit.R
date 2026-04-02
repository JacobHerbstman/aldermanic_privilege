source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# outcome_family <- "high_discretion"
# date_basis <- "issue"
# model_type <- "ppml"
# weighting <- "uniform"
# bandwidth <- 1000
# post_window <- "full"
# geo_fe_level <- "segment"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(outcome_family, date_basis, model_type, weighting, bandwidth, post_window, geo_fe_level)
}

if (length(cli_args) >= 6) {
  outcome_family <- cli_args[1]
  date_basis <- cli_args[2]
  model_type <- tolower(cli_args[3])
  weighting <- cli_args[4]
  bandwidth <- as.numeric(cli_args[5])
  post_window <- cli_args[6]
  geo_fe_level <- if (length(cli_args) >= 7) tolower(cli_args[7]) else "segment"
} else {
  stop(
    "FATAL: Script requires 6 args: <outcome_family> <date_basis> <model_type> <weighting> <bandwidth> <post_window> [<geo_fe_level>]",
    call. = FALSE
  )
}

if (!outcome_family %in% c("new_construction", "low_discretion_nosigns", "high_discretion", "unit_increase")) {
  stop("--outcome_family must be one of: new_construction, low_discretion_nosigns, high_discretion, unit_increase", call. = FALSE)
}
if (!date_basis %in% c("issue", "application")) {
  stop("--date_basis must be one of: issue, application", call. = FALSE)
}
if (model_type != "ppml") {
  stop("--model_type must be ppml for the permit DID table runner.", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("--weighting must be one of: uniform, triangular", call. = FALSE)
}
if (bandwidth <= 0 || bandwidth > 2000) {
  stop("--bandwidth must be positive and no larger than 2000.", call. = FALSE)
}
if (post_window != "full") {
  stop("--post_window must be full for the active permit DID table runner.", call. = FALSE)
}
if (!geo_fe_level %in% c("segment", "ward_pair")) {
  stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

safe_scale <- function(x) {
  x <- as.numeric(x)
  sigma <- sd(x, na.rm = TRUE)
  mu <- mean(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(0, length(x)))
  }
  (x - mu) / sigma
}

outcome_catalog <- tibble(
  outcome_family = c("new_construction", "new_construction", "low_discretion_nosigns", "low_discretion_nosigns", "high_discretion", "high_discretion", "unit_increase", "unit_increase"),
  date_basis = c("issue", "application", "issue", "application", "issue", "application", "issue", "application"),
  outcome_var = c(
    "n_new_construction_issue",
    "n_new_construction_application",
    "n_low_discretion_nosigns_issue",
    "n_low_discretion_nosigns_application",
    "n_high_discretion_issue",
    "n_high_discretion_application",
    "n_unit_increase_issue",
    "n_unit_increase_application"
  ),
  outcome_label = c(
    "Issued new-construction permits",
    "Issued new-construction permits (application timing)",
    "Issued low-discretion permits (excluding signs)",
    "Issued low-discretion permits (excluding signs, application timing)",
    "Issued high-discretion permits",
    "Issued high-discretion permits (application timing)",
    "Issued curated unit-increase permits",
    "Issued curated unit-increase permits (application timing)"
  )
)

outcome_row <- outcome_catalog %>%
  filter(outcome_family == !!outcome_family, date_basis == !!date_basis)
if (nrow(outcome_row) != 1) {
  stop("Failed to resolve permit DID outcome.", call. = FALSE)
}

outcome_var <- outcome_row$outcome_var[[1]]
outcome_label <- outcome_row$outcome_label[[1]]

control_vars <- c(
  "baseline_median_income",
  "baseline_homeownership_rate",
  "baseline_share_bach_plus",
  "baseline_median_age",
  "baseline_population_density",
  "baseline_percent_black",
  "baseline_percent_hispanic"
)

baseline_controls <- read_csv("../input/block_group_controls.csv", show_col_types = FALSE) %>%
  transmute(
    block_group_id = as.character(GEOID),
    baseline_year = as.integer(year),
    baseline_median_income = median_income,
    baseline_homeownership_rate = homeownership_rate,
    baseline_share_bach_plus = share_bach_plus,
    baseline_median_age = median_age,
    baseline_population_density = population_density,
    baseline_percent_black = percent_black,
    baseline_percent_hispanic = percent_hispanic
  )

data_2015 <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(!is.na(strictness_change), !is.na(.data[[outcome_var]])) %>%
  filter(dist_ft <= bandwidth) %>%
  mutate(weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1) %>%
  filter(relative_year >= -5, relative_year <= 5) %>%
  mutate(
    panel_label = "2015",
    block_fe_id = block_id,
    geo_fe_id = if (geo_fe_level == "segment") segment_id_cohort else ward_pair_id
  ) %>%
  filter(!is.na(geo_fe_id), geo_fe_id != "") %>%
  mutate(
    block_group_id = substr(as.character(block_id), 1, 12),
    baseline_year = case_when(
      cohort == "2015" ~ 2014L,
      cohort == "2023" ~ 2022L,
      TRUE ~ NA_integer_
    )
  ) %>%
  left_join(baseline_controls, by = c("block_group_id", "baseline_year")) %>%
  mutate(
    across(all_of(control_vars), safe_scale, .names = "{.col}_z"),
    post = as.integer(relative_year >= 0),
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

data_stacked <- read_parquet("../input/permit_block_year_panel.parquet") %>%
  filter(!is.na(strictness_change), !is.na(.data[[outcome_var]])) %>%
  filter(dist_ft <= bandwidth) %>%
  mutate(weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1) %>%
  filter(relative_year >= -5, relative_year <= 5) %>%
  mutate(
    panel_label = "Stacked",
    block_fe_id = cohort_block_id,
    geo_fe_id = if (geo_fe_level == "segment") cohort_segment else cohort_ward_pair
  ) %>%
  filter(!is.na(geo_fe_id), geo_fe_id != "") %>%
  mutate(
    block_group_id = substr(as.character(block_id), 1, 12),
    baseline_year = case_when(
      cohort == "2015" ~ 2014L,
      cohort == "2023" ~ 2022L,
      TRUE ~ NA_integer_
    )
  ) %>%
  left_join(baseline_controls, by = c("block_group_id", "baseline_year")) %>%
  mutate(
    across(all_of(control_vars), safe_scale, .names = "{.col}_z"),
    post = as.integer(relative_year >= 0),
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

data_2015_complete_controls <- data_2015 %>%
  filter(if_all(all_of(control_vars), ~ !is.na(.x)))
data_stacked_complete_controls <- data_stacked %>%
  filter(if_all(all_of(control_vars), ~ !is.na(.x)))

rhs_terms_none <- "post_treat"
rhs_terms_controls <- paste(
  rhs_terms_none,
  paste(sprintf("%s:factor(year)", paste0(control_vars, "_z")), collapse = " + "),
  sep = " + "
)

fe_formula_2015 <- if (geo_fe_level == "segment") {
  "block_id + segment_id_cohort^year"
} else {
  "block_id + ward_pair_id^year"
}
fe_formula_stacked <- if (geo_fe_level == "segment") {
  "cohort_block_id + cohort_segment^year"
} else {
  "cohort_block_id + cohort_ward_pair^year"
}
cluster_formula_2015 <- ~block_id
cluster_formula_stacked <- ~cohort_block_id

fml_2015_no_ctrl <- as.formula(sprintf("%s ~ %s | %s", outcome_var, rhs_terms_none, fe_formula_2015))
fml_2015_ctrl <- as.formula(sprintf("%s ~ %s | %s", outcome_var, rhs_terms_controls, fe_formula_2015))
fml_stacked_no_ctrl <- as.formula(sprintf("%s ~ %s | %s", outcome_var, rhs_terms_none, fe_formula_stacked))
fml_stacked_ctrl <- as.formula(sprintf("%s ~ %s | %s", outcome_var, rhs_terms_controls, fe_formula_stacked))

message(sprintf(
  "Running DID | panel=%s | controls=%s | n=%s",
  "cohort_2015", "none", format(nrow(data_2015_complete_controls), big.mark = ",")
))
message(sprintf("Formula: %s", deparse(fml_2015_no_ctrl)))
m_2015_no_ctrl <- fepois(
  fml_2015_no_ctrl,
  data = data_2015_complete_controls,
  weights = ~weight,
  cluster = cluster_formula_2015
)
ct_2015_no_ctrl <- coeftable(m_2015_no_ctrl)
p_col_2015_no_ctrl <- grep("^Pr\\(", colnames(ct_2015_no_ctrl), value = TRUE)[1]

message(sprintf(
  "Running DID | panel=%s | controls=%s | n=%s",
  "cohort_2015", "baseline_demographics", format(nrow(data_2015_complete_controls), big.mark = ",")
))
message(sprintf("Formula: %s", deparse(fml_2015_ctrl)))
m_2015_ctrl <- fepois(
  fml_2015_ctrl,
  data = data_2015_complete_controls,
  weights = ~weight,
  cluster = cluster_formula_2015
)
ct_2015_ctrl <- coeftable(m_2015_ctrl)
p_col_2015_ctrl <- grep("^Pr\\(", colnames(ct_2015_ctrl), value = TRUE)[1]

message(sprintf(
  "Running DID | panel=%s | controls=%s | n=%s",
  "stacked_implementation", "none", format(nrow(data_stacked_complete_controls), big.mark = ",")
))
message(sprintf("Formula: %s", deparse(fml_stacked_no_ctrl)))
m_stacked_no_ctrl <- fepois(
  fml_stacked_no_ctrl,
  data = data_stacked_complete_controls,
  weights = ~weight,
  cluster = cluster_formula_stacked
)
ct_stacked_no_ctrl <- coeftable(m_stacked_no_ctrl)
p_col_stacked_no_ctrl <- grep("^Pr\\(", colnames(ct_stacked_no_ctrl), value = TRUE)[1]

message(sprintf(
  "Running DID | panel=%s | controls=%s | n=%s",
  "stacked_implementation", "baseline_demographics", format(nrow(data_stacked_complete_controls), big.mark = ",")
))
message(sprintf("Formula: %s", deparse(fml_stacked_ctrl)))
m_stacked_ctrl <- fepois(
  fml_stacked_ctrl,
  data = data_stacked_complete_controls,
  weights = ~weight,
  cluster = cluster_formula_stacked
)
ct_stacked_ctrl <- coeftable(m_stacked_ctrl)
p_col_stacked_ctrl <- grep("^Pr\\(", colnames(ct_stacked_ctrl), value = TRUE)[1]

results <- bind_rows(
  tibble(
    panel_mode = "cohort_2015",
    panel_label = "2015",
    control_spec = "none",
    control_label = "No Controls",
    estimate = coef(m_2015_no_ctrl)[["post_treat"]],
    std_error = se(m_2015_no_ctrl)[["post_treat"]],
    p_value = ct_2015_no_ctrl["post_treat", p_col_2015_no_ctrl],
    effect_pct = 100 * (exp(coef(m_2015_no_ctrl)[["post_treat"]]) - 1),
    n_obs = nobs(m_2015_no_ctrl),
    n_blocks = n_distinct(data_2015_complete_controls$block_fe_id),
    dep_var_mean = mean(data_2015_complete_controls[[outcome_var]], na.rm = TRUE),
    ward_pairs = n_distinct(data_2015_complete_controls$ward_pair_id[!is.na(data_2015_complete_controls$ward_pair_id) & data_2015_complete_controls$ward_pair_id != ""]),
    total_outcome = sum(data_2015_complete_controls[[outcome_var]], na.rm = TRUE)
  ),
  tibble(
    panel_mode = "cohort_2015",
    panel_label = "2015",
    control_spec = "baseline_demographics",
    control_label = "Baseline Demographics",
    estimate = coef(m_2015_ctrl)[["post_treat"]],
    std_error = se(m_2015_ctrl)[["post_treat"]],
    p_value = ct_2015_ctrl["post_treat", p_col_2015_ctrl],
    effect_pct = 100 * (exp(coef(m_2015_ctrl)[["post_treat"]]) - 1),
    n_obs = nobs(m_2015_ctrl),
    n_blocks = n_distinct(data_2015_complete_controls$block_fe_id),
    dep_var_mean = mean(data_2015_complete_controls[[outcome_var]], na.rm = TRUE),
    ward_pairs = n_distinct(data_2015_complete_controls$ward_pair_id[!is.na(data_2015_complete_controls$ward_pair_id) & data_2015_complete_controls$ward_pair_id != ""]),
    total_outcome = sum(data_2015_complete_controls[[outcome_var]], na.rm = TRUE)
  ),
  tibble(
    panel_mode = "stacked_implementation",
    panel_label = "Stacked",
    control_spec = "none",
    control_label = "No Controls",
    estimate = coef(m_stacked_no_ctrl)[["post_treat"]],
    std_error = se(m_stacked_no_ctrl)[["post_treat"]],
    p_value = ct_stacked_no_ctrl["post_treat", p_col_stacked_no_ctrl],
    effect_pct = 100 * (exp(coef(m_stacked_no_ctrl)[["post_treat"]]) - 1),
    n_obs = nobs(m_stacked_no_ctrl),
    n_blocks = n_distinct(data_stacked_complete_controls$block_fe_id),
    dep_var_mean = mean(data_stacked_complete_controls[[outcome_var]], na.rm = TRUE),
    ward_pairs = n_distinct(data_stacked_complete_controls$ward_pair_id[!is.na(data_stacked_complete_controls$ward_pair_id) & data_stacked_complete_controls$ward_pair_id != ""]),
    total_outcome = sum(data_stacked_complete_controls[[outcome_var]], na.rm = TRUE)
  ),
  tibble(
    panel_mode = "stacked_implementation",
    panel_label = "Stacked",
    control_spec = "baseline_demographics",
    control_label = "Baseline Demographics",
    estimate = coef(m_stacked_ctrl)[["post_treat"]],
    std_error = se(m_stacked_ctrl)[["post_treat"]],
    p_value = ct_stacked_ctrl["post_treat", p_col_stacked_ctrl],
    effect_pct = 100 * (exp(coef(m_stacked_ctrl)[["post_treat"]]) - 1),
    n_obs = nobs(m_stacked_ctrl),
    n_blocks = n_distinct(data_stacked_complete_controls$block_fe_id),
    dep_var_mean = mean(data_stacked_complete_controls[[outcome_var]], na.rm = TRUE),
    ward_pairs = n_distinct(data_stacked_complete_controls$ward_pair_id[!is.na(data_stacked_complete_controls$ward_pair_id) & data_stacked_complete_controls$ward_pair_id != ""]),
    total_outcome = sum(data_stacked_complete_controls[[outcome_var]], na.rm = TRUE)
  )
)

results <- results %>%
  mutate(column_id = c("2015_no_ctrl", "2015_ctrl", "stacked_no_ctrl", "stacked_ctrl"))

format_coef <- function(est, se, p) {
  sprintf("%.4f%s", est, stars(p))
}

format_se <- function(se) {
  sprintf("(%.4f)", se)
}

fmt_n <- function(x) {
  format(x, big.mark = ",")
}

col_2015_no_ctrl <- results %>% filter(column_id == "2015_no_ctrl")
col_2015_ctrl <- results %>% filter(column_id == "2015_ctrl")
col_stacked_no_ctrl <- results %>% filter(column_id == "stacked_no_ctrl")
col_stacked_ctrl <- results %>% filter(column_id == "stacked_ctrl")

table_tex <- c(
  "\\begingroup",
  "\\centering",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & 2015 & 2015 & Stacked & Stacked \\\\",
  " & No Controls & Baseline Demographics & No Controls & Baseline Demographics \\\\",
  "\\midrule",
  sprintf(
    "Post $\\times$ Strictness $\\Delta$ & %s & %s & %s & %s \\\\",
    format_coef(col_2015_no_ctrl$estimate[[1]], col_2015_no_ctrl$std_error[[1]], col_2015_no_ctrl$p_value[[1]]),
    format_coef(col_2015_ctrl$estimate[[1]], col_2015_ctrl$std_error[[1]], col_2015_ctrl$p_value[[1]]),
    format_coef(col_stacked_no_ctrl$estimate[[1]], col_stacked_no_ctrl$std_error[[1]], col_stacked_no_ctrl$p_value[[1]]),
    format_coef(col_stacked_ctrl$estimate[[1]], col_stacked_ctrl$std_error[[1]], col_stacked_ctrl$p_value[[1]])
  ),
  sprintf(
    " & %s & %s & %s & %s \\\\",
    format_se(col_2015_no_ctrl$std_error[[1]]),
    format_se(col_2015_ctrl$std_error[[1]]),
    format_se(col_stacked_no_ctrl$std_error[[1]]),
    format_se(col_stacked_ctrl$std_error[[1]])
  ),
  "\\midrule",
  sprintf(
    "Implied effect (\\%%) & %.2f & %.2f & %.2f & %.2f \\\\",
    col_2015_no_ctrl$effect_pct[[1]],
    col_2015_ctrl$effect_pct[[1]],
    col_stacked_no_ctrl$effect_pct[[1]],
    col_stacked_ctrl$effect_pct[[1]]
  ),
  sprintf(
    "Observations & %s & %s & %s & %s \\\\",
    fmt_n(col_2015_no_ctrl$n_obs[[1]]),
    fmt_n(col_2015_ctrl$n_obs[[1]]),
    fmt_n(col_stacked_no_ctrl$n_obs[[1]]),
    fmt_n(col_stacked_ctrl$n_obs[[1]])
  ),
  sprintf(
    "Dep. Var. Mean & %.2f & %.2f & %.2f & %.2f \\\\",
    col_2015_no_ctrl$dep_var_mean[[1]],
    col_2015_ctrl$dep_var_mean[[1]],
    col_stacked_no_ctrl$dep_var_mean[[1]],
    col_stacked_ctrl$dep_var_mean[[1]]
  ),
  sprintf(
    "Ward Pairs & %s & %s & %s & %s \\\\",
    fmt_n(col_2015_no_ctrl$ward_pairs[[1]]),
    fmt_n(col_2015_ctrl$ward_pairs[[1]]),
    fmt_n(col_stacked_no_ctrl$ward_pairs[[1]]),
    fmt_n(col_stacked_ctrl$ward_pairs[[1]])
  ),
  sprintf(
    "Blocks & %s & %s & %s & %s \\\\",
    fmt_n(col_2015_no_ctrl$n_blocks[[1]]),
    fmt_n(col_2015_ctrl$n_blocks[[1]]),
    fmt_n(col_stacked_no_ctrl$n_blocks[[1]]),
    fmt_n(col_stacked_ctrl$n_blocks[[1]])
  ),
  "Outcome & \\multicolumn{4}{c}{Issued high-discretion permits} \\\\",
  "Weighting & \\multicolumn{4}{c}{Uniform} \\\\",
  "Bandwidth & \\multicolumn{4}{c}{1,000 feet} \\\\",
  "Window & \\multicolumn{4}{c}{-5 to +5} \\\\",
  if (geo_fe_level == "segment") {
    "Fixed Effects & \\multicolumn{2}{c}{Block + Segment $\\times$ Year} & \\multicolumn{2}{c}{Cohort Block + Cohort Segment $\\times$ Year} \\\\"
  } else {
    "Fixed Effects & \\multicolumn{2}{c}{Block + Border $\\times$ Year} & \\multicolumn{2}{c}{Cohort Block + Cohort Border $\\times$ Year} \\\\"
  },
  "Cluster Level & \\multicolumn{4}{c}{Block} \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  sprintf(
    "\\par\\vspace{0.5em}\\parbox{0.92\\linewidth}{\\footnotesize Notes: PPML DID on yearly block-level counts of %s. Post indicator equals one for relative years 0 through 5. All columns use the same within-block design as the permit event studies, with %s-specific year fixed effects. Baseline demographic controls are cohort-specific pre-treatment block-group characteristics (2014 for the 2015 cohort, 2022 for the 2023 cohort) interacted with year. Standard errors clustered by block.}",
    tolower(outcome_label)
    , ifelse(geo_fe_level == "segment", "segment", "border")
  ),
  "\\par\\endgroup"
)

output_stub <- sprintf(
  "did_table_%s_%s_%s_%s_%dft_%s%s",
  outcome_family,
  date_basis,
  model_type,
  weighting,
  as.integer(bandwidth),
  post_window,
  ifelse(geo_fe_level == "segment", "", "_geo_wardpair")
)

writeLines(table_tex, sprintf("../output/%s.tex", output_stub))

message("\nSaved DID table:")
message(sprintf("../output/%s.tex", output_stub))
print(results %>% select(panel_label, control_label, estimate, std_error, p_value, effect_pct, n_obs, n_blocks))
