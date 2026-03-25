source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# Rscript run_did_permit.R "high_discretion" "issue" "ppml" "uniform" 1000 "full" "segment"
# =======================================================================================

dir.create("../output", showWarnings = FALSE, recursive = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
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

load_panel <- function(panel_mode) {
  panel_input <- switch(panel_mode,
    "cohort_2015" = "../input/permit_block_year_panel_2015.parquet",
    "stacked_implementation" = "../input/permit_block_year_panel.parquet"
  )

  dt <- read_parquet(panel_input) %>%
    filter(!is.na(strictness_change), !is.na(.data[[outcome_var]])) %>%
    filter(dist_ft <= bandwidth) %>%
    mutate(weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1) %>%
    filter(relative_year >= -5, relative_year <= 5)

  if (panel_mode == "stacked_implementation") {
    dt <- dt %>%
      mutate(
        panel_label = "Stacked",
        block_fe_id = cohort_block_id,
        geo_fe_id = if (geo_fe_level == "segment") cohort_segment else cohort_ward_pair
      )
  } else {
    dt <- dt %>%
      mutate(
        panel_label = "2015",
        block_fe_id = block_id,
        geo_fe_id = if (geo_fe_level == "segment") segment_id_cohort else ward_pair_id
      )
  }

  dt <- dt %>%
    filter(!is.na(geo_fe_id), geo_fe_id != "")

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

  dt <- dt %>%
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

  dt
}

run_model <- function(data, panel_mode, control_spec) {
  complete_control_sample <- data %>%
    filter(if_all(all_of(control_vars), ~ !is.na(.x)))

  rhs_terms <- "post_treat"
  if (control_spec == "baseline_demographics") {
    rhs_terms <- paste(
      rhs_terms,
      paste(sprintf("%s:factor(year)", paste0(control_vars, "_z")), collapse = " + "),
      sep = " + "
    )
  }

  fe_formula <- if (panel_mode == "stacked_implementation") {
    if (geo_fe_level == "segment") "cohort_block_id + cohort_segment^year" else "cohort_block_id + cohort_ward_pair^year"
  } else {
    if (geo_fe_level == "segment") "block_id + segment_id_cohort^year" else "block_id + ward_pair_id^year"
  }

  cluster_formula <- if (panel_mode == "stacked_implementation") ~cohort_block_id else ~block_id
  fml <- as.formula(sprintf("%s ~ %s | %s", outcome_var, rhs_terms, fe_formula))

  message(sprintf(
    "Running DID | panel=%s | controls=%s | n=%s",
    panel_mode, control_spec, format(nrow(complete_control_sample), big.mark = ",")
  ))
  message(sprintf("Formula: %s", deparse(fml)))

  model <- fepois(
    fml,
    data = complete_control_sample,
    weights = ~weight,
    cluster = cluster_formula
  )

  estimate <- coef(model)[["post_treat"]]
  std_error <- se(model)[["post_treat"]]
  p_value <- coeftable(model)["post_treat", grep("^Pr\\(", colnames(coeftable(model)), value = TRUE)[1]]

  tibble(
    panel_mode = panel_mode,
    panel_label = unique(complete_control_sample$panel_label),
    control_spec = control_spec,
    control_label = ifelse(control_spec == "baseline_demographics", "Baseline Demographics", "No Controls"),
    estimate = estimate,
    std_error = std_error,
    p_value = p_value,
    effect_pct = 100 * (exp(estimate) - 1),
    n_obs = nobs(model),
    n_blocks = n_distinct(complete_control_sample$block_fe_id),
    total_outcome = sum(complete_control_sample[[outcome_var]], na.rm = TRUE)
  )
}

data_2015 <- load_panel("cohort_2015")
data_stacked <- load_panel("stacked_implementation")

results <- bind_rows(
  run_model(data_2015, "cohort_2015", "none"),
  run_model(data_2015, "cohort_2015", "baseline_demographics"),
  run_model(data_stacked, "stacked_implementation", "none"),
  run_model(data_stacked, "stacked_implementation", "baseline_demographics")
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
