source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")
source("../../_lib/permit_event_study_sample_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# panel_mode <- "cohort_2015"
# outcome_family <- "high_discretion"
# date_basis <- "issue"
# model_type <- "ppml"
# treatment_type <- "continuous"
# weighting <- "uniform"
# bandwidth <- 300
# fe_type <- "within_block"
# post_window <- "full"
# geo_fe_level <- "ward_pair"
# cluster_level <- "block"
# control_spec <- "none"
# sample_restriction <- "none"
# bandwidth_label <- "300m"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_mode, outcome_family, date_basis, model_type, treatment_type, weighting, bandwidth, fe_type, post_window, geo_fe_level, cluster_level, control_spec, sample_restriction, bandwidth_label)
}

if (length(cli_args) >= 11) {
  panel_mode <- cli_args[1]
  outcome_family <- cli_args[2]
  date_basis <- cli_args[3]
  model_type <- cli_args[4]
  treatment_type <- cli_args[5]
  weighting <- cli_args[6]
  bandwidth <- as.numeric(cli_args[7])
  fe_type <- cli_args[8]
  post_window <- cli_args[9]
  geo_fe_level <- tolower(cli_args[10])
  cluster_level <- tolower(cli_args[11])
  control_spec <- if (length(cli_args) >= 12) cli_args[12] else "none"
  sample_restriction <- if (length(cli_args) >= 13) cli_args[13] else "none"
  bandwidth_label <- if (length(cli_args) >= 14) cli_args[14] else sprintf("%dm", as.integer(round(bandwidth)))
} else {
  stop(
    "FATAL: Script requires at least 11 args: <panel_mode> <outcome_family> <date_basis> <model_type> <treatment_type> <weighting> <bandwidth> <fe_type> <post_window> <geo_fe_level> <cluster_level> [<control_spec>] [<sample_restriction>] [<bandwidth_label>]",
    call. = FALSE
  )
}

min_segment_length_raw <- Sys.getenv("MIN_SEGMENT_LENGTH_FT", "")
MIN_SEGMENT_LENGTH_FT <- if (nzchar(min_segment_length_raw)) suppressWarnings(as.numeric(min_segment_length_raw)) else NA_real_
if (!is.na(MIN_SEGMENT_LENGTH_FT) && (!is.finite(MIN_SEGMENT_LENGTH_FT) || MIN_SEGMENT_LENGTH_FT < 0)) {
  stop("MIN_SEGMENT_LENGTH_FT must be a nonnegative number when supplied.", call. = FALSE)
}
min_segment_suffix <- ""
if (is.finite(MIN_SEGMENT_LENGTH_FT)) {
  min_segment_label <- if (abs(MIN_SEGMENT_LENGTH_FT - round(MIN_SEGMENT_LENGTH_FT)) < sqrt(.Machine$double.eps)) {
    as.character(as.integer(round(MIN_SEGMENT_LENGTH_FT)))
  } else {
    sub("\\.?0+$", "", format(MIN_SEGMENT_LENGTH_FT, trim = TRUE, scientific = FALSE))
  }
  min_segment_suffix <- paste0("_minsegment", gsub("\\.", "p", min_segment_label), "ft")
}

PANEL_MODE <- panel_mode
OUTCOME_FAMILY <- outcome_family
DATE_BASIS <- date_basis
MODEL_TYPE <- tolower(model_type)
TREATMENT_TYPE <- tolower(treatment_type)
WEIGHTING <- weighting
BANDWIDTH <- bandwidth
BANDWIDTH_LABEL <- bandwidth_label
FE_TYPE <- fe_type
POST_WINDOW <- post_window
GEO_FE_LEVEL <- geo_fe_level
CLUSTER_LEVEL <- cluster_level
CONTROL_SPEC <- control_spec
SAMPLE_RESTRICTION <- tolower(sample_restriction)

if (PANEL_MODE == "pooled_implementation") {
  PANEL_MODE <- "stacked_implementation"
}

valid_panel_modes <- c("stacked_implementation", "cohort_2015", "cohort_2023")
if (!PANEL_MODE %in% valid_panel_modes) {
  stop(sprintf("--panel_mode must be one of: %s", paste(valid_panel_modes, collapse = ", ")), call. = FALSE)
}
if (!OUTCOME_FAMILY %in% c("new_construction", "new_construction_demolition", "low_discretion_nosigns", "high_discretion", "unit_increase")) {
  stop("--outcome_family must be one of: new_construction, new_construction_demolition, low_discretion_nosigns, high_discretion, unit_increase", call. = FALSE)
}
if (!DATE_BASIS %in% c("issue", "application")) {
  stop("--date_basis must be one of: issue, application", call. = FALSE)
}
if (!MODEL_TYPE %in% c("ppml", "binary", "log")) {
  stop("--model_type must be one of: ppml, binary, log", call. = FALSE)
}
if (TREATMENT_TYPE == "binary_split") {
  TREATMENT_TYPE <- "binary_direction"
}
if (!TREATMENT_TYPE %in% c("continuous", "binary_direction", "continuous_split")) {
  stop("--treatment_type must be one of: continuous, binary_direction, continuous_split", call. = FALSE)
}
if (!WEIGHTING %in% c("uniform", "triangular")) {
  stop("--weighting must be one of: uniform, triangular", call. = FALSE)
}
if (FE_TYPE != "within_block") {
  stop("--fe_type must be within_block.", call. = FALSE)
}
if (POST_WINDOW != "full") {
  stop("--post_window must be full for the active permit event-study runner.", call. = FALSE)
}
if (!GEO_FE_LEVEL %in% c("segment", "ward_pair", "none")) {
  stop("--geo_fe_level must be one of: segment, ward_pair, none", call. = FALSE)
}
if (!CLUSTER_LEVEL %in% c("block", "ward_pair")) {
  stop("--cluster_level must be one of: block, ward_pair", call. = FALSE)
}
if (!CONTROL_SPEC %in% c("none", "baseline_demographics", "pre_high_level")) {
  stop("--control_spec must be one of: none, baseline_demographics, pre_high_level", call. = FALSE)
}
sample_restriction_info <- get_permit_sample_restriction_info(SAMPLE_RESTRICTION)
if (BANDWIDTH <= 0) {
  stop("--bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", BANDWIDTH_LABEL)) {
  stop("--bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 800) {
  stop("Segment FE requested with bandwidth > 800m. Use bandwidth <= 800m.", call. = FALSE)
}

outcome_catalog <- tibble(
  outcome_family = c(
    "new_construction",
    "new_construction",
    "new_construction_demolition",
    "new_construction_demolition",
    "low_discretion_nosigns",
    "low_discretion_nosigns",
    "high_discretion",
    "high_discretion",
    "unit_increase",
    "unit_increase"
  ),
  date_basis = c("issue", "application", "issue", "application", "issue", "application", "issue", "application", "issue", "application"),
  count_var = c(
    "n_new_construction_issue",
    "n_new_construction_application",
    "n_new_construction_demolition_issue",
    "n_new_construction_demolition_application",
    "n_low_discretion_nosigns_issue",
    "n_low_discretion_nosigns_application",
    "n_high_discretion_issue",
    "n_high_discretion_application",
    "n_unit_increase_issue",
    "n_unit_increase_application"
  ),
  binary_var = c(
    "has_new_construction_issue",
    "has_new_construction_application",
    "has_new_construction_demolition_issue",
    "has_new_construction_demolition_application",
    "has_low_discretion_nosigns_issue",
    "has_low_discretion_nosigns_application",
    "has_high_discretion_issue",
    "has_high_discretion_application",
    "has_unit_increase_issue",
    "has_unit_increase_application"
  ),
  count_label = c(
    "issued new-construction permits",
    "issued new-construction permits (application timing)",
    "issued new-construction or demolition permits",
    "issued new-construction or demolition permits (application timing)",
    "issued low-discretion permits (excluding signs)",
    "issued low-discretion permits (excluding signs, application timing)",
    "issued high-discretion permits",
    "issued high-discretion permits (application timing)",
    "issued curated unit-increase permits",
    "issued curated unit-increase permits (application timing)"
  ),
  binary_label = c(
    "any issued new-construction permit",
    "any issued new-construction permit (application timing)",
    "any issued new-construction or demolition permit",
    "any issued new-construction or demolition permit (application timing)",
    "any issued low-discretion permit (excluding signs)",
    "any issued low-discretion permit (excluding signs, application timing)",
    "any issued high-discretion permit",
    "any issued high-discretion permit (application timing)",
    "any issued curated unit-increase permit",
    "any issued curated unit-increase permit (application timing)"
  )
)

outcome_row <- outcome_catalog %>%
  filter(outcome_family == OUTCOME_FAMILY, date_basis == DATE_BASIS)
if (nrow(outcome_row) != 1) {
  stop("Failed to resolve permit outcome selector.", call. = FALSE)
}

base_outcome_var <- if (MODEL_TYPE == "binary") outcome_row$binary_var[[1]] else outcome_row$count_var[[1]]
outcome_label <- if (MODEL_TYPE == "binary") outcome_row$binary_label[[1]] else outcome_row$count_label[[1]]

panel_title <- switch(PANEL_MODE,
  "stacked_implementation" = "2015 + 2023 implementation cohorts (stacked)",
  "cohort_2015" = "2015 implementation cohort",
  "cohort_2023" = "2023 implementation cohort"
)

panel_input <- switch(PANEL_MODE,
  "stacked_implementation" = "../input/permit_block_year_panel.parquet",
  "cohort_2015" = "../input/permit_block_year_panel_2015.parquet",
  "cohort_2023" = "../input/permit_block_year_panel_2023.parquet"
)

suffix <- sprintf(
  "yearly_%s_%s_%s_%s_%s_%s_%s_within_block_%s_clust_%s",
  PANEL_MODE,
  OUTCOME_FAMILY,
  DATE_BASIS,
  MODEL_TYPE,
  TREATMENT_TYPE,
  WEIGHTING,
  BANDWIDTH_LABEL,
  POST_WINDOW,
  gsub("_", "", CLUSTER_LEVEL)
)

if (CONTROL_SPEC != "none") {
  suffix <- paste0(suffix, "_ctrl_", CONTROL_SPEC)
}
if (GEO_FE_LEVEL == "ward_pair") {
  suffix <- paste0(suffix, "_geo_wardpair")
} else if (GEO_FE_LEVEL == "none") {
  suffix <- paste0(suffix, "_geo_none")
}
if (SAMPLE_RESTRICTION != "none") {
  suffix <- paste0(suffix, "_samp_", sample_restriction_info$suffix_tag)
}
suffix <- paste0(suffix, min_segment_suffix)

message("\n=== Permit Event Study ===")
message(sprintf("Panel mode: %s", PANEL_MODE))
message(sprintf("Outcome: %s", outcome_label))
message(sprintf("Model type: %s", MODEL_TYPE))
message(sprintf("Treatment type: %s", TREATMENT_TYPE))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("Bandwidth: %s", BANDWIDTH_LABEL))
message(sprintf("FE type: %s", FE_TYPE))
message(sprintf("Post window: %s", POST_WINDOW))
message(sprintf("Geo FE level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster level: %s", CLUSTER_LEVEL))
message(sprintf("Control spec: %s", CONTROL_SPEC))
message(sprintf("Sample restriction: %s", sample_restriction_info$label))
message(sprintf(
  "Minimum segment length: %s",
  if (is.finite(MIN_SEGMENT_LENGTH_FT)) sprintf("%.1f ft", MIN_SEGMENT_LENGTH_FT) else "none"
))

y_axis_label <- if (MODEL_TYPE == "log") {
  sprintf("Effect on log %s", outcome_label)
} else {
  sprintf("Effect on %s", outcome_label)
}
display_suffix <- if (MODEL_TYPE == "binary") " pp" else "%"

message("\nLoading permit block-year panel...")
data <- read_parquet(panel_input) %>%
  filter(!is.na(.data[[base_outcome_var]]))
panel_input_n <- nrow(data)
panel_input_missing_strictness_change_n <- sum(is.na(data$strictness_change))

if (GEO_FE_LEVEL == "none") {
  data <- data %>% mutate(common_geo_fe = "all_blocks")
}

stacked_mode <- grepl("^stacked_", PANEL_MODE)
block_var <- if (stacked_mode) "cohort_block_id" else "block_id"
geo_group_var <- if (GEO_FE_LEVEL == "none") {
  "common_geo_fe"
} else if (stacked_mode) {
  if (GEO_FE_LEVEL == "segment") "cohort_segment" else "cohort_ward_pair"
} else {
  if (GEO_FE_LEVEL == "segment") "segment_id_cohort" else "ward_pair_id"
}
raw_n <- nrow(data)
raw_blocks <- n_distinct(data[[block_var]])

min_period <- -5
max_period <- 5
available_min_period <- min(data$relative_year, na.rm = TRUE)
available_max_period <- max(data$relative_year, na.rm = TRUE)

required_cols <- c(block_var, geo_group_var, "year", "relative_year", "dist_m", "treat", base_outcome_var)
missing_cols <- setdiff(required_cols, names(data))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}
if (is.finite(MIN_SEGMENT_LENGTH_FT)) {
  missing_segment_cols <- setdiff(c("segment_id_cohort", "segment_length_ft_cohort"), names(data))
  if (length(missing_segment_cols) > 0) {
    stop(sprintf(
      "MIN_SEGMENT_LENGTH_FT requires missing panel columns: %s",
      paste(missing_segment_cols, collapse = ", ")
    ), call. = FALSE)
  }
}

segment_length_input_n <- NA_integer_
segment_length_drop_n <- NA_integer_
segment_length_missing_n <- NA_integer_
segment_length_short_n <- NA_integer_

data <- data %>%
  filter(dist_m <= BANDWIDTH) %>%
  filter(relative_year >= min_period, relative_year <= max_period) %>%
  filter(!is.na(.data[[geo_group_var]]), .data[[geo_group_var]] != "")

score_gate_n <- nrow(data)
score_gate_missing_origin_n <- sum(is.na(data$strictness_origin))
score_gate_missing_dest_n <- sum(is.na(data$strictness_dest))
score_gate_missing_change_n <- sum(is.na(data$strictness_change))
if (score_gate_missing_origin_n > 0L ||
    score_gate_missing_dest_n > 0L ||
    score_gate_missing_change_n > 0L) {
  stop("Requested permit event-study regression sample has missing score values.", call. = FALSE)
}

data <- data %>%
  mutate(
    weight = if (WEIGHTING == "triangular") pmax(0, 1 - dist_m / BANDWIDTH) else 1,
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0),
    treatment_stricter_binary = as.integer(strictness_change > 0),
    treatment_lenient_binary = as.integer(strictness_change < 0)
  )

if (is.finite(MIN_SEGMENT_LENGTH_FT)) {
  segment_length_input_n <- nrow(data)
  segment_length_missing_n <- sum(is.na(data$segment_length_ft_cohort))
  segment_length_short_n <- sum(!is.na(data$segment_length_ft_cohort) & data$segment_length_ft_cohort < MIN_SEGMENT_LENGTH_FT)
  data <- data %>%
    filter(!is.na(segment_id_cohort), segment_id_cohort != "") %>%
    filter(!is.na(segment_length_ft_cohort), segment_length_ft_cohort >= MIN_SEGMENT_LENGTH_FT)
  segment_length_drop_n <- segment_length_input_n - nrow(data)
  message(sprintf(
    "Segment-length filter kept %s of %s rows; dropped %s missing-segment rows and %s rows below %.1f ft.",
    format(nrow(data), big.mark = ","),
    format(segment_length_input_n, big.mark = ","),
    format(segment_length_missing_n, big.mark = ","),
    format(segment_length_short_n, big.mark = ","),
    MIN_SEGMENT_LENGTH_FT
  ))
}

sample_restriction_summary <- apply_permit_bg_sample_restriction(
  df = data,
  block_var = block_var,
  pair_var = if (stacked_mode) "cohort_ward_pair" else "ward_pair_id",
  sample_restriction = SAMPLE_RESTRICTION,
  block_id_var = "block_id",
  cohort_var = "cohort",
  treat_var = "treat"
)
data <- sample_restriction_summary$data

message(sprintf(
  paste(
    "Sample restriction kept %s of %s observations",
    "(%s of %s blocks; %s of %s block groups)."
  ),
  format(sample_restriction_summary$summary$n_obs_after, big.mark = ","),
  format(sample_restriction_summary$summary$n_obs_before, big.mark = ","),
  format(sample_restriction_summary$summary$n_blocks_after, big.mark = ","),
  format(sample_restriction_summary$summary$n_blocks_before, big.mark = ","),
  format(sample_restriction_summary$summary$n_bg_groups_after, big.mark = ","),
  format(sample_restriction_summary$summary$n_bg_groups_before, big.mark = ",")
))

control_vars <- character(0)
missing_control_rows <- 0L
if (CONTROL_SPEC == "baseline_demographics") {
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
  if (anyDuplicated(baseline_controls[c("block_group_id", "baseline_year")]) > 0) {
    stop("Baseline controls must be unique by block group-year before joining.", call. = FALSE)
  }

  data <- data %>%
    mutate(
      block_group_id = substr(as.character(block_id), 1, 12),
      baseline_year = case_when(
        cohort == "2015" ~ 2014L,
        cohort == "2023" ~ 2022L,
        TRUE ~ NA_integer_
      )
    ) %>%
    left_join(
      baseline_controls,
      by = c("block_group_id", "baseline_year"),
      relationship = "many-to-one"
    )

  missing_control_rows <- sum(!complete.cases(data[, control_vars]), na.rm = TRUE)
  data <- data %>%
    filter(if_all(all_of(control_vars), ~ !is.na(.x))) %>%
    mutate(across(
      all_of(control_vars),
      ~ {
        x <- as.numeric(.x)
        sigma <- sd(x, na.rm = TRUE)
        if (is.na(sigma) || sigma == 0) {
          rep(0, length(x))
        } else {
          (x - mean(x, na.rm = TRUE)) / sigma
        }
      },
      .names = "{.col}_z"
    ))
} else if (CONTROL_SPEC == "pre_high_level") {
  if (!"n_high_discretion_issue" %in% names(data)) {
    stop("pre_high_level controls require n_high_discretion_issue in the permit panel.", call. = FALSE)
  }

  pre_high_controls <- data %>%
    filter(relative_year < 0) %>%
    group_by(.data[[block_var]]) %>%
    summarise(
      pre_high_discretion_issue = sum(n_high_discretion_issue, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(no_pre_high_discretion = as.integer(pre_high_discretion_issue == 0L))

  if (anyDuplicated(pre_high_controls[[block_var]]) > 0) {
    stop("Pre-period high-discretion controls are not unique by block.", call. = FALSE)
  }

  data <- data %>%
    left_join(pre_high_controls, by = block_var, relationship = "many-to-one") %>%
    mutate(
      pre_high_discretion_issue = replace_na(pre_high_discretion_issue, 0),
      no_pre_high_discretion = replace_na(no_pre_high_discretion, 1L)
    )
}

if (MODEL_TYPE == "log") {
  zero_rows_dropped <- sum(data[[base_outcome_var]] <= 0, na.rm = TRUE)
  data <- data %>%
    filter(.data[[base_outcome_var]] > 0) %>%
    mutate(outcome_estimation = log(.data[[base_outcome_var]]))
} else {
  zero_rows_dropped <- 0L
  data <- data %>%
    mutate(outcome_estimation = .data[[base_outcome_var]])
}

time_fe_var <- "year"
event_var <- "relative_year"
fe_group_var <- geo_group_var
fe_formula <- if (stacked_mode) {
  if (GEO_FE_LEVEL == "segment") {
    "cohort_block_id + cohort_segment^year"
  } else if (GEO_FE_LEVEL == "ward_pair") {
    "cohort_block_id + cohort_ward_pair^year"
  } else {
    "cohort_block_id + year"
  }
} else {
  if (GEO_FE_LEVEL == "segment") {
    "block_id + segment_id_cohort^year"
  } else if (GEO_FE_LEVEL == "ward_pair") {
    "block_id + ward_pair_id^year"
  } else {
    "block_id + year"
  }
}
cluster_formula <- if (CLUSTER_LEVEL == "block") {
  if (stacked_mode) ~cohort_block_id else ~block_id
} else {
  if (stacked_mode) ~cohort_ward_pair else ~ward_pair_id
}

control_terms <- switch(
  CONTROL_SPEC,
  "baseline_demographics" = paste(sprintf("%s:factor(year)", paste0(control_vars, "_z")), collapse = " + "),
  "pre_high_level" = paste(
    "pre_high_discretion_issue:factor(year)",
    "no_pre_high_discretion:factor(year)",
    sep = " + "
  ),
  NULL
)

analysis_n <- nrow(data)
if (analysis_n == 0) {
  stop("No observations remain after applying the requested permit event-study sample restrictions.", call. = FALSE)
}

if (TREATMENT_TYPE == "continuous") {
  support_by_event_time <- build_event_study_support_table(
    df = data,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = PANEL_MODE,
    outcome_var = base_outcome_var,
    treatment_var = "strictness_change",
    block_var = block_var,
    segment_var = geo_group_var
  )

  rhs_terms <- c(sprintf("i(%s, strictness_change, ref = -1)", event_var), control_terms)
  formula_str <- sprintf(
    "outcome_estimation ~ %s | %s",
    paste(rhs_terms[!is.na(rhs_terms) & nzchar(rhs_terms)], collapse = " + "),
    fe_formula
  )
  message(sprintf("Running %s model with %s observations", MODEL_TYPE, format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_str))
  if (MODEL_TYPE == "ppml") {
    model <- fepois(
      as.formula(formula_str),
      data = data,
      weights = ~weight,
      cluster = cluster_formula
    )
  } else {
    model <- feols(
      as.formula(formula_str),
      data = data,
      weights = ~weight,
      cluster = cluster_formula
    )
  }
  plot_data <- build_event_study_plot_data(
    model,
    support_by_event_time,
    min_period,
    max_period,
    "All blocks",
    if (MODEL_TYPE == "binary") "multiply100" else "exp_minus_one"
  )
  if (MODEL_TYPE == "binary") {
    plot_data <- plot_data %>% mutate(display_unit = "pp")
  }

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested permit specification.", call. = FALSE)
  }

  ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = sprintf("Permit event study: %s", panel_title),
      x_label = "Years relative to alderman switch",
      y_label = y_axis_label,
      display_suffix = display_suffix
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )

} else if (TREATMENT_TYPE == "binary_direction") {
  data_stricter <- data %>% filter(treatment_lenient_binary == 0)
  data_lenient <- data %>% filter(treatment_stricter_binary == 0)

  support_stricter <- build_event_study_support_table(
    df = data_stricter,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = PANEL_MODE,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_stricter_binary",
    block_var = block_var,
    segment_var = geo_group_var
  )
  support_lenient <- build_event_study_support_table(
    df = data_lenient,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = PANEL_MODE,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_lenient_binary",
    block_var = block_var,
    segment_var = geo_group_var
  )

  rhs_terms_stricter <- c(sprintf("i(%s, treatment_stricter_binary, ref = -1)", event_var), control_terms)
  rhs_terms_lenient <- c(sprintf("i(%s, treatment_lenient_binary, ref = -1)", event_var), control_terms)

  formula_stricter <- sprintf(
    "outcome_estimation ~ %s | %s",
    paste(rhs_terms_stricter[!is.na(rhs_terms_stricter) & nzchar(rhs_terms_stricter)], collapse = " + "),
    fe_formula
  )
  formula_lenient <- sprintf(
    "outcome_estimation ~ %s | %s",
    paste(rhs_terms_lenient[!is.na(rhs_terms_lenient) & nzchar(rhs_terms_lenient)], collapse = " + "),
    fe_formula
  )
  message(sprintf("Running %s model with %s observations", MODEL_TYPE, format(nrow(data_stricter), big.mark = ",")))
  message(sprintf("Formula: %s", formula_stricter))
  if (MODEL_TYPE == "ppml") {
    model_stricter <- fepois(
      as.formula(formula_stricter),
      data = data_stricter,
      weights = ~weight,
      cluster = cluster_formula
    )
  } else {
    model_stricter <- feols(
      as.formula(formula_stricter),
      data = data_stricter,
      weights = ~weight,
      cluster = cluster_formula
    )
  }

  message(sprintf("Running %s model with %s observations", MODEL_TYPE, format(nrow(data_lenient), big.mark = ",")))
  message(sprintf("Formula: %s", formula_lenient))
  if (MODEL_TYPE == "ppml") {
    model_lenient <- fepois(
      as.formula(formula_lenient),
      data = data_lenient,
      weights = ~weight,
      cluster = cluster_formula
    )
  } else {
    model_lenient <- feols(
      as.formula(formula_lenient),
      data = data_lenient,
      weights = ~weight,
      cluster = cluster_formula
    )
  }

  plot_data <- bind_rows(
    build_event_study_plot_data(model_stricter, support_stricter, min_period, max_period, "Moved to Stricter", if (MODEL_TYPE == "binary") "multiply100" else "exp_minus_one"),
    build_event_study_plot_data(model_lenient, support_lenient, min_period, max_period, "Moved to More Lenient", if (MODEL_TYPE == "binary") "multiply100" else "exp_minus_one")
  ) %>%
    filter(!is.na(estimate))
  if (MODEL_TYPE == "binary") {
    plot_data <- plot_data %>% mutate(display_unit = "pp")
  }

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested permit specification.", call. = FALSE)
  }

  directional_plots <- make_event_study_directional_plots(
    plot_data,
    plot_title = sprintf("Permit event study: %s", panel_title),
    x_label = "Years relative to alderman switch",
    y_label = y_axis_label,
    display_suffix = display_suffix
  )

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")

} else {
  support_stricter <- build_event_study_support_table(
    df = data,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = PANEL_MODE,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_stricter_continuous",
    block_var = block_var,
    segment_var = geo_group_var
  )
  support_lenient <- build_event_study_support_table(
    df = data,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = PANEL_MODE,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_lenient_continuous",
    block_var = block_var,
    segment_var = geo_group_var
  )

  rhs_terms_stricter <- c(sprintf("i(%s, treatment_stricter_continuous, ref = -1)", event_var), control_terms)
  rhs_terms_lenient <- c(sprintf("i(%s, treatment_lenient_continuous, ref = -1)", event_var), control_terms)

  formula_stricter <- sprintf(
    "outcome_estimation ~ %s | %s",
    paste(rhs_terms_stricter[!is.na(rhs_terms_stricter) & nzchar(rhs_terms_stricter)], collapse = " + "),
    fe_formula
  )
  formula_lenient <- sprintf(
    "outcome_estimation ~ %s | %s",
    paste(rhs_terms_lenient[!is.na(rhs_terms_lenient) & nzchar(rhs_terms_lenient)], collapse = " + "),
    fe_formula
  )
  message(sprintf("Running %s model with %s observations", MODEL_TYPE, format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_stricter))
  if (MODEL_TYPE == "ppml") {
    model_stricter <- fepois(
      as.formula(formula_stricter),
      data = data,
      weights = ~weight,
      cluster = cluster_formula
    )
  } else {
    model_stricter <- feols(
      as.formula(formula_stricter),
      data = data,
      weights = ~weight,
      cluster = cluster_formula
    )
  }

  message(sprintf("Running %s model with %s observations", MODEL_TYPE, format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_lenient))
  if (MODEL_TYPE == "ppml") {
    model_lenient <- fepois(
      as.formula(formula_lenient),
      data = data,
      weights = ~weight,
      cluster = cluster_formula
    )
  } else {
    model_lenient <- feols(
      as.formula(formula_lenient),
      data = data,
      weights = ~weight,
      cluster = cluster_formula
    )
  }

  plot_data <- bind_rows(
    build_event_study_plot_data(model_stricter, support_stricter, min_period, max_period, "Moved to Stricter", if (MODEL_TYPE == "binary") "multiply100" else "exp_minus_one"),
    build_event_study_plot_data(model_lenient, support_lenient, min_period, max_period, "Moved to More Lenient", if (MODEL_TYPE == "binary") "multiply100" else "exp_minus_one")
  ) %>%
    filter(!is.na(estimate))
  if (MODEL_TYPE == "binary") {
    plot_data <- plot_data %>% mutate(display_unit = "pp")
  }

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested permit specification.", call. = FALSE)
  }

  directional_plots <- make_event_study_directional_plots(
    plot_data,
    plot_title = sprintf("Permit event study: %s", panel_title),
    x_label = "Years relative to alderman switch",
    y_label = y_axis_label,
    display_suffix = display_suffix
  )

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")

}

message("\nDone!")
