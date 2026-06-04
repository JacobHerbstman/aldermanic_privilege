# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# outcome_family <- "high_discretion"
# treatment_type <- "continuous"
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"
# min_period <- -5
# max_period <- 5

source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(outcome_family, treatment_type, bandwidth, bandwidth_label, min_period, max_period)
}

if (length(cli_args) != 6) {
  stop("FATAL: Script requires 6 args: <outcome_family> <treatment_type> <bandwidth> <bandwidth_label> <min_period> <max_period>.", call. = FALSE)
}

outcome_family <- cli_args[1]
treatment_type <- tolower(cli_args[2])
bandwidth <- as.numeric(cli_args[3])
bandwidth_label <- cli_args[4]
min_period <- suppressWarnings(as.integer(cli_args[5]))
max_period <- suppressWarnings(as.integer(cli_args[6]))

if (!outcome_family %in% c("high_discretion", "low_discretion_nosigns")) {
  stop("outcome_family must be high_discretion or low_discretion_nosigns.", call. = FALSE)
}
if (!treatment_type %in% c("continuous", "continuous_split")) {
  stop("treatment_type must be continuous or continuous_split.", call. = FALSE)
}
if (!is.finite(bandwidth) || bandwidth <= 0) {
  stop("bandwidth must be positive.", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", bandwidth_label)) {
  stop("bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (!is.finite(min_period) || !is.finite(max_period) || min_period >= max_period) {
  stop("min_period and max_period must define an increasing event window.", call. = FALSE)
}

base_outcome_var <- if (outcome_family == "high_discretion") {
  "n_high_discretion_issue"
} else {
  "n_low_discretion_nosigns_issue"
}
outcome_label <- if (outcome_family == "high_discretion") {
  "issued high-discretion permits"
} else {
  "issued low-discretion permits (excluding signs)"
}
suffix <- sprintf(
  "yearly_cohort_2015_%s_issue_ppml_%s_uniform_%s_within_block_full_clust_block_geo_wardpair",
  outcome_family,
  treatment_type,
  bandwidth_label
)

data <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    !is.na(.data[[base_outcome_var]]),
    dist_m <= bandwidth,
    relative_year >= min_period,
    relative_year <= max_period,
    !is.na(ward_pair_id),
    ward_pair_id != ""
  )

if (sum(
  is.na(data$strictness_origin) |
    is.na(data$strictness_dest) |
    is.na(data$strictness_change)
) > 0L) {
  stop("Requested permit event-study regression sample has missing score values.", call. = FALSE)
}

data <- data %>%
  mutate(
    outcome_estimation = .data[[base_outcome_var]],
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0)
  )

fe_formula <- "block_id + ward_pair_id^year"
cluster_formula <- ~block_id
y_axis_label <- sprintf("Effect on %s", outcome_label)

if (treatment_type == "continuous") {
  identification_by_event_time <- build_event_study_support_table(
    df = data,
    event_var = "relative_year",
    time_fe_var = "year",
    fe_group_var = "ward_pair_id",
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = "cohort_2015",
    outcome_var = base_outcome_var,
    treatment_var = "strictness_change",
    block_var = "block_id",
    segment_var = "ward_pair_id"
  )

  model <- fepois(
    as.formula(sprintf(
      "outcome_estimation ~ i(relative_year, strictness_change, ref = -1) | %s",
      fe_formula
    )),
    data = data,
    cluster = cluster_formula,
    notes = FALSE
  )

  plot_data <- build_event_study_plot_data(
    model,
    identification_by_event_time,
    min_period,
    max_period,
    "All blocks",
    "exp_minus_one"
  )
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No coefficients were available for the requested permit specification.", call. = FALSE)
  }

  ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = "Permit event study: 2015 implementation cohort",
      x_label = "Years relative to alderman switch",
      y_label = y_axis_label,
      display_suffix = "%"
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )
} else {
  identification_stricter <- build_event_study_support_table(
    df = data,
    event_var = "relative_year",
    time_fe_var = "year",
    fe_group_var = "ward_pair_id",
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = "cohort_2015",
    outcome_var = base_outcome_var,
    treatment_var = "treatment_stricter_continuous",
    block_var = "block_id",
    segment_var = "ward_pair_id"
  )
  identification_lenient <- build_event_study_support_table(
    df = data,
    event_var = "relative_year",
    time_fe_var = "year",
    fe_group_var = "ward_pair_id",
    min_period = min_period,
    max_period = max_period,
    support_mode = "treatment_variation",
    cohort_label = "cohort_2015",
    outcome_var = base_outcome_var,
    treatment_var = "treatment_lenient_continuous",
    block_var = "block_id",
    segment_var = "ward_pair_id"
  )

  model_stricter <- fepois(
    as.formula(sprintf(
      "outcome_estimation ~ i(relative_year, treatment_stricter_continuous, ref = -1) | %s",
      fe_formula
    )),
    data = data,
    cluster = cluster_formula,
    notes = FALSE
  )
  model_lenient <- fepois(
    as.formula(sprintf(
      "outcome_estimation ~ i(relative_year, treatment_lenient_continuous, ref = -1) | %s",
      fe_formula
    )),
    data = data,
    cluster = cluster_formula,
    notes = FALSE
  )

  plot_data <- bind_rows(
    build_event_study_plot_data(model_stricter, identification_stricter, min_period, max_period, "Moved to Stricter", "exp_minus_one"),
    build_event_study_plot_data(model_lenient, identification_lenient, min_period, max_period, "Moved to More Lenient", "exp_minus_one")
  ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No coefficients were available for the requested permit specification.", call. = FALSE)
  }

  ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_directional_plots(
      plot_data,
      plot_title = "Permit event study: 2015 implementation cohort",
      x_label = "Years relative to alderman switch",
      y_label = y_axis_label,
      display_suffix = "%"
    )$facet,
    width = 7,
    height = 6,
    bg = "white"
  )
}
