source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")
source("../../_lib/land_transaction_model_helpers.R")

options(dplyr.summarise.inform = FALSE)

make_incidence_support_table <- function(
    df,
    event_var,
    time_fe_var,
    fe_group_var,
    outcome_var,
    treat_indicator_var,
    control_indicator_var,
    min_period,
    max_period) {
  support_base <- df %>%
    dplyr::filter(.data[[event_var]] >= min_period, .data[[event_var]] <= max_period)

  cell_support <- support_base %>%
    dplyr::group_by(
      event_time = .data[[event_var]],
      fe_group = .data[[fe_group_var]],
      calendar_time = .data[[time_fe_var]]
    ) %>%
    dplyr::summarise(
      n_treated = sum(.data[[treat_indicator_var]], na.rm = TRUE),
      n_control = sum(.data[[control_indicator_var]], na.rm = TRUE),
      .groups = "drop"
    )

  event_support <- support_base %>%
    dplyr::group_by(event_time = .data[[event_var]]) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      n_treated = sum(.data[[treat_indicator_var]], na.rm = TRUE),
      n_control = sum(.data[[control_indicator_var]], na.rm = TRUE),
      n_pin10 = dplyr::n_distinct(pin10),
      n_blocks = dplyr::n_distinct(block_id),
      n_segments = dplyr::n_distinct(segment_id[!is.na(segment_id)]),
      n_fe_groups = dplyr::n_distinct(.data[[fe_group_var]]),
      total_outcome = sum(.data[[outcome_var]], na.rm = TRUE),
      n_positive_rows = sum(.data[[outcome_var]] > 0, na.rm = TRUE),
      .groups = "drop"
    )

  cell_event_support <- cell_support %>%
    dplyr::group_by(event_time) %>%
    dplyr::summarise(
      n_fe_group_time_cells = dplyr::n(),
      n_identifying_fe_group_time_cells = sum(n_treated > 0 & n_control > 0),
      n_identifying_fe_groups = dplyr::n_distinct(fe_group[n_treated > 0 & n_control > 0]),
      .groups = "drop"
    )

  event_support %>%
    dplyr::left_join(cell_event_support, by = "event_time") %>%
    dplyr::mutate(
      n_fe_group_time_cells = tidyr::replace_na(n_fe_group_time_cells, 0L),
      n_identifying_fe_group_time_cells = tidyr::replace_na(n_identifying_fe_group_time_cells, 0L),
      n_identifying_fe_groups = tidyr::replace_na(n_identifying_fe_groups, 0L),
      has_treated_and_control = n_treated > 0 & n_control > 0,
      has_identifying_support = n_identifying_fe_group_time_cells > 0,
      passes_display_rule = has_identifying_support
    ) %>%
    dplyr::arrange(event_time)
}

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_land_transaction_incidence/code")
# cohort <- "cohort_2015"
# outcome_var <- "sold_any"
# treatment_type <- "continuous"
# weighting <- "uniform"
# bandwidth <- 1000
# geo_fe_level <- "segment"
# sample_restriction <- "none"
# event_time_style <- "binned"
# event_window <- "long"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(cohort, outcome_var, treatment_type, weighting, bandwidth, geo_fe_level, sample_restriction, event_time_style, event_window)
}

if (!length(cli_args) %in% c(7, 8, 9)) {
  stop(
    paste(
      "FATAL: Script requires 7, 8, or 9 args:",
      "<cohort> <outcome_var> <treatment_type> <weighting>",
      "<bandwidth> <geo_fe_level> <sample_restriction> [<event_time_style>] [<event_window>]"
    ),
    call. = FALSE
  )
}

cohort <- cli_args[1]
outcome_var <- cli_args[2]
treatment_type <- tolower(cli_args[3])
weighting <- cli_args[4]
bandwidth <- as.numeric(cli_args[5])
geo_fe_level <- tolower(cli_args[6])
sample_restriction <- tolower(cli_args[7])
event_time_style <- if (length(cli_args) >= 8) tolower(cli_args[8]) else "yearly"
event_window <- if (length(cli_args) >= 9) tolower(cli_args[9]) else "full"

valid_outcomes <- c(
  "sold_any",
  "sold_any_arm_length",
  "sold_land_like",
  "sold_land_like_arm_length",
  "sold_land_tag",
  "sold_land_tag_arm_length"
)
if (!cohort %in% c("cohort_2012", "cohort_2015")) {
  stop("--cohort must be one of: cohort_2012, cohort_2015", call. = FALSE)
}
if (!outcome_var %in% valid_outcomes) {
  stop(sprintf("--outcome_var must be one of: %s", paste(valid_outcomes, collapse = ", ")), call. = FALSE)
}
if (!treatment_type %in% c("continuous", "continuous_split")) {
  stop("--treatment_type must be one of: continuous, continuous_split", call. = FALSE)
}
if (!weighting %in% c("uniform", "triangular")) {
  stop("--weighting must be one of: uniform, triangular", call. = FALSE)
}
if (!bandwidth %in% c(500, 1000)) {
  stop("--bandwidth must be one of: 500, 1000", call. = FALSE)
}
if (!geo_fe_level %in% c("segment", "ward_pair")) {
  stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!event_time_style %in% c("yearly", "binned")) {
  stop("--event_time_style must be one of: yearly, binned", call. = FALSE)
}
if (!event_window %in% c("full", "long")) {
  stop("--event_window must be one of: full, long", call. = FALSE)
}
sample_restriction_info <- get_land_transaction_sample_restriction_info(sample_restriction)

min_period <- if (event_window == "long") -8L else -5L
max_period <- if (event_window == "long") 8L else 5L

suffix <- sprintf(
  "land_transaction_incidence_yearly_%s_%s_%s_%s_%dft_geo_%s_clust_block_full",
  cohort,
  outcome_var,
  treatment_type,
  weighting,
  as.integer(bandwidth),
  geo_fe_level
)
if (sample_restriction != "none") {
  suffix <- paste0(suffix, "_samp_", sample_restriction_info$suffix_tag)
}
if (event_time_style != "yearly") {
  suffix <- paste0(suffix, "_", event_time_style)
}
if (event_window != "full") {
  suffix <- paste0(suffix, "_", event_window)
}

message("\n=== Land Transaction Incidence Event Study ===")
message(sprintf("Cohort: %s", cohort))
message(sprintf("Outcome: %s", outcome_var))
message(sprintf("Treatment type: %s", treatment_type))
message(sprintf("Weighting: %s", weighting))
message(sprintf("Bandwidth: %d ft", as.integer(bandwidth)))
message(sprintf("Geo FE level: %s", geo_fe_level))
message(sprintf("Sample restriction: %s", sample_restriction_info$label))
message(sprintf("Event time style: %s", event_time_style))
message(sprintf("Event window: %s", event_window))

data <- arrow::read_parquet("../input/land_transaction_incidence_model_panel.parquet") %>%
  tibble::as_tibble() %>%
  dplyr::filter(cohort == !!cohort) %>%
  dplyr::filter(relative_year >= min_period, relative_year <= max_period) %>%
  dplyr::filter(if (bandwidth == 500) in_500ft %in% TRUE else dist_to_boundary_ft <= bandwidth) %>%
  dplyr::mutate(
    weight = if (weighting == "triangular") pmax(0, 1 - dist_to_boundary_ft / bandwidth) else 1,
    support_treated = as.integer(treat == 1),
    support_control = as.integer(treat == 0)
  )

if (geo_fe_level == "segment") {
  data <- data %>%
    dplyr::filter(!is.na(segment_id), segment_id != "")
  fe_group_var <- "segment_id"
  fe_formula <- "pin10 + segment_id^calendar_year"
} else {
  data <- data %>%
    dplyr::filter(!is.na(ward_pair_id), ward_pair_id != "")
  fe_group_var <- "ward_pair_id"
  fe_formula <- "pin10 + ward_pair_id^calendar_year"
}

sample_restriction_result <- apply_land_transaction_sample_restriction(
  data,
  sample_restriction = sample_restriction,
  unit_id_var = "pin10"
)
data <- sample_restriction_result$data

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested incidence sample restrictions.", call. = FALSE)
}

event_time_result <- prepare_land_transaction_event_time(
  data,
  style = event_time_style,
  outcome_family = "incidence",
  event_time_var = "relative_year",
  min_period = min_period,
  max_period = max_period
)
data <- event_time_result$data

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested event-time style.", call. = FALSE)
}

metadata <- tibble::tibble(
  cohort = cohort,
  outcome_var = outcome_var,
  treatment_type = treatment_type,
  weighting = weighting,
  bandwidth = bandwidth,
  geo_fe_level = geo_fe_level,
  cluster_level = "block",
  event_time_style = event_time_style,
  event_window = event_window,
  sample_restriction = sample_restriction_info$sample_restriction,
  sample_restriction_label = sample_restriction_info$label,
  sample_restriction_obs_before = sample_restriction_result$summary$n_obs_before,
  sample_restriction_obs_after = sample_restriction_result$summary$n_obs_after,
  sample_restriction_obs_dropped = sample_restriction_result$summary$n_obs_dropped,
  sample_restriction_units_before = sample_restriction_result$summary$n_units_before,
  sample_restriction_units_after = sample_restriction_result$summary$n_units_after,
  sample_restriction_units_dropped = sample_restriction_result$summary$n_units_dropped,
  raw_n = nrow(data),
  analysis_n = nrow(data),
  analysis_pin10 = dplyr::n_distinct(data$pin10),
  analysis_blocks = dplyr::n_distinct(data$block_id),
  treated_rows = sum(data$treat == 1, na.rm = TRUE),
  control_rows = sum(data$treat == 0, na.rm = TRUE),
  treated_pin10 = dplyr::n_distinct(data$pin10[data$treat == 1]),
  control_pin10 = dplyr::n_distinct(data$pin10[data$treat == 0]),
  effective_weight_n = sum(data$weight, na.rm = TRUE),
  min_event_time = event_time_result$min_period,
  max_event_time = event_time_result$max_period,
  fe_formula = fe_formula
)

if (treatment_type == "continuous") {
  support_by_event_time <- make_incidence_support_table(
    df = data,
    event_var = "event_time_model",
    time_fe_var = "calendar_year",
    fe_group_var = fe_group_var,
    outcome_var = outcome_var,
    treat_indicator_var = "support_treated",
    control_indicator_var = "support_control",
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period
  ) %>%
    dplyr::left_join(event_time_result$labels, by = c("event_time" = "event_time_model")) %>%
    dplyr::mutate(group = "All parcels")

  formula_str <- sprintf(
    "%s ~ i(event_time_model, strictness_change, ref = -1) | %s",
    outcome_var,
    fe_formula
  )
  message(sprintf("Formula: %s", formula_str))

  model <- fixest::feols(
    stats::as.formula(formula_str),
    data = data,
    weights = ~weight,
    cluster = ~block_id
  )

  coefficients <- extract_fixest_event_study_coefficients(
    model,
    support_by_event_time %>% dplyr::select(-group),
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period,
    group_label = "All parcels",
    display_mode = "multiply100"
  )
  if (is.null(coefficients) || nrow(coefficients) == 0) {
    stop("No event-study coefficients were produced for the requested incidence specification.", call. = FALSE)
  }

  plot_data <- coefficients %>%
    dplyr::filter(is_reference | has_identifying_support)
  if (nrow(plot_data) == 0) {
    stop("No supported event times were available for plotting in the requested incidence specification.", call. = FALSE)
  }

  pretrend <- compute_event_study_pretrend(model, plot_data, "All parcels")

  ggplot2::ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = sprintf(
        "Land transaction incidence event study: %s%s",
        cohort,
        if (event_time_style == "binned") " (binned)" else ""
      ),
      x_label = "Years relative to redistricting",
      y_label = "Effect on sale incidence",
      display_suffix = " pp"
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )

  readr::write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
  readr::write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
  readr::write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
  readr::write_csv(
    metadata %>%
      dplyr::mutate(
        plotted_supported_periods = paste(
          support_by_event_time$event_time_label[support_by_event_time$has_identifying_support],
          collapse = "|"
        ),
        formula = formula_str
      ),
    sprintf("../output/event_study_metadata_%s.csv", suffix)
  )
} else {
  data <- data %>%
    dplyr::mutate(
      support_stricter = as.integer(treatment_stricter_continuous > 0),
      support_lenient = as.integer(treatment_lenient_continuous > 0)
    )

  support_stricter <- make_incidence_support_table(
    df = data,
    event_var = "event_time_model",
    time_fe_var = "calendar_year",
    fe_group_var = fe_group_var,
    outcome_var = outcome_var,
    treat_indicator_var = "support_stricter",
    control_indicator_var = "support_control",
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period
  ) %>%
    dplyr::left_join(event_time_result$labels, by = c("event_time" = "event_time_model")) %>%
    dplyr::mutate(group = "Moved to Stricter")

  support_lenient <- make_incidence_support_table(
    df = data,
    event_var = "event_time_model",
    time_fe_var = "calendar_year",
    fe_group_var = fe_group_var,
    outcome_var = outcome_var,
    treat_indicator_var = "support_lenient",
    control_indicator_var = "support_control",
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period
  ) %>%
    dplyr::left_join(event_time_result$labels, by = c("event_time" = "event_time_model")) %>%
    dplyr::mutate(group = "Moved to More Lenient")

  formula_stricter <- sprintf(
    "%s ~ i(event_time_model, treatment_stricter_continuous, ref = -1) | %s",
    outcome_var,
    fe_formula
  )
  formula_lenient <- sprintf(
    "%s ~ i(event_time_model, treatment_lenient_continuous, ref = -1) | %s",
    outcome_var,
    fe_formula
  )
  message(sprintf("Formula (stricter): %s", formula_stricter))
  message(sprintf("Formula (lenient): %s", formula_lenient))

  model_stricter <- fixest::feols(
    stats::as.formula(formula_stricter),
    data = data,
    weights = ~weight,
    cluster = ~block_id
  )
  model_lenient <- fixest::feols(
    stats::as.formula(formula_lenient),
    data = data,
    weights = ~weight,
    cluster = ~block_id
  )

  coefficients_stricter <- extract_fixest_event_study_coefficients(
    model_stricter,
    support_stricter %>% dplyr::select(-group),
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period,
    group_label = "Moved to Stricter",
    display_mode = "multiply100"
  )
  coefficients_lenient <- extract_fixest_event_study_coefficients(
    model_lenient,
    support_lenient %>% dplyr::select(-group),
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period,
    group_label = "Moved to More Lenient",
    display_mode = "multiply100"
  )

  coefficients <- dplyr::bind_rows(coefficients_stricter, coefficients_lenient)
  if (nrow(coefficients) == 0) {
    stop("No event-study coefficients were produced for the requested split incidence specification.", call. = FALSE)
  }

  plot_data <- coefficients %>%
    dplyr::filter(is_reference | has_identifying_support)
  if (nrow(plot_data) == 0) {
    stop("No supported event times were available for plotting in the requested split incidence specification.", call. = FALSE)
  }

  pretrend <- dplyr::bind_rows(
    compute_event_study_pretrend(
      model_stricter,
      plot_data %>% dplyr::filter(group == "Moved to Stricter"),
      "Moved to Stricter"
    ),
    compute_event_study_pretrend(
      model_lenient,
      plot_data %>% dplyr::filter(group == "Moved to More Lenient"),
      "Moved to More Lenient"
    )
  )

  directional_plots <- make_event_study_directional_plots(
    plot_data,
    plot_title = sprintf(
      "Land transaction incidence event study: %s%s",
      cohort,
      if (event_time_style == "binned") " (binned)" else ""
    ),
    x_label = "Years relative to redistricting",
    y_label = "Effect on sale incidence",
    display_suffix = " pp"
  )

  ggplot2::ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    directional_plots$facet,
    width = 7,
    height = 6,
    bg = "white"
  )
  ggplot2::ggsave(
    sprintf("../output/event_study_combined_%s.pdf", suffix),
    directional_plots$combined,
    width = 7,
    height = 4.5,
    bg = "white"
  )

  readr::write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
  readr::write_csv(
    dplyr::bind_rows(support_stricter, support_lenient),
    sprintf("../output/event_study_support_%s.csv", suffix)
  )
  readr::write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
  readr::write_csv(
    metadata %>%
      dplyr::mutate(
        plotted_supported_periods_stricter = paste(
          support_stricter$event_time_label[support_stricter$has_identifying_support],
          collapse = "|"
        ),
        plotted_supported_periods_lenient = paste(
          support_lenient$event_time_label[support_lenient$has_identifying_support],
          collapse = "|"
        ),
        formula_stricter = formula_stricter,
        formula_lenient = formula_lenient
      ),
    sprintf("../output/event_study_metadata_%s.csv", suffix)
  )
}
