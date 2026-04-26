source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")
source("../../_lib/land_transaction_model_helpers.R")

options(dplyr.summarise.inform = FALSE)

make_price_support_table <- function(
    df,
    event_var,
    time_fe_var,
    fe_group_var,
    treat_indicator_var,
    control_indicator_var,
    min_period,
    max_period,
    support_style = c("strict", "sales_style")) {
  support_style <- match.arg(support_style)

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
      n_sale_events = dplyr::n_distinct(sale_event_id),
      n_pin10 = dplyr::n_distinct(pin10),
      n_blocks = dplyr::n_distinct(block_id),
      n_segments = dplyr::n_distinct(segment_id[!is.na(segment_id)]),
      n_fe_groups = dplyr::n_distinct(.data[[fe_group_var]]),
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

  support_dt <- event_support %>%
    dplyr::left_join(cell_event_support, by = "event_time") %>%
    dplyr::mutate(
      n_fe_group_time_cells = tidyr::replace_na(n_fe_group_time_cells, 0L),
      n_identifying_fe_group_time_cells = tidyr::replace_na(n_identifying_fe_group_time_cells, 0L),
      n_identifying_fe_groups = tidyr::replace_na(n_identifying_fe_groups, 0L),
      has_treated_and_control = n_treated > 0 & n_control > 0,
      passes_strict_display_rule = (
        n_treated >= 10 &
          n_control >= 20 &
          n_identifying_fe_group_time_cells >= 5
      ),
      passes_sales_style_rule = (
        n_treated >= 10 &
          n_control >= 10 &
          n_identifying_fe_group_time_cells >= 5 &
          n_identifying_fe_groups >= 5
      )
    )

  support_dt <- support_dt %>%
    dplyr::mutate(
      passes_display_rule = if (support_style == "sales_style") {
        passes_sales_style_rule
      } else {
        passes_strict_display_rule
      },
      has_identifying_support = passes_display_rule
    ) %>%
    dplyr::arrange(event_time)

  support_dt
}

sample_label_from_flag <- function(sample_flag) {
  dplyr::case_when(
    sample_flag == "price_sample_arm_length_land_like" ~ "Arm's-length land-like sales",
    sample_flag == "price_sample_broad_land_like" ~ "Broad land-like sales",
    sample_flag == "price_sample_arm_length_raw_land" ~ "Arm's-length raw LAND-tagged sales",
    sample_flag == "price_sample_raw_land_small_package_le2" ~ "Raw LAND-tagged sales, nonpackage or <=2 parcels",
    sample_flag == "price_sample_warranty_small_package_le2" ~ "Warranty sales, nonpackage or <=2 parcels",
    TRUE ~ sample_flag
  )
}

outcome_label_from_var <- function(outcome_var) {
  dplyr::case_when(
    outcome_var == "log_sale_price" ~ "sale price",
    outcome_var == "log_sale_price_psf_current" ~ "sale price per current lot sqft",
    TRUE ~ outcome_var
  )
}

raw_price_outcome_from_var <- function(outcome_var) {
  dplyr::case_when(
    outcome_var == "log_sale_price" ~ "sale_price",
    outcome_var == "log_sale_price_psf_current" ~ "sale_price_psf_current_raw",
    TRUE ~ NA_character_
  )
}

prepare_price_outcome <- function(df, outcome_var, winsorization) {
  raw_outcome_var <- raw_price_outcome_from_var(outcome_var)

  df <- df %>%
    dplyr::filter(!is.na(.data[[raw_outcome_var]]), .data[[raw_outcome_var]] > 0)

  if (winsorization == "none") {
    df <- df %>%
      dplyr::mutate(
        outcome_raw_for_estimation = .data[[raw_outcome_var]],
        outcome_estimation = log(outcome_raw_for_estimation),
        outcome_winsorized_flag = FALSE
      )

    return(list(
      data = df,
      raw_outcome_var = raw_outcome_var,
      lower = NA_real_,
      upper = NA_real_,
      n_modified = 0L,
      share_modified = 0
    ))
  }

  if (winsorization == "p01_p99") {
    winsor_result <- winsorize_numeric_series(df[[raw_outcome_var]], 0.01, 0.99)

    df <- df %>%
      dplyr::mutate(
        outcome_raw_for_estimation = winsor_result$values,
        outcome_estimation = log(outcome_raw_for_estimation),
        outcome_winsorized_flag = abs(outcome_raw_for_estimation - .data[[raw_outcome_var]]) > 1e-8
      )

    return(list(
      data = df,
      raw_outcome_var = raw_outcome_var,
      lower = winsor_result$lower,
      upper = winsor_result$upper,
      n_modified = winsor_result$n_modified,
      share_modified = winsor_result$share_modified
    ))
  }

  stop("--winsorization must be one of: none, p01_p99", call. = FALSE)
}

build_selection_summary <- function(df, event_var = "relative_year") {
  df %>%
    dplyr::mutate(
      treatment_group = dplyr::case_when(
        treat == 0 ~ "Valid controls",
        strictness_change > 0 ~ "Moved to Stricter",
        strictness_change < 0 ~ "Moved to More Lenient",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::group_by(
      event_time = .data[[event_var]],
      event_time_label,
      treatment_group
    ) %>%
    dplyr::summarise(
      n_sale_events = dplyr::n(),
      n_pin10 = dplyr::n_distinct(pin10),
      mean_lot_sqft_current = mean(lot_sqft_current, na.rm = TRUE),
      median_lot_sqft_current = stats::median(lot_sqft_current, na.rm = TRUE),
      mean_baseline_land_psf = mean(baseline_land_psf, na.rm = TRUE),
      median_baseline_land_psf = stats::median(baseline_land_psf, na.rm = TRUE),
      mean_baseline_land_sum = mean(baseline_land_sum, na.rm = TRUE),
      median_baseline_land_sum = stats::median(baseline_land_sum, na.rm = TRUE),
      mean_dist_to_boundary_ft = mean(dist_to_boundary_ft, na.rm = TRUE),
      median_dist_to_boundary_ft = stats::median(dist_to_boundary_ft, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(event_time, treatment_group)
}

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_land_transaction_price/code")
# cohort <- "cohort_2015"
# outcome_var <- "log_sale_price"
# sample_flag <- "price_sample_arm_length_land_like"
# treatment_type <- "continuous"
# weighting <- "uniform"
# bandwidth <- 1000
# post_window <- "long"
# geo_fe_level <- "ward_pair"
# sample_restriction <- "none"
# winsorization <- "none"
# support_style <- "strict"
# event_time_style <- "binned"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(cohort, outcome_var, sample_flag, treatment_type, weighting, bandwidth, post_window, geo_fe_level, sample_restriction, winsorization, support_style, event_time_style)
}

if (!length(cli_args) %in% c(9, 10, 11, 12)) {
  stop(
    paste(
      "FATAL: Script requires 9, 10, 11, or 12 args:",
      "<cohort> <outcome_var> <sample_flag> <treatment_type>",
      "<weighting> <bandwidth> <post_window> <geo_fe_level> <sample_restriction> [<winsorization>] [<support_style>] [<event_time_style>]"
    ),
    call. = FALSE
  )
}

cohort <- cli_args[1]
outcome_var <- cli_args[2]
sample_flag <- cli_args[3]
treatment_type <- tolower(cli_args[4])
weighting <- cli_args[5]
bandwidth <- as.numeric(cli_args[6])
post_window <- tolower(cli_args[7])
geo_fe_level <- tolower(cli_args[8])
sample_restriction <- tolower(cli_args[9])
winsorization <- if (length(cli_args) >= 10) tolower(cli_args[10]) else "none"
support_style <- if (length(cli_args) >= 11) tolower(cli_args[11]) else "strict"
event_time_style <- if (length(cli_args) >= 12) tolower(cli_args[12]) else "yearly"

valid_sample_flags <- c(
  "price_sample_arm_length_land_like",
  "price_sample_broad_land_like",
  "price_sample_arm_length_raw_land",
  "price_sample_raw_land_small_package_le2",
  "price_sample_warranty_small_package_le2"
)
if (!cohort %in% c("cohort_2012", "cohort_2015")) {
  stop("--cohort must be one of: cohort_2012, cohort_2015", call. = FALSE)
}
if (!outcome_var %in% c("log_sale_price", "log_sale_price_psf_current")) {
  stop("--outcome_var must be one of: log_sale_price, log_sale_price_psf_current", call. = FALSE)
}
if (!sample_flag %in% valid_sample_flags) {
  stop(sprintf("--sample_flag must be one of: %s", paste(valid_sample_flags, collapse = ", ")), call. = FALSE)
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
if (!post_window %in% c("short", "full", "long")) {
  stop("--post_window must be one of: short, full, long", call. = FALSE)
}
if (!geo_fe_level %in% c("ward_pair", "segment")) {
  stop("--geo_fe_level must be one of: ward_pair, segment", call. = FALSE)
}
if (!winsorization %in% c("none", "p01_p99")) {
  stop("--winsorization must be one of: none, p01_p99", call. = FALSE)
}
if (!support_style %in% c("strict", "sales_style")) {
  stop("--support_style must be one of: strict, sales_style", call. = FALSE)
}
if (!event_time_style %in% c("yearly", "binned")) {
  stop("--event_time_style must be one of: yearly, binned", call. = FALSE)
}
sample_restriction_info <- get_land_transaction_sample_restriction_info(sample_restriction)

min_period <- if (post_window == "long") -8L else -5L
max_period <- dplyr::case_when(
  post_window == "short" ~ 3L,
  post_window == "full" ~ 5L,
  post_window == "long" ~ 8L
)
suffix <- sprintf(
  "land_transaction_price_yearly_%s_%s_%s_%s_%s_%dft_geo_%s_clust_block_%s",
  cohort,
  outcome_var,
  sample_flag,
  treatment_type,
  weighting,
  as.integer(bandwidth),
  geo_fe_level,
  post_window
)
if (sample_restriction != "none") {
  suffix <- paste0(suffix, "_samp_", sample_restriction_info$suffix_tag)
}
if (event_time_style != "yearly") {
  suffix <- paste0(suffix, "_", event_time_style)
}
if (support_style != "strict") {
  suffix <- paste0(suffix, "_support_", gsub("_", "", support_style))
}
if (winsorization != "none") {
  suffix <- paste0(suffix, "_winsor_", gsub("_", "", winsorization))
}

message("\n=== Land Transaction Price Event Study ===")
message(sprintf("Cohort: %s", cohort))
message(sprintf("Outcome: %s", outcome_label_from_var(outcome_var)))
message(sprintf("Sample: %s", sample_label_from_flag(sample_flag)))
message(sprintf("Treatment type: %s", treatment_type))
message(sprintf("Weighting: %s", weighting))
message(sprintf("Bandwidth: %d ft", as.integer(bandwidth)))
message(sprintf("Post window: %s", post_window))
message(sprintf("Geo FE level: %s", geo_fe_level))
message(sprintf("Sample restriction: %s", sample_restriction_info$label))
message(sprintf("Winsorization: %s", winsorization))
message(sprintf("Support style: %s", support_style))
message(sprintf("Event time style: %s", event_time_style))

data <- arrow::read_parquet("../input/land_transaction_price_model_panel.parquet") %>%
  tibble::as_tibble() %>%
  dplyr::filter(cohort == !!cohort) %>%
  dplyr::filter(.data[[sample_flag]] %in% TRUE) %>%
  dplyr::filter(relative_year >= min_period, relative_year <= max_period) %>%
  dplyr::filter(if (bandwidth == 500) in_500ft %in% TRUE else dist_to_boundary_ft <= bandwidth) %>%
  dplyr::mutate(
    sale_price_psf_current_raw = dplyr::if_else(
      is.finite(sale_price) & sale_price > 0 &
        is.finite(lot_sqft_current) & lot_sqft_current > 0,
      sale_price / lot_sqft_current,
      NA_real_
    ),
    weight = if (weighting == "triangular") pmax(0, 1 - dist_to_boundary_ft / bandwidth) else 1,
    support_treated = as.integer(treat == 1),
    support_control = as.integer(treat == 0)
  )

if (geo_fe_level == "ward_pair") {
  data <- data %>%
    dplyr::filter(!is.na(ward_pair_side), ward_pair_side != "", !is.na(ward_pair_id), ward_pair_id != "")
  fe_group_var <- "ward_pair_id"
  fe_formula <- "ward_pair_side + ward_pair_id^sale_year"
} else {
  data <- data %>%
    dplyr::filter(!is.na(segment_side), segment_side != "", !is.na(segment_id), segment_id != "")
  fe_group_var <- "segment_id"
  fe_formula <- "segment_side + segment_id^sale_year"
}

sample_restriction_result <- apply_land_transaction_sample_restriction(
  data,
  sample_restriction = sample_restriction,
  unit_id_var = "sale_event_id"
)
data <- sample_restriction_result$data

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested price sample restrictions.", call. = FALSE)
}

event_time_result <- prepare_land_transaction_event_time(
  data,
  style = event_time_style,
  outcome_family = "price",
  event_time_var = "relative_year",
  min_period = min_period,
  max_period = max_period
)
data <- event_time_result$data

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested event-time style.", call. = FALSE)
}

price_outcome_result <- prepare_price_outcome(data, outcome_var, winsorization)
data <- price_outcome_result$data

if (nrow(data) == 0) {
  stop("No observations remain after preparing the requested price outcome.", call. = FALSE)
}

selection_summary <- build_selection_summary(data, event_var = "event_time_model")

metadata <- tibble::tibble(
  cohort = cohort,
  outcome_var = outcome_var,
  outcome_label = outcome_label_from_var(outcome_var),
  sample_flag = sample_flag,
  sample_label = sample_label_from_flag(sample_flag),
  treatment_type = treatment_type,
  weighting = weighting,
  bandwidth = bandwidth,
  geo_fe_level = geo_fe_level,
  cluster_level = "block",
  post_window = post_window,
  winsorization = winsorization,
  support_style = support_style,
  event_time_style = event_time_style,
  sample_restriction = sample_restriction_info$sample_restriction,
  sample_restriction_label = sample_restriction_info$label,
  sample_restriction_obs_before = sample_restriction_result$summary$n_obs_before,
  sample_restriction_obs_after = sample_restriction_result$summary$n_obs_after,
  sample_restriction_obs_dropped = sample_restriction_result$summary$n_obs_dropped,
  sample_restriction_units_before = sample_restriction_result$summary$n_units_before,
  sample_restriction_units_after = sample_restriction_result$summary$n_units_after,
  sample_restriction_units_dropped = sample_restriction_result$summary$n_units_dropped,
  analysis_n = nrow(data),
  analysis_sale_events = dplyr::n_distinct(data$sale_event_id),
  analysis_pin10 = dplyr::n_distinct(data$pin10),
  analysis_blocks = dplyr::n_distinct(data$block_id),
  treated_sale_events = sum(data$treat == 1, na.rm = TRUE),
  control_sale_events = sum(data$treat == 0, na.rm = TRUE),
  treated_pin10 = dplyr::n_distinct(data$pin10[data$treat == 1]),
  control_pin10 = dplyr::n_distinct(data$pin10[data$treat == 0]),
  effective_weight_n = sum(data$weight, na.rm = TRUE),
  raw_outcome_var = price_outcome_result$raw_outcome_var,
  winsor_lower = price_outcome_result$lower,
  winsor_upper = price_outcome_result$upper,
  n_winsorized = price_outcome_result$n_modified,
  share_winsorized = price_outcome_result$share_modified,
  min_event_time = event_time_result$min_period,
  max_event_time = event_time_result$max_period,
  fe_formula = fe_formula,
  outcome_caveat = dplyr::if_else(
    outcome_var == "log_sale_price_psf_current",
    "Price per square foot uses current lot square feet because sale-time lot size history is unavailable in this branch.",
    ""
  ),
  display_transform = "100_log_points"
)

if (treatment_type == "continuous") {
  support_by_event_time <- make_price_support_table(
    df = data,
    event_var = "event_time_model",
    time_fe_var = "sale_year",
    fe_group_var = fe_group_var,
    treat_indicator_var = "support_treated",
    control_indicator_var = "support_control",
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period,
    support_style = support_style
  ) %>%
    dplyr::left_join(event_time_result$labels, by = c("event_time" = "event_time_model")) %>%
    dplyr::mutate(group = "All sales")

  formula_str <- sprintf(
    "outcome_estimation ~ i(event_time_model, strictness_change, ref = -1) | %s",
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
    group_label = "All sales",
    display_mode = "log_points"
  )
  if (is.null(coefficients) || nrow(coefficients) == 0) {
    stop("No event-study coefficients were produced for the requested price specification.", call. = FALSE)
  }

  plot_data <- coefficients %>%
    dplyr::filter(is_reference | has_identifying_support)
  if (nrow(plot_data) == 0) {
    stop("No event times satisfy the display rule for the requested price specification.", call. = FALSE)
  }

  pretrend <- compute_event_study_pretrend(model, plot_data, "All sales")

  ggplot2::ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = sprintf(
        "Land transaction price event study: %s%s",
        sample_label_from_flag(sample_flag),
        if (event_time_style == "binned") " (binned)" else ""
      ),
      x_label = "Years relative to redistricting",
      y_label = sprintf("Effect on log %s (100 x log points)", outcome_label_from_var(outcome_var)),
      display_suffix = ""
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )

  readr::write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
  readr::write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
  readr::write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
  readr::write_csv(selection_summary, sprintf("../output/event_study_selection_%s.csv", suffix))
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

  support_stricter <- make_price_support_table(
    df = data,
    event_var = "event_time_model",
    time_fe_var = "sale_year",
    fe_group_var = fe_group_var,
    treat_indicator_var = "support_stricter",
    control_indicator_var = "support_control",
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period,
    support_style = support_style
  ) %>%
    dplyr::left_join(event_time_result$labels, by = c("event_time" = "event_time_model")) %>%
    dplyr::mutate(group = "Moved to Stricter")

  support_lenient <- make_price_support_table(
    df = data,
    event_var = "event_time_model",
    time_fe_var = "sale_year",
    fe_group_var = fe_group_var,
    treat_indicator_var = "support_lenient",
    control_indicator_var = "support_control",
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period,
    support_style = support_style
  ) %>%
    dplyr::left_join(event_time_result$labels, by = c("event_time" = "event_time_model")) %>%
    dplyr::mutate(group = "Moved to More Lenient")

  formula_stricter <- sprintf(
    "outcome_estimation ~ i(event_time_model, treatment_stricter_continuous, ref = -1) | %s",
    fe_formula
  )
  formula_lenient <- sprintf(
    "outcome_estimation ~ i(event_time_model, treatment_lenient_continuous, ref = -1) | %s",
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
    display_mode = "log_points"
  )
  coefficients_lenient <- extract_fixest_event_study_coefficients(
    model_lenient,
    support_lenient %>% dplyr::select(-group),
    min_period = event_time_result$min_period,
    max_period = event_time_result$max_period,
    group_label = "Moved to More Lenient",
    display_mode = "log_points"
  )

  coefficients <- dplyr::bind_rows(coefficients_stricter, coefficients_lenient)
  if (nrow(coefficients) == 0) {
    stop("No event-study coefficients were produced for the requested split price specification.", call. = FALSE)
  }

  plot_data <- coefficients %>%
    dplyr::filter(is_reference | has_identifying_support)
  if (nrow(plot_data) == 0) {
    stop("No event times satisfy the display rule for the requested split price specification.", call. = FALSE)
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
      "Land transaction price event study: %s%s",
      sample_label_from_flag(sample_flag),
      if (event_time_style == "binned") " (binned)" else ""
    ),
    x_label = "Years relative to redistricting",
    y_label = sprintf("Effect on log %s (100 x log points)", outcome_label_from_var(outcome_var)),
    display_suffix = ""
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
  readr::write_csv(selection_summary, sprintf("../output/event_study_selection_%s.csv", suffix))
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
