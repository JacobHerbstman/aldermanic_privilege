source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_permit/code")
# Rscript run_event_study_permit.R "stacked_implementation" "high_discretion" "issue" "ppml" "binary_direction" "uniform" 1000 "within_block" "full" "ward_pair" "block" "none"
# =======================================================================================

dir.create("../output", showWarnings = FALSE, recursive = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
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
} else {
  stop(
    "FATAL: Script requires at least 11 args: <panel_mode> <outcome_family> <date_basis> <model_type> <treatment_type> <weighting> <bandwidth> <fe_type> <post_window> <geo_fe_level> <cluster_level> [<control_spec>]",
    call. = FALSE
  )
}

write_sidecars <- tolower(Sys.getenv("WRITE_SIDECARS", "0")) %in% c("1", "true", "yes")

PANEL_MODE <- panel_mode
OUTCOME_FAMILY <- outcome_family
DATE_BASIS <- date_basis
MODEL_TYPE <- tolower(model_type)
TREATMENT_TYPE <- tolower(treatment_type)
WEIGHTING <- weighting
BANDWIDTH <- bandwidth
FE_TYPE <- fe_type
POST_WINDOW <- post_window
GEO_FE_LEVEL <- geo_fe_level
CLUSTER_LEVEL <- cluster_level
CONTROL_SPEC <- control_spec

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
if (!CONTROL_SPEC %in% c("none", "baseline_demographics")) {
  stop("--control_spec must be one of: none, baseline_demographics", call. = FALSE)
}
if (BANDWIDTH <= 0) {
  stop("--bandwidth must be positive.", call. = FALSE)
}
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 2000) {
  stop("Segment FE requested with bandwidth > 2000. Use bandwidth <= 2000.", call. = FALSE)
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
  "yearly_%s_%s_%s_%s_%s_%s_%dft_within_block_%s_clust_%s",
  PANEL_MODE,
  OUTCOME_FAMILY,
  DATE_BASIS,
  MODEL_TYPE,
  TREATMENT_TYPE,
  WEIGHTING,
  as.integer(BANDWIDTH),
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

message("\n=== Permit Event Study ===")
message(sprintf("Panel mode: %s", PANEL_MODE))
message(sprintf("Outcome: %s", outcome_label))
message(sprintf("Model type: %s", MODEL_TYPE))
message(sprintf("Treatment type: %s", TREATMENT_TYPE))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("FE type: %s", FE_TYPE))
message(sprintf("Post window: %s", POST_WINDOW))
message(sprintf("Geo FE level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster level: %s", CLUSTER_LEVEL))
message(sprintf("Control spec: %s", CONTROL_SPEC))
message(sprintf("Write sidecars: %s", write_sidecars))

safe_scale <- function(x) {
  x <- as.numeric(x)
  sigma <- sd(x, na.rm = TRUE)
  mu <- mean(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(0, length(x)))
  }
  (x - mu) / sigma
}

display_transform <- function(estimate, ci_low, ci_high) {
  if (MODEL_TYPE == "binary") {
    tibble(
      estimate_display = 100 * estimate,
      ci_low_display = 100 * ci_low,
      ci_high_display = 100 * ci_high,
      display_unit = "pp"
    )
  } else {
    tibble(
      estimate_display = 100 * (exp(estimate) - 1),
      ci_low_display = 100 * (exp(ci_low) - 1),
      ci_high_display = 100 * (exp(ci_high) - 1),
      display_unit = "%"
    )
  }
}

make_support_table <- function(df, event_var, time_fe_var, fe_group_var, block_var, segment_var, outcome_var, treatment_var, min_period, max_period) {
  support_base <- df %>%
    filter(.data[[event_var]] >= min_period, .data[[event_var]] <= max_period)

  cell_support <- support_base %>%
    group_by(
      event_time = .data[[event_var]],
      fe_group = .data[[fe_group_var]],
      calendar_time = .data[[time_fe_var]]
    ) %>%
    summarise(
      n_blocks_cell = n_distinct(.data[[block_var]]),
      n_treated = sum(treat == 1, na.rm = TRUE),
      n_control = sum(treat == 0, na.rm = TRUE),
      n_distinct_treatment_values = n_distinct(.data[[treatment_var]][!is.na(.data[[treatment_var]])]),
      has_within_cell_treatment_variation = n_distinct(.data[[treatment_var]][!is.na(.data[[treatment_var]])]) > 1,
      .groups = "drop"
    )

  event_support <- support_base %>%
    group_by(event_time = .data[[event_var]]) %>%
    summarise(
      n_obs = n(),
      n_treated = sum(treat == 1, na.rm = TRUE),
      n_control = sum(treat == 0, na.rm = TRUE),
      contributing_cohorts = if ("cohort" %in% names(support_base)) paste(sort(unique(cohort)), collapse = "|") else PANEL_MODE,
      n_fe_groups = n_distinct(.data[[fe_group_var]]),
      n_blocks = n_distinct(.data[[block_var]]),
      n_segments = n_distinct(.data[[segment_var]][!is.na(.data[[segment_var]])]),
      total_outcome = sum(.data[[outcome_var]], na.rm = TRUE),
      n_positive_rows = sum(.data[[outcome_var]] > 0, na.rm = TRUE),
      .groups = "drop"
    )

  cell_event_support <- cell_support %>%
    group_by(event_time) %>%
    summarise(
      n_fe_group_time_cells = n(),
      n_identifying_fe_group_time_cells = sum(has_within_cell_treatment_variation, na.rm = TRUE),
      n_identifying_fe_groups = n_distinct(fe_group[has_within_cell_treatment_variation]),
      .groups = "drop"
    )

  event_support %>%
    left_join(cell_event_support, by = "event_time") %>%
    mutate(
      n_fe_group_time_cells = replace_na(n_fe_group_time_cells, 0L),
      n_identifying_fe_group_time_cells = replace_na(n_identifying_fe_group_time_cells, 0L),
      n_identifying_fe_groups = replace_na(n_identifying_fe_groups, 0L),
      has_treated_and_control = n_treated > 0 & n_control > 0,
      has_identifying_support = n_identifying_fe_group_time_cells > 0
    ) %>%
    arrange(event_time)
}

extract_plot_data <- function(model, support_by_event_time, min_period, max_period, group_label) {
  iplot_data <- tryCatch(iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
  if (is.null(iplot_data) || nrow(iplot_data) == 0) {
    return(NULL)
  }

  supported_periods <- support_by_event_time %>%
    filter(has_identifying_support) %>%
    pull(event_time)

  out <- iplot_data %>%
    as_tibble() %>%
    transmute(
      event_time = as.integer(x),
      estimate,
      ci_low,
      ci_high,
      std_error = if_else(is_ref, 0, (ci_high - estimate) / qnorm(0.975)),
      estimate_name = estimate_names,
      estimate_name_raw = estimate_names_raw,
      is_reference = is_ref,
      group = group_label
    ) %>%
    filter(event_time >= min_period, event_time <= max_period) %>%
    filter(is_reference | event_time %in% supported_periods) %>%
    left_join(support_by_event_time, by = "event_time")

  bind_cols(out, display_transform(out$estimate, out$ci_low, out$ci_high))
}

compute_pretrend_test <- function(model, plot_data, group_label) {
  lead_terms <- plot_data %>%
    filter(event_time <= -2, !is_reference) %>%
    pull(estimate_name_raw)

  if (length(lead_terms) == 0) {
    return(tibble(
      group = group_label,
      n_leads = 0L,
      min_lead = NA_integer_,
      max_lead = NA_integer_,
      wald_stat = NA_real_,
      p_value = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_
    ))
  }

  joint_test <- tryCatch(wald(model, lead_terms), error = function(e) NULL)
  tibble(
    group = group_label,
    n_leads = length(lead_terms),
    min_lead = min(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    max_lead = max(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    wald_stat = if (is.null(joint_test)) NA_real_ else joint_test$stat,
    p_value = if (is.null(joint_test)) NA_real_ else joint_test$p,
    df1 = if (is.null(joint_test)) NA_real_ else joint_test$df1,
    df2 = if (is.null(joint_test)) NA_real_ else joint_test$df2
  )
}

axis_label <- function() {
  if (MODEL_TYPE == "log") {
    sprintf("Effect on log %s", outcome_label)
  } else {
    sprintf("Effect on %s", outcome_label)
  }
}

display_formatter <- function(x) {
  if (MODEL_TYPE == "binary") {
    paste0(x, " pp")
  } else {
    paste0(x, "%")
  }
}

make_single_series_plot <- function(plot_data) {
  ggplot(plot_data, aes(x = event_time, y = estimate_display)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), fill = "#009E73", alpha = 0.2, color = NA) +
    geom_line(color = "#009E73", linewidth = 1) +
    geom_point(color = "#009E73", size = 2.5) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = display_formatter) +
    labs(
      title = sprintf("Permit event study: %s", panel_title),
      x = "Years relative to alderman switch",
      y = axis_label()
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "gray40", linewidth = 0.3),
      axis.ticks = element_line(color = "gray40", linewidth = 0.3),
      axis.title = element_text(size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )
}

make_directional_plots <- function(plot_data) {
  color_values <- c(
    "Moved to Stricter" = "#c23616",
    "Moved to More Lenient" = "#7f8fa6"
  )

  facet_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_display, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = display_formatter) +
    facet_wrap(~group, ncol = 1) +
    labs(
      title = sprintf("Permit event study: %s", panel_title),
      x = "Years relative to alderman switch",
      y = axis_label()
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "gray40", linewidth = 0.3),
      axis.ticks = element_line(color = "gray40", linewidth = 0.3),
      axis.title = element_text(size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      legend.position = "none",
      strip.text = element_text(face = "bold", size = 10),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )

  combined_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_display, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_display, ymax = ci_high_display), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = display_formatter) +
    labs(
      title = sprintf("Permit event study: %s", panel_title),
      x = "Years relative to alderman switch",
      y = axis_label()
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      axis.line = element_line(color = "gray40", linewidth = 0.3),
      axis.ticks = element_line(color = "gray40", linewidth = 0.3),
      axis.title = element_text(size = 10, color = "gray20"),
      axis.text = element_text(size = 9, color = "gray30"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )

  list(facet = facet_plot, combined = combined_plot)
}

message("\nLoading permit block-year panel...")
data <- read_parquet(panel_input) %>%
  filter(!is.na(strictness_change), !is.na(.data[[base_outcome_var]]))

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

required_cols <- c(block_var, geo_group_var, "year", "relative_year", "dist_ft", "treat", base_outcome_var)
missing_cols <- setdiff(required_cols, names(data))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

data <- data %>%
  filter(dist_ft <= BANDWIDTH) %>%
  mutate(
    weight = if (WEIGHTING == "triangular") pmax(0, 1 - dist_ft / BANDWIDTH) else 1,
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0),
    treatment_stricter_binary = as.integer(strictness_change > 0),
    treatment_lenient_binary = as.integer(strictness_change < 0)
  ) %>%
  filter(relative_year >= min_period, relative_year <= max_period) %>%
  filter(!is.na(.data[[geo_group_var]]), .data[[geo_group_var]] != "")

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

  data <- data %>%
    mutate(
      block_group_id = substr(as.character(block_id), 1, 12),
      baseline_year = case_when(
        cohort == "2015" ~ 2014L,
        cohort == "2023" ~ 2022L,
        TRUE ~ NA_integer_
      )
    ) %>%
    left_join(baseline_controls, by = c("block_group_id", "baseline_year"))

  missing_control_rows <- sum(!complete.cases(data[, control_vars]), na.rm = TRUE)
  data <- data %>%
    filter(if_all(all_of(control_vars), ~ !is.na(.x))) %>%
    mutate(across(all_of(control_vars), safe_scale, .names = "{.col}_z"))
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

control_terms <- if (CONTROL_SPEC == "baseline_demographics") {
  paste(sprintf("%s:factor(year)", paste0(control_vars, "_z")), collapse = " + ")
} else {
  NULL
}

analysis_n <- nrow(data)
if (analysis_n == 0) {
  stop("No observations remain after applying the requested permit event-study sample restrictions.", call. = FALSE)
}

run_model <- function(formula_str, data_arg = data) {
  message(sprintf("Running %s model with %s observations", MODEL_TYPE, format(nrow(data_arg), big.mark = ",")))
  message(sprintf("Formula: %s", formula_str))
  if (MODEL_TYPE == "ppml") {
    fepois(
      as.formula(formula_str),
      data = data_arg,
      weights = ~weight,
      cluster = cluster_formula
    )
  } else {
    feols(
      as.formula(formula_str),
      data = data_arg,
      weights = ~weight,
      cluster = cluster_formula
    )
  }
}

metadata <- tibble(
  panel_mode = PANEL_MODE,
  panel_title = panel_title,
  outcome_family = OUTCOME_FAMILY,
  date_basis = DATE_BASIS,
  outcome_var = base_outcome_var,
  outcome_label = outcome_label,
  model_type = MODEL_TYPE,
  treatment_type = TREATMENT_TYPE,
  weighting = WEIGHTING,
  bandwidth = BANDWIDTH,
  fe_type = FE_TYPE,
  post_window = POST_WINDOW,
  geo_fe_level = GEO_FE_LEVEL,
  cluster_level = CLUSTER_LEVEL,
  raw_n = raw_n,
  raw_blocks = raw_blocks,
  analysis_n = analysis_n,
  analysis_blocks = n_distinct(data[[block_var]]),
  treated_n = sum(data$treat == 1, na.rm = TRUE),
  control_n = sum(data$treat == 0, na.rm = TRUE),
  positive_outcome_rows = sum(data[[base_outcome_var]] > 0, na.rm = TRUE),
  total_outcome = sum(data[[base_outcome_var]], na.rm = TRUE),
  effective_weight_n = sum(data$weight),
  zero_rows_dropped_for_log = zero_rows_dropped,
  control_spec = CONTROL_SPEC,
  missing_rows_dropped_for_controls = missing_control_rows,
  available_min_event_time = available_min_period,
  available_max_event_time = available_max_period,
  plotted_min_event_time = min_period,
  plotted_max_event_time = max_period
)

if (TREATMENT_TYPE == "continuous") {
  support_by_event_time <- make_support_table(
    df = data,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    block_var = block_var,
    segment_var = geo_group_var,
    outcome_var = base_outcome_var,
    treatment_var = "strictness_change",
    min_period = min_period,
    max_period = max_period
  )

  metadata <- metadata %>%
    mutate(plotted_supported_periods = paste(support_by_event_time$event_time[support_by_event_time$has_identifying_support], collapse = "|"))

  rhs_terms <- c(sprintf("i(%s, strictness_change, ref = -1)", event_var), control_terms)
  formula_str <- sprintf(
    "outcome_estimation ~ %s | %s",
    paste(rhs_terms[!is.na(rhs_terms) & nzchar(rhs_terms)], collapse = " + "),
    fe_formula
  )
  model <- run_model(formula_str)
  plot_data <- extract_plot_data(model, support_by_event_time, min_period, max_period, "All blocks")

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested permit specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(
      group, event_time, estimate, std_error, ci_low, ci_high,
      estimate_display, ci_low_display, ci_high_display, display_unit,
      estimate_name, estimate_name_raw, is_reference,
      n_obs, n_treated, n_control, contributing_cohorts, n_fe_groups, n_blocks, n_segments, total_outcome,
      n_positive_rows, n_fe_group_time_cells, n_identifying_fe_group_time_cells,
      n_identifying_fe_groups, has_treated_and_control, has_identifying_support
    )
  pretrend <- compute_pretrend_test(model, plot_data, "All blocks")

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), make_single_series_plot(plot_data), width = 7, height = 4.5, bg = "white")

  if (write_sidecars) {
    write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
    write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
    write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
    write_csv(metadata, sprintf("../output/event_study_metadata_%s.csv", suffix))
  }
} else if (TREATMENT_TYPE == "binary_direction") {
  data_stricter <- data %>% filter(treatment_lenient_binary == 0)
  data_lenient <- data %>% filter(treatment_stricter_binary == 0)

  support_stricter <- make_support_table(
    df = data_stricter,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    block_var = block_var,
    segment_var = geo_group_var,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_stricter_binary",
    min_period = min_period,
    max_period = max_period
  )
  support_lenient <- make_support_table(
    df = data_lenient,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    block_var = block_var,
    segment_var = geo_group_var,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_lenient_binary",
    min_period = min_period,
    max_period = max_period
  )

  metadata <- metadata %>%
    mutate(
      stricter_analysis_n = nrow(data_stricter),
      lenient_analysis_n = nrow(data_lenient),
      plotted_supported_periods = paste(sort(unique(c(
        support_stricter$event_time[support_stricter$has_identifying_support],
        support_lenient$event_time[support_lenient$has_identifying_support]
      ))), collapse = "|")
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

  model_stricter <- run_model(formula_stricter, data_stricter)
  model_lenient <- run_model(formula_lenient, data_lenient)

  plot_data <- bind_rows(
    extract_plot_data(model_stricter, support_stricter, min_period, max_period, "Moved to Stricter"),
    extract_plot_data(model_lenient, support_lenient, min_period, max_period, "Moved to More Lenient")
  ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested permit specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(
      group, event_time, estimate, std_error, ci_low, ci_high,
      estimate_display, ci_low_display, ci_high_display, display_unit,
      estimate_name, estimate_name_raw, is_reference,
      n_obs, n_treated, n_control, contributing_cohorts, n_fe_groups, n_blocks, n_segments, total_outcome,
      n_positive_rows, n_fe_group_time_cells, n_identifying_fe_group_time_cells,
      n_identifying_fe_groups, has_treated_and_control, has_identifying_support
    )
  pretrend <- bind_rows(
    compute_pretrend_test(model_stricter, plot_data %>% filter(group == "Moved to Stricter"), "Moved to Stricter"),
    compute_pretrend_test(model_lenient, plot_data %>% filter(group == "Moved to More Lenient"), "Moved to More Lenient")
  )
  support_by_event_time <- bind_rows(
    support_stricter %>% mutate(group = "Moved to Stricter"),
    support_lenient %>% mutate(group = "Moved to More Lenient")
  )
  directional_plots <- make_directional_plots(plot_data)

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")
  ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), directional_plots$combined, width = 7, height = 4.5, bg = "white")

  if (write_sidecars) {
    write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
    write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
    write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
    write_csv(metadata, sprintf("../output/event_study_metadata_%s.csv", suffix))
  }
} else {
  support_stricter <- make_support_table(
    df = data,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    block_var = block_var,
    segment_var = geo_group_var,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_stricter_continuous",
    min_period = min_period,
    max_period = max_period
  )
  support_lenient <- make_support_table(
    df = data,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = fe_group_var,
    block_var = block_var,
    segment_var = geo_group_var,
    outcome_var = base_outcome_var,
    treatment_var = "treatment_lenient_continuous",
    min_period = min_period,
    max_period = max_period
  )

  metadata <- metadata %>%
    mutate(
      stricter_analysis_n = nrow(data),
      lenient_analysis_n = nrow(data),
      plotted_supported_periods = paste(sort(unique(c(
        support_stricter$event_time[support_stricter$has_identifying_support],
        support_lenient$event_time[support_lenient$has_identifying_support]
      ))), collapse = "|")
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

  model_stricter <- run_model(formula_stricter, data)
  model_lenient <- run_model(formula_lenient, data)

  plot_data <- bind_rows(
    extract_plot_data(model_stricter, support_stricter, min_period, max_period, "Moved to Stricter"),
    extract_plot_data(model_lenient, support_lenient, min_period, max_period, "Moved to More Lenient")
  ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested permit specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(
      group, event_time, estimate, std_error, ci_low, ci_high,
      estimate_display, ci_low_display, ci_high_display, display_unit,
      estimate_name, estimate_name_raw, is_reference,
      n_obs, n_treated, n_control, contributing_cohorts, n_fe_groups, n_blocks, n_segments, total_outcome,
      n_positive_rows, n_fe_group_time_cells, n_identifying_fe_group_time_cells,
      n_identifying_fe_groups, has_treated_and_control, has_identifying_support
    )
  pretrend <- bind_rows(
    compute_pretrend_test(model_stricter, plot_data %>% filter(group == "Moved to Stricter"), "Moved to Stricter"),
    compute_pretrend_test(model_lenient, plot_data %>% filter(group == "Moved to More Lenient"), "Moved to More Lenient")
  )
  support_by_event_time <- bind_rows(
    support_stricter %>% mutate(group = "Moved to Stricter"),
    support_lenient %>% mutate(group = "Moved to More Lenient")
  )
  directional_plots <- make_directional_plots(plot_data)

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")
  ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), directional_plots$combined, width = 7, height = 4.5, bg = "white")

  if (write_sidecars) {
    write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
    write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
    write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
    write_csv(metadata, sprintf("../output/event_study_metadata_%s.csv", suffix))
  }
}

message("\nDone!")
