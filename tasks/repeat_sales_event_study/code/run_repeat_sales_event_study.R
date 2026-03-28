source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/repeat_sales_event_study/code")
# panel_mode <- "cohort_2015"
# approach <- "bw1000"
# treatment_mode <- "continuous"
# input_panel <- "../output/repeat_sales_panel_cohort_2015_bw1000.parquet"
# plot_pdf <- "../output/repeat_sales_event_study_cohort_2015_bw1000_continuous.pdf"
# combined_pdf <- "../output/repeat_sales_event_study_combined_cohort_2015_bw1000_continuous.pdf"
# coefficients_csv <- "../output/repeat_sales_coefficients_cohort_2015_bw1000_continuous.csv"
# support_csv <- "../output/repeat_sales_support_cohort_2015_bw1000_continuous.csv"
# pretrend_csv <- "../output/repeat_sales_pretrend_cohort_2015_bw1000_continuous.csv"
# metadata_csv <- "../output/repeat_sales_metadata_cohort_2015_bw1000_continuous.csv"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(panel_mode, approach, treatment_mode, input_panel, plot_pdf, combined_pdf, coefficients_csv, support_csv, pretrend_csv, metadata_csv)
}

if (length(args) != 10) {
  stop(
    "Usage: Rscript run_repeat_sales_event_study.R <panel_mode> <approach> <treatment_mode> <input_panel> <plot_pdf> <combined_pdf> <coefficients_csv> <support_csv> <pretrend_csv> <metadata_csv>",
    call. = FALSE
  )
}

panel_mode <- args[1]
approach <- args[2]
treatment_mode <- args[3]
input_panel <- args[4]
plot_pdf <- args[5]
combined_pdf <- args[6]
coefficients_csv <- args[7]
support_csv <- args[8]
pretrend_csv <- args[9]
metadata_csv <- args[10]

valid_panel_modes <- c(
  "cohort_2012",
  "cohort_2015",
  "cohort_2022",
  "cohort_2023",
  "stacked_announcement",
  "stacked_implementation"
)
valid_approaches <- c("bw1000", "corridor1320", "citywide_valid")
valid_treatments <- c("continuous", "continuous_split")

if (!panel_mode %in% valid_panel_modes) {
  stop(sprintf("panel_mode must be one of: %s", paste(valid_panel_modes, collapse = ", ")), call. = FALSE)
}
if (!approach %in% valid_approaches) {
  stop(sprintf("approach must be one of: %s", paste(valid_approaches, collapse = ", ")), call. = FALSE)
}
if (!treatment_mode %in% valid_treatments) {
  stop(sprintf("treatment_mode must be one of: %s", paste(valid_treatments, collapse = ", ")), call. = FALSE)
}

panel_title <- switch(panel_mode,
  cohort_2012 = "2012 announcement cohort",
  cohort_2015 = "2015 implementation cohort",
  cohort_2022 = "2022 announcement cohort",
  cohort_2023 = "2023 implementation cohort",
  stacked_announcement = "2012 + 2022 announcement cohorts",
  stacked_implementation = "2015 + 2023 implementation cohorts"
)

approach_title <- switch(approach,
  bw1000 = "1000 ft border sample",
  corridor1320 = "1320 ft border corridor sample",
  citywide_valid = "Citywide valid repeat-sales sample"
)

event_year_label <- switch(panel_mode,
  cohort_2012 = "2012",
  cohort_2015 = "2015",
  cohort_2022 = "2022",
  cohort_2023 = "2023",
  stacked_announcement = "2012|2022",
  stacked_implementation = "2015|2023"
)

window_lookup <- list(
  cohort_2012 = c(-5L, 5L),
  cohort_2015 = c(-5L, 5L),
  cohort_2022 = c(-5L, 3L),
  cohort_2023 = c(-5L, 2L),
  stacked_announcement = c(-5L, 3L),
  stacked_implementation = c(-5L, 2L)
)
min_period <- window_lookup[[panel_mode]][1]
max_period <- window_lookup[[panel_mode]][2]

make_support_table <- function(df, event_var, time_var, geography_var, property_var) {
  support_base <- df %>%
    filter(.data[[event_var]] >= min_period, .data[[event_var]] <= max_period)

  cell_support <- support_base %>%
    group_by(
      event_time = .data[[event_var]],
      geography = .data[[geography_var]],
      calendar_time = .data[[time_var]]
    ) %>%
    summarise(
      n_obs = n(),
      n_treated = sum(treat == 1, na.rm = TRUE),
      n_control = sum(treat == 0, na.rm = TRUE),
      n_properties = n_distinct(.data[[property_var]]),
      .groups = "drop"
    )

  event_support <- support_base %>%
    group_by(event_time = .data[[event_var]]) %>%
    summarise(
      n_obs = n(),
      n_treated = sum(treat == 1, na.rm = TRUE),
      n_control = sum(treat == 0, na.rm = TRUE),
      contributing_cohorts = paste(sort(unique(cohort)), collapse = "|"),
      n_geographies = n_distinct(.data[[geography_var]]),
      n_blocks = n_distinct(block_id),
      n_properties = n_distinct(.data[[property_var]]),
      .groups = "drop"
    )

  cell_event_support <- cell_support %>%
    group_by(event_time) %>%
    summarise(
      n_geography_year_cells = n(),
      n_identifying_geography_year_cells = sum(n_treated > 0 & n_control > 0),
      n_identifying_geographies = n_distinct(geography[n_treated > 0 & n_control > 0]),
      .groups = "drop"
    )

  event_support %>%
    left_join(cell_event_support, by = "event_time") %>%
    mutate(
      n_geography_year_cells = replace_na(n_geography_year_cells, 0L),
      n_identifying_geography_year_cells = replace_na(n_identifying_geography_year_cells, 0L),
      n_identifying_geographies = replace_na(n_identifying_geographies, 0L),
      has_treated_and_control = n_treated > 0 & n_control > 0,
      has_identifying_support = n_identifying_geography_year_cells > 0
    ) %>%
    arrange(event_time)
}

extract_plot_data <- function(model, support_by_event_time, group_label) {
  iplot_data <- tryCatch(iplot(model, .plot = FALSE)[[1]], error = function(e) NULL)
  if (is.null(iplot_data) || nrow(iplot_data) == 0) {
    return(NULL)
  }

  supported_periods <- support_by_event_time %>%
    filter(has_identifying_support) %>%
    pull(event_time)

  iplot_data %>%
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
    left_join(support_by_event_time, by = "event_time") %>%
    mutate(
      estimate_pct = estimate * 100,
      ci_low_pct = ci_low * 100,
      ci_high_pct = ci_high * 100
    )
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

make_single_series_plot <- function(plot_data) {
  ggplot(plot_data, aes(x = event_time, y = estimate_pct)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), fill = "#1b9e77", alpha = 0.2, color = NA) +
    geom_line(color = "#1b9e77", linewidth = 1) +
    geom_point(color = "#1b9e77", size = 2.5) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
    labs(
      title = sprintf("Repeat sales event study: %s", panel_title),
      subtitle = approach_title,
      x = "Years relative to redistricting",
      y = "Effect on home prices"
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
      plot.title = element_text(face = "bold"),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )
}

make_directional_plots <- function(plot_data) {
  color_values <- c(
    "Moved to Stricter" = "#c23616",
    "Moved to More Lenient" = "#7f8fa6"
  )

  facet_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_pct, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
    facet_wrap(~group, ncol = 1) +
    labs(
      title = sprintf("Repeat sales event study: %s", panel_title),
      subtitle = approach_title,
      x = "Years relative to redistricting",
      y = "Effect on home prices"
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
      plot.title = element_text(face = "bold"),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )

  combined_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_pct, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
    labs(
      title = sprintf("Repeat sales event study: %s", panel_title),
      subtitle = approach_title,
      x = "Years relative to redistricting",
      y = "Effect on home prices"
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
      plot.title = element_text(face = "bold"),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )

  list(facet = facet_plot, combined = combined_plot)
}

run_model <- function(formula_str, data, cluster_formula) {
  message(sprintf("Running %s, %s, %s with %s observations", panel_mode, approach, treatment_mode, format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_str))
  feols(as.formula(formula_str), data = data, cluster = cluster_formula)
}

data <- read_parquet(input_panel) %>%
  as_tibble()

if (!"cohort_pin_id" %in% names(data)) {
  data$cohort_pin_id <- NA_character_
}
if (!"cohort_segment" %in% names(data)) {
  data$cohort_segment <- NA_character_
}
if (!"cohort_ward_origin" %in% names(data)) {
  data$cohort_ward_origin <- NA_character_
}

data <- data %>%
  mutate(
    pin = as.character(pin),
    block_id = as.character(block_id),
    cohort = as.character(cohort),
    sale_year = as.integer(sale_year),
    relative_year = as.integer(relative_year),
    relative_year_capped = as.integer(relative_year_capped),
    ward_origin = as.character(ward_origin),
    segment_id_cohort = as.character(segment_id_cohort),
    cohort_pin_id = as.character(cohort_pin_id),
    cohort_segment = as.character(cohort_segment),
    cohort_ward_origin = as.character(cohort_ward_origin),
    treat = as.integer(treat),
    strictness_change = as.numeric(strictness_change),
    sale_price = as.numeric(sale_price)
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    !is.na(strictness_change),
    !is.na(treat)
  ) %>%
  mutate(
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0)
  )

stacked_mode <- grepl("^stacked_", panel_mode)
property_var <- if (stacked_mode) "cohort_pin_id" else "pin"
geography_var <- case_when(
  approach == "citywide_valid" && stacked_mode ~ "cohort_ward_origin",
  approach == "citywide_valid" ~ "ward_origin",
  approach %in% c("bw1000", "corridor1320") && stacked_mode ~ "cohort_segment",
  TRUE ~ "segment_id_cohort"
)

data <- data %>%
  filter(
    !is.na(.data[[property_var]]),
    .data[[property_var]] != "",
    !is.na(.data[[geography_var]]),
    .data[[geography_var]] != ""
  ) %>%
  filter(relative_year >= min_period, relative_year <= max_period) %>%
  mutate(relative_year_capped = relative_year)

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested repeat-sales sample restrictions.", call. = FALSE)
}

support_by_event_time <- make_support_table(
  df = data,
  event_var = "relative_year_capped",
  time_var = "sale_year",
  geography_var = geography_var,
  property_var = property_var
)

analysis_n <- nrow(data)
n_repeat_pins <- n_distinct(data[[property_var]])
n_treated_repeat_pins <- data %>%
  distinct(.data[[property_var]], treat) %>%
  filter(treat == 1) %>%
  nrow()
n_control_repeat_pins <- data %>%
  distinct(.data[[property_var]], treat) %>%
  filter(treat == 0) %>%
  nrow()

cluster_formula <- as.formula(sprintf("~%s + %s", property_var, geography_var))
fe_formula <- sprintf("%s + %s^sale_year", property_var, geography_var)

metadata <- tibble(
  panel_mode = panel_mode,
  panel_title = panel_title,
  approach = approach,
  approach_title = approach_title,
  treatment_mode = treatment_mode,
  weighting = "uniform",
  include_hedonics = FALSE,
  event_year_label = event_year_label,
  plotted_min_event_time = min_period,
  plotted_max_event_time = max_period,
  plotted_supported_periods = paste(support_by_event_time$event_time[support_by_event_time$has_identifying_support], collapse = "|"),
  analysis_n = analysis_n,
  n_repeat_pins = n_repeat_pins,
  n_treated_repeat_pins = n_treated_repeat_pins,
  n_control_repeat_pins = n_control_repeat_pins,
  n_pin_year_obs = analysis_n,
  n_identifying_geography_year_cells_total = sum(support_by_event_time$n_identifying_geography_year_cells),
  n_identifying_geographies_total = max(support_by_event_time$n_identifying_geographies),
  has_any_identifying_support = any(support_by_event_time$has_identifying_support)
)

if (!metadata$has_any_identifying_support[1]) {
  stop("No identifying support remains after applying the requested repeat-sales specification.", call. = FALSE)
}

if (treatment_mode == "continuous") {
  formula_str <- sprintf(
    "log(sale_price) ~ i(relative_year_capped, strictness_change, ref = -1) | %s",
    fe_formula
  )
  model <- run_model(formula_str, data, cluster_formula)
  plot_data <- extract_plot_data(model, support_by_event_time, "All repeat sales")

  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested repeat-sales specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(
      group,
      event_time,
      estimate,
      std_error,
      ci_low,
      ci_high,
      estimate_pct,
      ci_low_pct,
      ci_high_pct,
      estimate_name,
      estimate_name_raw,
      is_reference,
      n_obs,
      n_treated,
      n_control,
      contributing_cohorts,
      n_geographies,
      n_blocks,
      n_properties,
      n_geography_year_cells,
      n_identifying_geography_year_cells,
      n_identifying_geographies,
      has_treated_and_control,
      has_identifying_support
    )

  pretrend <- compute_pretrend_test(model, plot_data, "All repeat sales")

  ggsave(plot_pdf, make_single_series_plot(plot_data), width = 7, height = 4.5, bg = "white")
  write_csv(coefficients, coefficients_csv)
  write_csv(support_by_event_time, support_csv)
  write_csv(pretrend, pretrend_csv)
  write_csv(metadata, metadata_csv)
} else {
  formula_stricter <- sprintf(
    "log(sale_price) ~ i(relative_year_capped, treatment_stricter_continuous, ref = -1) | %s",
    fe_formula
  )
  formula_lenient <- sprintf(
    "log(sale_price) ~ i(relative_year_capped, treatment_lenient_continuous, ref = -1) | %s",
    fe_formula
  )

  model_stricter <- run_model(formula_stricter, data, cluster_formula)
  model_lenient <- run_model(formula_lenient, data, cluster_formula)

  plot_data <- bind_rows(
    extract_plot_data(model_stricter, support_by_event_time, "Moved to Stricter"),
    extract_plot_data(model_lenient, support_by_event_time, "Moved to More Lenient")
  ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested repeat-sales specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(
      group,
      event_time,
      estimate,
      std_error,
      ci_low,
      ci_high,
      estimate_pct,
      ci_low_pct,
      ci_high_pct,
      estimate_name,
      estimate_name_raw,
      is_reference,
      n_obs,
      n_treated,
      n_control,
      contributing_cohorts,
      n_geographies,
      n_blocks,
      n_properties,
      n_geography_year_cells,
      n_identifying_geography_year_cells,
      n_identifying_geographies,
      has_treated_and_control,
      has_identifying_support
    )

  pretrend <- bind_rows(
    compute_pretrend_test(model_stricter, plot_data %>% filter(group == "Moved to Stricter"), "Moved to Stricter"),
    compute_pretrend_test(model_lenient, plot_data %>% filter(group == "Moved to More Lenient"), "Moved to More Lenient")
  )

  directional_plots <- make_directional_plots(plot_data)
  ggsave(plot_pdf, directional_plots$facet, width = 7, height = 6, bg = "white")
  ggsave(combined_pdf, directional_plots$combined, width = 7, height = 4.5, bg = "white")
  write_csv(coefficients, coefficients_csv)
  write_csv(support_by_event_time, support_csv)
  write_csv(pretrend, pretrend_csv)
  write_csv(metadata, metadata_csv)
}
