source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_permit_style/code")
# Rscript run_event_study_sales_permit_style.R block_year stacked_implementation main uniform
# =======================================================================================

dir.create("../output", showWarnings = FALSE, recursive = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (!length(cli_args) %in% c(3, 4)) {
  stop("FATAL: Script requires 3 or 4 args: <unit_mode> <panel_mode> <hedonics_spec> [<weighting_spec>]", call. = FALSE)
}

unit_mode <- cli_args[1]
panel_mode <- cli_args[2]
hedonics_spec <- cli_args[3]
weighting_spec <- if (length(cli_args) == 4) cli_args[4] else if (unit_mode == "block_year") "sales_count_weighted" else "uniform"

if (!unit_mode %in% c("block_year", "transaction")) {
  stop("--unit_mode must be one of: block_year, transaction", call. = FALSE)
}
if (!panel_mode %in% c("cohort_2012", "cohort_2015", "stacked_announcement", "stacked_implementation")) {
  stop("--panel_mode must be one of: cohort_2012, cohort_2015, stacked_announcement, stacked_implementation", call. = FALSE)
}
if (unit_mode == "block_year" && hedonics_spec != "main") {
  stop("--hedonics_spec must be main for block_year runs", call. = FALSE)
}
if (unit_mode == "transaction" && !hedonics_spec %in% c("no_hedonics", "with_hedonics")) {
  stop("--hedonics_spec must be one of: no_hedonics, with_hedonics for transaction runs", call. = FALSE)
}
if (unit_mode == "block_year" && !weighting_spec %in% c("sales_count_weighted", "uniform")) {
  stop("--weighting_spec must be one of: sales_count_weighted, uniform for block_year runs", call. = FALSE)
}
if (unit_mode == "transaction" && weighting_spec != "uniform") {
  stop("--weighting_spec must be uniform for transaction runs", call. = FALSE)
}

suffix <- sprintf("sales_permit_style_%s_%s_%s", unit_mode, panel_mode, hedonics_spec)
if (unit_mode == "block_year" && weighting_spec == "uniform") {
  suffix <- paste0(suffix, "_uniform")
}

make_support_table <- function(df, event_var, time_fe_var, fe_group_var, unit_var, outcome_var, treatment_var, min_period, max_period) {
  support_base <- df %>%
    filter(.data[[event_var]] >= min_period, .data[[event_var]] <= max_period)

  cell_support <- support_base %>%
    group_by(
      event_time = .data[[event_var]],
      fe_group = .data[[fe_group_var]],
      calendar_time = .data[[time_fe_var]]
    ) %>%
    summarise(
      n_units_cell = n_distinct(.data[[unit_var]]),
      n_treated = sum(treat == 1L, na.rm = TRUE),
      n_control = sum(treat == 0L, na.rm = TRUE),
      n_distinct_treatment_values = n_distinct(.data[[treatment_var]][!is.na(.data[[treatment_var]])]),
      has_within_cell_treatment_variation = n_distinct(.data[[treatment_var]][!is.na(.data[[treatment_var]])]) > 1L,
      .groups = "drop"
    )

  event_support <- support_base %>%
    group_by(event_time = .data[[event_var]]) %>%
    summarise(
      n_obs = n(),
      n_units = n_distinct(.data[[unit_var]]),
      n_fe_groups = n_distinct(.data[[fe_group_var]]),
      n_treated = sum(treat == 1L, na.rm = TRUE),
      n_control = sum(treat == 0L, na.rm = TRUE),
      total_outcome = sum(.data[[outcome_var]], na.rm = TRUE),
      n_positive_rows = sum(.data[[outcome_var]] > 0L, na.rm = TRUE),
      .groups = "drop"
    )

  identifying_support <- cell_support %>%
    group_by(event_time) %>%
    summarise(
      n_fe_group_time_cells = n(),
      n_identifying_fe_group_time_cells = sum(has_within_cell_treatment_variation, na.rm = TRUE),
      n_identifying_fe_groups = n_distinct(fe_group[has_within_cell_treatment_variation]),
      .groups = "drop"
    )

  event_support %>%
    left_join(identifying_support, by = "event_time") %>%
    mutate(
      n_fe_group_time_cells = replace_na(n_fe_group_time_cells, 0L),
      n_identifying_fe_group_time_cells = replace_na(n_identifying_fe_group_time_cells, 0L),
      n_identifying_fe_groups = replace_na(n_identifying_fe_groups, 0L),
      has_treated_and_control = n_treated > 0L & n_control > 0L,
      has_identifying_support = n_identifying_fe_group_time_cells > 0L
    ) %>%
    arrange(event_time)
}

extract_plot_data <- function(model, support_by_event_time, min_period, max_period) {
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
      is_reference = is_ref
    ) %>%
    filter(event_time >= min_period, event_time <= max_period) %>%
    filter(is_reference | event_time %in% supported_periods) %>%
    left_join(support_by_event_time, by = "event_time") %>%
    mutate(
      estimate_pct = 100 * (exp(estimate) - 1),
      ci_low_pct = 100 * (exp(ci_low) - 1),
      ci_high_pct = 100 * (exp(ci_high) - 1)
    )

  out
}

compute_pretrend_test <- function(model, plot_data) {
  lead_terms <- plot_data %>%
    filter(event_time <= -2, !is_reference) %>%
    pull(estimate_name_raw)

  if (length(lead_terms) == 0) {
    return(tibble(
      group = "All sales",
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
    group = "All sales",
    n_leads = length(lead_terms),
    min_lead = min(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    max_lead = max(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    wald_stat = if (is.null(joint_test)) NA_real_ else joint_test$stat,
    p_value = if (is.null(joint_test)) NA_real_ else joint_test$p,
    df1 = if (is.null(joint_test)) NA_real_ else joint_test$df1,
    df2 = if (is.null(joint_test)) NA_real_ else joint_test$df2
  )
}

make_plot <- function(plot_data, title_text) {
  ggplot(plot_data, aes(x = event_time, y = estimate_pct)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, color = "gray60", linetype = "dashed", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), fill = "#0f766e", alpha = 0.18, color = NA) +
    geom_line(color = "#0f766e", linewidth = 1) +
    geom_point(color = "#0f766e", size = 2.5) +
    scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
    scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    labs(
      title = title_text,
      x = "Years relative to redistricting",
      y = "Effect on home prices"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
    )
}

source("sales_permit_style_helpers.R")

if (unit_mode == "block_year") {
  raw_block_year <- read_csv(
    "../input/sales_block_year_panel.csv",
    col_types = cols(
      block_id = col_character(),
      ward_pre_2015 = col_character(),
      ward_pre_2023 = col_character(),
      ward = col_character(),
      ward_pair_id = col_character(),
      block_group_id = col_character()
    ),
    show_col_types = FALSE
  )

  if (panel_mode == "cohort_2012") {
    data <- prepare_block_year_cohort(raw_block_year, "2015", "2012", 3)
    block_var <- "block_id"
    fe_group_var <- "ward_pair_id"
    time_fe_var <- "year"
    event_var <- "relative_year"
    cluster_formula <- ~block_id
    fe_formula <- "block_id + ward_pair_id^year"
    title_text <- "Sales event study: permit-style block-year, 2012 announcement cohort"
  } else if (panel_mode == "cohort_2015") {
    data <- prepare_block_year_cohort(raw_block_year, "2015", "2015", 0)
    block_var <- "block_id"
    fe_group_var <- "ward_pair_id"
    time_fe_var <- "year"
    event_var <- "relative_year"
    cluster_formula <- ~block_id
    fe_formula <- "block_id + ward_pair_id^year"
    title_text <- "Sales event study: permit-style block-year, 2015 cohort"
  } else if (panel_mode == "stacked_announcement") {
    data <- bind_rows(
      prepare_block_year_cohort(raw_block_year, "2015", "2012", 3),
      prepare_block_year_cohort(raw_block_year, "2023", "2022", 1)
    )
    block_var <- "cohort_block_id"
    fe_group_var <- "cohort_ward_pair"
    time_fe_var <- "year"
    event_var <- "relative_year"
    cluster_formula <- ~cohort_block_id
    fe_formula <- "cohort_block_id + cohort_ward_pair^year"
    title_text <- "Sales event study: permit-style block-year, stacked announcement"
  } else {
    data <- bind_rows(
      prepare_block_year_cohort(raw_block_year, "2015", "2015", 0),
      prepare_block_year_cohort(raw_block_year, "2023", "2023", 0)
    )
    block_var <- "cohort_block_id"
    fe_group_var <- "cohort_ward_pair"
    time_fe_var <- "year"
    event_var <- "relative_year"
    cluster_formula <- ~cohort_block_id
    fe_formula <- "cohort_block_id + cohort_ward_pair^year"
    title_text <- "Sales event study: permit-style block-year, stacked implementation"
  }

  data <- data %>%
    filter(
      has_sales,
      !is.na(mean_price), mean_price > 0,
      !is.na(ward_pair_id), ward_pair_id != "",
      !is.na(mean_dist_to_boundary), mean_dist_to_boundary <= 1000,
      .data[[event_var]] >= -5, .data[[event_var]] <= 5
    ) %>%
    mutate(
      weight = if (weighting_spec == "uniform") 1 else as.numeric(n_sales),
      outcome_estimation = log(mean_price)
    )

  outcome_var <- "mean_price"
  hedonics_included <- FALSE
} else {
  panel_input <- switch(panel_mode,
    "cohort_2012" = "../input/sales_transaction_panel_2012.parquet",
    "cohort_2015" = "../input/sales_transaction_panel_2015.parquet",
    "stacked_announcement" = "../input/sales_transaction_panel_announcement.parquet",
    "stacked_implementation" = "../input/sales_transaction_panel.parquet"
  )

  data <- read_parquet(panel_input) %>%
    as_tibble() %>%
    mutate(across(any_of(c("cohort", "block_id", "cohort_block_id", "ward_pair_id")), as.character))

  if (panel_mode %in% c("cohort_2012", "cohort_2015")) {
    block_var <- "block_id"
    fe_group_var <- "ward_pair_id"
    time_fe_var <- "sale_year"
    event_var <- "relative_year_capped"
    cluster_formula <- ~block_id
    fe_formula <- "block_id + ward_pair_id^sale_year"
    title_text <- paste(
      sprintf("Sales event study: permit-style transactions, %s cohort", ifelse(panel_mode == "cohort_2012", "2012 announcement", "2015")),
      if (hedonics_spec == "with_hedonics") "with hedonics" else "no hedonics"
    )
  } else {
    data <- data %>%
      mutate(cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_"))
    block_var <- "cohort_block_id"
    fe_group_var <- "cohort_ward_pair"
    time_fe_var <- "sale_year"
    event_var <- "relative_year_capped"
    cluster_formula <- ~cohort_block_id
    fe_formula <- "cohort_block_id + cohort_ward_pair^sale_year"
    title_text <- paste(
      sprintf(
        "Sales event study: permit-style transactions, %s",
        ifelse(panel_mode == "stacked_announcement", "stacked announcement", "stacked implementation")
      ),
      if (hedonics_spec == "with_hedonics") "with hedonics" else "no hedonics"
    )
  }

  data <- data %>%
    filter(
      !is.na(sale_price), sale_price > 0,
      !is.na(dist_ft), dist_ft <= 1000,
      !is.na(ward_pair_id), ward_pair_id != "",
      !is.na(.data[[event_var]]), .data[[event_var]] >= -5, .data[[event_var]] <= 5
    ) %>%
    mutate(
      weight = 1,
      outcome_estimation = log(sale_price)
    )

  hedonics_included <- hedonics_spec == "with_hedonics"
  if (hedonics_included) {
    data <- data %>%
      filter(if_all(
        all_of(c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")),
        ~ !is.na(.x)
      ))
  }

  outcome_var <- "sale_price"
}

if (nrow(data) == 0) {
  stop("No observations remain after applying the requested sales permit-style sample restrictions.", call. = FALSE)
}

support_by_event_time <- make_support_table(
  df = data,
  event_var = event_var,
  time_fe_var = time_fe_var,
  fe_group_var = fe_group_var,
  unit_var = block_var,
  outcome_var = outcome_var,
  treatment_var = "strictness_change",
  min_period = -5,
  max_period = 5
)

rhs_terms <- c(sprintf("i(%s, strictness_change, ref = -1)", event_var))
if (unit_mode == "transaction" && hedonics_included) {
  rhs_terms <- c(
    rhs_terms,
    "log_sqft",
    "log_land_sqft",
    "log_building_age",
    "log_bedrooms",
    "log_baths",
    "has_garage"
  )
}

formula_str <- sprintf(
  "outcome_estimation ~ %s | %s",
  paste(rhs_terms, collapse = " + "),
  fe_formula
)

message(sprintf("Running sales permit-style spec: %s", suffix))
message(sprintf("Formula: %s", formula_str))

model <- feols(
  as.formula(formula_str),
  data = data,
  weights = ~weight,
  cluster = cluster_formula
)

plot_data <- extract_plot_data(model, support_by_event_time, -5, 5)
if (is.null(plot_data) || nrow(plot_data) == 0) {
  stop("No supported coefficients were available for the requested sales permit-style specification.", call. = FALSE)
}

pretrend <- compute_pretrend_test(model, plot_data)

metadata <- tibble(
  spec_id = suffix,
  unit_mode = unit_mode,
  panel_mode = panel_mode,
  hedonics_spec = hedonics_spec,
  include_hedonics = hedonics_included,
  weighting = if (unit_mode == "block_year") weighting_spec else "uniform",
  bandwidth = 1000L,
  geo_fe_level = "ward_pair",
  cluster_level = "block",
  event_window_min = -5L,
  event_window_max = 5L,
  raw_n = nrow(data),
  analysis_n = nrow(data),
  analysis_units = n_distinct(data[[block_var]]),
  analysis_fe_groups = n_distinct(data[[fe_group_var]]),
  treated_rows = sum(data$treat == 1L, na.rm = TRUE),
  control_rows = sum(data$treat == 0L, na.rm = TRUE),
  effective_weight_n = sum(data$weight, na.rm = TRUE),
  total_outcome = sum(data[[outcome_var]], na.rm = TRUE),
  plotted_supported_periods = paste(plot_data$event_time[!plot_data$is_reference], collapse = "|"),
  formula = formula_str
)

ggsave(sprintf("../output/event_study_%s.pdf", suffix), make_plot(plot_data, title_text), width = 7, height = 4.5, bg = "white")
write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
write_csv(metadata, sprintf("../output/event_study_metadata_%s.csv", suffix))

message("Done!")
