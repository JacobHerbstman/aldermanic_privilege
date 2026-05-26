# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_disaggregate/code")
# panel_mode <- "cohort_2023"
# treatment_type <- "continuous"
# include_controls <- TRUE

source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_mode, treatment_type, include_controls)
}

if (length(cli_args) != 3) {
  stop("FATAL: Script requires args: <panel_mode> <treatment_type> <include_controls>", call. = FALSE)
}

panel_mode <- cli_args[1]
treatment_type <- cli_args[2]
include_controls_arg <- tolower(cli_args[3])

if (!panel_mode %in% c("cohort_2015", "cohort_2023", "stacked_implementation")) {
  stop("panel_mode must be cohort_2015, cohort_2023, or stacked_implementation.", call. = FALSE)
}
if (!treatment_type %in% c("continuous", "continuous_split")) {
  stop("treatment_type must be continuous or continuous_split.", call. = FALSE)
}
if (!include_controls_arg %in% c("true", "t", "1", "yes", "false", "f", "0", "no")) {
  stop("include_controls must be TRUE or FALSE.", call. = FALSE)
}

include_controls <- include_controls_arg %in% c("true", "t", "1", "yes")
post_window <- if (panel_mode == "stacked_implementation") "overlap" else "short"
bandwidth <- 250

panel_title <- switch(
  panel_mode,
  "stacked_implementation" = "2015 + 2023 implementation cohorts (stacked)",
  "cohort_2015" = "2015 implementation cohort",
  "cohort_2023" = "2023 implementation cohort"
)

panel_input <- switch(
  panel_mode,
  "stacked_implementation" = "../input/rental_listing_panel.parquet",
  "cohort_2015" = "../input/rental_listing_panel_2015.parquet",
  "cohort_2023" = "../input/rental_listing_panel_2023.parquet"
)

hedonic_suffix <- if (include_controls) "" else "_no_hedonics"
suffix <- sprintf(
  "disaggregate_yearly_%s_%s_triangular_250m_mf%s_%s",
  panel_mode,
  treatment_type,
  hedonic_suffix,
  post_window
)

data <- read_parquet(panel_input) %>%
  filter(
    !is.na(strictness_change),
    !is.na(rent_price),
    rent_price > 0,
    building_type_clean == "multi_family",
    dist_m <= bandwidth
  ) %>%
  mutate(
    weight = pmax(0, 1 - dist_m / bandwidth),
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0)
  )

if (include_controls) {
  data <- data[complete.cases(data[, c("log_sqft", "log_beds", "log_baths", "building_type_clean")]), ]
}

if (panel_mode == "stacked_implementation") {
  required_segment_cols <- c("segment_id_cohort", "segment_side", "cohort_segment", "cohort_segment_side")
} else {
  required_segment_cols <- c("segment_id_cohort", "segment_side")
}
missing_cols <- setdiff(required_segment_cols, names(data))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required segment columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

data <- data %>%
  filter(!is.na(segment_id_cohort), segment_id_cohort != "")

if (panel_mode == "stacked_implementation") {
  data <- data %>%
    filter(!is.na(cohort_segment), cohort_segment != "", !is.na(cohort_segment_side), cohort_segment_side != "")
  fe_side_var <- "cohort_segment_side"
  fe_group_var <- "cohort_segment"
  segment_var <- "cohort_segment"
  cluster_formula <- ~cohort_block_id + cohort_segment
} else {
  data <- data %>%
    filter(!is.na(segment_side), segment_side != "")
  fe_side_var <- "segment_side"
  fe_group_var <- "segment_id_cohort"
  segment_var <- "segment_id_cohort"
  cluster_formula <- ~block_id + segment_id_cohort
}

min_period <- if (post_window == "overlap") -1 else -5
max_period <- 2
if (post_window == "overlap") {
  data <- data %>%
    filter(relative_year_capped >= min_period, relative_year_capped <= max_period)
}

support_by_event_time <- build_event_study_support_table(
  data,
  event_var = "relative_year_capped",
  time_fe_var = "year",
  fe_group_var = fe_group_var,
  min_period = min_period,
  max_period = max_period,
  support_mode = "two_sided_cells",
  cohort_label = panel_mode,
  side_var = fe_side_var,
  block_var = "block_id",
  segment_var = segment_var
)

hedonic_formula <- if (include_controls) {
  "+ building_type_factor + log_sqft + log_beds + log_baths"
} else {
  ""
}
fe_formula <- sprintf("%s + %s^year", fe_side_var, fe_group_var)

if (treatment_type == "continuous") {
  model <- feols(
    as.formula(sprintf(
      "log(rent_price) ~ i(relative_year_capped, strictness_change, ref = -1) %s | %s",
      hedonic_formula,
      fe_formula
    )),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )
  plot_data <- build_event_study_plot_data(
    model,
    support_by_event_time,
    min_period,
    max_period,
    "All listings",
    "multiply100"
  )
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested rental specification.", call. = FALSE)
  }
  ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = sprintf("Rental event study: %s", panel_title),
      x_label = "Years relative to alderman switch",
      y_label = "Effect on rents",
      display_suffix = "%"
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )
} else {
  model_stricter <- feols(
    as.formula(sprintf(
      "log(rent_price) ~ i(relative_year_capped, treatment_stricter_continuous, ref = -1) %s | %s",
      hedonic_formula,
      fe_formula
    )),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )
  model_lenient <- feols(
    as.formula(sprintf(
      "log(rent_price) ~ i(relative_year_capped, treatment_lenient_continuous, ref = -1) %s | %s",
      hedonic_formula,
      fe_formula
    )),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )
  plot_data <- bind_rows(
    build_event_study_plot_data(model_stricter, support_by_event_time, min_period, max_period, "Moved to Stricter", "multiply100"),
    build_event_study_plot_data(model_lenient, support_by_event_time, min_period, max_period, "Moved to More Lenient", "multiply100")
  ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested rental specification.", call. = FALSE)
  }

  directional_plots <- make_event_study_directional_plots(
    plot_data,
    plot_title = sprintf("Rental event study: %s", panel_title),
    x_label = "Years relative to alderman switch",
    y_label = "Effect on rents",
    display_suffix = "%"
  )

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")
  ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), directional_plots$combined, width = 7, height = 4.5, bg = "white")
}
