# --- Interactive Test Block ---
# setwd("tasks/audits/run_event_study_sales_disaggregate/code")
# panel_mode <- "cohort_2015"
# treatment_type <- "continuous"
# bandwidth <- 304.8
# bandwidth_label <- "1000ft"

source("../../../setup_environment/code/packages.R")
source("../../../_lib/event_study_plot_helpers.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(
    panel_mode,
    treatment_type,
    bandwidth,
    bandwidth_label
  )
}

if (length(cli_args) != 4) {
  stop("FATAL: Script requires args: <panel_mode> <treatment_type> <bandwidth> <bandwidth_label>", call. = FALSE)
}

panel_mode <- cli_args[1]
treatment_type <- cli_args[2]
bandwidth <- as.numeric(cli_args[3])
bandwidth_label <- cli_args[4]

if (!panel_mode %in% c("cohort_2015", "stacked_implementation")) {
  stop("panel_mode must be cohort_2015 or stacked_implementation.", call. = FALSE)
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

time_unit <- "yearly"
weighting <- "uniform"
post_window <- "full"

panel_title <- switch(
  panel_mode,
  "cohort_2015" = "2015 implementation cohort",
  "stacked_implementation" = "2015 + 2023 implementation cohorts (stacked)"
)
panel_input <- switch(
  panel_mode,
  "cohort_2015" = "../input/sales_transaction_panel_2015.parquet",
  "stacked_implementation" = "../input/sales_transaction_panel.parquet"
)
suffix <- sprintf(
  "disaggregate_%s_%s_%s_%s_%s_amenity_%s_geo_wardpair_clust_block",
  time_unit,
  panel_mode,
  treatment_type,
  weighting,
  bandwidth_label,
  post_window
)

data <- read_parquet(panel_input) %>%
  as_tibble() %>%
  filter(!is.na(sale_price), sale_price > 0, dist_m <= bandwidth)

if (sum(is.na(data$strictness_change)) > 0L) {
  stop("Requested sales event-study regression sample has missing score values.", call. = FALSE)
}

data <- data %>%
  mutate(
    weight = 1,
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0)
  ) %>%
  filter(
    is.finite(log_sqft),
    is.finite(log_land_sqft),
    is.finite(log_building_age),
    is.finite(log_bedrooms),
    is.finite(log_baths),
    !is.na(has_garage),
    is.finite(nearest_school_dist_m),
    is.finite(nearest_park_dist_m),
    is.finite(nearest_major_road_dist_m),
    is.finite(lake_michigan_dist_m)
  ) %>%
  mutate(relative_period = relative_year_capped)

if (panel_mode == "stacked_implementation") {
  data <- data %>%
    mutate(
      ward_pair_side_temp = sub("^[0-9]+_", "", cohort_ward_pair_side),
      ward_pair = sub("_[0-9]+$", "", ward_pair_side_temp),
      cohort_ward_pair = paste(cohort, ward_pair, sep = "_")
    )
  fe_side_var <- "cohort_ward_pair_side"
  fe_group_var <- "cohort_ward_pair"
  cluster_formula <- ~cohort_block_id
  block_var <- "cohort_block_id"
} else {
  data <- data %>%
    mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side))
  fe_side_var <- "ward_pair_side"
  fe_group_var <- "ward_pair"
  cluster_formula <- ~block_id
  block_var <- "block_id"
}

min_period <- -5
max_period <- 5
control_formula <- paste(
  "+ log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths +",
  "has_garage + nearest_school_dist_m + nearest_park_dist_m +",
  "nearest_major_road_dist_m + lake_michigan_dist_m"
)
fe_formula <- sprintf("%s + %s^sale_year", fe_side_var, fe_group_var)
support_by_event_time <- build_event_study_support_table(
  data,
  event_var = "relative_period",
  time_fe_var = "sale_year",
  fe_group_var = fe_group_var,
  min_period = min_period,
  max_period = max_period,
  support_mode = "two_sided_cells",
  cohort_label = panel_mode,
  side_var = fe_side_var,
  block_var = block_var,
  pin_var = "pin"
)

if (treatment_type == "continuous") {
  model <- feols(
    as.formula(sprintf(
      "log(sale_price) ~ i(relative_period, strictness_change, ref = -1) %s | %s",
      control_formula,
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
    "All sales",
    "multiply100"
  )
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested sales specification.", call. = FALSE)
  }
  ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = sprintf("Sales event study: %s", panel_title),
      x_label = "Years relative to redistricting",
      y_label = "Effect on home prices",
      display_suffix = "%"
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )
} else {
  model_stricter <- feols(
    as.formula(sprintf(
      "log(sale_price) ~ i(relative_period, treatment_stricter_continuous, ref = -1) %s | %s",
      control_formula,
      fe_formula
    )),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )
  model_lenient <- feols(
    as.formula(sprintf(
      "log(sale_price) ~ i(relative_period, treatment_lenient_continuous, ref = -1) %s | %s",
      control_formula,
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
    stop("No supported coefficients were available for the requested sales specification.", call. = FALSE)
  }
  ggsave(
    sprintf("../output/event_study_combined_%s.pdf", suffix),
    make_event_study_directional_plots(
      plot_data,
      plot_title = sprintf("Sales event study: %s", panel_title),
      x_label = "Years relative to redistricting",
      y_label = "Effect on home prices",
      display_suffix = "%"
    )$combined,
    width = 7,
    height = 4.5,
    bg = "white"
  )
}
