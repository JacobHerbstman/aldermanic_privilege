source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_sales_disaggregate/code")
# panel_mode <- "cohort_2015"
# treatment_type <- "continuous"
# include_hedonics <- TRUE
# control_mode <- "amenity"
# time_unit <- "yearly"
# fe_type <- "strict_pair_x_year"
# weighting <- "uniform"
# bandwidth <- 300
# post_window <- "full"
# geo_fe_level <- "ward_pair"
# cluster_level <- "block"
# bandwidth_label <- "300m"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_mode, treatment_type, include_hedonics, time_unit, fe_type, weighting, bandwidth, post_window, geo_fe_level, cluster_level, control_mode, bandwidth_label)
}

if (!length(cli_args) %in% c(10, 11, 12)) {
  stop("FATAL: Script requires args: <panel_mode> <treatment_type> <include_hedonics> <time_unit> <fe_type> <weighting> <bandwidth> <post_window> <geo_fe_level> <cluster_level> [<control_mode>] [<bandwidth_label>]", call. = FALSE)
}
panel_mode <- cli_args[1]
treatment_type <- cli_args[2]
include_hedonics <- tolower(cli_args[3]) %in% c("true", "t", "1", "yes")
time_unit <- cli_args[4]
fe_type <- cli_args[5]
weighting <- cli_args[6]
bandwidth <- as.numeric(cli_args[7])
post_window <- cli_args[8]
geo_fe_level <- tolower(cli_args[9])
cluster_level <- tolower(cli_args[10])
control_mode <- if (length(cli_args) >= 11) tolower(cli_args[11]) else if (include_hedonics) "hedonic" else "none"
bandwidth_label <- if (length(cli_args) >= 12) cli_args[12] else sprintf("%dm", as.integer(round(bandwidth)))

PANEL_MODE <- panel_mode
TREATMENT_TYPE <- treatment_type
INCLUDE_HEDONICS <- include_hedonics
CONTROL_MODE <- control_mode
TIME_UNIT <- time_unit
FE_TYPE <- fe_type
WEIGHTING <- weighting
BANDWIDTH <- bandwidth
BANDWIDTH_LABEL <- bandwidth_label
POST_WINDOW <- post_window
GEO_FE_LEVEL <- geo_fe_level
CLUSTER_LEVEL <- cluster_level
WRITE_SIDECARS <- tolower(Sys.getenv("WRITE_SIDECARS", "0")) %in% c("1", "true", "yes")
SIDECAR_OUTPUT_DIR <- Sys.getenv("SIDECAR_OUTPUT_DIR", "../output")
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

valid_panel_modes <- c(
  "stacked_announcement",
  "stacked_implementation",
  "cohort_2012",
  "cohort_2015",
  "cohort_2022",
  "cohort_2023"
)
if (!PANEL_MODE %in% valid_panel_modes) {
  stop(sprintf("--panel_mode must be one of: %s", paste(valid_panel_modes, collapse = ", ")), call. = FALSE)
}
if (!TREATMENT_TYPE %in% c("continuous", "continuous_split", "binary_direction")) {
  stop("--treatment_type must be one of: continuous, continuous_split, binary_direction", call. = FALSE)
}
if (!TIME_UNIT %in% c("yearly", "quarterly")) {
  stop("--time_unit must be one of: yearly, quarterly", call. = FALSE)
}
if (!FE_TYPE %in% c("strict_pair_x_year", "pair_trend_plus_year", "side_plus_year")) {
  stop("--fe_type must be one of: strict_pair_x_year, pair_trend_plus_year, side_plus_year", call. = FALSE)
}
if (!WEIGHTING %in% c("uniform", "triangular")) {
  stop("--weighting must be one of: uniform, triangular", call. = FALSE)
}
if (!POST_WINDOW %in% c("short", "full")) {
  stop("--post_window must be one of: short, full", call. = FALSE)
}
if (!GEO_FE_LEVEL %in% c("segment", "ward_pair")) {
  stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!CLUSTER_LEVEL %in% c("twoway_block_segment", "block", "segment")) {
  stop("--cluster_level must be one of: twoway_block_segment, block, segment", call. = FALSE)
}
if (!CONTROL_MODE %in% c("none", "hedonic", "amenity")) {
  stop("--control_mode must be one of: none, hedonic, amenity", call. = FALSE)
}
if (!grepl("^[A-Za-z0-9_-]+$", BANDWIDTH_LABEL)) {
  stop("--bandwidth_label may only contain letters, numbers, underscores, and hyphens.", call. = FALSE)
}
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 800) {
  stop("Segment FE requested with bandwidth > 800m. Use bandwidth <= 800m.", call. = FALSE)
}

panel_title <- switch(PANEL_MODE,
  "stacked_announcement" = "2012 + 2022 announcement cohorts (stacked)",
  "stacked_implementation" = "2015 + 2023 implementation cohorts (stacked)",
  "cohort_2012" = "2012 announcement cohort",
  "cohort_2015" = "2015 implementation cohort",
  "cohort_2022" = "2022 announcement cohort",
  "cohort_2023" = "2023 implementation cohort"
)

panel_input <- switch(PANEL_MODE,
  "stacked_announcement" = "../input/sales_transaction_panel_announcement.parquet",
  "stacked_implementation" = "../input/sales_transaction_panel.parquet",
  "cohort_2012" = "../input/sales_transaction_panel_2012.parquet",
  "cohort_2015" = "../input/sales_transaction_panel_2015.parquet",
  "cohort_2022" = "../input/sales_transaction_panel_2022.parquet",
  "cohort_2023" = "../input/sales_transaction_panel_2023.parquet"
)

control_suffix <- case_when(
  CONTROL_MODE == "none" ~ "_no_hedonics",
  CONTROL_MODE == "hedonic" ~ "",
  CONTROL_MODE == "amenity" ~ "_amenity"
)
fe_suffix <- case_when(
  FE_TYPE == "strict_pair_x_year" ~ "",
  FE_TYPE == "pair_trend_plus_year" ~ "_pairtrend",
  FE_TYPE == "side_plus_year" ~ "_yearfe"
)
suffix <- sprintf(
  "disaggregate_%s_%s_%s_%s_%s%s%s_%s",
  TIME_UNIT,
  PANEL_MODE,
  TREATMENT_TYPE,
  WEIGHTING,
  BANDWIDTH_LABEL,
  fe_suffix,
  control_suffix,
  POST_WINDOW
)
if (GEO_FE_LEVEL != "segment") {
  suffix <- paste0(suffix, "_geo_wardpair")
}
if (CLUSTER_LEVEL == "block") {
  suffix <- paste0(suffix, "_clust_block")
} else if (CLUSTER_LEVEL == "segment") {
  suffix <- paste0(suffix, "_clust_segment")
}
suffix <- paste0(suffix, min_segment_suffix)

message("\n=== Sales Event Study ===")
message(sprintf("Panel mode: %s", PANEL_MODE))
message(sprintf("Panel title: %s", panel_title))
message(sprintf("Treatment type: %s", TREATMENT_TYPE))
message(sprintf("Control mode: %s", CONTROL_MODE))
message(sprintf("Time unit: %s", TIME_UNIT))
message(sprintf("FE type: %s", FE_TYPE))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("Bandwidth: %s", BANDWIDTH_LABEL))
message(sprintf("Post window: %s", POST_WINDOW))
message(sprintf("Geo FE level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster level: %s", CLUSTER_LEVEL))
message(sprintf(
  "Minimum segment length: %s",
  if (is.finite(MIN_SEGMENT_LENGTH_FT)) sprintf("%.1f ft", MIN_SEGMENT_LENGTH_FT) else "none"
))
message(sprintf("Write sidecars: %s", WRITE_SIDECARS))


message("\nLoading transaction panel...")
data <- read_parquet(panel_input) %>%
  as_tibble() %>%
  filter(!is.na(sale_price), sale_price > 0)
panel_input_n <- nrow(data)
panel_input_missing_strictness_change_n <- sum(is.na(data$strictness_change))

raw_n <- nrow(data)
raw_blocks <- n_distinct(data$block_id)
raw_pins <- n_distinct(data$pin)

needs_segment <- GEO_FE_LEVEL == "segment" || CLUSTER_LEVEL %in% c("segment", "twoway_block_segment")
if (needs_segment) {
  required_segment_cols <- if (grepl("^stacked_", PANEL_MODE)) {
    c("segment_id_cohort", "segment_side", "cohort_segment", "cohort_segment_side")
  } else {
    c("segment_id_cohort", "segment_side")
  }
  missing_cols <- setdiff(required_segment_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing required segment columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  data <- data %>% filter(!is.na(segment_id_cohort), segment_id_cohort != "")
}
after_segment_filter_n <- nrow(data)
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
  filter(dist_m <= BANDWIDTH)
score_gate_n <- nrow(data)
score_gate_missing_change_n <- sum(is.na(data$strictness_change))
if (score_gate_missing_change_n > 0L) {
  stop("Requested sales event-study regression sample has missing score values.", call. = FALSE)
}

data <- data %>%
  mutate(
    weight = if (WEIGHTING == "triangular") pmax(0, 1 - dist_m / BANDWIDTH) else 1,
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0),
    treatment_stricter_binary = as.integer(strictness_change > 0),
    treatment_lenient_binary = as.integer(strictness_change < 0)
  )
after_bandwidth_n <- nrow(data)

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

complete_hedonic <- data %>%
  transmute(
    complete = is.finite(log_sqft) &
      is.finite(log_land_sqft) &
      is.finite(log_building_age) &
      is.finite(log_bedrooms) &
      is.finite(log_baths) &
      !is.na(has_garage)
  ) %>%
  pull(complete)
complete_hedonic_n <- sum(complete_hedonic)
complete_amenity <- data %>%
  transmute(
    complete = is.finite(nearest_school_dist_m) &
      is.finite(nearest_park_dist_m) &
      is.finite(nearest_major_road_dist_m) &
      is.finite(lake_michigan_dist_m)
  ) %>%
  pull(complete)
complete_amenity_n <- sum(complete_amenity)
if (CONTROL_MODE %in% c("hedonic", "amenity")) {
  data <- data[complete_hedonic, ]
}
if (CONTROL_MODE == "amenity") {
  data <- data[complete_amenity[complete_hedonic], ]
}
after_hedonic_filter_n <- nrow(data)

if (TIME_UNIT == "quarterly") {
  data <- data %>%
    mutate(
      sale_quarter = quarter(sale_date),
      sale_yearqtr = year(sale_date) + (quarter(sale_date) - 1) / 4
    )

  if (grepl("^stacked_", PANEL_MODE)) {
    data <- data %>%
      mutate(cohort_event_year = as.integer(cohort))
  } else {
    data <- data %>%
      mutate(cohort_event_year = as.integer(sub("cohort_", "", PANEL_MODE)))
  }

  data <- data %>%
    mutate(relative_period = round((sale_yearqtr - cohort_event_year) * 4))

  if (POST_WINDOW == "short") {
    min_period <- -20
    max_period <- 8
  } else {
    min_period <- -12
    max_period <- 12
  }
  time_fe_var <- "sale_yearqtr"
  event_var <- "relative_period"
} else {
  data <- data %>%
    mutate(relative_period = relative_year_capped)
  if (POST_WINDOW == "short") {
    min_period <- -5
    max_period <- 2
  } else {
    min_period <- -5
    max_period <- 5
  }
  time_fe_var <- "sale_year"
  event_var <- "relative_period"
}

trend_var <- "sale_year"
stacked_mode <- grepl("^stacked_", PANEL_MODE)
if (stacked_mode) {
  if (GEO_FE_LEVEL == "ward_pair") {
    data <- data %>%
      mutate(
        ward_pair_side_temp = sub("^[0-9]+_", "", cohort_ward_pair_side),
        ward_pair = sub("_[0-9]+$", "", ward_pair_side_temp),
        cohort_ward_pair = paste(cohort, ward_pair, sep = "_")
      )
    fe_side_var <- "cohort_ward_pair_side"
    fe_group_var <- "cohort_ward_pair"
  } else {
    fe_side_var <- "cohort_segment_side"
    fe_group_var <- "cohort_segment"
  }

  fe_formula <- case_when(
    FE_TYPE == "strict_pair_x_year" ~ sprintf("%s + %s^%s", fe_side_var, fe_group_var, time_fe_var),
    FE_TYPE == "pair_trend_plus_year" ~ sprintf("%s + cohort^%s + %s[%s]", fe_side_var, time_fe_var, fe_group_var, trend_var),
    FE_TYPE == "side_plus_year" ~ sprintf("%s + cohort^%s", fe_side_var, time_fe_var)
  )

  if (CLUSTER_LEVEL == "twoway_block_segment") {
    cluster_formula <- ~cohort_block_id + cohort_segment
  } else if (CLUSTER_LEVEL == "segment") {
    cluster_formula <- ~cohort_segment
  } else {
    cluster_formula <- ~cohort_block_id
  }
  segment_var <- "cohort_segment"
} else {
  if (GEO_FE_LEVEL == "ward_pair") {
    data <- data %>%
      mutate(ward_pair = sub("_[0-9]+$", "", ward_pair_side))
    fe_side_var <- "ward_pair_side"
    fe_group_var <- "ward_pair"
  } else {
    fe_side_var <- "segment_side"
    fe_group_var <- "segment_id_cohort"
  }

  fe_formula <- case_when(
    FE_TYPE == "strict_pair_x_year" ~ sprintf("%s + %s^%s", fe_side_var, fe_group_var, time_fe_var),
    FE_TYPE == "pair_trend_plus_year" ~ sprintf("%s + %s + %s[%s]", fe_side_var, time_fe_var, fe_group_var, trend_var),
    FE_TYPE == "side_plus_year" ~ sprintf("%s + %s", fe_side_var, time_fe_var)
  )

  if (CLUSTER_LEVEL == "twoway_block_segment") {
    cluster_formula <- ~block_id + segment_id_cohort
  } else if (CLUSTER_LEVEL == "segment") {
    cluster_formula <- ~segment_id_cohort
  } else {
    cluster_formula <- ~block_id
  }
  segment_var <- "segment_id_cohort"
}

if (GEO_FE_LEVEL == "segment" || CLUSTER_LEVEL %in% c("segment", "twoway_block_segment")) {
  data <- data %>% filter(!is.na(.data[[segment_var]]), .data[[segment_var]] != "")
}
if (GEO_FE_LEVEL == "segment") {
  data <- data %>% filter(!is.na(.data[[fe_side_var]]), .data[[fe_side_var]] != "")
}

analysis_n <- nrow(data)
support_by_event_time <- build_event_study_support_table(
  data,
  event_var = event_var,
  time_fe_var = time_fe_var,
  fe_group_var = fe_group_var,
  min_period = min_period,
  max_period = max_period,
  support_mode = "two_sided_cells",
  cohort_label = PANEL_MODE,
  side_var = fe_side_var,
  block_var = "block_id",
  segment_var = segment_var,
  pin_var = "pin"
)

control_formula <- if (CONTROL_MODE == "hedonic") {
  "+ log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage"
} else if (CONTROL_MODE == "amenity") {
  "+ log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_m + nearest_park_dist_m + nearest_major_road_dist_m + lake_michigan_dist_m"
} else {
  ""
}

metadata <- tibble(
  panel_mode = PANEL_MODE,
  panel_title = panel_title,
  time_unit = TIME_UNIT,
  treatment_type = TREATMENT_TYPE,
  include_hedonics = CONTROL_MODE %in% c("hedonic", "amenity"),
  control_mode = CONTROL_MODE,
  weighting = WEIGHTING,
  bandwidth = BANDWIDTH,
  bandwidth_label = BANDWIDTH_LABEL,
  fe_type = FE_TYPE,
  post_window = POST_WINDOW,
  geo_fe_level = GEO_FE_LEVEL,
  cluster_level = CLUSTER_LEVEL,
  min_segment_length_ft = if (is.finite(MIN_SEGMENT_LENGTH_FT)) MIN_SEGMENT_LENGTH_FT else NA_real_,
  raw_n = raw_n,
  raw_blocks = raw_blocks,
  raw_pins = raw_pins,
  panel_input_n = panel_input_n,
  panel_input_missing_strictness_change_n = panel_input_missing_strictness_change_n,
  score_gate_n = score_gate_n,
  score_gate_missing_change_n = score_gate_missing_change_n,
  after_segment_filter_n = after_segment_filter_n,
  after_bandwidth_n = after_bandwidth_n,
  complete_hedonic_n = complete_hedonic_n,
  complete_amenity_n = complete_amenity_n,
  segment_length_input_n = segment_length_input_n,
  segment_length_drop_n = segment_length_drop_n,
  segment_length_missing_n = segment_length_missing_n,
  segment_length_short_n = segment_length_short_n,
  analysis_n = analysis_n,
  treated_n = sum(data$treat == 1, na.rm = TRUE),
  control_n = sum(data$treat == 0, na.rm = TRUE),
  effective_weight_n = sum(data$weight),
  plotted_min_event_time = min_period,
  plotted_max_event_time = max_period,
  plotted_supported_periods = paste(support_by_event_time$event_time[support_by_event_time$has_identifying_support], collapse = "|")
)

if (TREATMENT_TYPE == "continuous") {
  formula_str <- sprintf(
    "log(sale_price) ~ i(%s, strictness_change, ref = -1) %s | %s",
    event_var, control_formula, fe_formula
  )
  message(sprintf("Running regression with %s observations", format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_str))
  model <- feols(
    as.formula(formula_str),
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
  plot_data <- plot_data %>%
    mutate(
      estimate_pct = estimate_display,
      ci_low_pct = ci_low_display,
      ci_high_pct = ci_high_display
    )

  coefficients <- plot_data %>%
    select(group, event_time, estimate, std_error, ci_low, ci_high, estimate_display, ci_low_display, ci_high_display,
      estimate_name, estimate_name_raw, is_reference, n_obs, n_treated, n_control, contributing_cohorts,
      n_fe_groups, n_blocks, n_segments, n_pins, n_fe_group_time_cells, n_identifying_fe_group_time_cells,
      n_identifying_fe_groups, has_treated_and_control, has_identifying_support
    )
  pretrend <- compute_event_study_pretrend(model, plot_data, "All sales")

  ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = sprintf("Sales event study: %s", panel_title),
      x_label = if (TIME_UNIT == "yearly") "Years relative to redistricting" else "Quarters relative to redistricting",
      y_label = "Effect on home prices",
      display_suffix = "%"
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )
  if (WRITE_SIDECARS) {
    write_csv(coefficients, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_coefficients_%s.csv", suffix)))
    write_csv(support_by_event_time, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_support_%s.csv", suffix)))
    write_csv(pretrend, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_pretrend_%s.csv", suffix)))
    write_csv(metadata, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_metadata_%s.csv", suffix)))
  }
} else {
  if (TREATMENT_TYPE == "binary_direction") {
    data_stricter <- data %>% filter(treatment_lenient_binary == 0)
    data_lenient <- data %>% filter(treatment_stricter_binary == 0)
    stricter_treatment_var <- "treatment_stricter_binary"
    lenient_treatment_var <- "treatment_lenient_binary"
    support_stricter <- build_event_study_support_table(
      data_stricter,
      event_var = event_var,
      time_fe_var = time_fe_var,
      fe_group_var = fe_group_var,
      min_period = min_period,
      max_period = max_period,
      support_mode = "two_sided_cells",
      cohort_label = PANEL_MODE,
      side_var = fe_side_var,
      block_var = "block_id",
      segment_var = segment_var,
      pin_var = "pin"
    )
    support_lenient <- build_event_study_support_table(
      data_lenient,
      event_var = event_var,
      time_fe_var = time_fe_var,
      fe_group_var = fe_group_var,
      min_period = min_period,
      max_period = max_period,
      support_mode = "two_sided_cells",
      cohort_label = PANEL_MODE,
      side_var = fe_side_var,
      block_var = "block_id",
      segment_var = segment_var,
      pin_var = "pin"
    )
  } else {
    data_stricter <- data
    data_lenient <- data
    stricter_treatment_var <- "treatment_stricter_continuous"
    lenient_treatment_var <- "treatment_lenient_continuous"
    support_stricter <- support_by_event_time
    support_lenient <- support_by_event_time
  }

  metadata <- metadata %>%
    mutate(
      stricter_analysis_n = nrow(data_stricter),
      lenient_analysis_n = nrow(data_lenient),
      plotted_supported_periods = paste(sort(unique(c(
        support_stricter$event_time[support_stricter$has_identifying_support],
        support_lenient$event_time[support_lenient$has_identifying_support]
      ))), collapse = "|")
    )

  formula_stricter <- sprintf(
    "log(sale_price) ~ i(%s, %s, ref = -1) %s | %s",
    event_var, stricter_treatment_var, control_formula, fe_formula
  )
  formula_lenient <- sprintf(
    "log(sale_price) ~ i(%s, %s, ref = -1) %s | %s",
    event_var, lenient_treatment_var, control_formula, fe_formula
  )

  message(sprintf("Running regression with %s observations", format(nrow(data_stricter), big.mark = ",")))
  message(sprintf("Formula: %s", formula_stricter))
  model_stricter <- feols(
    as.formula(formula_stricter),
    data = data_stricter,
    weights = ~weight,
    cluster = cluster_formula
  )

  message(sprintf("Running regression with %s observations", format(nrow(data_lenient), big.mark = ",")))
  message(sprintf("Formula: %s", formula_lenient))
  model_lenient <- feols(
    as.formula(formula_lenient),
    data = data_lenient,
    weights = ~weight,
    cluster = cluster_formula
  )

  plot_data <- bind_rows(
    build_event_study_plot_data(model_stricter, support_stricter, min_period, max_period, "Moved to Stricter", "multiply100"),
    build_event_study_plot_data(model_lenient, support_lenient, min_period, max_period, "Moved to More Lenient", "multiply100")
  ) %>%
    mutate(
      estimate_pct = estimate_display,
      ci_low_pct = ci_low_display,
      ci_high_pct = ci_high_display
    ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested sales specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(group, event_time, estimate, std_error, ci_low, ci_high, estimate_display, ci_low_display, ci_high_display,
      estimate_name, estimate_name_raw, is_reference, n_obs, n_treated, n_control, contributing_cohorts,
      n_fe_groups, n_blocks, n_segments, n_pins, n_fe_group_time_cells, n_identifying_fe_group_time_cells,
      n_identifying_fe_groups, has_treated_and_control, has_identifying_support
    )
  pretrend <- bind_rows(
    compute_event_study_pretrend(model_stricter, plot_data %>% filter(group == "Moved to Stricter"), "Moved to Stricter"),
    compute_event_study_pretrend(model_lenient, plot_data %>% filter(group == "Moved to More Lenient"), "Moved to More Lenient")
  )
  directional_plots <- make_event_study_directional_plots(
    plot_data,
    plot_title = sprintf("Sales event study: %s", panel_title),
    x_label = if (TIME_UNIT == "yearly") "Years relative to redistricting" else "Quarters relative to redistricting",
    y_label = "Effect on home prices",
    display_suffix = "%"
  )

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")
  ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), directional_plots$combined, width = 7, height = 4.5, bg = "white")
  if (WRITE_SIDECARS) {
    write_csv(coefficients, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_coefficients_%s.csv", suffix)))
    write_csv(
      bind_rows(
        support_stricter %>% mutate(group = "Moved to Stricter"),
        support_lenient %>% mutate(group = "Moved to More Lenient")
      ),
      file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_support_%s.csv", suffix))
    )
    write_csv(pretrend, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_pretrend_%s.csv", suffix)))
    write_csv(metadata, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_metadata_%s.csv", suffix)))
  }
}

message("\nDone!")
