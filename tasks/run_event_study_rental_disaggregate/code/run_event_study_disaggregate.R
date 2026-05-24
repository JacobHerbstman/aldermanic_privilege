source("../../setup_environment/code/packages.R")
source("../../_lib/event_study_plot_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_disaggregate/code")
# panel_mode <- "cohort_2023"
# frequency <- "yearly"
# treatment_type <- "continuous"
# include_controls <- TRUE
# weighting <- "triangular"
# bandwidth <- 250
# sample_filter <- "multifamily_only"
# fe_type <- "strict_pair_x_year"
# post_window <- "short"
# geo_fe_level <- "segment"
# cluster_level <- "twoway_block_segment"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_mode, frequency, treatment_type, include_controls, weighting, bandwidth, sample_filter, fe_type, post_window, geo_fe_level, cluster_level)
}

if (length(cli_args) != 11) {
  stop("FATAL: Script requires args: <panel_mode> <frequency> <treatment_type> <include_controls> <weighting> <bandwidth> <sample_filter> <fe_type> <post_window> <geo_fe_level> <cluster_level>", call. = FALSE)
}
panel_mode <- cli_args[1]
frequency <- cli_args[2]
treatment_type <- cli_args[3]
include_controls <- tolower(cli_args[4]) %in% c("true", "t", "1", "yes")
weighting <- cli_args[5]
bandwidth <- as.numeric(cli_args[6])
sample_filter <- cli_args[7]
fe_type <- cli_args[8]
post_window <- cli_args[9]
geo_fe_level <- tolower(cli_args[10])
cluster_level <- tolower(cli_args[11])

PANEL_MODE <- panel_mode
FREQUENCY <- frequency
TREATMENT_TYPE <- treatment_type
INCLUDE_CONTROLS <- include_controls
WEIGHTING <- weighting
BANDWIDTH <- bandwidth
SAMPLE_FILTER <- sample_filter
FE_TYPE <- fe_type
POST_WINDOW <- post_window
GEO_FE_LEVEL <- geo_fe_level
CLUSTER_LEVEL <- cluster_level
WRITE_SIDECARS <- tolower(Sys.getenv("WRITE_SIDECARS", "0")) %in% c("1", "true", "yes")
SIDECAR_OUTPUT_DIR <- Sys.getenv("SIDECAR_OUTPUT_DIR", "../output")

valid_panel_modes <- c("stacked_implementation", "cohort_2015", "cohort_2023")
if (!PANEL_MODE %in% valid_panel_modes) {
  stop(sprintf("--panel_mode must be one of: %s", paste(valid_panel_modes, collapse = ", ")), call. = FALSE)
}
if (!FREQUENCY %in% c("yearly", "quarterly")) {
  stop("--frequency must be one of: yearly, quarterly", call. = FALSE)
}
if (!TREATMENT_TYPE %in% c("continuous", "continuous_split")) {
  stop("--treatment_type must be one of: continuous, continuous_split", call. = FALSE)
}
if (!SAMPLE_FILTER %in% c("full_sample", "multifamily_only")) {
  stop("--sample_filter must be one of: full_sample, multifamily_only", call. = FALSE)
}
if (!WEIGHTING %in% c("uniform", "triangular")) {
  stop("--weighting must be one of: uniform, triangular", call. = FALSE)
}
if (!FE_TYPE %in% c("strict_pair_x_year", "pair_trend_plus_year", "side_plus_year")) {
  stop("--fe_type must be one of: strict_pair_x_year, pair_trend_plus_year, side_plus_year", call. = FALSE)
}
if (!POST_WINDOW %in% c("short", "full", "overlap")) {
  stop("--post_window must be one of: short, full, overlap", call. = FALSE)
}
if (!GEO_FE_LEVEL %in% c("segment", "ward_pair")) {
  stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!CLUSTER_LEVEL %in% c("twoway_block_segment", "block", "segment")) {
  stop("--cluster_level must be one of: twoway_block_segment, block, segment", call. = FALSE)
}
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 800) {
  stop("Segment FE requested with bandwidth > 800m. Use bandwidth <= 800m.", call. = FALSE)
}
if (POST_WINDOW == "overlap" && (PANEL_MODE != "stacked_implementation" || FREQUENCY != "yearly")) {
  stop("The overlap window is only valid for the stacked yearly rental specification.", call. = FALSE)
}

panel_title <- switch(PANEL_MODE,
  "stacked_implementation" = "2015 + 2023 implementation cohorts (stacked)",
  "cohort_2015" = "2015 implementation cohort",
  "cohort_2023" = "2023 implementation cohort"
)

panel_input <- switch(PANEL_MODE,
  "stacked_implementation" = "../input/rental_listing_panel.parquet",
  "cohort_2015" = "../input/rental_listing_panel_2015.parquet",
  "cohort_2023" = "../input/rental_listing_panel_2023.parquet"
)

sample_suffix <- if (SAMPLE_FILTER == "multifamily_only") "_mf" else ""
hedonic_suffix <- if (INCLUDE_CONTROLS) "" else "_no_hedonics"
fe_suffix <- case_when(
  FE_TYPE == "strict_pair_x_year" ~ "",
  FE_TYPE == "pair_trend_plus_year" ~ "_pairtrend",
  FE_TYPE == "side_plus_year" ~ "_yearfe"
)
suffix <- sprintf(
  "disaggregate_%s_%s_%s_%s_%dm%s%s%s_%s",
  FREQUENCY,
  PANEL_MODE,
  TREATMENT_TYPE,
  WEIGHTING,
  as.integer(BANDWIDTH),
  sample_suffix,
  hedonic_suffix,
  fe_suffix,
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

message("\n=== Rental Event Study ===")
message(sprintf("Panel mode: %s", PANEL_MODE))
message(sprintf("Panel title: %s", panel_title))
message(sprintf("Frequency: %s", FREQUENCY))
message(sprintf("Treatment type: %s", TREATMENT_TYPE))
message(sprintf("Include hedonics: %s", INCLUDE_CONTROLS))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("Bandwidth: %dm", as.integer(round(BANDWIDTH))))
message(sprintf("Sample filter: %s", SAMPLE_FILTER))
message(sprintf("FE type: %s", FE_TYPE))
message(sprintf("Post window: %s", POST_WINDOW))
message(sprintf("Geo FE level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster level: %s", CLUSTER_LEVEL))
message(sprintf("Write sidecars: %s", WRITE_SIDECARS))

message("\nLoading listing-level panel data...")
data <- read_parquet(panel_input) %>%
  filter(!is.na(strictness_change), !is.na(rent_price), rent_price > 0)

raw_n <- nrow(data)
raw_blocks <- n_distinct(data$block_id)
raw_ids <- n_distinct(data$id)

if (FREQUENCY == "yearly") {
  time_fe_var <- "year"
  event_var <- "relative_year_capped"
  if (POST_WINDOW == "short") {
    min_period <- -5
    max_period <- 2
  } else if (POST_WINDOW == "full") {
    min_period <- -5
    max_period <- 5
  } else {
    min_period <- -1
    max_period <- 2
  }
} else {
  time_fe_var <- "year_quarter"
  event_var <- "relative_quarter_capped"
  if (POST_WINDOW == "short") {
    min_period <- -20
    max_period <- 8
  } else {
    min_period <- -8
    max_period <- 16
  }
}

if (SAMPLE_FILTER == "multifamily_only") {
  data <- data %>% filter(building_type_clean == "multi_family")
}
after_sample_filter_n <- nrow(data)

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

data <- data %>%
  filter(dist_m <= BANDWIDTH) %>%
  mutate(
    weight = if (WEIGHTING == "triangular") pmax(0, 1 - dist_m / BANDWIDTH) else 1,
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0)
  )
after_bandwidth_n <- nrow(data)

complete_hedonic <- complete.cases(data[, c("log_sqft", "log_beds", "log_baths", "building_type_clean")])
complete_hedonic_n <- sum(complete_hedonic)
if (INCLUDE_CONTROLS) {
  data <- data[complete_hedonic, ]
}
after_hedonic_filter_n <- nrow(data)

if (POST_WINDOW == "overlap") {
  data <- data %>%
    filter(.data[[event_var]] >= min_period, .data[[event_var]] <= max_period)
}

trend_var <- "year"
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
  segment_var = segment_var
)

hedonic_formula <- if (INCLUDE_CONTROLS) {
  "+ building_type_factor + log_sqft + log_beds + log_baths"
} else {
  ""
}

metadata <- tibble(
  panel_mode = PANEL_MODE,
  panel_title = panel_title,
  frequency = FREQUENCY,
  treatment_type = TREATMENT_TYPE,
  include_hedonics = INCLUDE_CONTROLS,
  weighting = WEIGHTING,
  bandwidth = BANDWIDTH,
  sample_filter = SAMPLE_FILTER,
  fe_type = FE_TYPE,
  post_window = POST_WINDOW,
  geo_fe_level = GEO_FE_LEVEL,
  cluster_level = CLUSTER_LEVEL,
  raw_n = raw_n,
  raw_blocks = raw_blocks,
  raw_ids = raw_ids,
  after_sample_filter_n = after_sample_filter_n,
  after_segment_filter_n = after_segment_filter_n,
  after_bandwidth_n = after_bandwidth_n,
  complete_hedonic_n = complete_hedonic_n,
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
    "log(rent_price) ~ i(%s, strictness_change, ref = -1) %s | %s",
    event_var, hedonic_formula, fe_formula
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
    "All listings",
    "multiply100"
  )
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested rental specification.", call. = FALSE)
  }
  plot_data <- plot_data %>%
    mutate(
      estimate_pct = estimate_display,
      ci_low_pct = ci_low_display,
      ci_high_pct = ci_high_display
    )

  ggsave(
    sprintf("../output/event_study_%s.pdf", suffix),
    make_event_study_single_series_plot(
      plot_data,
      plot_title = sprintf("Rental event study: %s", panel_title),
      x_label = if (FREQUENCY == "yearly") "Years relative to alderman switch" else "Quarters relative to alderman switch",
      y_label = "Effect on rents",
      display_suffix = "%"
    ),
    width = 7,
    height = 4.5,
    bg = "white"
  )
  if (WRITE_SIDECARS) {
    write_csv(
      plot_data %>%
        select(group, event_time, estimate, std_error, ci_low, ci_high, estimate_pct, ci_low_pct, ci_high_pct,
          estimate_name, estimate_name_raw, is_reference, n_obs, n_treated, n_control, contributing_cohorts,
          n_fe_groups, n_segments, n_fe_group_time_cells, n_identifying_fe_group_time_cells, n_identifying_fe_groups,
          has_treated_and_control, has_identifying_support
        ),
      file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_coefficients_%s.csv", suffix))
    )
    write_csv(support_by_event_time, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_support_%s.csv", suffix)))
    write_csv(compute_event_study_pretrend(model, plot_data, "All listings"), file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_pretrend_%s.csv", suffix)))
    write_csv(metadata, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_metadata_%s.csv", suffix)))
  }
} else {
  formula_stricter <- sprintf(
    "log(rent_price) ~ i(%s, treatment_stricter_continuous, ref = -1) %s | %s",
    event_var, hedonic_formula, fe_formula
  )
  formula_lenient <- sprintf(
    "log(rent_price) ~ i(%s, treatment_lenient_continuous, ref = -1) %s | %s",
    event_var, hedonic_formula, fe_formula
  )

  message(sprintf("Running regression with %s observations", format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_stricter))
  model_stricter <- feols(
    as.formula(formula_stricter),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )

  message(sprintf("Running regression with %s observations", format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_lenient))
  model_lenient <- feols(
    as.formula(formula_lenient),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )

  plot_data <- bind_rows(
    build_event_study_plot_data(model_stricter, support_by_event_time, min_period, max_period, "Moved to Stricter", "multiply100"),
    build_event_study_plot_data(model_lenient, support_by_event_time, min_period, max_period, "Moved to More Lenient", "multiply100")
  ) %>%
    mutate(
      estimate_pct = estimate_display,
      ci_low_pct = ci_low_display,
      ci_high_pct = ci_high_display
    ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested rental specification.", call. = FALSE)
  }

  directional_plots <- make_event_study_directional_plots(
    plot_data,
    plot_title = sprintf("Rental event study: %s", panel_title),
    x_label = if (FREQUENCY == "yearly") "Years relative to alderman switch" else "Quarters relative to alderman switch",
    y_label = "Effect on rents",
    display_suffix = "%"
  )

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")
  ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), directional_plots$combined, width = 7, height = 4.5, bg = "white")
  if (WRITE_SIDECARS) {
    write_csv(
      plot_data %>%
        select(group, event_time, estimate, std_error, ci_low, ci_high, estimate_pct, ci_low_pct, ci_high_pct,
          estimate_name, estimate_name_raw, is_reference, n_obs, n_treated, n_control, contributing_cohorts,
          n_fe_groups, n_segments, n_fe_group_time_cells, n_identifying_fe_group_time_cells, n_identifying_fe_groups,
          has_treated_and_control, has_identifying_support
        ),
      file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_coefficients_%s.csv", suffix))
    )
    write_csv(support_by_event_time, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_support_%s.csv", suffix)))
    write_csv(
      bind_rows(
        compute_event_study_pretrend(model_stricter, plot_data %>% filter(group == "Moved to Stricter"), "Moved to Stricter"),
        compute_event_study_pretrend(model_lenient, plot_data %>% filter(group == "Moved to More Lenient"), "Moved to More Lenient")
      ),
      file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_pretrend_%s.csv", suffix))
    )
    write_csv(metadata, file.path(SIDECAR_OUTPUT_DIR, sprintf("event_study_metadata_%s.csv", suffix)))
  }
}

message("\nDone!")
