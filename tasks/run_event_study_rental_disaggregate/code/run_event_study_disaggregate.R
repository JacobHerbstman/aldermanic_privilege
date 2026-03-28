source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_disaggregate/code")
# panel_mode <- "cohort_2023"
# frequency <- "yearly"
# treatment_type <- "continuous"
# include_controls <- TRUE
# weighting <- "triangular"
# bandwidth <- 1000
# sample_filter <- "multifamily_only"
# fe_type <- "strict_pair_x_year"
# post_window <- "short"
# geo_fe_level <- "segment"
# cluster_level <- "twoway_block_segment"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(panel_mode, frequency, treatment_type, include_controls, weighting, bandwidth, sample_filter, fe_type, post_window, geo_fe_level, cluster_level)
}

if (length(cli_args) >= 11) {
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
} else {
  if (!exists("panel_mode") || !exists("frequency") || !exists("treatment_type") ||
    !exists("include_controls") || !exists("weighting") || !exists("bandwidth") ||
    !exists("sample_filter") || !exists("fe_type") || !exists("post_window") ||
    !exists("geo_fe_level") || !exists("cluster_level")) {
    stop("FATAL: Script requires args: <panel_mode> <frequency> <treatment_type> <include_controls> <weighting> <bandwidth> <sample_filter> <fe_type> <post_window> <geo_fe_level> <cluster_level>", call. = FALSE)
  }
}

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
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 1000) {
  stop("Segment FE requested with bandwidth > 1000. Use bandwidth <= 1000.", call. = FALSE)
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
  "disaggregate_%s_%s_%s_%s_%dft%s%s%s_%s",
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
message(sprintf("Bandwidth: %d ft", BANDWIDTH))
message(sprintf("Sample filter: %s", SAMPLE_FILTER))
message(sprintf("FE type: %s", FE_TYPE))
message(sprintf("Post window: %s", POST_WINDOW))
message(sprintf("Geo FE level: %s", GEO_FE_LEVEL))
message(sprintf("Cluster level: %s", CLUSTER_LEVEL))

make_support_table <- function(df, event_var, time_fe_var, fe_group_var, fe_side_var, segment_var, min_period, max_period) {
  support_base <- df %>%
    filter(.data[[event_var]] >= min_period, .data[[event_var]] <= max_period)

  cell_support <- support_base %>%
    group_by(
      event_time = .data[[event_var]],
      fe_group = .data[[fe_group_var]],
      calendar_time = .data[[time_fe_var]]
    ) %>%
    summarise(
      n_treated = sum(treat == 1, na.rm = TRUE),
      n_control = sum(treat == 0, na.rm = TRUE),
      n_sides = n_distinct(.data[[fe_side_var]]),
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
      n_blocks = n_distinct(block_id),
      n_segments = if (segment_var %in% names(support_base)) n_distinct(.data[[segment_var]][!is.na(.data[[segment_var]])]) else NA_integer_,
      .groups = "drop"
    )

  cell_event_support <- cell_support %>%
    group_by(event_time) %>%
    summarise(
      n_fe_group_time_cells = n(),
      n_identifying_fe_group_time_cells = sum(n_treated > 0 & n_control > 0 & n_sides == 2),
      n_identifying_fe_groups = n_distinct(fe_group[n_treated > 0 & n_control > 0 & n_sides == 2]),
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
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), fill = "#009E73", alpha = 0.2, color = NA) +
    geom_line(color = "#009E73", linewidth = 1) +
    geom_point(color = "#009E73", size = 2.5) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = sprintf("Rental event study: %s", panel_title),
      x = if (FREQUENCY == "yearly") "Years relative to alderman switch" else "Quarters relative to alderman switch",
      y = "Effect on rents"
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

  facet_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_pct, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    facet_wrap(~group, ncol = 1) +
    labs(
      title = sprintf("Rental event study: %s", panel_title),
      x = if (FREQUENCY == "yearly") "Years relative to alderman switch" else "Quarters relative to alderman switch",
      y = "Effect on rents"
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

  combined_plot <- ggplot(plot_data, aes(x = event_time, y = estimate_pct, color = group, fill = group)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5, shape = 21, stroke = 0.5) +
    scale_color_manual(values = color_values, name = NULL) +
    scale_fill_manual(values = color_values, name = NULL) +
    scale_x_continuous(breaks = sort(unique(plot_data$event_time))) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = sprintf("Rental event study: %s", panel_title),
      x = if (FREQUENCY == "yearly") "Years relative to alderman switch" else "Quarters relative to alderman switch",
      y = "Effect on rents"
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
  filter(dist_ft <= BANDWIDTH) %>%
  mutate(
    weight = if (WEIGHTING == "triangular") pmax(0, 1 - dist_ft / BANDWIDTH) else 1,
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
support_by_event_time <- make_support_table(data, event_var, time_fe_var, fe_group_var, fe_side_var, segment_var, min_period, max_period)

hedonic_formula <- if (INCLUDE_CONTROLS) {
  "+ building_type_factor + log_sqft + log_beds + log_baths"
} else {
  ""
}

run_model <- function(formula_str) {
  message(sprintf("Running regression with %s observations", format(nrow(data), big.mark = ",")))
  message(sprintf("Formula: %s", formula_str))
  feols(
    as.formula(formula_str),
    data = data,
    weights = ~weight,
    cluster = cluster_formula
  )
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
  model <- run_model(formula_str)
  plot_data <- extract_plot_data(model, support_by_event_time, min_period, max_period, "All listings")
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested rental specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(group, event_time, estimate, std_error, ci_low, ci_high, estimate_pct, ci_low_pct, ci_high_pct,
      estimate_name, estimate_name_raw, is_reference, n_obs, n_treated, n_control, contributing_cohorts,
      n_fe_groups, n_segments, n_fe_group_time_cells, n_identifying_fe_group_time_cells, n_identifying_fe_groups,
      has_treated_and_control, has_identifying_support
    )
  pretrend <- compute_pretrend_test(model, plot_data, "All listings")

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), make_single_series_plot(plot_data), width = 7, height = 4.5, bg = "white")
  write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
  write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
  write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
  write_csv(metadata, sprintf("../output/event_study_metadata_%s.csv", suffix))
} else {
  formula_stricter <- sprintf(
    "log(rent_price) ~ i(%s, treatment_stricter_continuous, ref = -1) %s | %s",
    event_var, hedonic_formula, fe_formula
  )
  formula_lenient <- sprintf(
    "log(rent_price) ~ i(%s, treatment_lenient_continuous, ref = -1) %s | %s",
    event_var, hedonic_formula, fe_formula
  )

  model_stricter <- run_model(formula_stricter)
  model_lenient <- run_model(formula_lenient)

  plot_data <- bind_rows(
    extract_plot_data(model_stricter, support_by_event_time, min_period, max_period, "Moved to Stricter"),
    extract_plot_data(model_lenient, support_by_event_time, min_period, max_period, "Moved to More Lenient")
  ) %>%
    filter(!is.na(estimate))

  if (nrow(plot_data) == 0) {
    stop("No supported coefficients were available for the requested rental specification.", call. = FALSE)
  }

  coefficients <- plot_data %>%
    select(group, event_time, estimate, std_error, ci_low, ci_high, estimate_pct, ci_low_pct, ci_high_pct,
      estimate_name, estimate_name_raw, is_reference, n_obs, n_treated, n_control, contributing_cohorts,
      n_fe_groups, n_segments, n_fe_group_time_cells, n_identifying_fe_group_time_cells, n_identifying_fe_groups,
      has_treated_and_control, has_identifying_support
    )
  pretrend <- bind_rows(
    compute_pretrend_test(model_stricter, plot_data %>% filter(group == "Moved to Stricter"), "Moved to Stricter"),
    compute_pretrend_test(model_lenient, plot_data %>% filter(group == "Moved to More Lenient"), "Moved to More Lenient")
  )
  directional_plots <- make_directional_plots(plot_data)

  ggsave(sprintf("../output/event_study_%s.pdf", suffix), directional_plots$facet, width = 7, height = 6, bg = "white")
  ggsave(sprintf("../output/event_study_combined_%s.pdf", suffix), directional_plots$combined, width = 7, height = 4.5, bg = "white")
  write_csv(coefficients, sprintf("../output/event_study_coefficients_%s.csv", suffix))
  write_csv(support_by_event_time, sprintf("../output/event_study_support_%s.csv", suffix))
  write_csv(pretrend, sprintf("../output/event_study_pretrend_%s.csv", suffix))
  write_csv(metadata, sprintf("../output/event_study_metadata_%s.csv", suffix))
}

message("\nDone!")
