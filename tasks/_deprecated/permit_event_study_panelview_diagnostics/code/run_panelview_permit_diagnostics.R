source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_event_study_panelview_diagnostics/code")
# Rscript run_panelview_permit_diagnostics.R "cohort_2015" "high_discretion" "issue" "ppml" "continuous" "uniform" 1000 "ward_pair" "none"
# =======================================================================================

dir.create("../output", showWarnings = FALSE, recursive = TRUE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 9) {
  panel_mode <- cli_args[1]
  outcome_family <- cli_args[2]
  date_basis <- cli_args[3]
  model_type <- tolower(cli_args[4])
  treatment_type <- tolower(cli_args[5])
  weighting <- cli_args[6]
  bandwidth <- as.numeric(cli_args[7])
  geo_fe_level <- tolower(cli_args[8])
  control_spec <- cli_args[9]
} else {
  stop(
    "FATAL: Script requires 9 args: <panel_mode> <outcome_family> <date_basis> <model_type> <treatment_type> <weighting> <bandwidth> <geo_fe_level> <control_spec>",
    call. = FALSE
  )
}

if (!requireNamespace("panelView", quietly = TRUE)) {
  stop(
    "panelView is not installed. Run tasks/setup_environment/code/packages.R to install xuyiqing/panelView@dev.",
    call. = FALSE
  )
}

PANEL_MODE <- panel_mode
OUTCOME_FAMILY <- outcome_family
DATE_BASIS <- date_basis
MODEL_TYPE <- model_type
TREATMENT_TYPE <- treatment_type
WEIGHTING <- weighting
BANDWIDTH <- bandwidth
GEO_FE_LEVEL <- geo_fe_level
CONTROL_SPEC <- control_spec

valid_panel_modes <- c("stacked_implementation", "cohort_2015", "cohort_2023")
if (!PANEL_MODE %in% valid_panel_modes) {
  stop(sprintf("--panel_mode must be one of: %s", paste(valid_panel_modes, collapse = ", ")), call. = FALSE)
}
if (!OUTCOME_FAMILY %in% c("new_construction", "high_discretion", "unit_increase")) {
  stop("--outcome_family must be one of: new_construction, high_discretion, unit_increase", call. = FALSE)
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
if (!GEO_FE_LEVEL %in% c("segment", "ward_pair")) {
  stop("--geo_fe_level must be one of: segment, ward_pair", call. = FALSE)
}
if (!CONTROL_SPEC %in% c("none", "baseline_demographics")) {
  stop("--control_spec must be one of: none, baseline_demographics", call. = FALSE)
}
if (!is.finite(BANDWIDTH) || BANDWIDTH <= 0) {
  stop("--bandwidth must be positive.", call. = FALSE)
}
if (GEO_FE_LEVEL == "segment" && BANDWIDTH > 2000) {
  stop("Segment FE requested with bandwidth > 2000. Use bandwidth <= 2000.", call. = FALSE)
}

message("\n=== Permit panelView diagnostics ===")
message(sprintf("Panel mode: %s", PANEL_MODE))
message(sprintf("Outcome family: %s", OUTCOME_FAMILY))
message(sprintf("Date basis: %s", DATE_BASIS))
message(sprintf("Model type: %s", MODEL_TYPE))
message(sprintf("Treatment type: %s", TREATMENT_TYPE))
message(sprintf("Weighting: %s", WEIGHTING))
message(sprintf("Bandwidth: %d ft", as.integer(BANDWIDTH)))
message(sprintf("Geo FE level: %s", GEO_FE_LEVEL))
message(sprintf("Control spec: %s", CONTROL_SPEC))

safe_scale <- function(x) {
  x <- as.numeric(x)
  sigma <- sd(x, na.rm = TRUE)
  mu <- mean(x, na.rm = TRUE)
  if (is.na(sigma) || sigma == 0) {
    return(rep(0, length(x)))
  }
  (x - mu) / sigma
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

make_missingness_table <- function(df, block_var, group_label) {
  df %>%
    group_by(event_time = relative_year) %>%
    summarise(
      n_rows_window = n(),
      n_blocks_window = n_distinct(.data[[block_var]]),
      n_rows_missing_outcome = sum(is.na(outcome_required)),
      n_rows_missing_geo_fe = sum(is.na(geo_fe_required)),
      n_rows_missing_controls = sum(control_missing),
      n_rows_nonpositive_for_log = sum(nonpositive_for_log),
      n_rows_kept = sum(analysis_keep),
      n_blocks_kept = n_distinct(.data[[block_var]][analysis_keep]),
      .groups = "drop"
    ) %>%
    mutate(group = group_label) %>%
    select(group, everything())
}

write_panelview_plot <- function(path, expr) {
  grDevices::pdf(path, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  set.seed(123)
  force(expr)
}

summarize_network <- function(network_obj, network_data, analysis_data, block_var, reference_metadata) {
  singletons_df <- if (is.null(network_obj$singletons)) tibble() else as_tibble(network_obj$singletons)
  multi_edges_df <- if (is.null(network_obj$multi_edges)) tibble() else as_tibble(network_obj$multi_edges)
  deg <- tryCatch(igraph::degree(network_obj$graph), error = function(e) numeric(0))

  tibble(
    panel_mode = PANEL_MODE,
    outcome_family = OUTCOME_FAMILY,
    date_basis = DATE_BASIS,
    model_type = MODEL_TYPE,
    treatment_type = TREATMENT_TYPE,
    weighting = WEIGHTING,
    bandwidth = BANDWIDTH,
    geo_fe_level = GEO_FE_LEVEL,
    control_spec = CONTROL_SPEC,
    analysis_n = nrow(analysis_data),
    analysis_blocks = n_distinct(analysis_data[[block_var]]),
    network_block_nodes = n_distinct(network_data$block_fe),
    network_geo_time_nodes = n_distinct(network_data$geo_time_fe),
    n_connected_components = ifelse(is.null(network_obj$n_components), NA_integer_, as.integer(network_obj$n_components)),
    n_singleton_nodes = nrow(singletons_df),
    n_singleton_block_nodes = if ("singleton_fe" %in% names(singletons_df)) sum(singletons_df$singleton_fe == "block_fe", na.rm = TRUE) else NA_integer_,
    n_singleton_geo_time_nodes = if ("singleton_fe" %in% names(singletons_df)) sum(singletons_df$singleton_fe == "geo_time_fe", na.rm = TRUE) else NA_integer_,
    n_duplicate_fe_combinations = nrow(multi_edges_df),
    duplicate_observation_count = if ("count" %in% names(multi_edges_df)) sum(pmax(multi_edges_df$count - 1, 0), na.rm = TRUE) else 0,
    degree_min = if (length(deg) == 0) NA_real_ else min(deg),
    degree_p25 = if (length(deg) == 0) NA_real_ else as.numeric(quantile(deg, 0.25, names = FALSE)),
    degree_median = if (length(deg) == 0) NA_real_ else as.numeric(stats::median(deg)),
    degree_mean = if (length(deg) == 0) NA_real_ else mean(deg),
    degree_p75 = if (length(deg) == 0) NA_real_ else as.numeric(quantile(deg, 0.75, names = FALSE)),
    degree_max = if (length(deg) == 0) NA_real_ else max(deg),
    reference_metadata_found = !is.null(reference_metadata),
    reference_analysis_n = if (is.null(reference_metadata)) NA_integer_ else reference_metadata$analysis_n[[1]],
    reference_analysis_blocks = if (is.null(reference_metadata)) NA_integer_ else reference_metadata$analysis_blocks[[1]],
    analysis_n_matches_reference = if (is.null(reference_metadata)) NA else nrow(analysis_data) == reference_metadata$analysis_n[[1]],
    analysis_blocks_matches_reference = if (is.null(reference_metadata)) NA else n_distinct(analysis_data[[block_var]]) == reference_metadata$analysis_blocks[[1]]
  )
}

outcome_catalog <- tibble(
  outcome_family = c("new_construction", "new_construction", "high_discretion", "high_discretion", "unit_increase", "unit_increase"),
  date_basis = c("issue", "application", "issue", "application", "issue", "application"),
  count_var = c(
    "n_new_construction_issue",
    "n_new_construction_application",
    "n_high_discretion_issue",
    "n_high_discretion_application",
    "n_unit_increase_issue",
    "n_unit_increase_application"
  ),
  binary_var = c(
    "has_new_construction_issue",
    "has_new_construction_application",
    "has_high_discretion_issue",
    "has_high_discretion_application",
    "has_unit_increase_issue",
    "has_unit_increase_application"
  )
)

outcome_row <- outcome_catalog %>%
  filter(outcome_family == OUTCOME_FAMILY, date_basis == DATE_BASIS)
if (nrow(outcome_row) != 1) {
  stop("Failed to resolve permit outcome selector.", call. = FALSE)
}

base_outcome_var <- if (MODEL_TYPE == "binary") outcome_row$binary_var[[1]] else outcome_row$count_var[[1]]

panel_input <- switch(PANEL_MODE,
  "stacked_implementation" = "../input/permit_block_year_panel.parquet",
  "cohort_2015" = "../input/permit_block_year_panel_2015.parquet",
  "cohort_2023" = "../input/permit_block_year_panel_2023.parquet"
)

suffix <- sprintf(
  "yearly_%s_%s_%s_%s_%s_%s_%dft_within_block_full_clust_block",
  PANEL_MODE,
  OUTCOME_FAMILY,
  DATE_BASIS,
  MODEL_TYPE,
  TREATMENT_TYPE,
  WEIGHTING,
  as.integer(BANDWIDTH)
)
if (CONTROL_SPEC != "none") {
  suffix <- paste0(suffix, "_ctrl_", CONTROL_SPEC)
}
if (GEO_FE_LEVEL == "ward_pair") {
  suffix <- paste0(suffix, "_geo_wardpair")
}

panelview_treat_output <- sprintf("../output/panelview_treat_%s.pdf", suffix)
panelview_missing_output <- sprintf("../output/panelview_missing_%s.pdf", suffix)
panelview_network_output <- sprintf("../output/panelview_network_summary_%s.csv", suffix)
panelview_crosswalk_output <- sprintf("../output/panelview_support_crosswalk_%s.csv", suffix)

reference_support_path <- sprintf("../../run_event_study_permit/output/event_study_support_%s.csv", suffix)
reference_metadata_path <- sprintf("../../run_event_study_permit/output/event_study_metadata_%s.csv", suffix)

control_vars <- c(
  "baseline_median_income",
  "baseline_homeownership_rate",
  "baseline_share_bach_plus",
  "baseline_median_age",
  "baseline_population_density",
  "baseline_percent_black",
  "baseline_percent_hispanic"
)

message("\nLoading permit block-year panel...")
data <- read_parquet(panel_input) %>%
  filter(!is.na(strictness_change)) %>%
  filter(dist_ft <= BANDWIDTH) %>%
  filter(relative_year >= -5, relative_year <= 5)

stacked_mode <- grepl("^stacked_", PANEL_MODE)
block_var <- if (stacked_mode) "cohort_block_id" else "block_id"
geo_group_var <- if (stacked_mode) {
  if (GEO_FE_LEVEL == "segment") "cohort_segment" else "cohort_ward_pair"
} else {
  if (GEO_FE_LEVEL == "segment") "segment_id_cohort" else "ward_pair_id"
}

required_cols <- c(block_var, geo_group_var, "year", "relative_year", "treat", "strictness_change", base_outcome_var)
missing_cols <- setdiff(required_cols, names(data))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

data <- data %>%
  mutate(
    weight = if (WEIGHTING == "triangular") pmax(0, 1 - dist_ft / BANDWIDTH) else 1,
    treatment_stricter_continuous = pmax(strictness_change, 0),
    treatment_lenient_continuous = pmax(-strictness_change, 0),
    treatment_stricter_binary = as.integer(strictness_change > 0),
    treatment_lenient_binary = as.integer(strictness_change < 0),
    panel_treat_post = as.integer(treat == 1 & relative_year >= 0),
    geo_fe_required = case_when(
      is.na(.data[[geo_group_var]]) ~ NA_character_,
      as.character(.data[[geo_group_var]]) == "" ~ NA_character_,
      TRUE ~ as.character(.data[[geo_group_var]])
    ),
    geo_fe_required_flag = case_when(
      is.na(.data[[geo_group_var]]) ~ NA_real_,
      as.character(.data[[geo_group_var]]) == "" ~ NA_real_,
      TRUE ~ 1
    )
  )

if (CONTROL_SPEC == "baseline_demographics") {
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
    left_join(baseline_controls, by = c("block_group_id", "baseline_year")) %>%
    mutate(across(all_of(control_vars), safe_scale, .names = "{.col}_z"))
}

data <- data %>%
  mutate(
    nonpositive_for_log = MODEL_TYPE == "log" & !is.na(.data[[base_outcome_var]]) & .data[[base_outcome_var]] <= 0,
    outcome_required = case_when(
      MODEL_TYPE == "log" & !is.na(.data[[base_outcome_var]]) & .data[[base_outcome_var]] > 0 ~ as.numeric(.data[[base_outcome_var]]),
      MODEL_TYPE == "log" ~ NA_real_,
      TRUE ~ as.numeric(.data[[base_outcome_var]])
    )
  )

if (CONTROL_SPEC == "baseline_demographics") {
  data <- data %>%
    mutate(control_missing = !stats::complete.cases(dplyr::select(., all_of(control_vars))))
} else {
  data <- data %>%
    mutate(control_missing = FALSE)
}

data <- data %>%
  mutate(
    estimation_ready_flag = if_else(!is.na(geo_fe_required) & !control_missing, 1, NA_real_),
    analysis_keep = !is.na(outcome_required) & !is.na(geo_fe_required) & !control_missing
  )

analysis_data <- data %>%
  filter(analysis_keep)

if (nrow(analysis_data) == 0) {
  stop("No observations remain after applying the requested permit panelView diagnostic restrictions.", call. = FALSE)
}

time_fe_var <- "year"
event_var <- "relative_year"
segment_var <- geo_group_var

if (TREATMENT_TYPE == "continuous") {
  support_by_event_time <- make_support_table(
    df = analysis_data,
    event_var = event_var,
    time_fe_var = time_fe_var,
    fe_group_var = geo_group_var,
    block_var = block_var,
    segment_var = segment_var,
    outcome_var = base_outcome_var,
    treatment_var = "strictness_change",
    min_period = -5,
    max_period = 5
  ) %>%
    mutate(group = "All blocks", .before = 1)

  missingness_by_event_time <- make_missingness_table(data, block_var, "All blocks")
} else if (TREATMENT_TYPE == "binary_direction") {
  data_stricter <- data %>% filter(treatment_lenient_binary == 0)
  data_lenient <- data %>% filter(treatment_stricter_binary == 0)
  analysis_stricter <- analysis_data %>% filter(treatment_lenient_binary == 0)
  analysis_lenient <- analysis_data %>% filter(treatment_stricter_binary == 0)

  support_by_event_time <- bind_rows(
    make_support_table(
      df = analysis_stricter,
      event_var = event_var,
      time_fe_var = time_fe_var,
      fe_group_var = geo_group_var,
      block_var = block_var,
      segment_var = segment_var,
      outcome_var = base_outcome_var,
      treatment_var = "treatment_stricter_binary",
      min_period = -5,
      max_period = 5
    ) %>%
      mutate(group = "Moved to Stricter", .before = 1),
    make_support_table(
      df = analysis_lenient,
      event_var = event_var,
      time_fe_var = time_fe_var,
      fe_group_var = geo_group_var,
      block_var = block_var,
      segment_var = segment_var,
      outcome_var = base_outcome_var,
      treatment_var = "treatment_lenient_binary",
      min_period = -5,
      max_period = 5
    ) %>%
      mutate(group = "Moved to More Lenient", .before = 1)
  )

  missingness_by_event_time <- bind_rows(
    make_missingness_table(data_stricter, block_var, "Moved to Stricter"),
    make_missingness_table(data_lenient, block_var, "Moved to More Lenient")
  )
} else {
  support_by_event_time <- bind_rows(
    make_support_table(
      df = analysis_data,
      event_var = event_var,
      time_fe_var = time_fe_var,
      fe_group_var = geo_group_var,
      block_var = block_var,
      segment_var = segment_var,
      outcome_var = base_outcome_var,
      treatment_var = "treatment_stricter_continuous",
      min_period = -5,
      max_period = 5
    ) %>%
      mutate(group = "Moved to Stricter", .before = 1),
    make_support_table(
      df = analysis_data,
      event_var = event_var,
      time_fe_var = time_fe_var,
      fe_group_var = geo_group_var,
      block_var = block_var,
      segment_var = segment_var,
      outcome_var = base_outcome_var,
      treatment_var = "treatment_lenient_continuous",
      min_period = -5,
      max_period = 5
    ) %>%
      mutate(group = "Moved to More Lenient", .before = 1)
  )

  missingness_by_event_time <- bind_rows(
    make_missingness_table(data, block_var, "Moved to Stricter"),
    make_missingness_table(data, block_var, "Moved to More Lenient")
  )
}

reference_metadata <- if (file.exists(reference_metadata_path)) {
  read_csv(reference_metadata_path, show_col_types = FALSE)
} else {
  NULL
}

reference_support <- if (file.exists(reference_support_path)) {
  ref <- read_csv(reference_support_path, show_col_types = FALSE)
  if (!"group" %in% names(ref)) {
    ref <- ref %>% mutate(group = "All blocks", .before = 1)
  }
  ref
} else {
  NULL
}

network_data <- analysis_data %>%
  transmute(
    block_fe = as.character(.data[[block_var]]),
    geo_time_fe = paste0(as.character(.data[[geo_group_var]]), "::", year)
  )

temp_network_plot <- tempfile(fileext = ".pdf")
grDevices::pdf(temp_network_plot, width = 8, height = 6)
network_obj <- panelView::panelview(
  data = network_data,
  formula = ~1,
  index = c("block_fe", "geo_time_fe"),
  type = "network",
  main = sprintf("Network diagnostic: %s", suffix)
)
grDevices::dev.off()
unlink(temp_network_plot)

network_summary <- summarize_network(network_obj, network_data, analysis_data, block_var, reference_metadata) %>%
  mutate(
    n_rows_window = nrow(data),
    n_rows_dropped_for_outcome = sum(is.na(data$outcome_required)),
    n_rows_dropped_for_geo_fe = sum(is.na(data$geo_fe_required)),
    n_rows_dropped_for_controls = sum(data$control_missing),
    n_rows_nonpositive_for_log = sum(data$nonpositive_for_log)
  )

support_crosswalk <- support_by_event_time %>%
  left_join(missingness_by_event_time, by = c("group", "event_time")) %>%
  mutate(
    analysis_n = network_summary$analysis_n[[1]],
    analysis_blocks = network_summary$analysis_blocks[[1]],
    network_n_components = network_summary$n_connected_components[[1]],
    network_n_singleton_nodes = network_summary$n_singleton_nodes[[1]],
    network_n_duplicate_fe_combinations = network_summary$n_duplicate_fe_combinations[[1]]
  )

if (!is.null(reference_support)) {
  support_crosswalk <- support_crosswalk %>%
    left_join(
      reference_support %>%
        rename_with(~ paste0("reference_", .x), -c(group, event_time)),
      by = c("group", "event_time")
    ) %>%
    mutate(
      matches_reference_n_obs = if ("reference_n_obs" %in% names(.)) n_obs == reference_n_obs else NA,
      matches_reference_n_blocks = if ("reference_n_blocks" %in% names(.)) n_blocks == reference_n_blocks else NA,
      matches_reference_identifying_support = if ("reference_has_identifying_support" %in% names(.)) has_identifying_support == reference_has_identifying_support else NA
    )
}

missing_formula <- outcome_required ~ estimation_ready_flag
missing_plot_data <- data %>%
  group_by(unit_id_for_missing_plot = .data[[block_var]]) %>%
  mutate(unit_has_missing = any(is.na(outcome_required) | is.na(estimation_ready_flag))) %>%
  ungroup() %>%
  filter(unit_has_missing)

message("Writing treatment-status panel plot...")
write_panelview_plot(panelview_treat_output, {
  panelView::panelview(
    data = analysis_data,
    formula = outcome_required ~ panel_treat_post,
    index = c(block_var, "year"),
    type = "treat",
    by.timing = TRUE,
    pre.post = TRUE,
    collapse.history = TRUE,
    xlab = "Year",
    ylab = "Block history",
    main = sprintf("Permit treatment support: %s", suffix)
  )
})

message("Writing missingness panel plot...")
if (nrow(missing_plot_data) == 0) {
  ggsave(
    panelview_missing_output,
    ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "No missingness in estimation variables for this specification.") +
      xlim(0, 1) +
      ylim(0, 1) +
      theme_void(),
    width = 11,
    height = 8.5,
    bg = "white"
  )
} else {
  write_panelview_plot(panelview_missing_output, {
    panelView::panelview(
      data = missing_plot_data,
      formula = missing_formula,
      index = c(block_var, "year"),
      type = "missing",
      by.timing = TRUE,
      display.all = TRUE,
      xlab = "Year",
      ylab = "Block",
      main = sprintf("Permit estimation missingness: %s", suffix)
    )
  })
}

write_csv(network_summary, panelview_network_output)
write_csv(support_crosswalk, panelview_crosswalk_output)

message(sprintf("Saved: %s", panelview_treat_output))
message(sprintf("Saved: %s", panelview_missing_output))
message(sprintf("Saved: %s", panelview_network_output))
message(sprintf("Saved: %s", panelview_crosswalk_output))
message("Done!")
