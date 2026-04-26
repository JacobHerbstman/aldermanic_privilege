source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_guangbin/code")
# panel_path <- "../output/rental_listing_panel_2023_guangbin.parquet"
# sample_filter <- "multifamily_only"
# coef_out <- "../output/event_study_coefficients_guangbin_quarterly_mf.csv"
# support_out <- "../output/event_study_support_guangbin_quarterly_mf.csv"
# pretrend_out <- "../output/event_study_pretrend_guangbin_quarterly_mf.csv"
# metadata_out <- "../output/event_study_metadata_guangbin_quarterly_mf.csv"
# summary_out <- "../output/event_study_summary_guangbin_quarterly_mf.csv"
# plot_out <- "../output/event_study_guangbin_quarterly_mf.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(panel_path, sample_filter, coef_out, support_out, pretrend_out, metadata_out, summary_out, plot_out)
}

if (length(args) != 8) {
  stop("FATAL: Script requires args: <panel_path> <sample_filter> <coef_out> <support_out> <pretrend_out> <metadata_out> <summary_out> <plot_out>", call. = FALSE)
}
panel_path <- args[1]
sample_filter <- args[2]
coef_out <- args[3]
support_out <- args[4]
pretrend_out <- args[5]
metadata_out <- args[6]
summary_out <- args[7]
plot_out <- args[8]

if (!sample_filter %in% c("multifamily_only", "full_sample")) {
  stop("sample_filter must be one of: multifamily_only, full_sample", call. = FALSE)
}

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
      n_blocks = n_distinct(block_id),
      n_segments = n_distinct(.data[[segment_var]][!is.na(.data[[segment_var]])]),
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

extract_plot_data <- function(model, support_by_event_time, min_period, max_period) {
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
      is_reference = is_ref
    ) %>%
    filter(event_time >= min_period, event_time <= max_period) %>%
    filter(is_reference | event_time %in% supported_periods) %>%
    left_join(support_by_event_time, by = "event_time") %>%
    mutate(
      estimate_pct = 100 * estimate,
      ci_low_pct = 100 * ci_low,
      ci_high_pct = 100 * ci_high
    )
}

compute_pretrend_test <- function(model, plot_data) {
  lead_terms <- plot_data %>%
    filter(event_time <= -2, !is_reference) %>%
    pull(estimate_name_raw)

  if (length(lead_terms) == 0) {
    return(tibble(
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
    n_leads = length(lead_terms),
    min_lead = min(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    max_lead = max(plot_data$event_time[plot_data$event_time <= -2 & !plot_data$is_reference]),
    wald_stat = if (is.null(joint_test)) NA_real_ else joint_test$stat,
    p_value = if (is.null(joint_test)) NA_real_ else joint_test$p,
    df1 = if (is.null(joint_test)) NA_real_ else joint_test$df1,
    df2 = if (is.null(joint_test)) NA_real_ else joint_test$df2
  )
}

make_plot <- function(plot_data, sample_label, chosen_trim_spec) {
  trim_label <- if (chosen_trim_spec == "strict_p1") "1/99" else "2.5/97.5"
  ggplot(plot_data, aes(x = event_time, y = estimate_pct)) +
    geom_hline(yintercept = 0, color = "gray40", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "gray60", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_low_pct, ymax = ci_high_pct), fill = "#009E73", alpha = 0.2, color = NA) +
    geom_line(color = "#009E73", linewidth = 1) +
    geom_point(color = "#009E73", size = 2.4) +
    scale_x_continuous(breaks = seq(-8, 8, by = 1)) +
    scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
    labs(
      title = sprintf("Quarterly Rent Event Study, 2023 Cohort: %s", sample_label),
      subtitle = sprintf("Guangbin-style posting weights + %s trim on sqft and rent per sqft, 2021 Q2 to 2025 Q2", trim_label),
      x = "Quarters relative to May 2023 switch",
      y = "Effect on trimmed rents"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
    )
}

panel_raw <- read_parquet(panel_path) %>%
  mutate(file_date = as.Date(file_date)) %>%
  filter(
    year >= 2021,
    year <= 2025,
    !is.na(analysis_key),
    !is.na(posting_weight),
    !is.na(segment_id_cohort),
    !is.na(segment_side),
    dist_ft <= 1000,
    relative_quarter >= -8,
    relative_quarter <= 8,
    !is.na(strictness_change)
  )

if (sample_filter == "multifamily_only") {
  panel_raw <- panel_raw %>% filter(building_type_clean == "multi_family")
}

chosen_trim_spec <- unique(panel_raw$chosen_trim_spec[!is.na(panel_raw$chosen_trim_spec)])
if (length(chosen_trim_spec) != 1) {
  stop("Expected exactly one chosen_trim_spec in the Guangbin panel.", call. = FALSE)
}
chosen_trim_spec <- chosen_trim_spec[1]

share_keep_trimmed_sample_full <- mean(panel_raw$keep_trimmed_sample == 1, na.rm = TRUE)

panel <- panel_raw %>%
  filter(
    keep_trimmed_sample == 1,
    !is.na(rent_price_trimmed),
    rent_price_trimmed > 0,
    !is.na(log_rent_price_trimmed),
    !is.na(log_sqft_trimmed)
  ) %>%
  mutate(
    year_quarter = factor(year_quarter),
    building_type_factor = factor(
      coalesce(building_type_clean, "other"),
      levels = c("multi_family", "condo", "single_family", "townhouse", "other")
    ),
    tri_weight = pmax(0, 1 - dist_ft / 1000),
    analysis_weight = tri_weight * posting_weight
  ) %>%
  filter(analysis_weight > 0)

hedonic_vars <- c("log_sqft_trimmed", "log_beds_clean", "log_baths_clean")
if (n_distinct(panel$building_type_clean[!is.na(panel$building_type_clean)]) > 1) {
  control_rhs <- paste(c("building_type_factor", hedonic_vars), collapse = " + ")
  complete_idx <- complete.cases(panel[, c("log_sqft_trimmed", "log_beds_clean", "log_baths_clean", "building_type_factor")])
} else {
  control_rhs <- paste(hedonic_vars, collapse = " + ")
  complete_idx <- complete.cases(panel[, hedonic_vars])
}
panel <- panel[complete_idx, ]

if (nrow(panel) == 0) {
  stop("No observations remain after Guangbin-style filters.", call. = FALSE)
}

fe_formula <- "segment_side + segment_id_cohort^year_quarter"
cluster_formula <- ~block_id + segment_id_cohort
event_var <- "relative_quarter"
time_fe_var <- "year_quarter"
fe_group_var <- "segment_id_cohort"
fe_side_var <- "segment_side"
segment_var <- "segment_id_cohort"
min_period <- -8
max_period <- 8

support_by_event_time <- make_support_table(panel, event_var, time_fe_var, fe_group_var, fe_side_var, segment_var, min_period, max_period)

formula_str <- sprintf(
  "log_rent_price_trimmed ~ i(relative_quarter, strictness_change, ref = -1) + %s | %s",
  control_rhs,
  fe_formula
)

message(sprintf("Running Guangbin event study with %s observations", format(nrow(panel), big.mark = ",")))
message(sprintf("Sample filter: %s", sample_filter))
message(sprintf("Formula: %s", formula_str))

model <- feols(
  as.formula(formula_str),
  data = panel,
  weights = ~analysis_weight,
  cluster = cluster_formula
)

plot_data <- extract_plot_data(model, support_by_event_time, min_period, max_period)
if (is.null(plot_data) || nrow(plot_data) == 0) {
  stop("No supported coefficients were available for the Guangbin event study.", call. = FALSE)
}

coefficients <- plot_data %>%
  select(
    event_time, estimate, std_error, ci_low, ci_high, estimate_pct, ci_low_pct, ci_high_pct,
    estimate_name, estimate_name_raw, is_reference, n_obs, n_treated, n_control,
    n_blocks, n_segments, n_fe_group_time_cells, n_identifying_fe_group_time_cells, n_identifying_fe_groups,
    has_treated_and_control, has_identifying_support
  )

pretrend <- compute_pretrend_test(model, plot_data)

sample_label <- if (sample_filter == "multifamily_only") "Multifamily only" else "Full sample"
metadata <- tibble(
  sample_filter = sample_filter,
  sample_label = sample_label,
  chosen_trim_spec = chosen_trim_spec,
  analysis_n = nrow(panel),
  treated_n = sum(panel$treat == 1, na.rm = TRUE),
  control_n = sum(panel$treat == 0, na.rm = TRUE),
  effective_weight_n = sum(panel$analysis_weight, na.rm = TRUE),
  mean_posting_weight = mean(panel$posting_weight, na.rm = TRUE),
  share_unit_id_rows = mean(panel$key_source == "unit_id", na.rm = TRUE),
  share_fingerprint_rows = mean(panel$key_source == "fingerprint", na.rm = TRUE),
  share_keep_trimmed_sample_full = share_keep_trimmed_sample_full,
  share_rent_winsorized = mean(panel$rent_winsorized_flag == 1, na.rm = TRUE),
  share_sqft_winsorized = mean(panel$sqft_winsorized_flag == 1, na.rm = TRUE),
  plotted_window = "[-8, 8]",
  supported_event_times = paste(coefficients$event_time[coefficients$has_identifying_support & !coefficients$is_reference], collapse = "|")
)

summary_row <- tibble(
  sample_filter = sample_filter,
  sample_label = sample_label,
  chosen_trim_spec = chosen_trim_spec,
  analysis_n = nrow(panel),
  treated_n = sum(panel$treat == 1, na.rm = TRUE),
  control_n = sum(panel$treat == 0, na.rm = TRUE),
  effective_weight_n = sum(panel$analysis_weight, na.rm = TRUE),
  share_unit_id_rows = mean(panel$key_source == "unit_id", na.rm = TRUE),
  share_fingerprint_rows = mean(panel$key_source == "fingerprint", na.rm = TRUE),
  share_keep_trimmed_sample_full = share_keep_trimmed_sample_full,
  share_rent_winsorized = mean(panel$rent_winsorized_flag == 1, na.rm = TRUE),
  pretrend_p_value = pretrend$p_value[1],
  mean_post_effect_pct = mean(coefficients$estimate_pct[coefficients$event_time >= 0 & !coefficients$is_reference], na.rm = TRUE),
  terminal_post_effect_pct = coefficients$estimate_pct[coefficients$event_time == 8][1],
  effect_at_q0_pct = coefficients$estimate_pct[coefficients$event_time == 0][1],
  effect_at_q4_pct = coefficients$estimate_pct[coefficients$event_time == 4][1],
  effect_at_q8_pct = coefficients$estimate_pct[coefficients$event_time == 8][1],
  supported_event_times = paste(coefficients$event_time[coefficients$has_identifying_support & !coefficients$is_reference], collapse = "|")
)

write_csv(coefficients, coef_out)
write_csv(support_by_event_time, support_out)
write_csv(pretrend, pretrend_out)
write_csv(metadata, metadata_out)
write_csv(summary_row, summary_out)
ggsave(plot_out, make_plot(plot_data, sample_label, chosen_trim_spec), width = 7.2, height = 4.8, bg = "white")
