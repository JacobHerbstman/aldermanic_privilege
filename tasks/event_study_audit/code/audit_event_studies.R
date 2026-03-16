source("../../setup_environment/code/packages.R")

read_spec_bundle <- function(spec_name, metadata_file, support_file, pretrend_file, coefficients_file) {
  metadata <- read_csv(metadata_file, show_col_types = FALSE)
  support <- read_csv(support_file, show_col_types = FALSE)
  pretrend <- read_csv(pretrend_file, show_col_types = FALSE)
  coefficients <- read_csv(coefficients_file, show_col_types = FALSE)

  if ("plotted_supported_periods" %in% names(metadata)) {
    metadata <- metadata %>%
      mutate(plotted_supported_periods = as.character(plotted_supported_periods))
  }

  if ("contributing_cohorts" %in% names(support)) {
    support <- support %>%
      mutate(contributing_cohorts = as.character(contributing_cohorts))
  }

  list(
    spec_name = spec_name,
    metadata = metadata,
    support = support,
    pretrend = pretrend,
    coefficients = coefficients
  )
}

spec_summary <- function(bundle, selected_baseline) {
  post_coefficients <- bundle$coefficients %>%
    filter(event_time >= 0, !is_reference)

  pretrend_p_value <- bundle$pretrend %>%
    filter(row_number() == 1) %>%
    pull(p_value)

  supported_event_times <- bundle$metadata$plotted_supported_periods[1]

  tibble(
    spec_name = bundle$spec_name,
    selected_baseline = selected_baseline,
    panel_mode = bundle$metadata$panel_mode[1],
    include_hedonics = bundle$metadata$include_hedonics[1],
    analysis_n = bundle$metadata$analysis_n[1],
    treated_n = bundle$metadata$treated_n[1],
    control_n = bundle$metadata$control_n[1],
    plotted_window = sprintf("[%s, %s]", bundle$metadata$plotted_min_event_time[1], bundle$metadata$plotted_max_event_time[1]),
    supported_event_times = ifelse(
      is.na(supported_event_times),
      NA_character_,
      as.character(supported_event_times)
    ),
    cohort_support = paste(unique(bundle$support$contributing_cohorts), collapse = " | "),
    pretrend_p_value = if (length(pretrend_p_value) == 0) NA_real_ else as.numeric(pretrend_p_value[1]),
    mean_post_effect_pct = if (nrow(post_coefficients) == 0) NA_real_ else mean(post_coefficients$estimate_pct, na.rm = TRUE),
    terminal_post_effect_pct = if (nrow(post_coefficients) == 0) NA_real_ else post_coefficients %>%
      filter(event_time == max(event_time)) %>%
      summarise(value = mean(estimate_pct, na.rm = TRUE)) %>%
      pull(value)
  )
}

support_output <- function(specs, selected_name) {
  bind_rows(lapply(specs, function(bundle) {
    bundle$support %>%
      mutate(
        spec_name = bundle$spec_name,
        selected_baseline = bundle$spec_name == selected_name
      )
  }))
}

pretrend_output <- function(specs, selected_name) {
  bind_rows(lapply(specs, function(bundle) {
    bundle$pretrend %>%
      mutate(
        spec_name = bundle$spec_name,
        selected_baseline = bundle$spec_name == selected_name
      )
  }))
}

write_rental_summary <- function(spec_comparison, repeat_diagnostics, assignment_stability) {
  main_spec <- spec_comparison %>% filter(spec_name == "cohort_2023_main")
  no_hedonics_spec <- spec_comparison %>% filter(spec_name == "cohort_2023_no_hedonics")
  cohort_2015_spec <- spec_comparison %>% filter(spec_name == "cohort_2015_candidate")
  stacked_spec <- spec_comparison %>% filter(spec_name == "stacked_overlap_candidate")
  repeat_row <- repeat_diagnostics %>% filter(panel_mode == "cohort_2023")
  assignment_row <- assignment_stability %>% filter(panel_mode == "cohort_2023")

  lines <- c(
    "Selected baseline: cohort_2023_main",
    sprintf(
      "Main 2023 rental baseline: n = %s, pretrend p-value = %.3f, mean post effect = %.2f%%, supported event times = %s.",
      format(main_spec$analysis_n[1], big.mark = ","),
      main_spec$pretrend_p_value[1],
      main_spec$mean_post_effect_pct[1],
      main_spec$supported_event_times[1]
    ),
    sprintf(
      "No-hedonics 2023 comparison: n = %s versus %s with hedonics.",
      format(no_hedonics_spec$analysis_n[1], big.mark = ","),
      format(main_spec$analysis_n[1], big.mark = ",")
    ),
    sprintf(
      "2015-only rental candidate: n = %s, supported event times = %s. It was not promoted because the listing data do not supply a meaningful pre-period beyond the reference year.",
      format(cohort_2015_spec$analysis_n[1], big.mark = ","),
      cohort_2015_spec$supported_event_times[1]
    ),
    sprintf(
      "Stacked overlap robustness: n = %s, pretrend p-value = %s, supported event times = %s.",
      format(stacked_spec$analysis_n[1], big.mark = ","),
      ifelse(is.na(stacked_spec$pretrend_p_value[1]), "NA", sprintf("%.3f", stacked_spec$pretrend_p_value[1])),
      stacked_spec$supported_event_times[1]
    ),
    "The long-window stacked rental event study was not promoted because its long leads and later lags come from different cohorts. The overlap-only stacked version is retained only as a robustness check.",
    sprintf(
      "Repeat-listing diagnostics for the promoted rental baseline panel: mean observations per listing id = %.2f, repeated-id share = %.3f.",
      repeat_row$mean_obs_per_id[1],
      repeat_row$share_repeated_ids[1]
    ),
    sprintf(
      "Assignment stability for the promoted rental baseline panel: segment coverage = %.1f%%, treated blocks with multiple ward pairs = %.1f%%.",
      assignment_row$segment_coverage_pct[1],
      assignment_row$pct_blocks_multi_ward_pair[1]
    )
  )

  writeLines(lines, "../output/rental_audit_summary.txt")
}

write_sales_summary <- function(spec_comparison, assignment_stability) {
  main_spec <- spec_comparison %>% filter(spec_name == "cohort_2015_main")
  no_hedonics_spec <- spec_comparison %>% filter(spec_name == "cohort_2015_no_hedonics")
  cohort_2012_spec <- spec_comparison %>% filter(spec_name == "cohort_2012_announcement")
  stacked_announcement_spec <- spec_comparison %>% filter(spec_name == "stacked_announcement_candidate")
  stacked_implementation_spec <- spec_comparison %>% filter(spec_name == "stacked_implementation_candidate")
  assignment_row <- assignment_stability %>% filter(panel_mode == "cohort_2015")

  lines <- c(
    "Selected baseline: cohort_2015_main",
    sprintf(
      "Main 2015 sales baseline: n = %s, pretrend p-value = %.3f, mean post effect = %.2f%%, supported event times = %s.",
      format(main_spec$analysis_n[1], big.mark = ","),
      main_spec$pretrend_p_value[1],
      main_spec$mean_post_effect_pct[1],
      main_spec$supported_event_times[1]
    ),
    sprintf(
      "No-hedonics 2015 comparison: n = %s versus %s with hedonics.",
      format(no_hedonics_spec$analysis_n[1], big.mark = ","),
      format(main_spec$analysis_n[1], big.mark = ",")
    ),
    sprintf(
      "2012 announcement candidate: n = %s, pretrend p-value = %.3f, mean post effect = %.2f%%.",
      format(cohort_2012_spec$analysis_n[1], big.mark = ","),
      cohort_2012_spec$pretrend_p_value[1],
      cohort_2012_spec$mean_post_effect_pct[1]
    ),
    sprintf(
      "Stacked announcement candidate: n = %s, pretrend p-value = %.3f.",
      format(stacked_announcement_spec$analysis_n[1], big.mark = ","),
      stacked_announcement_spec$pretrend_p_value[1]
    ),
    sprintf(
      "Stacked implementation candidate: n = %s, pretrend p-value = %.3f.",
      format(stacked_implementation_spec$analysis_n[1], big.mark = ","),
      stacked_implementation_spec$pretrend_p_value[1]
    ),
    "The promoted sales baseline remains the 2015 implementation cohort because it provides the cleanest full pre/post dynamic support within the current transaction data.",
    sprintf(
      "Assignment stability for the promoted sales baseline panel: segment coverage = %.1f%%, treated blocks with multiple ward pairs = %.1f%%.",
      assignment_row$segment_coverage_pct[1],
      assignment_row$pct_blocks_multi_ward_pair[1]
    )
  )

  writeLines(lines, "../output/sales_audit_summary.txt")
}

rental_specs <- list(
  read_spec_bundle(
    "cohort_2023_main",
    "../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv",
    "../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv",
    "../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv",
    "../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv"
  ),
  read_spec_bundle(
    "cohort_2023_no_hedonics",
    "../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv",
    "../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv",
    "../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv",
    "../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv"
  ),
  read_spec_bundle(
    "cohort_2015_candidate",
    "../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv",
    "../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv",
    "../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv",
    "../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv"
  ),
  read_spec_bundle(
    "stacked_overlap_candidate",
    "../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv",
    "../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv",
    "../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv",
    "../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv"
  )
)

sales_specs <- list(
  read_spec_bundle(
    "cohort_2015_main",
    "../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv",
    "../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv",
    "../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv",
    "../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv"
  ),
  read_spec_bundle(
    "cohort_2015_no_hedonics",
    "../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv",
    "../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv",
    "../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv",
    "../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv"
  ),
  read_spec_bundle(
    "cohort_2012_announcement",
    "../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv",
    "../input/event_study_support_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv",
    "../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv",
    "../input/event_study_coefficients_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv"
  ),
  read_spec_bundle(
    "stacked_announcement_candidate",
    "../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv",
    "../input/event_study_support_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv",
    "../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv",
    "../input/event_study_coefficients_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv"
  ),
  read_spec_bundle(
    "stacked_implementation_candidate",
    "../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv",
    "../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv",
    "../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv",
    "../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv"
  )
)

rental_spec_comparison <- bind_rows(lapply(rental_specs, spec_summary, selected_baseline = "cohort_2023_main"))
sales_spec_comparison <- bind_rows(lapply(sales_specs, spec_summary, selected_baseline = "cohort_2015_main"))

write_csv(
  support_output(rental_specs, "cohort_2023_main"),
  "../output/rental_support_by_event_time.csv"
)
write_csv(
  read_csv("../input/rental_listing_panel_support_by_calendar_time.csv", show_col_types = FALSE) %>%
    filter(panel_mode %in% c("cohort_2015", "cohort_2023", "stacked_implementation")),
  "../output/rental_support_by_calendar_time.csv"
)
write_csv(pretrend_output(rental_specs, "cohort_2023_main"), "../output/rental_pretrend_tests.csv")
write_csv(rental_spec_comparison, "../output/rental_spec_comparison.csv")
write_rental_summary(
  rental_spec_comparison,
  read_csv("../input/rental_listing_panel_repeat_listing_diagnostics.csv", show_col_types = FALSE),
  read_csv("../input/rental_listing_panel_assignment_stability.csv", show_col_types = FALSE)
)

write_csv(
  support_output(sales_specs, "cohort_2015_main"),
  "../output/sales_support_by_event_time.csv"
)
write_csv(
  read_csv("../input/sales_transaction_panel_support_by_calendar_time.csv", show_col_types = FALSE) %>%
    filter(panel_mode %in% c("cohort_2012", "cohort_2015", "cohort_2023", "stacked_announcement", "stacked_implementation")),
  "../output/sales_support_by_calendar_time.csv"
)
write_csv(pretrend_output(sales_specs, "cohort_2015_main"), "../output/sales_pretrend_tests.csv")
write_csv(sales_spec_comparison, "../output/sales_spec_comparison.csv")
write_sales_summary(
  sales_spec_comparison,
  read_csv("../input/sales_transaction_panel_assignment_stability.csv", show_col_types = FALSE)
)
