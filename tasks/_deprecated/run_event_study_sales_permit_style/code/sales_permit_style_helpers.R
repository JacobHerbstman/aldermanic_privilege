prepare_block_year_cohort <- function(df, timing_mode, cohort_value, relative_shift) {
  suffix <- if (timing_mode == "2015") "2015" else "2023"

  df %>%
    transmute(
      cohort = cohort_value,
      block_id,
      year,
      ward_pair_id,
      mean_dist_to_boundary,
      has_sales,
      n_sales,
      mean_price,
      valid = .data[[paste0("valid_", suffix)]],
      switched = .data[[paste0("switched_", suffix)]],
      strictness_change_raw = .data[[paste0("strictness_change_", suffix)]],
      relative_year_raw = .data[[paste0("relative_year_", suffix)]]
    ) %>%
    filter(valid) %>%
    mutate(
      relative_year = relative_year_raw + relative_shift,
      treat = as.integer(switched),
      strictness_change = if_else(treat == 1L, strictness_change_raw, 0),
      cohort_block_id = paste(cohort, block_id, sep = "_"),
      cohort_ward_pair = paste(cohort, ward_pair_id, sep = "_")
    ) %>%
    select(
      cohort, block_id, cohort_block_id, year, ward_pair_id, cohort_ward_pair,
      mean_dist_to_boundary, has_sales, n_sales, mean_price,
      treat, strictness_change, relative_year
    )
}
