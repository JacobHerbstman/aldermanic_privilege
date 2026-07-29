# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

panel <- read_parquet("../../../process_rent_data/output/chicago_rent_panel.parquet") %>%
  as_tibble()
quality <- read_parquet("../../renthub_quality_diagnostics/output/chicago_rent_panel_quality_flags.parquet") %>%
  as_tibble()
pre_scores <- read_parquet("../../../calculate_rent_distances/output/rent_pre_scores_full.parquet") %>%
  as_tibble()
scored <- read_parquet("../../../merge_rent_scores/output/rent_with_ward_distances_full_through2022.parquet") %>%
  as_tibble()
rent <- read_parquet("../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble()

for (x in list(panel, quality, pre_scores, scored, rent)) {
  if (anyDuplicated(x$rent_panel_id) > 0) {
    stop("A rental pipeline output is not unique by rent_panel_id.", call. = FALSE)
  }
}

rent <- rent %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    right = as.integer(signed_dist_ft >= 0),
    log_sqft_audit = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    beds_factor_audit = factor(beds),
    log_baths_audit = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
    building_type_factor_audit = factor(coalesce(building_type_clean, "other")),
    nearest_school_dist_kft_audit = nearest_school_dist_ft / 1000,
    nearest_park_dist_kft_audit = nearest_park_dist_ft / 1000,
    nearest_major_road_dist_kft_audit = nearest_major_road_dist_ft / 1000,
    nearest_cta_stop_dist_kft_audit = nearest_cta_stop_dist_ft / 1000,
    lake_michigan_dist_kft_audit = lake_michigan_dist_ft / 1000
  )

stages <- list(
  "Monthly cleaned panel" = panel$rent_panel_id,
  "Assigned to a ward boundary" = pre_scores$rent_panel_id,
  "Own and neighboring aldermen have unequal through-2022 scores" = scored$rent_panel_id,
  "Within 500 feet with segment and amenity measures" = rent$rent_panel_id,
  "Positive real rent, valid treatment and segment" = rent %>%
    filter(
      year >= 2014,
      year <= 2022,
      is.finite(rent_price),
      rent_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= 500,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair)
    ) %>%
    pull(rent_panel_id),
  "Clean geographic assignment" = rent %>%
    filter(
      year >= 2014,
      year <= 2022,
      is.finite(rent_price),
      rent_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= 500,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      flag_clean_location_sample
    ) %>%
    pull(rent_panel_id),
  "Nonnegative bedroom count (studios retained)" = rent %>%
    filter(
      year >= 2014,
      year <= 2022,
      is.finite(rent_price),
      rent_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= 500,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      flag_clean_location_sample,
      is.finite(beds),
      beds >= 0
    ) %>%
    pull(rent_panel_id),
  "Nonmissing positive square footage and bathrooms" = rent %>%
    filter(
      year >= 2014,
      year <= 2022,
      is.finite(rent_price),
      rent_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= 500,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      flag_clean_location_sample,
      is.finite(beds),
      beds >= 0,
      !is.na(log_sqft_audit),
      !is.na(log_baths_audit)
    ) %>%
    pull(rent_panel_id),
  "Complete amenity controls" = rent %>%
    filter(
      year >= 2014,
      year <= 2022,
      is.finite(rent_price),
      rent_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= 500,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      flag_clean_location_sample,
      is.finite(beds),
      beds >= 0,
      !is.na(log_sqft_audit),
      !is.na(log_baths_audit),
      if_all(
        all_of(c(
          "nearest_school_dist_kft_audit",
          "nearest_park_dist_kft_audit",
          "nearest_major_road_dist_kft_audit",
          "nearest_cta_stop_dist_kft_audit",
          "lake_michigan_dist_kft_audit"
        )),
        is.finite
      )
    ) %>%
    pull(rent_panel_id)
)

attrition <- tibble(
  stage = names(stages),
  n = vapply(stages, length, integer(1))
) %>%
  mutate(dropped_from_previous = lag(n) - n)
write_csv(attrition, "../output/rental_sample_attrition.csv")

model_data <- rent %>%
  filter(rent_panel_id %in% stages[["Complete amenity controls"]])

model <- feols(
  log(rent_price) ~ right + log_sqft_audit + beds_factor_audit + log_baths_audit +
    building_type_factor_audit + nearest_school_dist_kft_audit + nearest_park_dist_kft_audit +
    nearest_major_road_dist_kft_audit + nearest_cta_stop_dist_kft_audit +
    lake_michigan_dist_kft_audit | segment_id^year_month,
  data = model_data,
  cluster = ~segment_id
)
model_ct <- coeftable(model)
model_ids <- model_data$rent_panel_id[obs(model)]
model_sample <- model_data %>% filter(rent_panel_id %in% model_ids)

cell_support <- model_sample %>%
  count(segment_id, year_month, right, name = "n") %>%
  count(segment_id, year_month, name = "sides")

model_summary <- tibble(
  estimate = unname(model_ct["right", "Estimate"]),
  se = unname(model_ct["right", "Std. Error"]),
  p_value = unname(model_ct["right", "Pr(>|t|)"]),
  percent_effect = 100 * expm1(unname(coef(model)["right"])),
  n = nobs(model),
  segments = n_distinct(model_sample$segment_id),
  segment_month_cells = nrow(cell_support),
  two_sided_segment_month_cells = sum(cell_support$sides == 2),
  one_sided_segment_month_cells = sum(cell_support$sides == 1),
  studios = sum(model_sample$beds == 0, na.rm = TRUE),
  unit_id_key_rows = sum(model_sample$key_source == "unit_id", na.rm = TRUE),
  floorplan_fingerprint_rows = sum(model_sample$key_source == "floorplan_fingerprint", na.rm = TRUE)
)
write_csv(model_summary, "../output/rental_model_reproduction.csv")

robustness_specs <- list(
  production = rep(TRUE, nrow(model_data)),
  modal_sensitivity_checked = model_data$flag_modal_sensitivity_checked,
  address_present = model_data$address_missing == 0
)
robustness_rows <- list()
for (spec_name in names(robustness_specs)) {
  spec_data <- model_data[coalesce(robustness_specs[[spec_name]], FALSE), , drop = FALSE]
  spec_model <- feols(
    log(rent_price) ~ right + log_sqft_audit + beds_factor_audit + log_baths_audit +
      building_type_factor_audit + nearest_school_dist_kft_audit + nearest_park_dist_kft_audit +
      nearest_major_road_dist_kft_audit + nearest_cta_stop_dist_kft_audit +
      lake_michigan_dist_kft_audit | segment_id^year_month,
    data = spec_data,
    cluster = ~segment_id
  )
  spec_ct <- coeftable(spec_model)
  robustness_rows[[length(robustness_rows) + 1L]] <- tibble(
    specification = spec_name,
    estimate = unname(spec_ct["right", "Estimate"]),
    se = unname(spec_ct["right", "Std. Error"]),
    p_value = unname(spec_ct["right", "Pr(>|t|)"]),
    percent_effect = 100 * expm1(unname(coef(spec_model)["right"])),
    n = nobs(spec_model)
  )
}
write_csv(bind_rows(robustness_rows), "../output/rental_location_robustness.csv")

location_flags <- c(
  "flag_location_questionable",
  "flag_modal_sensitivity_checked",
  "flag_modal_assignment_missing",
  "flag_modal_changes_ward",
  "flag_modal_changes_neighbor_ward",
  "flag_modal_changes_pair",
  "flag_modal_dist_diff_gt100ft",
  "flag_rd_location_questionable",
  "flag_clean_location_sample"
)
location_summary <- bind_rows(lapply(location_flags, function(flag_name) {
  tibble(
    sample = c("within_500ft", "regression_sample"),
    flag = flag_name,
    n_true = c(
      sum(coalesce(as.logical(rent[[flag_name]]), FALSE)),
      sum(coalesce(as.logical(model_sample[[flag_name]]), FALSE))
    ),
    n_total = c(nrow(rent), nrow(model_sample))
  )
})) %>%
  bind_rows(tibble(
    sample = c("within_500ft", "regression_sample"),
    flag = "clean_but_modal_sensitivity_not_checked",
    n_true = c(
      sum(rent$flag_clean_location_sample & !rent$flag_modal_sensitivity_checked),
      sum(model_sample$flag_clean_location_sample & !model_sample$flag_modal_sensitivity_checked)
    ),
    n_total = c(nrow(rent), nrow(model_sample))
  ))
write_csv(location_summary, "../output/rental_location_flag_summary.csv")

unchecked_location_summary <- rent %>%
  filter(flag_clean_location_sample, !flag_modal_sensitivity_checked) %>%
  count(address_missing, location_quality_status, name = "n") %>%
  arrange(desc(n))
write_csv(unchecked_location_summary, "../output/rental_unchecked_location_summary.csv")

key_summary <- bind_rows(
  panel %>% count(key_source, name = "n") %>% mutate(sample = "monthly_panel"),
  rent %>% count(key_source, name = "n") %>% mutate(sample = "within_500ft"),
  model_sample %>% count(key_source, name = "n") %>% mutate(sample = "regression_sample")
) %>%
  select(sample, key_source, n)
write_csv(key_summary, "../output/rental_key_source_summary.csv")

unit_panel <- panel %>%
  filter(key_source == "unit_id", !is.na(unit_id), unit_id != "") %>%
  group_by(unit_id) %>%
  summarise(
    panel_rows = n(),
    distinct_property_keys = n_distinct(property_key, na.rm = TRUE),
    distinct_addresses = n_distinct(address_norm, na.rm = TRUE),
    distinct_floorplans = n_distinct(floorplan_key, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    property_collision = distinct_property_keys > 1,
    address_collision = distinct_addresses > 1
  )

unit_collision_summary <- tibble(
  unit_ids = nrow(unit_panel),
  unit_ids_multiple_property_keys = sum(unit_panel$property_collision),
  unit_ids_multiple_addresses = sum(unit_panel$address_collision),
  panel_rows_multiple_property_keys = sum(unit_panel$panel_rows[unit_panel$property_collision]),
  panel_rows_multiple_addresses = sum(unit_panel$panel_rows[unit_panel$address_collision]),
  regression_rows_multiple_property_keys = sum(
    model_sample$key_source == "unit_id" &
      model_sample$unit_id %in% unit_panel$unit_id[unit_panel$property_collision]
  ),
  regression_rows_multiple_addresses = sum(
    model_sample$key_source == "unit_id" &
      model_sample$unit_id %in% unit_panel$unit_id[unit_panel$address_collision]
  )
)
write_csv(unit_collision_summary, "../output/rental_unit_id_collision_summary.csv")
write_csv(
  unit_panel %>%
    filter(property_collision | address_collision) %>%
    arrange(desc(panel_rows), unit_id),
  "../output/rental_unit_id_collisions.csv"
)

deflator_check <- pre_scores %>%
  transmute(
    rent_panel_id,
    reported_real_rent = rent_price,
    reconstructed_real_rent = rent_price_nominal * rent_price_deflator_to_2022,
    absolute_difference = abs(reported_real_rent - reconstructed_real_rent)
  )
write_csv(
  tibble(
    n = nrow(deflator_check),
    max_absolute_difference = max(deflator_check$absolute_difference, na.rm = TRUE),
    mean_absolute_difference = mean(deflator_check$absolute_difference, na.rm = TRUE)
  ),
  "../output/rental_deflator_validation.csv"
)

may_2015 <- pre_scores %>%
  filter(format(as.Date(file_date), "%Y-%m") == "2015-05")
write_csv(
  tibble(
    may_2015_rows = nrow(may_2015),
    file_dates = paste(sort(unique(as.character(may_2015$file_date))), collapse = ";"),
    boundary_years = paste(sort(unique(may_2015$boundary_year)), collapse = ";")
  ),
  "../output/rental_transition_month_summary.csv"
)
