# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

pair_confidence <- read_csv("../output/score_pair_order_uncertainty.csv", show_col_types = FALSE) %>%
  select(sample, alderman_a, alderman_b, published_order_probability)
if (anyDuplicated(pair_confidence[c("sample", "alderman_a", "alderman_b")]) > 0) {
  stop("Pair-confidence input must be unique by sample and ordered alderman pair.", call. = FALSE)
}

thresholds <- c(0, 0.90, 0.95, 0.99)
model_rows <- list()

density <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = suppressWarnings(as.integer(construction_year)),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0)
  )

density_controls <- c(
  "share_white_own", "share_black_own", "median_hh_income_own",
  "share_bach_plus_own", "homeownership_rate_own"
)
density_specs <- tribble(
  ~sample, ~minimum_units, ~outcome,
  "density_all_far", 1, "density_far",
  "density_all_dupac", 1, "density_dupac",
  "density_multifamily_far", 2, "density_far",
  "density_multifamily_dupac", 2, "density_dupac"
)

for (i in seq_len(nrow(density_specs))) {
  sample_name <- density_specs$sample[i]
  outcome <- density_specs$outcome[i]
  data <- density %>%
    filter(
      construction_year >= 2006,
      construction_year <= 2022,
      arealotsf > 1,
      areabuilding > 1,
      unitscount >= density_specs$minimum_units[i],
      is.finite(dist_to_boundary_m),
      dist_to_boundary_m <= 152.4,
      !is.na(ward_pair),
      is.finite(signed_distance_m),
      !is.na(zone_code),
      !is.na(segment_id),
      segment_id != "",
      is.finite(.data[[outcome]]),
      .data[[outcome]] > 0,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      if_all(all_of(density_controls), ~ !is.na(.x))
    ) %>%
    mutate(outcome_value = log(.data[[outcome]])) %>%
    left_join(
      pair_confidence %>%
        filter(sample == sample_name) %>%
        select(alderman_a, alderman_b, published_order_probability),
      by = c("alderman_own" = "alderman_a", "alderman_neighbor" = "alderman_b"),
      relationship = "many-to-one"
    )
  if (anyNA(data$published_order_probability)) {
    stop(sprintf("Missing pair confidence in %s.", sample_name), call. = FALSE)
  }

  for (threshold in thresholds) {
    threshold_data <- data %>% filter(published_order_probability >= threshold)
    for (treatment_spec in c("continuous", "binary")) {
      treatment_var <- if (treatment_spec == "continuous") "strictness_own" else "side"
      controls <- if (treatment_spec == "continuous") {
        c("strictness_own", "lenient_dist", "strict_dist", density_controls)
      } else {
        c("side", "lenient_dist", "strict_dist", density_controls)
      }
      fit <- feols(
        as.formula(paste0(
          "outcome_value ~ ", paste(controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = threshold_data,
        cluster = ~ward_pair
      )
      ct <- coeftable(fit)
      model_rows[[length(model_rows) + 1L]] <- tibble(
        sample = sample_name,
        model_family = "density_rd",
        treatment_spec = treatment_spec,
        confidence_threshold = threshold,
        input_n = nrow(threshold_data),
        estimate = unname(ct[treatment_var, "Estimate"]),
        se = unname(ct[treatment_var, "Std. Error"]),
        p_value = unname(ct[treatment_var, "Pr(>|t|)"]),
        n = nobs(fit),
        clusters = n_distinct(threshold_data$ward_pair)
      )
    }
  }
}

rent <- read_parquet("../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
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
  ) %>%
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
    !is.na(ward_pair_id),
    flag_clean_location_sample,
    is.finite(beds),
    beds >= 0,
    !is.na(log_sqft_audit),
    !is.na(log_baths_audit),
    if_all(
      all_of(c(
        "nearest_school_dist_kft_audit", "nearest_park_dist_kft_audit",
        "nearest_major_road_dist_kft_audit", "nearest_cta_stop_dist_kft_audit",
        "lake_michigan_dist_kft_audit"
      )),
      is.finite
    )
  ) %>%
  left_join(
    pair_confidence %>%
      filter(sample == "rental_rd") %>%
      select(alderman_a, alderman_b, published_order_probability),
    by = c("alderman_own" = "alderman_a", "alderman_neighbor" = "alderman_b"),
    relationship = "many-to-one"
  )
if (anyNA(rent$published_order_probability)) {
  stop("Missing pair confidence in the rental sample.", call. = FALSE)
}

for (threshold in thresholds) {
  threshold_data <- rent %>% filter(published_order_probability >= threshold)
  fit <- feols(
    log(rent_price) ~ right + log_sqft_audit + beds_factor_audit + log_baths_audit +
      building_type_factor_audit + nearest_school_dist_kft_audit + nearest_park_dist_kft_audit +
      nearest_major_road_dist_kft_audit + nearest_cta_stop_dist_kft_audit +
      lake_michigan_dist_kft_audit | segment_id^year_month,
    data = threshold_data,
    cluster = ~segment_id
  )
  ct <- coeftable(fit)
  model_rows[[length(model_rows) + 1L]] <- tibble(
    sample = "rental_rd",
    model_family = "rental_rd",
    treatment_spec = "binary",
    confidence_threshold = threshold,
    input_n = nrow(threshold_data),
    estimate = unname(ct["right", "Estimate"]),
    se = unname(ct["right", "Std. Error"]),
    p_value = unname(ct["right", "Pr(>|t|)"]),
    n = nobs(fit),
    clusters = n_distinct(threshold_data$segment_id)
  )
}

sales <- read_parquet("../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    year = suppressWarnings(as.integer(year)),
    signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    right = as.integer(signed_dist_ft >= 0)
  )
sales_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage",
  "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)
sales <- sales %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair_id),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    if_all(all_of(sales_controls), ~ !is.na(.x))
  ) %>%
  left_join(
    pair_confidence %>%
      filter(sample == "sales_rd") %>%
      select(alderman_a, alderman_b, published_order_probability),
    by = c("alderman_own" = "alderman_a", "alderman_neighbor" = "alderman_b"),
    relationship = "many-to-one"
  )
if (anyNA(sales$published_order_probability)) {
  stop("Missing pair confidence in the sales sample.", call. = FALSE)
}

sales_rhs <- paste(c("right", sales_controls), collapse = " + ")
for (threshold in thresholds) {
  threshold_data <- sales %>% filter(published_order_probability >= threshold)
  fit <- feols(
    as.formula(paste0("log(sale_price) ~ ", sales_rhs, " | segment_id^year_quarter")),
    data = threshold_data,
    cluster = ~segment_id
  )
  ct <- coeftable(fit)
  model_rows[[length(model_rows) + 1L]] <- tibble(
    sample = "sales_rd",
    model_family = "sales_rd",
    treatment_spec = "binary",
    confidence_threshold = threshold,
    input_n = nrow(threshold_data),
    estimate = unname(ct["right", "Estimate"]),
    se = unname(ct["right", "Std. Error"]),
    p_value = unname(ct["right", "Pr(>|t|)"]),
    n = nobs(fit),
    clusters = n_distinct(threshold_data$segment_id)
  )
}

permit <- read_parquet("../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet") %>%
  as_tibble() %>%
  filter(
    dist_m <= 152.4,
    relative_year >= -5,
    relative_year <= 5,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  left_join(
    pair_confidence %>%
      filter(sample == "permit_event_assigned_switchers") %>%
      select(alderman_a, alderman_b, published_order_probability),
    by = c("alderman_origin_2014" = "alderman_a", "alderman_dest_2014" = "alderman_b"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    published_order_probability = if_else(switched, published_order_probability, 1),
    outcome = n_high_discretion_application,
    strictness_change = strictness_change_frozen
  )
if (anyNA(permit$published_order_probability)) {
  stop("Missing pair confidence among switched event-study blocks.", call. = FALSE)
}

for (threshold in thresholds) {
  threshold_data <- permit %>% filter(published_order_probability >= threshold)
  pre_controls <- threshold_data %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(pre_period_permit_volume = sum(outcome, na.rm = TRUE), .groups = "drop") %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))
  threshold_data <- threshold_data %>%
    left_join(pre_controls, by = "block_id", relationship = "many-to-one") %>%
    mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

  fit <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = threshold_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  coefficient_estimated <- "post_treat" %in% names(coef(fit))
  model_rows[[length(model_rows) + 1L]] <- tibble(
    sample = "permit_event_assigned_switchers",
    model_family = "permit_event_pooled",
    treatment_spec = "continuous",
    confidence_threshold = threshold,
    input_n = nrow(threshold_data),
    estimate = if (coefficient_estimated) unname(coef(fit)["post_treat"]) else NA_real_,
    se = if (coefficient_estimated) unname(se(fit)["post_treat"]) else NA_real_,
    p_value = if (coefficient_estimated) unname(pvalue(fit)["post_treat"]) else NA_real_,
    n = nobs(fit),
    clusters = n_distinct(threshold_data$ward_pair_id)
  )
}

write_csv(bind_rows(model_rows), "../output/score_confident_pair_model_sensitivity.csv")
