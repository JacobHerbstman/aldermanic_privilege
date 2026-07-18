# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../_lib/alderman_uncertainty_helpers.R")
source("../../../_lib/border_pair_helpers.R")

config <- default_uncertainty_config()
permits_all <- load_uncertainty_permits(
  "../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv"
)

score_outputs <- list()
metadata_rows <- list()
alderman_rows <- list()

for (vintage in c("2014", "2022")) {
  max_month <- as.yearmon(as.Date(paste0(vintage, "-12-01")))
  result <- build_residualized_uncertainty_index(
    permits = permits_all %>% filter(month <= max_month),
    config = config,
    variant_id = "baseline",
    stage1_outcome = "log_processing_time",
    drop_covariates = "share_bach_plus",
    construction_rule = "Baseline residualized score dropping share_bach_plus"
  )

  published <- read_csv(
    sprintf(
      "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through%s.csv",
      vintage
    ),
    show_col_types = FALSE
  ) %>%
    select(alderman, published_score = uncertainty_index)

  alderman_index <- result$alderman_index %>%
    left_join(published, by = "alderman", relationship = "one-to-one")
  if (anyNA(alderman_index$published_score)) {
    stop(sprintf("The rebuilt %s score does not match the published alderman set.", vintage), call. = FALSE)
  }

  effect_terms <- tibble(
    term = names(coef(result$stage2_model)),
    coefficient = as.numeric(coef(result$stage2_model))
  ) %>%
    filter(str_detect(term, "^alderman::")) %>%
    mutate(alderman = str_remove(term, "^alderman::"))
  if (nrow(effect_terms) != nrow(alderman_index)) {
    stop(sprintf("The %s stage-two model has the wrong number of alderman effects.", vintage), call. = FALSE)
  }

  stage2_vcov <- vcov(result$stage2_model, se = "hetero")
  raw_vcov <- stage2_vcov[effect_terms$term, effect_terms$term, drop = FALSE]
  centering_matrix <- diag(nrow(effect_terms)) -
    matrix(1 / nrow(effect_terms), nrow(effect_terms), nrow(effect_terms))
  centered_vcov <- centering_matrix %*% raw_vcov %*% centering_matrix

  index_order <- match(alderman_index$alderman, effect_terms$alderman)
  if (anyNA(index_order)) {
    stop(sprintf("Could not align the %s score and covariance matrix.", vintage), call. = FALSE)
  }
  centered_vcov <- centered_vcov[index_order, index_order, drop = FALSE]
  shrinkage_matrix <- diag(alderman_index$shrinkage_B)
  shrunk_vcov <- shrinkage_matrix %*% centered_vcov %*% shrinkage_matrix
  score_scale <- sd(alderman_index$mean_resid)
  score_centering <- diag(nrow(alderman_index)) -
    matrix(1 / nrow(alderman_index), nrow(alderman_index), nrow(alderman_index))
  score_vcov <- score_centering %*% shrunk_vcov %*% score_centering / score_scale^2
  dimnames(score_vcov) <- list(alderman_index$alderman, alderman_index$alderman)

  score_se <- sqrt(pmax(diag(score_vcov), 0))
  score_outputs[[vintage]] <- list(
    index = alderman_index,
    vcov = score_vcov
  )

  metadata_rows[[length(metadata_rows) + 1L]] <- tibble(
    score_vintage = vintage,
    aldermen = nrow(alderman_index),
    permits = result$metadata$n_permits_used,
    ward_month_stage2_rows = result$metadata$stage2_nobs,
    tau2 = result$metadata$stage2_tau2,
    median_shrinkage = median(alderman_index$shrinkage_B),
    min_shrinkage = min(alderman_index$shrinkage_B),
    max_shrinkage = max(alderman_index$shrinkage_B),
    median_score_se = median(score_se),
    min_score_se = min(score_se),
    max_score_se = max(score_se),
    max_abs_rebuild_difference = max(abs(alderman_index$uncertainty_index - alderman_index$published_score))
  )

  alderman_rows[[length(alderman_rows) + 1L]] <- alderman_index %>%
    transmute(
      score_vintage = vintage,
      alderman,
      score = uncertainty_index,
      score_se = score_se,
      alderman_fe_raw,
      alderman_se,
      shrinkage_B,
      n_permits
    )
}

write_csv(bind_rows(metadata_rows), "../output/score_uncertainty_metadata.csv")
write_csv(bind_rows(alderman_rows), "../output/score_alderman_uncertainty.csv")

usage_rows <- list()

density <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(construction_year = suppressWarnings(as.integer(construction_year)))

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
  sample_data <- density %>%
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
      is.finite(.data[[density_specs$outcome[i]]]),
      .data[[density_specs$outcome[i]]] > 0,
      !is.na(strictness_own),
      !is.na(strictness_neighbor),
      if_all(all_of(density_controls), ~ !is.na(.x))
    )
  usage_rows[[length(usage_rows) + 1L]] <- sample_data %>%
    count(alderman_own, alderman_neighbor, name = "observations") %>%
    transmute(
      sample = density_specs$sample[i],
      score_vintage = "2022",
      alderman_a = alderman_own,
      alderman_b = alderman_neighbor,
      observations
    )
}

rent <- read_parquet("../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    signed_dist_ft = as.numeric(signed_dist),
    log_sqft_audit = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
    log_baths_audit = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_)
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
        "nearest_school_dist_ft", "nearest_park_dist_ft", "nearest_major_road_dist_ft",
        "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
      )),
      is.finite
    )
  )
usage_rows[[length(usage_rows) + 1L]] <- rent %>%
  count(alderman_own, alderman_neighbor, name = "observations") %>%
  transmute(
    sample = "rental_rd",
    score_vintage = "2022",
    alderman_a = alderman_own,
    alderman_b = alderman_neighbor,
    observations
  )

sales <- read_parquet("../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(signed_dist_ft = as.numeric(signed_dist_m) / 0.3048)
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
  )
usage_rows[[length(usage_rows) + 1L]] <- sales %>%
  count(alderman_own, alderman_neighbor, name = "observations") %>%
  transmute(
    sample = "sales_rd",
    score_vintage = "2022",
    alderman_a = alderman_own,
    alderman_b = alderman_neighbor,
    observations
  )

permit_event <- read_parquet("../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet") %>%
  as_tibble() %>%
  filter(
    dist_m <= 152.4,
    relative_year >= -5,
    relative_year <= 5,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != "",
    switched
  ) %>%
  distinct(block_id, alderman_origin_2014, alderman_dest_2014)
usage_rows[[length(usage_rows) + 1L]] <- permit_event %>%
  count(alderman_origin_2014, alderman_dest_2014, name = "observations") %>%
  transmute(
    sample = "permit_event_assigned_switchers",
    score_vintage = "2014",
    alderman_a = alderman_origin_2014,
    alderman_b = alderman_dest_2014,
    observations
  )

pair_usage <- bind_rows(usage_rows) %>%
  filter(!is.na(alderman_a), !is.na(alderman_b), alderman_a != alderman_b) %>%
  group_by(sample, score_vintage, alderman_a, alderman_b) %>%
  summarise(observations = sum(observations), .groups = "drop")

pair_rows <- vector("list", nrow(pair_usage))
for (i in seq_len(nrow(pair_usage))) {
  vintage <- pair_usage$score_vintage[i]
  score_index <- score_outputs[[vintage]]$index
  score_vcov <- score_outputs[[vintage]]$vcov
  a <- pair_usage$alderman_a[i]
  b <- pair_usage$alderman_b[i]
  ia <- match(a, score_index$alderman)
  ib <- match(b, score_index$alderman)
  if (is.na(ia) || is.na(ib)) {
    stop(sprintf("A paper sample uses an alderman absent from the %s score.", vintage), call. = FALSE)
  }

  score_gap <- score_index$uncertainty_index[ia] - score_index$uncertainty_index[ib]
  gap_variance <- score_vcov[ia, ia] + score_vcov[ib, ib] - 2 * score_vcov[ia, ib]
  gap_se <- sqrt(max(gap_variance, 0))
  z_order <- if (gap_se > 0) abs(score_gap) / gap_se else Inf

  pair_rows[[i]] <- pair_usage[i, ] %>%
    mutate(
      score_a = score_index$uncertainty_index[ia],
      score_b = score_index$uncertainty_index[ib],
      score_gap_a_minus_b = score_gap,
      score_gap_se = gap_se,
      absolute_gap_in_se = z_order,
      published_order_probability = pnorm(z_order),
      order_flip_probability = 1 - published_order_probability,
      unordered_pair = paste(sort(c(a, b)), collapse = " | ")
    )
}

pair_results <- bind_rows(pair_rows) %>%
  arrange(sample, desc(order_flip_probability), desc(observations))
write_csv(pair_results, "../output/score_pair_order_uncertainty.csv")

summary <- pair_results %>%
  group_by(sample, score_vintage) %>%
  summarise(
    total_observations = sum(observations),
    ordered_alderman_pairs = n(),
    unordered_alderman_pairs = n_distinct(unordered_pair),
    observation_weighted_mean_flip_probability = weighted.mean(order_flip_probability, observations),
    maximum_pair_flip_probability = max(order_flip_probability),
    share_observations_order_probability_below_90 =
      sum(observations[published_order_probability < 0.90]) / sum(observations),
    share_observations_order_probability_below_95 =
      sum(observations[published_order_probability < 0.95]) / sum(observations),
    share_observations_order_probability_below_99 =
      sum(observations[published_order_probability < 0.99]) / sum(observations),
    share_pairs_order_probability_below_90 = mean(published_order_probability < 0.90),
    share_pairs_order_probability_below_95 = mean(published_order_probability < 0.95),
    share_pairs_order_probability_below_99 = mean(published_order_probability < 0.99),
    .groups = "drop"
  )
write_csv(summary, "../output/score_order_uncertainty_summary.csv")
