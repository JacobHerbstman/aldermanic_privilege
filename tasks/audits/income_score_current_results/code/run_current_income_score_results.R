# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/income_score_current_results/code")

source("../../../_lib/alderman_uncertainty_helpers.R")
source("../../../_lib/border_pair_helpers.R")

config <- default_uncertainty_config()
permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv")

score_rows <- list()
stage1_rows <- list()

for (cutoff in c(2014L, 2022L)) {
  permits_cutoff <- permits %>%
    filter(month <= as.yearmon(sprintf("%d-12", cutoff)))

  for (variant in c("current_no_income", "education_added_back", "income_added_back", "all_covariates")) {
    dropped_covariates <- switch(
      variant,
      current_no_income = c("share_bach_plus", "median_hh_income_10k"),
      education_added_back = "median_hh_income_10k",
      income_added_back = "share_bach_plus",
      all_covariates = character()
    )

    score_result <- build_residualized_uncertainty_index(
      permits = permits_cutoff,
      config = config,
      variant_id = variant,
      stage1_outcome = "log_processing_time",
      drop_covariates = dropped_covariates,
      construction_rule = switch(
        variant,
        current_no_income = "Score omitting bachelor's share and median household income",
        education_added_back = "Bachelor's share restored; median household income remains omitted",
        income_added_back = "Median household income restored; bachelor's share remains omitted",
        all_covariates = "Median household income and bachelor's share both included"
      )
    )

    score_rows[[length(score_rows) + 1L]] <- score_result$alderman_index %>%
      transmute(
        cutoff,
        variant,
        alderman,
        n_permits,
        score = uncertainty_index,
        rank = rank(-uncertainty_index, ties.method = "average")
      )

    stage1_rows[[length(stage1_rows) + 1L]] <- score_result$stage1_terms %>%
      mutate(cutoff, variant, .before = 1)
  }
}

scores <- bind_rows(score_rows)
stage1_terms <- bind_rows(stage1_rows)

if (anyDuplicated(scores[c("cutoff", "variant", "alderman")]) > 0) {
  stop("Rebuilt scores are not unique by cutoff, variant, and alderman.", call. = FALSE)
}

for (cutoff in c(2014L, 2022L)) {
  production_scores <- read_csv(
    sprintf("../input/production_score_through%d.csv", cutoff),
    show_col_types = FALSE
  ) %>%
    select(alderman, production_score = uncertainty_index)

  rebuilt_scores <- scores %>%
    filter(cutoff == .env$cutoff, variant == "all_covariates") %>%
    select(alderman, rebuilt_score = score)

  score_check <- full_join(
    production_scores,
    rebuilt_scores,
    by = "alderman",
    relationship = "one-to-one"
  )

  if (anyNA(score_check) || max(abs(score_check$production_score - score_check$rebuilt_score)) > 1e-8) {
    stop(sprintf("The full-control score rebuild does not match production through %d.", cutoff), call. = FALSE)
  }
}

score_comparison <- scores %>%
  select(cutoff, variant, alderman, score, rank) %>%
  pivot_wider(names_from = variant, values_from = c(score, rank)) %>%
  group_by(cutoff) %>%
  summarise(
    n_aldermen = n(),
    score_correlation = cor(score_current_no_income, score_income_added_back),
    rank_correlation = cor(
      score_current_no_income,
      score_income_added_back,
      method = "spearman"
    ),
    education_score_correlation = cor(
      score_current_no_income,
      score_education_added_back
    ),
    education_rank_correlation = cor(
      score_current_no_income,
      score_education_added_back,
      method = "spearman"
    ),
    education_mean_absolute_score_change = mean(
      abs(score_education_added_back - score_current_no_income)
    ),
    education_maximum_absolute_score_change = max(
      abs(score_education_added_back - score_current_no_income)
    ),
    all_covariates_score_correlation = cor(
      score_current_no_income,
      score_all_covariates
    ),
    all_covariates_rank_correlation = cor(
      score_current_no_income,
      score_all_covariates,
      method = "spearman"
    ),
    all_covariates_mean_absolute_score_change = mean(
      abs(score_all_covariates - score_current_no_income)
    ),
    all_covariates_maximum_absolute_score_change = max(
      abs(score_all_covariates - score_current_no_income)
    ),
    mean_absolute_score_change = mean(abs(score_income_added_back - score_current_no_income)),
    maximum_absolute_score_change = max(abs(score_income_added_back - score_current_no_income)),
    n_rank_changes = sum(rank_current_no_income != rank_income_added_back),
    .groups = "drop"
  )

density_source <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    ward_pair = as.character(ward_pair),
    zone_group = construction_zone_group
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(segment_id),
    segment_id != "",
    !is.na(zone_group)
  )

rent_source <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble()
sales_source <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble()
permit_source <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  as_tibble()

pair_sources <- bind_rows(
  density_source %>%
    distinct(alderman_a = alderman_own, alderman_b = alderman_neighbor) %>%
    mutate(analysis = "density", cutoff = 2022L),
  rent_source %>%
    distinct(alderman_a = alderman_own, alderman_b = alderman_neighbor) %>%
    mutate(analysis = "rental", cutoff = 2022L),
  sales_source %>%
    distinct(alderman_a = alderman_own, alderman_b = alderman_neighbor) %>%
    mutate(analysis = "sales", cutoff = 2022L),
  permit_source %>%
    distinct(alderman_a = alderman_origin_2014, alderman_b = alderman_dest_2014) %>%
    mutate(analysis = "permit_event", cutoff = 2014L)
) %>%
  filter(!is.na(alderman_a), !is.na(alderman_b), alderman_a != alderman_b) %>%
  distinct(analysis, cutoff, alderman_a, alderman_b)

pair_orderings <- list()
for (cutoff in c(2014L, 2022L)) {
  current_map <- scores %>%
    filter(cutoff == .env$cutoff, variant == "current_no_income") %>%
    select(alderman, score) %>%
    deframe()
  income_map <- scores %>%
    filter(cutoff == .env$cutoff, variant == "income_added_back") %>%
    select(alderman, score) %>%
    deframe()

  pair_orderings[[length(pair_orderings) + 1L]] <- pair_sources %>%
    filter(cutoff == .env$cutoff) %>%
    mutate(
      current_difference = unname(current_map[alderman_a]) - unname(current_map[alderman_b]),
      income_difference = unname(income_map[alderman_a]) - unname(income_map[alderman_b]),
      ordering_changed = sign(current_difference) != sign(income_difference)
    )
}
pair_orderings <- bind_rows(pair_orderings)

result_rows <- list()

for (variant in c("current_no_income", "education_added_back", "income_added_back", "all_covariates")) {
  score_map_2022 <- scores %>%
    filter(cutoff == 2022L, variant == .env$variant) %>%
    select(alderman, score) %>%
    deframe()

  density <- density_source %>%
    mutate(
      score_own = unname(score_map_2022[alderman_own]),
      score_neighbor = unname(score_map_2022[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      lenient_dist = abs(signed_distance_m) * as.integer(side == 0L),
      strict_dist = abs(signed_distance_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    ) %>%
    filter(is.finite(score_own), is.finite(score_neighbor))

  for (sample_name in c("all", "multifamily")) {
    density_sample <- if (sample_name == "all") {
      density %>% filter(unitscount > 0)
    } else {
      density %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      model_data <- density_sample %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0)

      for (treatment in c("binary", "continuous")) {
        treatment_term <- if (treatment == "binary") "side" else "continuous_score_difference"
        controls <- if (treatment == "binary") {
          c("side", "pair_average_score", "lenient_dist", "strict_dist")
        } else {
          c("continuous_score_difference", "pair_average_score", "lenient_dist", "strict_dist")
        }
        controls <- c(
          controls,
          "share_white_own",
          "share_black_own",
          "median_hh_income_own",
          "share_bach_plus_own",
          "homeownership_rate_own"
        )

        model <- feols(
          as.formula(sprintf(
            "log(%s) ~ %s | zone_group + segment_id + construction_year",
            outcome,
            paste(controls, collapse = " + ")
          )),
          data = model_data,
          cluster = ~ward_pair
        )
        model_table <- coeftable(model)

        result_rows[[length(result_rows) + 1L]] <- tibble(
          analysis = "density",
          sample = sample_name,
          outcome,
          treatment,
          variant,
          estimate = unname(model_table[treatment_term, "Estimate"]),
          std_error = unname(model_table[treatment_term, "Std. Error"]),
          p_value = unname(model_table[treatment_term, "Pr(>|t|)"]),
          n = nobs(model),
          effect_percent = 100 * expm1(estimate)
        )
      }
    }
  }

  rent <- rent_source %>%
    mutate(
      file_date = as.Date(file_date),
      year = lubridate::year(file_date),
      year_month = format(file_date, "%Y-%m"),
      signed_dist_ft = as.numeric(signed_dist),
      ward_pair = as.character(ward_pair_id),
      score_own = unname(score_map_2022[alderman_own]),
      score_neighbor = unname(score_map_2022[alderman_neighbor]),
      right = as.integer(score_own > score_neighbor),
      relative_score = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2,
      log_sqft = if_else(is.finite(sqft) & sqft > 0, log(sqft), NA_real_),
      beds_factor = factor(beds),
      log_baths = if_else(is.finite(baths) & baths > 0, log(baths), NA_real_),
      building_type_factor = factor(coalesce(building_type_clean, "other")),
      nearest_school_dist_kft = nearest_school_dist_ft / 1000,
      nearest_park_dist_kft = nearest_park_dist_ft / 1000,
      nearest_major_road_dist_kft = nearest_major_road_dist_ft / 1000,
      nearest_cta_stop_dist_kft = nearest_cta_stop_dist_ft / 1000,
      lake_michigan_dist_kft = lake_michigan_dist_ft / 1000
    ) %>%
    filter(
      !is.na(file_date),
      year >= 2014,
      year <= 2022,
      is.finite(rent_price),
      rent_price > 0,
      is.finite(signed_dist_ft),
      abs(signed_dist_ft) <= 500,
      is.finite(score_own),
      is.finite(score_neighbor),
      !is.na(segment_id),
      segment_id != "",
      !is.na(ward_pair),
      flag_clean_location_sample,
      is.finite(beds),
      beds >= 0,
      !is.na(log_sqft),
      !is.na(log_baths),
      if_all(
        all_of(c(
          "nearest_school_dist_kft",
          "nearest_park_dist_kft",
          "nearest_major_road_dist_kft",
          "nearest_cta_stop_dist_kft",
          "lake_michigan_dist_kft"
        )),
        is.finite
      )
    )

  rent_controls <- "log_sqft + beds_factor + log_baths"
  if (n_distinct(rent$building_type_factor) > 1) {
    rent_controls <- paste(rent_controls, "+ building_type_factor")
  }
  rent_controls <- paste(
    rent_controls,
    "nearest_school_dist_kft",
    "nearest_park_dist_kft",
    "nearest_major_road_dist_kft",
    "nearest_cta_stop_dist_kft",
    "lake_michigan_dist_kft",
    sep = " + "
  )
  rent_model <- feols(
    as.formula(paste0(
      "log(rent_price) ~ right + ",
      rent_controls,
      " | segment_id^year_month"
    )),
    data = rent,
    cluster = ~segment_id
  )
  rent_table <- coeftable(rent_model)
  rent_estimate <- unname(rent_table["right", "Estimate"])
  result_rows[[length(result_rows) + 1L]] <- tibble(
    analysis = "rental",
    sample = "clean_location",
    outcome = "rent_price",
    treatment = "binary",
    variant,
    estimate = rent_estimate,
    std_error = unname(rent_table["right", "Std. Error"]),
    p_value = unname(rent_table["right", "Pr(>|t|)"]),
    n = nobs(rent_model),
    effect_percent = 100 * expm1(rent_estimate)
  )

  rent_continuous_model <- feols(
    as.formula(paste0(
      "log(rent_price) ~ relative_score + pair_average_score + ",
      rent_controls,
      " | segment_id^year_month"
    )),
    data = rent,
    cluster = ~segment_id
  )
  rent_continuous_table <- coeftable(rent_continuous_model)
  rent_continuous_estimate <- unname(
    rent_continuous_table["relative_score", "Estimate"]
  )
  result_rows[[length(result_rows) + 1L]] <- tibble(
    analysis = "rental",
    sample = "clean_location",
    outcome = "rent_price",
    treatment = "continuous",
    variant,
    estimate = rent_continuous_estimate,
    std_error = unname(rent_continuous_table["relative_score", "Std. Error"]),
    p_value = unname(rent_continuous_table["relative_score", "Pr(>|t|)"]),
    n = nobs(rent_continuous_model),
    effect_percent = 100 * expm1(rent_continuous_estimate)
  )

  sales <- sales_source %>%
    mutate(
      ward_pair = as.character(ward_pair_id),
      signed_dist = as.numeric(signed_dist_m) / 0.3048,
      score_own = unname(score_map_2022[alderman_own]),
      score_neighbor = unname(score_map_2022[alderman_neighbor]),
      right = as.integer(score_own > score_neighbor),
      relative_score = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    ) %>%
    filter(
      !is.na(sale_price),
      sale_price > 0,
      year >= 2006,
      year <= 2022,
      !is.na(ward_pair),
      !is.na(segment_id),
      segment_id != "",
      is.finite(signed_dist),
      abs(signed_dist) <= 500,
      is.finite(score_own),
      is.finite(score_neighbor),
      if_all(
        all_of(c(
          "log_sqft",
          "log_land_sqft",
          "log_building_age",
          "log_bedrooms",
          "log_baths",
          "has_garage",
          "nearest_school_dist_ft",
          "nearest_park_dist_ft",
          "nearest_major_road_dist_ft",
          "nearest_cta_stop_dist_ft",
          "lake_michigan_dist_ft"
        )),
        ~ !is.na(.x)
      )
    )

  sales_model <- feols(
    log(sale_price) ~ right + log_sqft + log_land_sqft + log_building_age +
      log_bedrooms + log_baths + has_garage + nearest_school_dist_ft +
      nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft +
      lake_michigan_dist_ft | segment_id^year_quarter,
    data = sales,
    cluster = ~segment_id
  )
  sales_table <- coeftable(sales_model)
  sales_estimate <- unname(sales_table["right", "Estimate"])
  result_rows[[length(result_rows) + 1L]] <- tibble(
    analysis = "sales",
    sample = "main",
    outcome = "sale_price",
    treatment = "binary",
    variant,
    estimate = sales_estimate,
    std_error = unname(sales_table["right", "Std. Error"]),
    p_value = unname(sales_table["right", "Pr(>|t|)"]),
    n = nobs(sales_model),
    effect_percent = 100 * expm1(sales_estimate)
  )

  sales_continuous_model <- feols(
    log(sale_price) ~ relative_score + pair_average_score + log_sqft +
      log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage +
      nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft +
      nearest_cta_stop_dist_ft + lake_michigan_dist_ft | segment_id^year_quarter,
    data = sales,
    cluster = ~segment_id
  )
  sales_continuous_table <- coeftable(sales_continuous_model)
  sales_continuous_estimate <- unname(
    sales_continuous_table["relative_score", "Estimate"]
  )
  result_rows[[length(result_rows) + 1L]] <- tibble(
    analysis = "sales",
    sample = "main",
    outcome = "sale_price",
    treatment = "continuous",
    variant,
    estimate = sales_continuous_estimate,
    std_error = unname(sales_continuous_table["relative_score", "Std. Error"]),
    p_value = unname(sales_continuous_table["relative_score", "Pr(>|t|)"]),
    n = nobs(sales_continuous_model),
    effect_percent = 100 * expm1(sales_continuous_estimate)
  )

  score_map_2014 <- scores %>%
    filter(cutoff == 2014L, variant == .env$variant) %>%
    select(alderman, score) %>%
    deframe()

  for (permit_spec in c("high_itt", "low_itt", "high_stable")) {
    outcome <- if (permit_spec == "low_itt") {
      "n_low_discretion_nosigns_application"
    } else {
      "n_high_discretion_application"
    }

    permit <- permit_source %>%
      filter(
        dist_m <= 152.4,
        relative_year >= -5,
        relative_year <= 5,
        !is.na(strictness_change_frozen),
        !is.na(ward_pair_id),
        ward_pair_id != ""
      ) %>%
      mutate(
        strictness_origin = unname(score_map_2014[alderman_origin_2014]),
        strictness_destination = unname(score_map_2014[alderman_dest_2014]),
        strictness_change = strictness_destination - strictness_origin,
        outcome = .data[[outcome]]
      ) %>%
      filter(is.finite(strictness_change))

    if (permit_spec == "high_stable") {
      permit <- permit %>% filter(stable_both)
    }

    pre_period_controls <- permit %>%
      filter(relative_year < 0) %>%
      group_by(block_id) %>%
      summarise(
        pre_period_permit_volume = sum(outcome, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

    permit <- permit %>%
      left_join(pre_period_controls, by = "block_id", relationship = "many-to-one") %>%
      mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

    permit_model <- fepois(
      outcome ~ post_treat +
        pre_period_permit_volume:factor(year) +
        no_pre_period_permits:factor(year) |
        block_id + ward_pair_id^year,
      data = permit,
      cluster = ~ward_pair_id,
      notes = FALSE
    )
    permit_estimate <- unname(coef(permit_model)[["post_treat"]])

    result_rows[[length(result_rows) + 1L]] <- tibble(
      analysis = "permit_event",
      sample = permit_spec,
      outcome,
      treatment = "continuous_itt",
      variant,
      estimate = permit_estimate,
      std_error = unname(se(permit_model)[["post_treat"]]),
      p_value = unname(pvalue(permit_model)[["post_treat"]]),
      n = nobs(permit_model),
      effect_percent = 100 * expm1(permit_estimate)
    )
  }
}

results <- bind_rows(result_rows) %>%
  arrange(analysis, sample, outcome, treatment, variant)

write_csv(results, "../output/current_income_score_results.csv")
write_csv(score_comparison, "../output/current_income_score_comparison.csv")
write_csv(pair_orderings, "../output/current_income_score_pair_orderings.csv")
write_csv(stage1_terms, "../output/current_income_score_stage1_terms.csv")
write_csv(scores, "../output/current_income_scores.csv")
