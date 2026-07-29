# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

model_rows_used <- function(model, data) {
  removed <- model$obs_selection$obsRemoved
  if (is.null(removed)) {
    used <- data
  } else {
    used <- data[setdiff(seq_len(nrow(data)), abs(as.integer(removed))), , drop = FALSE]
  }
  if (nrow(used) != nrow(model$scores)) {
    stop("Could not align model scores to estimation rows.", call. = FALSE)
  }
  used
}

coefficient_inference <- function(model, coefficient, cluster_formula) {
  table <- coeftable(model, cluster = cluster_formula)
  if (!coefficient %in% rownames(table)) {
    stop(sprintf("Coefficient %s is missing from the model.", coefficient), call. = FALSE)
  }
  p_value_column <- grep("^Pr\\(", colnames(table), value = TRUE)[1]
  if (is.na(p_value_column)) {
    stop("Could not identify the coefficient-table p-value column.", call. = FALSE)
  }
  tibble(
    estimate = unname(table[coefficient, "Estimate"]),
    se = unname(table[coefficient, "Std. Error"]),
    p_value = unname(table[coefficient, p_value_column])
  )
}

dyadic_inference <- function(model, data, coefficient) {
  used <- model_rows_used(model, data)
  pair_scores <- rowsum(model$scores, used$ward_pair, reorder = TRUE)
  pair_parts <- strsplit(gsub("-", "_", rownames(pair_scores), fixed = TRUE), "_", fixed = TRUE)
  valid_pair <- lengths(pair_parts) == 2
  if (!all(valid_pair)) {
    stop("A ward-pair cluster label could not be parsed into two endpoints.", call. = FALSE)
  }
  endpoint_a <- suppressWarnings(as.integer(vapply(pair_parts, `[`, character(1), 1)))
  endpoint_b <- suppressWarnings(as.integer(vapply(pair_parts, `[`, character(1), 2)))
  if (anyNA(endpoint_a) || anyNA(endpoint_b)) {
    stop("A ward-pair endpoint could not be parsed as an integer.", call. = FALSE)
  }

  node_scores <- rowsum(
    rbind(pair_scores, pair_scores),
    c(endpoint_a, endpoint_b),
    reorder = TRUE
  )
  bread <- model$cov.unscaled
  if (is.null(bread) || any(dim(bread) != dim(model$hessian))) {
    bread <- qr.solve(-model$hessian, diag(nrow(model$hessian)), tol = 1e-12)
  }
  pair_meat <- crossprod(pair_scores)
  dyadic_meat <- crossprod(node_scores) - pair_meat
  pair_vcov_raw <- bread %*% pair_meat %*% bread
  dyadic_vcov_raw <- bread %*% dyadic_meat %*% bread
  pair_vcov_fixest <- vcov(model, cluster = ~ward_pair, vcov_fix = FALSE)

  scale_candidates <- diag(pair_vcov_fixest) / diag(pair_vcov_raw)
  scale_candidates <- scale_candidates[
    is.finite(scale_candidates) & scale_candidates > 0
  ]
  if (length(scale_candidates) == 0) {
    stop("Could not recover the finite-sample scaling of the pair-clustered covariance.", call. = FALSE)
  }
  finite_sample_scale <- median(scale_candidates)
  dyadic_vcov <- dyadic_vcov_raw * finite_sample_scale

  coefficient_index <- match(coefficient, names(coef(model)))
  variance <- dyadic_vcov[coefficient_index, coefficient_index]
  standard_error <- if (is.finite(variance) && variance > 0) sqrt(variance) else NA_real_
  degrees_freedom <- nrow(node_scores) - 1
  statistic <- unname(coef(model)[coefficient_index]) / standard_error

  tibble(
    estimate = unname(coef(model)[coefficient_index]),
    se = standard_error,
    p_value = if_else(
      is.finite(statistic),
      2 * pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE),
      NA_real_
    ),
    dyadic_target_variance = variance,
    dyadic_pairs = nrow(pair_scores),
    dyadic_wards = nrow(node_scores),
    dyadic_minimum_eigenvalue = min(
      eigen(dyadic_vcov, symmetric = TRUE, only.values = TRUE)$values
    ),
    dyadic_finite_sample_scale = finite_sample_scale
  )
}

collect_inference <- function(model, data, coefficient, analysis, specification, official_cluster) {
  official_formula <- as.formula(paste0("~", official_cluster))

  bind_rows(
    coefficient_inference(model, coefficient, official_formula) %>%
      mutate(inference = paste0("published_", official_cluster)),
    coefficient_inference(model, coefficient, ~ward_pair) %>%
      mutate(inference = "ward_pair"),
    coefficient_inference(model, coefficient, ~endpoint_own) %>%
      mutate(inference = "own_ward"),
    coefficient_inference(model, coefficient, ~endpoint_neighbor) %>%
      mutate(inference = "neighbor_ward"),
    coefficient_inference(model, coefficient, ~endpoint_own + endpoint_neighbor) %>%
      mutate(inference = "directional_two_way_endpoints"),
    dyadic_inference(model, data, coefficient) %>%
      mutate(inference = "dyadic_shared_ward")
  ) %>%
    mutate(
      analysis,
      specification,
      coefficient,
      n = nobs(model),
      segments = n_distinct(model_rows_used(model, data)$segment_id, na.rm = TRUE),
      ward_pairs = n_distinct(model_rows_used(model, data)$ward_pair),
      wards = n_distinct(c(
        model_rows_used(model, data)$endpoint_own,
        model_rows_used(model, data)$endpoint_neighbor
      ), na.rm = TRUE)
    ) %>%
    relocate(analysis, specification, coefficient, inference)
}

inference_rows <- list()

density <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = as.integer(construction_year),
    ward_pair = as.character(ward_pair),
    segment_id = as.character(segment_id),
    endpoint_own = as.integer(ward),
    endpoint_neighbor = as.integer(other_ward),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    between(construction_year, 2006, 2022),
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != ""
  )

for (construction_sample in c("all", "multifamily")) {
  density_sample <- if (construction_sample == "all") {
    density %>% filter(unitscount > 0)
  } else {
    density %>% filter(unitscount > 1)
  }

  for (outcome in c("density_far", "density_dupac")) {
    outcome_data <- density_sample %>%
      filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0)

    for (treatment in c("continuous", "binary")) {
      treatment_var <- if (treatment == "continuous") "strictness_own" else "side"
      model <- feols(
        as.formula(paste0(
          "log(", outcome, ") ~ ", treatment_var, " + lenient_dist + strict_dist + ",
          paste(demographic_controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = outcome_data,
        warn = FALSE
      )
      inference_rows[[length(inference_rows) + 1L]] <- collect_inference(
        model,
        outcome_data,
        treatment_var,
        "density",
        paste(construction_sample, outcome, treatment, sep = "__"),
        "ward_pair"
      )
    }
  }
}

rent <- read_parquet(
  "../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw500.parquet"
) %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = year(file_date),
    year_month = format(file_date, "%Y-%m"),
    signed_dist_ft = as.numeric(signed_dist),
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    endpoint_own = as.integer(ward),
    endpoint_neighbor = as.integer(neighbor_ward),
    right = as.integer(signed_dist_ft >= 0),
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
    between(year, 2014, 2022),
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

rent_rhs <- "right + log_sqft + beds_factor + log_baths"
if (n_distinct(rent$building_type_factor) > 1) {
  rent_rhs <- paste(rent_rhs, "+ building_type_factor")
}
rent_rhs <- paste(
  rent_rhs,
  "nearest_school_dist_kft",
  "nearest_park_dist_kft",
  "nearest_major_road_dist_kft",
  "nearest_cta_stop_dist_kft",
  "lake_michigan_dist_kft",
  sep = " + "
)
rent_model <- feols(
  as.formula(paste0("log(rent_price) ~ ", rent_rhs, " | segment_id^year_month")),
  data = rent,
  warn = FALSE
)
inference_rows[[length(inference_rows) + 1L]] <- collect_inference(
  rent_model,
  rent,
  "right",
  "rental_rd",
  "clean_location__500ft__all_controls",
  "segment_id"
)

sales <- read_parquet(
  "../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet"
) %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    endpoint_own = as.integer(ward),
    endpoint_neighbor = as.integer(neighbor_ward),
    signed_dist = as.numeric(signed_dist_m) / 0.3048,
    right = as.integer(signed_dist >= 0)
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    between(year, 2006, 2022),
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist),
    abs(signed_dist) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )

sales_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms",
  "log_baths", "has_garage", "nearest_school_dist_ft", "nearest_park_dist_ft",
  "nearest_major_road_dist_ft", "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)
sales <- sales %>%
  filter(if_all(all_of(sales_controls), ~ !is.na(.x)))
sales_model <- feols(
  as.formula(paste0(
    "log(sale_price) ~ right + ", paste(sales_controls, collapse = " + "),
    " | segment_id^year_quarter"
  )),
  data = sales,
  warn = FALSE
)
inference_rows[[length(inference_rows) + 1L]] <- collect_inference(
  sales_model,
  sales,
  "right",
  "sales_rd",
  "500ft__hedonics_and_amenities",
  "segment_id"
)

permit_panel <- read_parquet(
  "../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"
) %>%
  filter(
    dist_m <= 152.4,
    between(relative_year, -5, 5),
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id_cohort),
    endpoint_own = as.integer(ward_origin),
    endpoint_neighbor = as.integer(ward_dest),
    strictness_change = strictness_change_frozen
  )

for (event_specification in c("high_itt", "low_itt", "high_stable")) {
  event_data <- permit_panel
  if (event_specification == "high_stable") {
    event_data <- event_data %>% filter(stable_both)
  }
  outcome_var <- if (event_specification == "low_itt") {
    "n_low_discretion_nosigns_application"
  } else {
    "n_high_discretion_application"
  }
  event_data <- event_data %>% mutate(outcome = .data[[outcome_var]])

  pre_period_controls <- event_data %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(pre_period_permit_volume = sum(outcome, na.rm = TRUE), .groups = "drop") %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))

  event_data <- event_data %>%
    left_join(pre_period_controls, by = "block_id", relationship = "many-to-one") %>%
    mutate(post_treat = as.integer(relative_year >= 0) * strictness_change)

  event_model <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair^year,
    data = event_data,
    notes = FALSE,
    warn = FALSE
  )
  inference_rows[[length(inference_rows) + 1L]] <- collect_inference(
    event_model,
    event_data,
    "post_treat",
    "permit_event_study_pooled",
    event_specification,
    "ward_pair"
  )
}

write_csv(bind_rows(inference_rows), "../output/inference_cluster_sensitivity.csv")
