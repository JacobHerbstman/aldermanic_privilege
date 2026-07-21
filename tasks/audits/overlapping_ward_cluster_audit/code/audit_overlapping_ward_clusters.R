# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/overlapping_ward_cluster_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

model_rows <- function(model, data) {
  row_index <- obs(model)
  if (length(row_index) != nobs(model)) {
    stop("Could not align the model with its estimation rows.", call. = FALSE)
  }
  data[row_index, , drop = FALSE]
}

coefficient_row <- function(model, coefficient, cluster_formula, inference) {
  table <- coeftable(model, cluster = cluster_formula)
  if (!coefficient %in% rownames(table)) {
    stop(sprintf("Coefficient %s is missing.", coefficient), call. = FALSE)
  }
  p_column <- grep("^Pr\\(", colnames(table), value = TRUE)[1]
  tibble(
    inference = inference,
    estimate = unname(table[coefficient, "Estimate"]),
    se = unname(table[coefficient, "Std. Error"]),
    p_value = unname(table[coefficient, p_column]),
    variance_positive = TRUE,
    dyadic_pairs = NA_integer_,
    dyadic_wards = NA_integer_,
    dyadic_minimum_eigenvalue = NA_real_
  )
}

dyadic_row <- function(model, data, coefficient) {
  used <- model_rows(model, data)
  if (!all(c("ward_pair", "endpoint_own", "endpoint_neighbor") %in% names(used))) {
    stop("Dyadic inference requires pair and endpoint columns.", call. = FALSE)
  }

  pair_map <- used %>%
    transmute(
      ward_pair,
      ward_a = pmin(endpoint_own, endpoint_neighbor),
      ward_b = pmax(endpoint_own, endpoint_neighbor)
    ) %>%
    distinct()
  if (anyDuplicated(pair_map$ward_pair) > 0) {
    stop("A ward-pair label maps to multiple endpoint sets.", call. = FALSE)
  }

  pair_scores <- rowsum(model$scores, used$ward_pair, reorder = TRUE)
  pair_map <- pair_map[match(rownames(pair_scores), pair_map$ward_pair), ]
  if (anyNA(pair_map$ward_a) || anyNA(pair_map$ward_b)) {
    stop("Could not recover every ward-pair endpoint.", call. = FALSE)
  }

  node_scores <- rowsum(
    rbind(pair_scores, pair_scores),
    c(pair_map$ward_a, pair_map$ward_b),
    reorder = TRUE
  )
  bread <- model$cov.unscaled
  if (is.null(bread) || any(dim(bread) != dim(model$hessian))) {
    bread <- qr.solve(-model$hessian, diag(nrow(model$hessian)), tol = 1e-12)
  }

  pair_vcov_raw <- bread %*% crossprod(pair_scores) %*% bread
  dyadic_vcov_raw <- bread %*%
    (crossprod(node_scores) - crossprod(pair_scores)) %*%
    bread
  coefficient_index <- match(coefficient, names(coef(model)))
  pair_vcov_fixest <- vcov(model, cluster = ~ward_pair, vcov_fix = FALSE)
  pair_variance_raw <- pair_vcov_raw[coefficient_index, coefficient_index]
  pair_variance_fixest <- pair_vcov_fixest[coefficient_index, coefficient_index]
  finite_sample_scale <- pair_variance_fixest / pair_variance_raw
  dyadic_vcov <- dyadic_vcov_raw * finite_sample_scale
  dyadic_variance <- dyadic_vcov[coefficient_index, coefficient_index]
  dyadic_se <- if (is.finite(dyadic_variance) && dyadic_variance > 0) {
    sqrt(dyadic_variance)
  } else {
    NA_real_
  }
  statistic <- unname(coef(model)[coefficient_index]) / dyadic_se
  degrees_freedom <- nrow(node_scores) - 1

  tibble(
    inference = "dyadic_shared_ward",
    estimate = unname(coef(model)[coefficient_index]),
    se = dyadic_se,
    p_value = if_else(
      is.finite(statistic),
      2 * pt(abs(statistic), df = degrees_freedom, lower.tail = FALSE),
      NA_real_
    ),
    variance_positive = is.finite(dyadic_variance) && dyadic_variance > 0,
    dyadic_pairs = nrow(pair_scores),
    dyadic_wards = nrow(node_scores),
    dyadic_minimum_eigenvalue = min(
      eigen(dyadic_vcov, symmetric = TRUE, only.values = TRUE)$values
    )
  )
}

collect_inference <- function(model, data, coefficient, analysis, specification, published_cluster) {
  published_formula <- as.formula(paste0("~", published_cluster))
  used <- model_rows(model, data)

  bind_rows(
    coefficient_row(model, coefficient, published_formula, paste0("published_", published_cluster)),
    coefficient_row(model, coefficient, ~ward_pair, "ward_pair"),
    coefficient_row(model, coefficient, ~endpoint_own, "own_ward"),
    coefficient_row(model, coefficient, ~endpoint_neighbor, "neighbor_ward"),
    coefficient_row(model, coefficient, ~endpoint_own + endpoint_neighbor, "directional_two_way_endpoints"),
    dyadic_row(model, data, coefficient)
  ) %>%
    mutate(
      analysis = analysis,
      specification = specification,
      coefficient = coefficient,
      observations = nobs(model),
      segments = n_distinct(used$segment_id, na.rm = TRUE),
      ward_pairs = n_distinct(used$ward_pair),
      wards = n_distinct(c(used$endpoint_own, used$endpoint_neighbor), na.rm = TRUE)
    ) %>%
    relocate(analysis, specification, coefficient, inference)
}

network_rows <- list()
add_network <- function(data, analysis) {
  pair_map <- data %>%
    transmute(
      ward_pair,
      ward_a = pmin(endpoint_own, endpoint_neighbor),
      ward_b = pmax(endpoint_own, endpoint_neighbor)
    ) %>%
    distinct()
  if (anyDuplicated(pair_map$ward_pair) > 0) {
    stop(sprintf("%s has inconsistent pair endpoints.", analysis), call. = FALSE)
  }
  bind_rows(
    pair_map %>% transmute(ward = ward_a, ward_pair),
    pair_map %>% transmute(ward = ward_b, ward_pair)
  ) %>%
    count(ward, name = "ward_pair_degree") %>%
    mutate(analysis = analysis, total_pairs = nrow(pair_map)) %>%
    relocate(analysis)
}

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

density <- read_csv(
  "../input/parcels_with_ward_distances.csv",
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
    zone_group = construction_zone_group,
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    between(construction_year, 2006, 2022),
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(construction_zone_group),
    !is.na(segment_id),
    segment_id != ""
  )

inference_rows <- list()
density_models <- list()
density_data <- list()

for (construction_sample in c("all", "multifamily")) {
  sample_data <- if (construction_sample == "all") {
    density %>% filter(unitscount > 0)
  } else {
    density %>% filter(unitscount > 1)
  }

  for (outcome in c("density_far", "density_dupac")) {
    outcome_data <- sample_data %>%
      filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0)

    for (treatment in c("continuous", "binary")) {
      treatment_var <- if (treatment == "continuous") "continuous_score_difference" else "side"
      controls <- c(
        treatment_var,
        "pair_average_score",
        "lenient_dist",
        "strict_dist",
        demographic_controls
      )
      model <- feols(
        as.formula(paste0(
          "log(", outcome, ") ~ ", paste(controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = outcome_data,
        warn = FALSE
      )
      specification <- paste(construction_sample, outcome, treatment, sep = "__")
      inference_rows[[length(inference_rows) + 1L]] <- collect_inference(
        model,
        outcome_data,
        treatment_var,
        "density",
        specification,
        "ward_pair"
      )
      if (construction_sample == "multifamily") {
        density_models[[specification]] <- model
        density_data[[specification]] <- outcome_data
      }
    }
  }
}
network_rows[["density"]] <- add_network(density, "density")

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
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
network_rows[["rental_rd"]] <- add_network(model_rows(rent_model, rent), "rental_rd")

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(segment_id),
    endpoint_own = as.integer(ward),
    endpoint_neighbor = as.integer(neighbor_ward),
    signed_dist_ft = as.numeric(signed_dist_m) / 0.3048,
    right = as.integer(signed_dist_ft >= 0)
  ) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    between(year, 2006, 2022),
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    is.finite(signed_dist_ft),
    abs(signed_dist_ft) <= 500,
    !is.na(strictness_own),
    !is.na(strictness_neighbor)
  )
sales_controls <- c(
  "log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms",
  "log_baths", "has_garage", "nearest_school_dist_ft", "nearest_park_dist_ft",
  "nearest_major_road_dist_ft", "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft"
)
sales <- sales %>% filter(if_all(all_of(sales_controls), ~ !is.na(.x)))
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
network_rows[["sales_rd"]] <- add_network(model_rows(sales_model, sales), "sales_rd")

permit_panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  filter(
    dist_m <= 152.4,
    between(relative_year, -5, 5),
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    segment_id = as.character(ward_pair_id),
    endpoint_own = as.integer(str_match(ward_pair, "^(\\d+)[_-](\\d+)$")[, 2]),
    endpoint_neighbor = as.integer(str_match(ward_pair, "^(\\d+)[_-](\\d+)$")[, 3]),
    strictness_change = strictness_change_frozen,
    post_treat = as.integer(relative_year >= 0) * strictness_change
  )

permit_models <- list()
permit_data <- list()
for (permit_specification in c("high_itt", "low_itt")) {
  outcome_var <- if (permit_specification == "high_itt") {
    "n_high_discretion_application"
  } else {
    "n_low_discretion_nosigns_application"
  }
  model_data <- permit_panel %>% mutate(outcome = .data[[outcome_var]])
  pre_period_controls <- model_data %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(pre_period_permit_volume = sum(outcome, na.rm = TRUE), .groups = "drop") %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))
  model_data <- model_data %>%
    left_join(pre_period_controls, by = "block_id", relationship = "many-to-one")
  model <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair^year,
    data = model_data,
    notes = FALSE,
    warn = FALSE
  )
  inference_rows[[length(inference_rows) + 1L]] <- collect_inference(
    model,
    model_data,
    "post_treat",
    "permit_event_study_pooled",
    permit_specification,
    "ward_pair"
  )
  permit_models[[permit_specification]] <- model
  permit_data[[permit_specification]] <- model_data
}
network_rows[["permit_event_study"]] <- add_network(
  model_rows(permit_models[["high_itt"]], permit_data[["high_itt"]]),
  "permit_event_study"
)

cluster_results <- bind_rows(inference_rows)
write_csv(cluster_results, "../output/cluster_inference_comparison.csv")
write_csv(bind_rows(network_rows), "../output/ward_pair_network_summary.csv")

leave_one_out_rows <- list()

for (specification in names(density_models)) {
  full_model <- density_models[[specification]]
  full_data <- density_data[[specification]]
  treatment_var <- if (grepl("__continuous$", specification)) "continuous_score_difference" else "side"
  outcome <- if (grepl("density_far", specification)) "density_far" else "density_dupac"
  controls <- c(
    treatment_var,
    "pair_average_score",
    "lenient_dist",
    "strict_dist",
    demographic_controls
  )
  wards <- sort(unique(c(full_data$endpoint_own, full_data$endpoint_neighbor)))

  for (omitted_ward in wards) {
    sample <- full_data %>%
      filter(endpoint_own != omitted_ward, endpoint_neighbor != omitted_ward)
    model <- tryCatch(
      feols(
        as.formula(paste0(
          "log(", outcome, ") ~ ", paste(controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = sample,
        cluster = ~ward_pair,
        warn = FALSE
      ),
      error = function(e) NULL
    )
    leave_one_out_rows[[length(leave_one_out_rows) + 1L]] <- tibble(
      analysis = "density",
      specification = specification,
      omitted_ward = omitted_ward,
      estimate = if (is.null(model)) NA_real_ else unname(coef(model)[treatment_var]),
      se = if (is.null(model)) NA_real_ else unname(se(model)[treatment_var]),
      observations = if (is.null(model)) NA_integer_ else nobs(model)
    )
  }
}

for (specification in names(permit_models)) {
  full_data <- permit_data[[specification]]
  wards <- sort(unique(c(full_data$endpoint_own, full_data$endpoint_neighbor)))

  for (omitted_ward in wards) {
    sample <- full_data %>%
      filter(endpoint_own != omitted_ward, endpoint_neighbor != omitted_ward)
    model <- tryCatch(
      fepois(
        outcome ~ post_treat +
          pre_period_permit_volume:factor(year) +
          no_pre_period_permits:factor(year) |
          block_id + ward_pair^year,
        data = sample,
        cluster = ~ward_pair,
        notes = FALSE,
        warn = FALSE
      ),
      error = function(e) NULL
    )
    leave_one_out_rows[[length(leave_one_out_rows) + 1L]] <- tibble(
      analysis = "permit_event_study_pooled",
      specification = specification,
      omitted_ward = omitted_ward,
      estimate = if (is.null(model)) NA_real_ else unname(coef(model)["post_treat"]),
      se = if (is.null(model)) NA_real_ else unname(se(model)["post_treat"]),
      observations = if (is.null(model)) NA_integer_ else nobs(model)
    )
  }
}

leave_one_out <- bind_rows(leave_one_out_rows)
write_csv(leave_one_out, "../output/leave_one_ward_out_estimates.csv")

published_estimates <- cluster_results %>%
  filter(grepl("^published_", inference)) %>%
  select(analysis, specification, published_estimate = estimate)

leave_one_out_summary <- leave_one_out %>%
  left_join(published_estimates, by = c("analysis", "specification"), relationship = "many-to-one") %>%
  group_by(analysis, specification, published_estimate) %>%
  summarise(
    successful_omissions = sum(is.finite(estimate)),
    minimum_estimate = min(estimate, na.rm = TRUE),
    maximum_estimate = max(estimate, na.rm = TRUE),
    standard_deviation = sd(estimate, na.rm = TRUE),
    largest_absolute_change = max(abs(estimate - published_estimate), na.rm = TRUE),
    sign_changes = sum(sign(estimate) != sign(published_estimate), na.rm = TRUE),
    .groups = "drop"
  )
write_csv(leave_one_out_summary, "../output/leave_one_ward_out_summary.csv")
