# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/income_score_current_results/code")
# n_draws <- 2000

source("../../../setup_environment/code/packages.R")
library(arrow)
setFixest_notes(FALSE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- n_draws
}
if (length(cli_args) != 1) {
  stop("Script requires the number of score draws.", call. = FALSE)
}

n_draws <- as.integer(cli_args[1])
if (!is.finite(n_draws) || n_draws < 1) {
  stop("n_draws must be a positive integer.", call. = FALSE)
}

score_draws <- list(
  all_covariates = read_parquet(sprintf(
    "../input/production_score_draws_through2022_%ddraws.parquet",
    n_draws
  )) %>%
    as_tibble() %>%
    select(draw, alderman, score),
  income_added_back = read_parquet(sprintf(
    "../output/income_score_draws_through2022_%ddraws.parquet",
    n_draws
  )) %>%
    as_tibble() %>%
    select(draw, alderman, score)
)

score_matrices <- lapply(score_draws, function(draws) {
  wide <- draws %>%
    mutate(draw = as.character(draw)) %>%
    pivot_wider(names_from = draw, values_from = score) %>%
    arrange(alderman)
  matrix_out <- as.matrix(wide[, -1, drop = FALSE])
  rownames(matrix_out) <- wide$alderman
  storage.mode(matrix_out) <- "double"
  matrix_out
})

if (any(vapply(score_matrices, ncol, integer(1)) != n_draws)) {
  stop("Score-draw inputs do not contain the requested number of draws.", call. = FALSE)
}
if (!identical(rownames(score_matrices$all_covariates),
               rownames(score_matrices$income_added_back))) {
  stop("The two score-draw files contain different aldermen.", call. = FALSE)
}

aldermen <- rownames(score_matrices$all_covariates)

production_scores <- read_csv(
  "../output/current_income_scores.csv",
  show_col_types = FALSE
) %>%
  filter(cutoff == 2022L) %>%
  select(alderman, variant, score) %>%
  pivot_wider(names_from = variant, values_from = score) %>%
  arrange(match(alderman, aldermen))

if (!identical(production_scores$alderman, aldermen)) {
  stop("Production scores do not align with the score draws.", call. = FALSE)
}

prepare_crossproducts <- function(data, outcome, controls, fixed_effect) {
  own_index <- match(data$alderman_own, aldermen)
  neighbor_index <- match(data$alderman_neighbor, aldermen)
  if (anyNA(own_index) || anyNA(neighbor_index)) {
    stop("An analysis alderman is absent from the score draws.", call. = FALSE)
  }

  n_observations <- nrow(data)
  n_aldermen <- length(aldermen)
  row_index <- seq_len(n_observations)

  relative_basis <- matrix(0, nrow = n_observations, ncol = n_aldermen)
  relative_basis[cbind(row_index, own_index)] <- 0.5
  relative_basis[cbind(row_index, neighbor_index)] <-
    relative_basis[cbind(row_index, neighbor_index)] - 0.5

  average_basis <- matrix(0, nrow = n_observations, ncol = n_aldermen)
  average_basis[cbind(row_index, own_index)] <- 0.5
  average_basis[cbind(row_index, neighbor_index)] <-
    average_basis[cbind(row_index, neighbor_index)] + 0.5

  base_demeaned <- demean(
    cbind(outcome = outcome, controls),
    f = list(fixed_effect),
    nthreads = 1,
    notes = FALSE
  )
  outcome_demeaned <- base_demeaned[, "outcome"]
  controls_demeaned <- base_demeaned[, colnames(base_demeaned) != "outcome", drop = FALSE]

  controls_qr <- qr(controls_demeaned, tol = 1e-10)
  if (controls_qr$rank < ncol(controls_demeaned)) {
    controls_demeaned <- controls_demeaned[
      , controls_qr$pivot[seq_len(controls_qr$rank)], drop = FALSE
    ]
    controls_qr <- qr(controls_demeaned, tol = 1e-10)
  }
  control_basis <- qr.Q(controls_qr)

  relative_demeaned <- demean(
    relative_basis,
    f = list(fixed_effect),
    nthreads = 1,
    notes = FALSE
  )
  average_demeaned <- demean(
    average_basis,
    f = list(fixed_effect),
    nthreads = 1,
    notes = FALSE
  )

  control_relative <- crossprod(control_basis, relative_demeaned)
  control_average <- crossprod(control_basis, average_demeaned)
  control_outcome <- crossprod(control_basis, outcome_demeaned)

  output <- list(
    relative_relative = crossprod(relative_demeaned) - crossprod(control_relative),
    average_average = crossprod(average_demeaned) - crossprod(control_average),
    relative_average = crossprod(relative_demeaned, average_demeaned) -
      crossprod(control_relative, control_average),
    relative_outcome = crossprod(relative_demeaned, outcome_demeaned) -
      crossprod(control_relative, control_outcome),
    average_outcome = crossprod(average_demeaned, outcome_demeaned) -
      crossprod(control_average, control_outcome),
    n = n_observations
  )

  rm(
    relative_basis,
    average_basis,
    base_demeaned,
    controls_demeaned,
    control_basis,
    relative_demeaned,
    average_demeaned
  )
  gc()
  output
}

estimate_from_crossproducts <- function(crossproducts, score_matrix) {
  relative_ss <- colSums(
    score_matrix * (crossproducts$relative_relative %*% score_matrix)
  )
  average_ss <- colSums(
    score_matrix * (crossproducts$average_average %*% score_matrix)
  )
  relative_average <- colSums(
    score_matrix * (crossproducts$relative_average %*% score_matrix)
  )
  relative_outcome <- as.numeric(
    crossprod(crossproducts$relative_outcome, score_matrix)
  )
  average_outcome <- as.numeric(
    crossprod(crossproducts$average_outcome, score_matrix)
  )

  denominator <- relative_ss * average_ss - relative_average^2
  if (any(!is.finite(denominator)) || any(abs(denominator) < 1e-12)) {
    stop("A score draw produces a singular continuous-price model.", call. = FALSE)
  }

  (relative_outcome * average_ss - average_outcome * relative_average) /
    denominator
}

rent <- read_parquet("../input/rental_rd_characteristics_panel_bw500.parquet") %>%
  as_tibble() %>%
  mutate(
    file_date = as.Date(file_date),
    year = lubridate::year(file_date),
    year_month = format(file_date, "%Y-%m"),
    ward_pair = as.character(ward_pair_id),
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
    is.finite(as.numeric(signed_dist)),
    abs(as.numeric(signed_dist)) <= 500,
    alderman_own %in% aldermen,
    alderman_neighbor %in% aldermen,
    alderman_own != alderman_neighbor,
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

rent_control_formula <- if (n_distinct(rent$building_type_factor) > 1) {
  ~ log_sqft + beds_factor + log_baths + building_type_factor +
    nearest_school_dist_kft + nearest_park_dist_kft +
    nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft
} else {
  ~ log_sqft + beds_factor + log_baths + nearest_school_dist_kft +
    nearest_park_dist_kft + nearest_major_road_dist_kft +
    nearest_cta_stop_dist_kft + lake_michigan_dist_kft
}
rent_controls <- model.matrix(rent_control_formula, data = rent)[, -1, drop = FALSE]
rent_crossproducts <- prepare_crossproducts(
  rent,
  log(rent$rent_price),
  rent_controls,
  interaction(rent$segment_id, rent$year_month, drop = TRUE)
)
rm(rent_controls)
gc()

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    signed_dist = as.numeric(signed_dist_m) / 0.3048
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
    alderman_own %in% aldermen,
    alderman_neighbor %in% aldermen,
    alderman_own != alderman_neighbor,
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

sales_controls <- model.matrix(
  ~ log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths +
    has_garage + nearest_school_dist_ft + nearest_park_dist_ft +
    nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft,
  data = sales
)[, -1, drop = FALSE]
sales_crossproducts <- prepare_crossproducts(
  sales,
  log(sales$sale_price),
  sales_controls,
  interaction(sales$segment_id, sales$year_quarter, drop = TRUE)
)
rm(sales_controls)
gc()

draw_results <- bind_rows(lapply(names(score_matrices), function(variant) {
  score_matrix <- score_matrices[[variant]][aldermen, , drop = FALSE]
  bind_rows(
    tibble(
      analysis = "rental",
      variant,
      draw = as.integer(colnames(score_matrix)),
      estimate = estimate_from_crossproducts(rent_crossproducts, score_matrix)
    ),
    tibble(
      analysis = "sales",
      variant,
      draw = as.integer(colnames(score_matrix)),
      estimate = estimate_from_crossproducts(sales_crossproducts, score_matrix)
    )
  )
}))

production_results <- read_csv(
  "../output/current_income_score_results.csv",
  show_col_types = FALSE
) %>%
  filter(
    analysis %in% c("rental", "sales"),
    treatment == "continuous",
    variant %in% names(score_matrices)
  ) %>%
  select(
    analysis,
    variant,
    production_estimate = estimate,
    production_clustered_se = std_error,
    n
  )

production_from_crossproducts <- bind_rows(lapply(names(score_matrices), function(variant) {
  score_vector <- production_scores[[variant]]
  score_matrix <- matrix(score_vector, ncol = 1, dimnames = list(aldermen, "production"))
  tibble(
    analysis = c("rental", "sales"),
    variant,
    crossproduct_estimate = c(
      estimate_from_crossproducts(rent_crossproducts, score_matrix),
      estimate_from_crossproducts(sales_crossproducts, score_matrix)
    )
  )
}))

production_check <- production_results %>%
  left_join(
    production_from_crossproducts,
    by = c("analysis", "variant"),
    relationship = "one-to-one"
  )
if (anyNA(production_check) ||
    max(abs(production_check$production_estimate - production_check$crossproduct_estimate)) > 1e-8) {
  stop("Cross-product estimates do not reproduce the continuous price models.", call. = FALSE)
}

write_csv(
  draw_results,
  sprintf("../output/continuous_price_score_uncertainty_%ddraws.csv", n_draws)
)

write_csv(
  draw_results %>%
    left_join(
      production_results,
      by = c("analysis", "variant"),
      relationship = "many-to-one"
    ) %>%
    group_by(analysis, variant, production_estimate, production_clustered_se, n) %>%
    summarise(
      bootstrap_mean = mean(estimate),
      bootstrap_sd = sd(estimate),
      p025 = quantile(estimate, 0.025),
      median = median(estimate),
      p975 = quantile(estimate, 0.975),
      probability_positive = mean(estimate > 0),
      .groups = "drop"
    ) %>%
    mutate(
      score_sd_to_clustered_se = bootstrap_sd / production_clustered_se,
      n_draws = n_draws
    ),
  sprintf("../output/continuous_price_score_uncertainty_summary_%ddraws.csv", n_draws)
)
