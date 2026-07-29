# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/income_score_current_results/code")

source("../../../_lib/alderman_uncertainty_helpers.R")
source("../../../_lib/border_pair_helpers.R")

scores <- read_csv("../output/current_income_scores.csv", show_col_types = FALSE) %>%
  select(cutoff, alderman, variant, score, rank) %>%
  pivot_wider(names_from = variant, values_from = c(score, rank)) %>%
  mutate(
    score_change = score_income_added_back - score_current_no_income,
    absolute_score_change = abs(score_change),
    rank_change = rank_income_added_back - rank_current_no_income
  ) %>%
  arrange(cutoff, desc(absolute_score_change))

write_csv(scores, "../output/current_income_score_movements.csv")

score_map <- scores %>%
  filter(cutoff == 2022L)

current_scores <- score_map$score_current_no_income
names(current_scores) <- score_map$alderman
income_scores <- score_map$score_income_added_back
names(income_scores) <- score_map$alderman

sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    ward_pair = as.character(ward_pair_id),
    signed_dist = as.numeric(signed_dist_m) / 0.3048,
    score_own_current = unname(current_scores[alderman_own]),
    score_neighbor_current = unname(current_scores[alderman_neighbor]),
    score_own_income = unname(income_scores[alderman_own]),
    score_neighbor_income = unname(income_scores[alderman_neighbor]),
    right_current = as.integer(score_own_current > score_neighbor_current),
    right_income = as.integer(score_own_income > score_neighbor_income),
    alderman_1 = pmin(alderman_own, alderman_neighbor),
    alderman_2 = pmax(alderman_own, alderman_neighbor),
    alderman_pair = paste(alderman_1, alderman_2, sep = " | ")
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
    is.finite(score_own_current),
    is.finite(score_neighbor_current),
    is.finite(score_own_income),
    is.finite(score_neighbor_income),
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

sales_outcome_residual <- resid(feols(
  log(sale_price) ~ log_sqft + log_land_sqft + log_building_age +
    log_bedrooms + log_baths + has_garage + nearest_school_dist_ft +
    nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft +
    lake_michigan_dist_ft | segment_id^year_quarter,
  data = sales
))
sales_current_residual <- resid(feols(
  right_current ~ log_sqft + log_land_sqft + log_building_age +
    log_bedrooms + log_baths + has_garage + nearest_school_dist_ft +
    nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft +
    lake_michigan_dist_ft | segment_id^year_quarter,
  data = sales
))
sales_income_residual <- resid(feols(
  right_income ~ log_sqft + log_land_sqft + log_building_age +
    log_bedrooms + log_baths + has_garage + nearest_school_dist_ft +
    nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft +
    lake_michigan_dist_ft | segment_id^year_quarter,
  data = sales
))

sales_current_estimate <- sum(sales_current_residual * sales_outcome_residual) /
  sum(sales_current_residual^2)
sales_income_estimate <- sum(sales_income_residual * sales_outcome_residual) /
  sum(sales_income_residual^2)

flipped_sales_pairs <- sales %>%
  filter(right_current != right_income) %>%
  distinct(alderman_pair) %>%
  pull(alderman_pair)

sales_delta_residuals <- sapply(flipped_sales_pairs, function(pair_name) {
  resid(feols(
    sales_pair_delta ~ log_sqft + log_land_sqft + log_building_age +
      log_bedrooms + log_baths + has_garage + nearest_school_dist_ft +
      nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft +
      lake_michigan_dist_ft | segment_id^year_quarter,
    data = sales %>%
      mutate(
        sales_pair_delta = as.integer(alderman_pair == pair_name) *
          (right_income - right_current)
      )
  ))
})

if (is.null(dim(sales_delta_residuals))) {
  sales_delta_residuals <- matrix(sales_delta_residuals, ncol = 1)
}

n_flipped_pairs <- length(flipped_sales_pairs)
subset_bits <- sapply(seq_len(n_flipped_pairs), function(column) {
  bitwAnd(0:(2^n_flipped_pairs - 1L), bitwShiftL(1L, column - 1L)) > 0L
}) * 1

outcome_cross_current <- sum(sales_current_residual * sales_outcome_residual)
outcome_cross_delta <- as.numeric(crossprod(sales_delta_residuals, sales_outcome_residual))
current_sum_squares <- sum(sales_current_residual^2)
current_cross_delta <- as.numeric(crossprod(sales_delta_residuals, sales_current_residual))
delta_cross_products <- crossprod(sales_delta_residuals)

subset_numerators <- outcome_cross_current +
  as.numeric(subset_bits %*% outcome_cross_delta)
subset_denominators <- current_sum_squares +
  2 * as.numeric(subset_bits %*% current_cross_delta) +
  rowSums((subset_bits %*% delta_cross_products) * subset_bits)
subset_estimates <- subset_numerators / subset_denominators

shapley_contributions <- numeric(n_flipped_pairs)
for (pair_index in seq_len(n_flipped_pairs)) {
  without_pair <- which(subset_bits[, pair_index] == 0L)
  with_pair <- without_pair + bitwShiftL(1L, pair_index - 1L)
  subset_size <- rowSums(subset_bits[without_pair, , drop = FALSE])
  shapley_weights <- 1 / n_flipped_pairs / choose(n_flipped_pairs - 1L, subset_size)
  shapley_contributions[pair_index] <- sum(
    shapley_weights * (subset_estimates[with_pair] - subset_estimates[without_pair])
  )
}

if (abs(sum(shapley_contributions) - (sales_income_estimate - sales_current_estimate)) > 1e-10) {
  stop("Sales pair contributions do not sum to the coefficient change.", call. = FALSE)
}

sales_pair_drivers <- sales %>%
  filter(alderman_pair %in% flipped_sales_pairs) %>%
  group_by(alderman_pair, alderman_1, alderman_2) %>%
  summarise(
    n_sales = n(),
    n_segments = n_distinct(segment_id),
    current_difference = first(score_own_current - score_neighbor_current) *
      if_else(first(alderman_own) == first(alderman_1), 1, -1),
    income_difference = first(score_own_income - score_neighbor_income) *
      if_else(first(alderman_own) == first(alderman_1), 1, -1),
    .groups = "drop"
  ) %>%
  mutate(
    shapley_contribution = shapley_contributions[match(alderman_pair, flipped_sales_pairs)],
    share_of_total_change = shapley_contribution /
      (sales_income_estimate - sales_current_estimate),
    current_estimate = sales_current_estimate,
    income_estimate = sales_income_estimate
  ) %>%
  arrange(desc(abs(shapley_contribution)))

write_csv(sales_pair_drivers, "../output/current_income_sales_pair_drivers.csv")

density <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    ward_pair = as.character(ward_pair),
    zone_group = construction_zone_group,
    alderman_1 = pmin(alderman_own, alderman_neighbor),
    alderman_2 = pmax(alderman_own, alderman_neighbor),
    alderman_pair = paste(alderman_1, alderman_2, sep = " | ")
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
    !is.na(zone_group),
    unitscount > 1
  )

density_models <- list()
for (variant in c("current", "income")) {
  own_scores <- if (variant == "current") current_scores else income_scores
  density_variant <- density %>%
    mutate(
      score_own = unname(own_scores[alderman_own]),
      score_neighbor = unname(own_scores[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      lenient_dist = abs(signed_distance_m) * as.integer(side == 0L),
      strict_dist = abs(signed_distance_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    ) %>%
    filter(is.finite(score_own), is.finite(score_neighbor))

  density_models[[variant]] <- density_variant
}

estimate_density_model <- function(data, outcome, treatment) {
  treatment_term <- if (treatment == "binary") "side" else "continuous_score_difference"
  model <- feols(
    as.formula(sprintf(
      paste0(
        "log(%s) ~ %s + pair_average_score + lenient_dist + strict_dist + ",
        "share_white_own + share_black_own + median_hh_income_own + ",
        "share_bach_plus_own + homeownership_rate_own | ",
        "zone_group + segment_id + construction_year"
      ),
      outcome,
      treatment_term
    )),
    data = data %>% filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0),
    cluster = ~ward_pair
  )
  unname(coef(model)[[treatment_term]])
}

density_full_results <- tidyr::crossing(
  outcome = c("density_far", "density_dupac"),
  treatment = c("binary", "continuous")
) %>%
  rowwise() %>%
  mutate(
    current_estimate = estimate_density_model(
      density_models$current,
      outcome,
      treatment
    ),
    income_estimate = estimate_density_model(
      density_models$income,
      outcome,
      treatment
    ),
    full_change = income_estimate - current_estimate
  ) %>%
  ungroup()

density_pairs <- sort(unique(density$alderman_pair))
density_pair_drivers <- list()

for (pair_name in density_pairs) {
  current_without_pair <- density_models$current %>%
    filter(alderman_pair != pair_name)
  income_without_pair <- density_models$income %>%
    filter(alderman_pair != pair_name)

  pair_counts <- density %>%
    filter(alderman_pair == pair_name) %>%
    summarise(
      alderman_1 = first(alderman_1),
      alderman_2 = first(alderman_2),
      n_projects = n(),
      n_segments = n_distinct(segment_id)
    )

  for (spec_index in seq_len(nrow(density_full_results))) {
    outcome <- density_full_results$outcome[[spec_index]]
    treatment <- density_full_results$treatment[[spec_index]]
    current_without_estimate <- estimate_density_model(
      current_without_pair,
      outcome,
      treatment
    )
    income_without_estimate <- estimate_density_model(
      income_without_pair,
      outcome,
      treatment
    )

    density_pair_drivers[[length(density_pair_drivers) + 1L]] <- pair_counts %>%
      mutate(
        alderman_pair = pair_name,
        outcome,
        treatment,
        current_estimate = density_full_results$current_estimate[[spec_index]],
        income_estimate = density_full_results$income_estimate[[spec_index]],
        full_change = density_full_results$full_change[[spec_index]],
        change_without_pair = income_without_estimate - current_without_estimate,
        leave_pair_out_influence = full_change - change_without_pair
      )
  }
}

write_csv(
  bind_rows(density_pair_drivers) %>%
    arrange(outcome, treatment, desc(abs(leave_pair_out_influence))),
  "../output/current_income_density_pair_drivers.csv"
)
