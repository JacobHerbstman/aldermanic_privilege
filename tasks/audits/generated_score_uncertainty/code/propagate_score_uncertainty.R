# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/generated_score_uncertainty/code")
# n_draws <- 2000
# workers <- 8

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")
library(arrow)
setFixest_notes(FALSE)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(n_draws, workers)
}
if (length(cli_args) != 2) {
  stop("Script requires number of draws and workers.", call. = FALSE)
}

n_draws <- as.integer(cli_args[1])
workers <- as.integer(cli_args[2])
if (!is.finite(n_draws) || n_draws < 1) {
  stop("n_draws must be a positive integer.", call. = FALSE)
}
if (!is.finite(workers) || workers < 1) {
  stop("workers must be a positive integer.", call. = FALSE)
}

score_draws_2022 <- read_parquet(
  sprintf("../output/score_draws_through2022_%ddraws.parquet", n_draws)
) %>%
  as_tibble() %>%
  select(draw, alderman, score)
score_draws_2014 <- read_parquet(
  sprintf("../output/score_draws_through2014_%ddraws.parquet", n_draws)
) %>%
  as_tibble() %>%
  select(draw, alderman, score)

if (n_distinct(score_draws_2022$draw) != n_draws ||
    n_distinct(score_draws_2014$draw) != n_draws) {
  stop("Score-draw inputs do not contain the requested number of draws.", call. = FALSE)
}

production_2022 <- read_csv(
  "../input/alderman_uncertainty_index_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)
production_2014 <- read_csv(
  "../input/alderman_uncertainty_index_through2014.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)

score_matrix <- function(draws) {
  wide <- draws %>%
    mutate(draw = as.character(draw)) %>%
    pivot_wider(names_from = draw, values_from = score) %>%
    arrange(alderman)
  out <- as.matrix(wide[, -1, drop = FALSE])
  rownames(out) <- wide$alderman
  storage.mode(out) <- "double"
  out
}

score_matrix_2022 <- score_matrix(score_draws_2022)
score_matrix_2014 <- score_matrix(score_draws_2014)
production_score_2022 <- setNames(production_2022$score, production_2022$alderman)
production_score_2014 <- setNames(production_2014$score, production_2014$alderman)

score_draw_diagnostics <- bind_rows(
  tibble(
    score_vintage = 2022L,
    draw = as.integer(colnames(score_matrix_2022)),
    pearson_correlation = apply(
      score_matrix_2022,
      2,
      cor,
      y = production_score_2022[rownames(score_matrix_2022)]
    ),
    rank_correlation = apply(
      score_matrix_2022,
      2,
      cor,
      y = production_score_2022[rownames(score_matrix_2022)],
      method = "spearman"
    ),
    score_rmse = apply(
      score_matrix_2022,
      2,
      function(x) sqrt(mean((x - production_score_2022[rownames(score_matrix_2022)])^2))
    )
  ),
  tibble(
    score_vintage = 2014L,
    draw = as.integer(colnames(score_matrix_2014)),
    pearson_correlation = apply(
      score_matrix_2014,
      2,
      cor,
      y = production_score_2014[rownames(score_matrix_2014)]
    ),
    rank_correlation = apply(
      score_matrix_2014,
      2,
      cor,
      y = production_score_2014[rownames(score_matrix_2014)],
      method = "spearman"
    ),
    score_rmse = apply(
      score_matrix_2014,
      2,
      function(x) sqrt(mean((x - production_score_2014[rownames(score_matrix_2014)])^2))
    )
  )
)

write_csv(
  score_draw_diagnostics,
  sprintf("../output/score_draw_diagnostics_%ddraws.csv", n_draws)
)

write_csv(
  score_draw_diagnostics %>%
    group_by(score_vintage) %>%
    summarise(
      across(
        c(pearson_correlation, rank_correlation, score_rmse),
        list(p025 = ~quantile(.x, 0.025), median = median, mean = mean, p975 = ~quantile(.x, 0.975))
      ),
      .groups = "drop"
    ),
  sprintf("../output/score_draw_diagnostics_summary_%ddraws.csv", n_draws)
)

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
  mutate(
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    unitscount > 1,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != "",
    is.finite(density_far),
    density_far > 0,
    is.finite(density_dupac),
    density_dupac > 0,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    if_all(all_of(demographic_controls), is.finite)
  )

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
    is.finite(dist_m),
    dist_m <= 152.4,
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

sales_hedonics <- c(
  "log_sqft",
  "log_land_sqft",
  "log_building_age",
  "log_bedrooms",
  "log_baths",
  "has_garage"
)
sales_amenities <- c(
  "nearest_school_dist_ft",
  "nearest_park_dist_ft",
  "nearest_major_road_dist_ft",
  "nearest_cta_stop_dist_ft",
  "lake_michigan_dist_ft"
)
sales <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(ward_pair = as.character(ward_pair_id)) %>%
  filter(
    !is.na(sale_price),
    sale_price > 0,
    year >= 2006,
    year <= 2022,
    !is.na(ward_pair),
    !is.na(segment_id),
    segment_id != "",
    is.finite(dist_m),
    dist_m <= 152.4,
    !is.na(strictness_own),
    !is.na(strictness_neighbor),
    if_all(all_of(c(sales_hedonics, sales_amenities)), ~ !is.na(.x))
  )

permit_panel <- read_parquet("../input/permit_block_year_panel_2015.parquet") %>%
  as_tibble() %>%
  filter(
    dist_m <= 152.4,
    relative_year >= -5,
    relative_year <= 5,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  )

prepare_permit_outcome <- function(outcome_name) {
  out <- permit_panel %>% mutate(outcome = .data[[outcome_name]])
  pre_period_controls <- out %>%
    filter(relative_year < 0) %>%
    group_by(block_id) %>%
    summarise(pre_period_permit_volume = sum(outcome, na.rm = TRUE), .groups = "drop") %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0))
  out %>%
    left_join(pre_period_controls, by = "block_id", relationship = "many-to-one")
}

permit_high <- prepare_permit_outcome("n_high_discretion_application")
permit_low <- prepare_permit_outcome("n_low_discretion_nosigns_application")

comparison_rows <- bind_rows(
  density %>% transmute(score_vintage = 2022L, analysis = "density_multifamily", alderman_1 = alderman_own, alderman_2 = alderman_neighbor),
  rent %>% transmute(score_vintage = 2022L, analysis = "rental_prices", alderman_1 = alderman_own, alderman_2 = alderman_neighbor),
  sales %>% transmute(score_vintage = 2022L, analysis = "home_sales", alderman_1 = alderman_own, alderman_2 = alderman_neighbor),
  permit_panel %>% distinct(block_id, alderman_origin_2014, alderman_dest_2014) %>%
    transmute(score_vintage = 2014L, analysis = "permit_event_study", alderman_1 = alderman_origin_2014, alderman_2 = alderman_dest_2014)
) %>%
  filter(!is.na(alderman_1), !is.na(alderman_2), alderman_1 != alderman_2) %>%
  mutate(
    alderman_a = pmin(alderman_1, alderman_2),
    alderman_b = pmax(alderman_1, alderman_2)
  ) %>%
  count(score_vintage, analysis, alderman_a, alderman_b, name = "n_analysis_rows")

pair_usage <- comparison_rows %>%
  group_by(score_vintage, alderman_a, alderman_b) %>%
  summarise(
    analyses = paste(sort(unique(analysis)), collapse = "; "),
    n_analysis_rows = sum(n_analysis_rows),
    .groups = "drop"
  )

pair_usage_by_analysis <- comparison_rows %>%
  pivot_wider(
    names_from = analysis,
    values_from = n_analysis_rows,
    values_fill = 0,
    names_prefix = "n_"
  )

pair_usage <- pair_usage %>%
  left_join(
    pair_usage_by_analysis,
    by = c("score_vintage", "alderman_a", "alderman_b"),
    relationship = "many-to-one"
  )

pair_probabilities <- lapply(seq_len(nrow(pair_usage)), function(i) {
  pair <- pair_usage[i, ]
  draws <- if (pair$score_vintage == 2022L) score_matrix_2022 else score_matrix_2014
  production <- if (pair$score_vintage == 2022L) production_score_2022 else production_score_2014
  draw_difference <- draws[pair$alderman_a, ] - draws[pair$alderman_b, ]
  production_difference <- production[[pair$alderman_a]] - production[[pair$alderman_b]]
  tibble(
    score_vintage = pair$score_vintage,
    alderman_a = pair$alderman_a,
    alderman_b = pair$alderman_b,
    analyses = pair$analyses,
    n_analysis_rows = pair$n_analysis_rows,
    n_density_multifamily = pair$n_density_multifamily,
    n_rental_prices = pair$n_rental_prices,
    n_home_sales = pair$n_home_sales,
    n_permit_event_study = pair$n_permit_event_study,
    production_difference = production_difference,
    probability_a_more_stringent = mean(draw_difference > 0),
    probability_production_order_retained = if (production_difference > 0) {
      mean(draw_difference > 0)
    } else {
      mean(draw_difference < 0)
    },
    difference_p025 = quantile(draw_difference, 0.025),
    difference_median = median(draw_difference),
    difference_p975 = quantile(draw_difference, 0.975)
  )
}) %>%
  bind_rows() %>%
  arrange(score_vintage, probability_production_order_retained, alderman_a, alderman_b)

write_csv(
  pair_probabilities,
  sprintf("../output/pair_ordering_probabilities_%ddraws.csv", n_draws)
)

pair_ordering_summary <- comparison_rows %>%
  left_join(
    pair_probabilities %>%
      select(score_vintage, alderman_a, alderman_b, probability_production_order_retained),
    by = c("score_vintage", "alderman_a", "alderman_b"),
    relationship = "many-to-one"
  ) %>%
  group_by(score_vintage, analysis) %>%
  summarise(
    n_pairs = n(),
    total_analysis_rows = sum(n_analysis_rows),
    expected_row_order_flip_rate = weighted.mean(
      1 - probability_production_order_retained,
      n_analysis_rows
    ),
    share_rows_in_pairs_below_95pct = sum(
      n_analysis_rows[probability_production_order_retained < 0.95]
    ) / sum(n_analysis_rows),
    share_rows_in_pairs_below_80pct = sum(
      n_analysis_rows[probability_production_order_retained < 0.80]
    ) / sum(n_analysis_rows),
    .groups = "drop"
  )

write_csv(
  pair_ordering_summary,
  sprintf("../output/pair_ordering_summary_%ddraws.csv", n_draws)
)

density_formula_continuous_far <- log(density_far) ~ score_own + lenient_dist + strict_dist +
  share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year
density_formula_continuous_dupac <- log(density_dupac) ~ score_own + lenient_dist + strict_dist +
  share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year
density_formula_binary_far <- log(density_far) ~ side + lenient_dist + strict_dist +
  share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year
density_formula_binary_dupac <- log(density_dupac) ~ side + lenient_dist + strict_dist +
  share_white_own + share_black_own + median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year

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
rent_formula <- as.formula(paste0("log(rent_price) ~ ", rent_rhs, " | segment_id^year_month"))
sales_formula <- as.formula(paste0(
  "log(sale_price) ~ right + ",
  paste(c(sales_hedonics, sales_amenities), collapse = " + "),
  " | segment_id^year_quarter"
))
permit_formula <- outcome ~ post_treat +
  pre_period_permit_volume:factor(year) +
  no_pre_period_permits:factor(year) |
  block_id + ward_pair_id^year

estimate_headlines <- function(score_2022, score_2014, draw_id, clustered_se = FALSE) {
  density_draw <- density %>%
    mutate(
      score_own = unname(score_2022[alderman_own]),
      score_neighbor = unname(score_2022[alderman_neighbor]),
      sign_draw = if_else(score_own > score_neighbor, 1, -1),
      signed_distance_draw = dist_to_boundary_m * sign_draw,
      lenient_dist = abs(signed_distance_draw) * as.integer(signed_distance_draw <= 0),
      strict_dist = abs(signed_distance_draw) * as.integer(signed_distance_draw > 0),
      side = as.integer(signed_distance_draw > 0)
    )

  density_cluster <- if (clustered_se) ~ward_pair else NULL
  density_far_continuous <- feols(density_formula_continuous_far, density_draw, cluster = density_cluster, warn = FALSE, notes = FALSE)
  density_dupac_continuous <- feols(density_formula_continuous_dupac, density_draw, cluster = density_cluster, warn = FALSE, notes = FALSE)
  density_far_binary <- feols(density_formula_binary_far, density_draw, cluster = density_cluster, warn = FALSE, notes = FALSE)
  density_dupac_binary <- feols(density_formula_binary_dupac, density_draw, cluster = density_cluster, warn = FALSE, notes = FALSE)

  rent_draw <- rent %>%
    mutate(
      score_own = unname(score_2022[alderman_own]),
      score_neighbor = unname(score_2022[alderman_neighbor]),
      right = as.integer(score_own > score_neighbor)
    )
  rent_cluster <- if (clustered_se) ~segment_id else NULL
  rent_model <- feols(rent_formula, rent_draw, cluster = rent_cluster, warn = FALSE, notes = FALSE)

  sales_draw <- sales %>%
    mutate(
      score_own = unname(score_2022[alderman_own]),
      score_neighbor = unname(score_2022[alderman_neighbor]),
      right = as.integer(score_own > score_neighbor)
    )
  sales_cluster <- if (clustered_se) ~segment_id else NULL
  sales_model <- feols(sales_formula, sales_draw, cluster = sales_cluster, warn = FALSE, notes = FALSE)

  permit_high_draw <- permit_high %>%
    mutate(
      strictness_change = unname(score_2014[alderman_dest_2014]) - unname(score_2014[alderman_origin_2014]),
      post_treat = as.integer(relative_year >= 0) * strictness_change
    )
  permit_low_draw <- permit_low %>%
    mutate(
      strictness_change = unname(score_2014[alderman_dest_2014]) - unname(score_2014[alderman_origin_2014]),
      post_treat = as.integer(relative_year >= 0) * strictness_change
    )
  permit_cluster <- if (clustered_se) ~ward_pair_id else NULL
  permit_high_model <- fepois(permit_formula, permit_high_draw, cluster = permit_cluster, warn = FALSE, notes = FALSE)
  permit_low_model <- fepois(permit_formula, permit_low_draw, cluster = permit_cluster, warn = FALSE, notes = FALSE)

  estimates <- tibble(
    draw = draw_id,
    density_multifamily_far_continuous = coef(density_far_continuous)[["score_own"]],
    density_multifamily_dupac_continuous = coef(density_dupac_continuous)[["score_own"]],
    density_multifamily_far_binary = coef(density_far_binary)[["side"]],
    density_multifamily_dupac_binary = coef(density_dupac_binary)[["side"]],
    rental_price_binary = coef(rent_model)[["right"]],
    home_sale_price_binary = coef(sales_model)[["right"]],
    permit_high_discretion_pooled = coef(permit_high_model)[["post_treat"]],
    permit_low_discretion_pooled = coef(permit_low_model)[["post_treat"]]
  )

  if (!clustered_se) {
    return(estimates)
  }

  tibble(
    draw = draw_id,
    density_multifamily_far_continuous = se(density_far_continuous)[["score_own"]],
    density_multifamily_dupac_continuous = se(density_dupac_continuous)[["score_own"]],
    density_multifamily_far_binary = se(density_far_binary)[["side"]],
    density_multifamily_dupac_binary = se(density_dupac_binary)[["side"]],
    rental_price_binary = se(rent_model)[["right"]],
    home_sale_price_binary = se(sales_model)[["right"]],
    permit_high_discretion_pooled = se(permit_high_model)[["post_treat"]],
    permit_low_discretion_pooled = se(permit_low_model)[["post_treat"]]
  )
}

production_estimates <- estimate_headlines(
  production_score_2022,
  production_score_2014,
  0L
)
production_standard_errors <- estimate_headlines(
  production_score_2022,
  production_score_2014,
  0L,
  clustered_se = TRUE
)

setFixest_nthreads(1)
available_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(available_cores) || available_cores < 1) {
  available_cores <- workers
}
workers <- max(1L, min(workers, available_cores, n_draws))

started_at <- Sys.time()
draw_estimates <- parallel::mclapply(
  seq_len(n_draws),
  function(draw_id) {
    estimate_headlines(
      setNames(score_matrix_2022[, as.character(draw_id)], rownames(score_matrix_2022)),
      setNames(score_matrix_2014[, as.character(draw_id)], rownames(score_matrix_2014)),
      draw_id
    )
  },
  mc.cores = workers,
  mc.preschedule = TRUE
)
elapsed_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

if (any(vapply(draw_estimates, inherits, logical(1), what = "try-error"))) {
  stop("At least one headline-estimate draw failed.", call. = FALSE)
}

draw_estimates <- bind_rows(draw_estimates) %>% arrange(draw)
if (nrow(draw_estimates) != n_draws || any(!is.finite(as.matrix(draw_estimates[, -1])))) {
  stop("Headline-estimate bootstrap output is incomplete or non-finite.", call. = FALSE)
}

write_csv(
  draw_estimates,
  sprintf("../output/headline_estimate_uncertainty_%ddraws.csv", n_draws)
)

estimate_summary <- draw_estimates %>%
  pivot_longer(-draw, names_to = "estimate", values_to = "bootstrap_value") %>%
  left_join(
    production_estimates %>%
      pivot_longer(-draw, names_to = "estimate", values_to = "production_estimate") %>%
      select(-draw),
    by = "estimate",
    relationship = "many-to-one"
  ) %>%
  left_join(
    production_standard_errors %>%
      pivot_longer(-draw, names_to = "estimate", values_to = "production_clustered_se") %>%
      select(-draw),
    by = "estimate",
    relationship = "many-to-one"
  ) %>%
  group_by(estimate, production_estimate, production_clustered_se) %>%
  summarise(
    bootstrap_mean = mean(bootstrap_value),
    bootstrap_sd = sd(bootstrap_value),
    p025 = quantile(bootstrap_value, 0.025),
    median = median(bootstrap_value),
    p975 = quantile(bootstrap_value, 0.975),
    probability_negative = mean(bootstrap_value < 0),
    probability_positive = mean(bootstrap_value > 0),
    .groups = "drop"
  ) %>%
  mutate(
    score_sd_to_clustered_se = bootstrap_sd / production_clustered_se,
    n_draws = n_draws,
    workers = workers,
    elapsed_seconds = elapsed_seconds
  )

write_csv(
  estimate_summary,
  sprintf("../output/headline_estimate_uncertainty_summary_%ddraws.csv", n_draws)
)
