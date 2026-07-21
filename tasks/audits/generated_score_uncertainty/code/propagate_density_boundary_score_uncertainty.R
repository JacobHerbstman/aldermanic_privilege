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
if (!is.finite(n_draws) || n_draws < 1 || !is.finite(workers) || workers < 1) {
  stop("Draws and workers must be positive integers.", call. = FALSE)
}

score_draws <- read_parquet(
  sprintf("../output/score_draws_through2022_%ddraws.parquet", n_draws)
) %>%
  as_tibble() %>%
  select(draw, alderman, score)

if (n_distinct(score_draws$draw) != n_draws) {
  stop("Score-draw input does not contain the requested number of draws.", call. = FALSE)
}

wide_scores <- score_draws %>%
  mutate(draw = as.character(draw)) %>%
  pivot_wider(names_from = draw, values_from = score) %>%
  arrange(alderman)

score_matrix <- as.matrix(wide_scores[, -1, drop = FALSE])
rownames(score_matrix) <- wide_scores$alderman
storage.mode(score_matrix) <- "double"

production_scores <- read_csv(
  "../input/alderman_uncertainty_index_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index) %>%
  deframe()

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
    construction_year = suppressWarnings(as.integer(construction_year)),
    segment_id = as.character(segment_id),
    zone_group = construction_zone_group
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    unitscount > 1,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    !is.na(construction_zone_group),
    !is.na(segment_id),
    segment_id != "",
    is.finite(density_far),
    density_far > 0,
    is.finite(density_dupac),
    density_dupac > 0,
    !is.na(alderman_own),
    !is.na(alderman_neighbor),
    if_all(all_of(demographic_controls), is.finite)
  )

density_formula_binary_far <- log(density_far) ~ side + pair_average_score +
  lenient_dist + strict_dist + share_white_own + share_black_own +
  median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year
density_formula_binary_dupac <- log(density_dupac) ~ side + pair_average_score +
  lenient_dist + strict_dist + share_white_own + share_black_own +
  median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year
density_formula_continuous_far <- log(density_far) ~ continuous_score_difference + pair_average_score +
  lenient_dist + strict_dist + share_white_own + share_black_own +
  median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year
density_formula_continuous_dupac <- log(density_dupac) ~ continuous_score_difference + pair_average_score +
  lenient_dist + strict_dist + share_white_own + share_black_own +
  median_hh_income_own + share_bach_plus_own + homeownership_rate_own |
  zone_group + segment_id + construction_year

estimate_density <- function(scores, draw_id, clustered_se = FALSE) {
  draw_data <- density %>%
    mutate(
      score_own = unname(scores[alderman_own]),
      score_neighbor = unname(scores[alderman_neighbor]),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2,
      sign_draw = if_else(score_own > score_neighbor, 1, -1),
      signed_distance_draw = dist_to_boundary_m * sign_draw,
      lenient_dist = abs(signed_distance_draw) * as.integer(signed_distance_draw <= 0),
      strict_dist = abs(signed_distance_draw) * as.integer(signed_distance_draw > 0),
      side = as.integer(signed_distance_draw > 0)
    )

  if (anyNA(draw_data$score_own) || anyNA(draw_data$score_neighbor)) {
    stop("A score draw is missing an alderman used by the density sample.", call. = FALSE)
  }

  cluster_formula <- if (clustered_se) ~ward_pair else NULL
  models <- list(
    density_multifamily_far_binary = feols(
      density_formula_binary_far,
      draw_data,
      cluster = cluster_formula,
      warn = FALSE,
      notes = FALSE
    ),
    density_multifamily_dupac_binary = feols(
      density_formula_binary_dupac,
      draw_data,
      cluster = cluster_formula,
      warn = FALSE,
      notes = FALSE
    ),
    density_multifamily_far_continuous = feols(
      density_formula_continuous_far,
      draw_data,
      cluster = cluster_formula,
      warn = FALSE,
      notes = FALSE
    ),
    density_multifamily_dupac_continuous = feols(
      density_formula_continuous_dupac,
      draw_data,
      cluster = cluster_formula,
      warn = FALSE,
      notes = FALSE
    )
  )

  treatment_terms <- c(
    density_multifamily_far_binary = "side",
    density_multifamily_dupac_binary = "side",
    density_multifamily_far_continuous = "continuous_score_difference",
    density_multifamily_dupac_continuous = "continuous_score_difference"
  )

  values <- vapply(names(models), function(model_name) {
    term <- treatment_terms[[model_name]]
    if (clustered_se) {
      return(unname(se(models[[model_name]])[[term]]))
    }
    unname(coef(models[[model_name]])[[term]])
  }, numeric(1))

  as_tibble_row(c(draw = draw_id, values))
}

production_estimates <- estimate_density(production_scores, 0L)
production_standard_errors <- estimate_density(production_scores, 0L, clustered_se = TRUE)

setFixest_nthreads(1)
workers <- max(1L, min(workers, parallel::detectCores(logical = FALSE), n_draws))

started_at <- Sys.time()
draw_estimates <- parallel::mclapply(
  seq_len(n_draws),
  function(draw_id) {
    estimate_density(
      setNames(score_matrix[, as.character(draw_id)], rownames(score_matrix)),
      draw_id
    )
  },
  mc.cores = workers,
  mc.preschedule = TRUE
) %>%
  bind_rows() %>%
  arrange(draw)
elapsed_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

if (nrow(draw_estimates) != n_draws || any(!is.finite(as.matrix(draw_estimates[, -1])))) {
  stop("Density estimate draws are incomplete or non-finite.", call. = FALSE)
}

write_csv(
  draw_estimates,
  sprintf("../output/density_boundary_estimate_uncertainty_%ddraws.csv", n_draws)
)

summary <- draw_estimates %>%
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
  summary,
  sprintf("../output/density_boundary_estimate_uncertainty_summary_%ddraws.csv", n_draws)
)
