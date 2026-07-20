# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/generated_score_uncertainty/code")
# n_draws <- 2000
# score_cutoff <- 2022
# seed <- 20260719
# workers <- 8

source("../../../_lib/alderman_uncertainty_helpers.R")
library(arrow)

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(n_draws, score_cutoff, seed, workers)
}
if (length(cli_args) != 4) {
  stop("Script requires number of draws, score cutoff, seed, and workers.", call. = FALSE)
}

n_draws <- as.integer(cli_args[1])
score_cutoff <- as.integer(cli_args[2])
seed <- as.integer(cli_args[3])
workers <- as.integer(cli_args[4])

if (!is.finite(n_draws) || n_draws < 1) {
  stop("n_draws must be a positive integer.", call. = FALSE)
}
if (!score_cutoff %in% c(2014L, 2022L)) {
  stop("score_cutoff must be 2014 or 2022.", call. = FALSE)
}
if (!is.finite(seed) || seed < 1) {
  stop("seed must be a positive integer.", call. = FALSE)
}
if (!is.finite(workers) || workers < 1) {
  stop("workers must be a positive integer.", call. = FALSE)
}

config <- default_uncertainty_config()

permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv") %>%
  filter(month <= as.yearmon(as.Date(sprintf("%d-12-01", score_cutoff))))

prepared <- prepare_uncertainty_sample(
  permits,
  include_porch = config$include_porch,
  volume_ctrl = config$volume_ctrl,
  volume_stage = config$volume_stage
)

covariates <- get_stage1_covariates(
  prepared$place_covariates,
  prepared$include_volume_stage1,
  prepared$volume_var,
  drop_covariates = c("share_bach_plus", "median_hh_income_10k")
)
fe_terms <- get_stage1_fe_terms(config)

stage1 <- fit_stage1_model(
  permits = prepared$permits,
  stage1_outcome = "log_processing_time",
  covariates = covariates,
  fe_terms = fe_terms,
  variant_id = "baseline"
)

stage1_data <- stage1$permits_for_reg %>%
  mutate(
    bootstrap_cell = interaction(ward, month, alderman, drop = TRUE),
    bootstrap_cell_id = as.integer(bootstrap_cell)
  )

cell_data <- stage1_data %>%
  group_by(bootstrap_cell_id, ward, month, alderman) %>%
  summarise(
    mean_resid_wm = mean(resid),
    n_permits_wm = n(),
    n_permits_wm_l1 = first(n_permits_wm_l1),
    .groups = "drop"
  ) %>%
  arrange(bootstrap_cell_id)

if (!identical(cell_data$bootstrap_cell_id, seq_len(nrow(cell_data)))) {
  stop("Bootstrap cell IDs are not consecutive.", call. = FALSE)
}

stage1_formula <- as.formula(paste0(
  "log_processing_time ~ ",
  paste(covariates, collapse = " + "),
  " | ",
  paste(fe_terms, collapse = " + ")
))
stage2_formula <- as.formula(paste0(
  "mean_resid_wm ~ 0 + i(alderman) + ",
  prepared$volume_var
))

rows_by_cell <- split(seq_len(nrow(stage1_data)), stage1_data$bootstrap_cell_id)
cells_by_alderman <- split(cell_data$bootstrap_cell_id, cell_data$alderman)

baseline <- build_residualized_uncertainty_index(
  permits = permits,
  config = config,
  variant_id = "baseline",
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus", "median_hh_income_10k"),
  construction_rule = "Paper score"
)$alderman_index %>%
  arrange(alderman)

published <- read_csv(
  sprintf("../input/alderman_uncertainty_index_through%d.csv", score_cutoff),
  show_col_types = FALSE
) %>%
  arrange(alderman)

if (!identical(baseline$alderman, published$alderman) ||
    max(abs(baseline$uncertainty_index - published$uncertainty_index)) > 1e-10) {
  stop("Audit score reconstruction does not match the production score.", call. = FALSE)
}

bootstrap_one_draw <- function(draw_id) {
  set.seed(seed + draw_id)
  sampled_cells <- unlist(
    lapply(cells_by_alderman, function(cell_ids) {
      sample(cell_ids, length(cell_ids), replace = TRUE)
    }),
    use.names = FALSE
  )
  sampled_rows <- rows_by_cell[sampled_cells]
  permit_indices <- unlist(sampled_rows, use.names = FALSE)
  bootstrap_instance <- rep(seq_along(sampled_cells), lengths(sampled_rows))

  draw_stage1_data <- stage1_data[permit_indices, , drop = FALSE]
  draw_stage1_data$bootstrap_instance <- bootstrap_instance

  stage1_model <- feols(
    stage1_formula,
    data = draw_stage1_data,
    warn = FALSE,
    notes = FALSE
  )

  residual_sums <- rowsum(
    as.numeric(resid(stage1_model)),
    draw_stage1_data$bootstrap_instance,
    reorder = FALSE
  )[, 1]

  draw_cell_data <- cell_data[sampled_cells, , drop = FALSE]
  draw_cell_data$mean_resid_wm <- residual_sums / draw_cell_data$n_permits_wm

  stage2_model <- feols(
    stage2_formula,
    data = draw_cell_data,
    weights = ~n_permits_wm,
    se = "hetero",
    warn = FALSE,
    notes = FALSE
  )

  alderman_effects <- enframe(
    coef(stage2_model),
    name = "term",
    value = "alderman_fe"
  ) %>%
    filter(str_detect(term, "^alderman::")) %>%
    mutate(alderman = str_remove(term, "^alderman::"))

  stage2_vcov <- vcov(stage2_model, se = "hetero")
  alderman_vcov <- stage2_vcov[
    alderman_effects$term,
    alderman_effects$term,
    drop = FALSE
  ]
  centering_matrix <- diag(nrow(alderman_effects)) -
    matrix(1 / nrow(alderman_effects), nrow(alderman_effects), nrow(alderman_effects))
  centered_vcov <- centering_matrix %*% alderman_vcov %*% centering_matrix

  alderman_effects <- alderman_effects %>%
    mutate(
      alderman_fe_centered = alderman_fe - mean(alderman_fe),
      alderman_se = sqrt(pmax(diag(centered_vcov), 0))
    )

  tau2 <- max(
    0,
    var(alderman_effects$alderman_fe_centered) - mean(alderman_effects$alderman_se^2)
  )

  alderman_effects %>%
    transmute(
      draw = draw_id,
      alderman,
      score = standardize_uncertainty(
        alderman_fe_centered * tau2 / (tau2 + alderman_se^2)
      ),
      tau2,
      raw_effect_variance = var(alderman_fe_centered),
      mean_squared_se = mean(alderman_se^2)
    )
}

setFixest_nthreads(1)
available_cores <- parallel::detectCores(logical = FALSE)
if (!is.finite(available_cores) || available_cores < 1) {
  available_cores <- workers
}
workers <- max(1L, min(workers, available_cores, n_draws))

started_at <- Sys.time()
draws <- parallel::mclapply(
  seq_len(n_draws),
  bootstrap_one_draw,
  mc.cores = workers,
  mc.preschedule = TRUE
)
elapsed_seconds <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))

if (any(vapply(draws, inherits, logical(1), what = "try-error"))) {
  stop("At least one score bootstrap draw failed.", call. = FALSE)
}

draws <- bind_rows(draws) %>%
  arrange(draw, alderman)

if (n_distinct(draws$draw) != n_draws ||
    any(count(draws, draw)$n != nrow(baseline)) ||
    any(!is.finite(draws$score))) {
  stop("Score bootstrap output is incomplete or non-finite.", call. = FALSE)
}

write_parquet(
  draws,
  sprintf("../output/score_draws_through%d_%ddraws.parquet", score_cutoff, n_draws),
  compression = "zstd"
)

write_csv(
  tibble(
    score_cutoff,
    n_draws,
    seed,
    workers,
    n_permits = nrow(stage1_data),
    n_bootstrap_cells = nrow(cell_data),
    n_aldermen = nrow(baseline),
    elapsed_seconds
  ),
  sprintf("../output/score_draws_through%d_%ddraws_summary.csv", score_cutoff, n_draws)
)
