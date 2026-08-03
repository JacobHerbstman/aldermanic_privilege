# setwd("tasks/audits/score_vintage_reflection/code")

source("../../../_lib/alderman_uncertainty_helpers.R")

config <- default_uncertainty_config()
permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv") |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::filter(month <= zoo::as.yearmon(as.Date("2019-06-01")))

estimate_score <- function(data, period) {
  prepared <- prepare_uncertainty_sample(
    data,
    include_porch = config$include_porch,
    volume_ctrl = config$volume_ctrl,
    volume_stage = config$volume_stage
  )
  covariates <- get_stage1_covariates(
    prepared$place_covariates,
    prepared$include_volume_stage1,
    prepared$volume_var,
    drop_covariates = "share_bach_plus"
  )
  stage1 <- fit_stage1_model(
    permits = prepared$permits,
    stage1_outcome = "log_processing_time",
    covariates = covariates,
    fe_terms = get_stage1_fe_terms(config),
    variant_id = period
  )
  stage2 <- build_two_stage_index(
    stage1$permits_for_reg,
    include_volume_stage2 = prepared$include_volume_stage2,
    volume_var = prepared$volume_var,
    stage2_weight = config$stage2_weight
  )

  permit_stats <- stage1$permits_for_reg |>
    dplyr::summarise(
      mean_processing_days = mean(processing_time),
      median_processing_days = stats::median(processing_time),
      p90_processing_days = stats::quantile(processing_time, 0.9),
      mean_stage1_residual = mean(resid),
      .by = alderman
    )

  score <- stage2$alderman_index |>
    dplyr::transmute(
      alderman,
      period,
      n_permits,
      score = uncertainty_index,
      raw_effect = alderman_fe_raw,
      shrunk_effect = mean_resid,
      shrinkage = shrinkage_B
    ) |>
    dplyr::left_join(permit_stats, by = "alderman", relationship = "one-to-one")

  scale <- tibble::tibble(
    period,
    n_aldermen = nrow(score),
    n_stage1_permits = nrow(stage1$permits_for_reg),
    mean_processing_days = mean(stage1$permits_for_reg$processing_time),
    median_processing_days = stats::median(stage1$permits_for_reg$processing_time)
  )

  list(score = score, scale = scale)
}

early_permits <- permits |>
  dplyr::filter(year <= 2014L)
transition_permits <- permits |>
  dplyr::filter(month >= zoo::as.yearmon(as.Date("2015-01-01")))

early <- estimate_score(early_permits, "2006_2014")
transition <- estimate_score(transition_permits, "2015_2019_06")

published_early <- readr::read_csv(
  "../input/alderman_uncertainty_index_through2014.csv",
  show_col_types = FALSE
) |>
  dplyr::transmute(alderman, published_score = uncertainty_index)
early_validation <- early$score |>
  dplyr::inner_join(published_early, by = "alderman", relationship = "one-to-one") |>
  dplyr::mutate(difference = score - published_score)
if (
  nrow(early_validation) != nrow(early$score) ||
    max(abs(early_validation$difference)) > 1e-10
) {
  stop("The reconstructed 2006-2014 score does not match the archived score.", call. = FALSE)
}

prepared_common <- prepare_uncertainty_sample(
  permits,
  include_porch = config$include_porch,
  volume_ctrl = config$volume_ctrl,
  volume_stage = config$volume_stage
)
common_covariates <- get_stage1_covariates(
  prepared_common$place_covariates,
  prepared_common$include_volume_stage1,
  prepared_common$volume_var,
  drop_covariates = "share_bach_plus"
)
common_stage1 <- fit_stage1_model(
  permits = prepared_common$permits,
  stage1_outcome = "log_processing_time",
  covariates = common_covariates,
  fe_terms = get_stage1_fe_terms(config),
  variant_id = "common_2006_2019_06"
)$permits_for_reg |>
  dplyr::mutate(year = as.integer(year))

common_permit_stats <- common_stage1 |>
  dplyr::mutate(period = dplyr::if_else(year <= 2014L, "2006_2014", "2015_2019_06")) |>
  dplyr::summarise(
    mean_processing_days = mean(processing_time),
    median_processing_days = stats::median(processing_time),
    p90_processing_days = stats::quantile(processing_time, 0.9),
    mean_stage1_residual = mean(resid),
    .by = c(period, alderman)
  )

common_early <- build_two_stage_index(
  common_stage1 |> dplyr::filter(year <= 2014L),
  include_volume_stage2 = prepared_common$include_volume_stage2,
  volume_var = prepared_common$volume_var,
  stage2_weight = config$stage2_weight
)$alderman_index |>
  dplyr::transmute(
    alderman,
    period = "2006_2014",
    n_permits,
    score = uncertainty_index,
    raw_effect = alderman_fe_raw,
    shrunk_effect = mean_resid,
    shrinkage = shrinkage_B
  ) |>
  dplyr::left_join(
    common_permit_stats |> dplyr::filter(period == "2006_2014"),
    by = c("alderman", "period"),
    relationship = "one-to-one"
  )
common_transition <- build_two_stage_index(
  common_stage1 |> dplyr::filter(year >= 2015L),
  include_volume_stage2 = prepared_common$include_volume_stage2,
  volume_var = prepared_common$volume_var,
  stage2_weight = config$stage2_weight
)$alderman_index |>
  dplyr::transmute(
    alderman,
    period = "2015_2019_06",
    n_permits,
    score = uncertainty_index,
    raw_effect = alderman_fe_raw,
    shrunk_effect = mean_resid,
    shrinkage = shrinkage_B
  ) |>
  dplyr::left_join(
    common_permit_stats |> dplyr::filter(period == "2015_2019_06"),
    by = c("alderman", "period"),
    relationship = "one-to-one"
  )

summarise_correlation <- function(early_score, later_score, adjustment) {
  comparison <- early_score |>
    dplyr::select(
      alderman,
      n_permits_early = n_permits,
      score_early = score,
      raw_effect_early = raw_effect,
      mean_days_early = mean_processing_days,
      median_days_early = median_processing_days,
      p90_days_early = p90_processing_days,
      mean_stage1_residual_early = mean_stage1_residual
    ) |>
    dplyr::inner_join(
      later_score |>
        dplyr::select(
          alderman,
          n_permits_later = n_permits,
          score_later = score,
          raw_effect_later = raw_effect,
          mean_days_later = mean_processing_days,
          median_days_later = median_processing_days,
          p90_days_later = p90_processing_days,
          mean_stage1_residual_later = mean_stage1_residual
        ),
      by = "alderman",
      relationship = "one-to-one"
    )

  summary <- lapply(c(0L, 100L, 250L), function(minimum_permits) {
    selected <- comparison |>
      dplyr::filter(
        n_permits_early >= minimum_permits,
        n_permits_later >= minimum_permits
      )
    tibble::tibble(
      adjustment,
      minimum_permits_each_period = minimum_permits,
      n_aldermen = nrow(selected),
      pearson_correlation = stats::cor(selected$score_early, selected$score_later),
      spearman_correlation = stats::cor(
        selected$score_early,
        selected$score_later,
        method = "spearman"
      )
    )
  }) |>
    dplyr::bind_rows()

  list(comparison = comparison, summary = summary)
}

period_specific <- summarise_correlation(
  early$score,
  transition$score,
  "period_specific_first_stage"
)
common_adjustment <- summarise_correlation(
  common_early,
  common_transition,
  "common_2006_2019_first_stage"
)

regime_score_comparison <- period_specific$comparison |>
  dplyr::rename_with(
    ~paste0(.x, "_period_specific"),
    -alderman
  ) |>
  dplyr::full_join(
    common_adjustment$comparison |>
      dplyr::rename_with(~paste0(.x, "_common_adjustment"), -alderman),
    by = "alderman",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    score_change_period_specific =
      score_later_period_specific - score_early_period_specific,
    rank_early = rank(score_early_period_specific, na.last = "keep"),
    rank_later = rank(score_later_period_specific, na.last = "keep"),
    rank_change = rank_later - rank_early
  ) |>
  dplyr::arrange(dplyr::desc(abs(score_change_period_specific)))

readr::write_csv(
  dplyr::bind_rows(period_specific$summary, common_adjustment$summary),
  "../output/regime_score_stability.csv"
)
readr::write_csv(
  regime_score_comparison,
  "../output/regime_score_comparison.csv"
)
readr::write_csv(
  dplyr::bind_rows(early$scale, transition$scale),
  "../output/regime_period_scale.csv"
)
saveRDS(
  list(
    early = early$score,
    transition = transition$score,
    common_early = common_early,
    common_transition = common_transition
  ),
  "../output/regime_scores.rds"
)
