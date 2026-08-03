# setwd("tasks/audits/score_vintage_reflection/code")

source("../../../_lib/alderman_uncertainty_helpers.R")

config <- default_uncertainty_config()
permits <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv") |>
  dplyr::mutate(year = as.integer(year)) |>
  dplyr::filter(month <= zoo::as.yearmon(as.Date("2022-12-01")))

summarise_period <- function(data, period) {
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
    permits_for_reg = stage1$permits_for_reg,
    include_volume_stage2 = prepared$include_volume_stage2,
    volume_var = prepared$volume_var,
    stage2_weight = config$stage2_weight
  )

  permit_stats <- stage1$permits_for_reg |>
    dplyr::group_by(alderman) |>
    dplyr::summarise(
      n_stage1_permits = dplyr::n(),
      n_ward_months = dplyr::n_distinct(paste(ward, month)),
      mean_days = mean(processing_time),
      median_days = stats::median(processing_time),
      p90_days = stats::quantile(processing_time, 0.9),
      mean_log_days = mean(log_processing_time),
      mean_stage1_residual = mean(resid),
      mean_lagged_ward_month_permits = mean(n_permits_wm_l1),
      share_new_construction = mean(permit_type_clean == "new_construction"),
      share_renovation = mean(permit_type_clean == "renovation"),
      share_demolition = mean(permit_type_clean == "demolition"),
      share_porch = mean(permit_type_clean == "porch"),
      share_self_cert = mean(review_type_clean == "SELF CERT"),
      .groups = "drop"
    )

  alderman_stats <- stage2$alderman_index |>
    dplyr::left_join(permit_stats, by = "alderman", relationship = "one-to-one") |>
    dplyr::mutate(period)

  scale_stats <- alderman_stats |>
    dplyr::summarise(
      period = dplyr::first(period),
      n_aldermen = dplyr::n(),
      n_stage1_permits = nrow(stage1$permits_for_reg),
      overall_mean_days = mean(stage1$permits_for_reg$processing_time),
      overall_median_days = stats::median(stage1$permits_for_reg$processing_time),
      stage2_tau2 = stage2$stage2_tau2,
      sd_raw_effect = stats::sd(alderman_fe_raw),
      sd_shrunk_effect = stats::sd(mean_resid),
      median_shrinkage = stats::median(shrinkage_B),
      minimum_shrinkage = min(shrinkage_B),
      maximum_shrinkage = max(shrinkage_B)
    )

  list(alderman = alderman_stats, scale = scale_stats)
}

early <- summarise_period(permits |> dplyr::filter(year <= 2014L), "2006_2014")
late <- summarise_period(permits |> dplyr::filter(year >= 2015L), "2015_2022")

score_change_decomposition <- early$alderman |>
  dplyr::inner_join(
    late$alderman,
    by = "alderman",
    relationship = "one-to-one",
    suffix = c("_2006_2014", "_2015_2022")
  ) |>
  dplyr::mutate(
    score_change = uncertainty_index_2015_2022 - uncertainty_index_2006_2014,
    raw_effect_change = alderman_fe_raw_2015_2022 - alderman_fe_raw_2006_2014,
    shrunk_effect_change = mean_resid_2015_2022 - mean_resid_2006_2014,
    mean_days_change = mean_days_2015_2022 - mean_days_2006_2014,
    mean_stage1_residual_change =
      mean_stage1_residual_2015_2022 - mean_stage1_residual_2006_2014
  ) |>
  dplyr::arrange(dplyr::desc(abs(score_change)))

readr::write_csv(
  score_change_decomposition,
  "../output/score_change_decomposition.csv"
)
readr::write_csv(
  dplyr::bind_rows(early$scale, late$scale),
  "../output/score_period_scale.csv"
)
