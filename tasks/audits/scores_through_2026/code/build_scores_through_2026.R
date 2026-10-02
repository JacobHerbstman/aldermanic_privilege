# setwd("tasks/audits/scores_through_2026/code")
# Exploratory: the paper's alderman stringency score (two-stage processing-time index, 2006-2022 permits) re-estimated
# on high-discretion permits filed through June 2026. The estimation follows tasks/density_score_robustness: the
# paper's configuration, rebuilt from the shared helpers and first checked against the published 2006-2022 scores.
# From the second half of 2023 the city's records give self-certification permits a positive processing time (97
# percent were issued the day they were filed in 2020-2022, under 1 percent from 2024), so they enter the logged
# outcome for the first time; each cutoff is therefore estimated with all permits and without self-certification
# permits in every year.
last_month <- "2026-06"

# The helper sources the package setup by a path relative to tasks/shared/code.
source("../../../shared/code/alderman_uncertainty_helpers.R", chdir = TRUE)

config <- default_uncertainty_config()
fit_score <- function(permits) {
  prepared <- prepare_uncertainty_sample(permits, include_porch = config$include_porch,
    volume_ctrl = config$volume_ctrl, volume_stage = config$volume_stage)
  stage1 <- fit_stage1_model(
    permits = prepared$permits,
    stage1_outcome = "log_processing_time",
    covariates = get_stage1_covariates(prepared$place_covariates, prepared$include_volume_stage1,
      prepared$volume_var, drop_covariates = "share_bach_plus"),
    fe_terms = get_stage1_fe_terms(config),
    variant_id = "paper"
  )
  build_two_stage_index(permits_for_reg = stage1$permits_for_reg, include_volume_stage2 = prepared$include_volume_stage2,
    volume_var = prepared$volume_var, stage2_weight = config$stage2_weight)$alderman_index
}

permits_2022 <- load_uncertainty_permits("../input/permits_for_uncertainty_index.csv") |>
  dplyr::mutate(id = as.character(id), ward = as.character(ward))
permits_2026 <- load_uncertainty_permits("../output/permits_2023_2026_for_index.csv") |>
  dplyr::mutate(id = as.character(id), ward = as.character(ward))
stopifnot(!any(permits_2026$id %in% permits_2022$id))
permits <- dplyr::bind_rows(permits_2022, permits_2026) |>
  dplyr::filter(month <= zoo::as.yearmon(as.Date(paste0(last_month, "-01"))))

# The rebuilt 2006-2022 score must equal the published one.
published <- readr::read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE)
rebuilt <- fit_score(dplyr::filter(permits, month <= zoo::as.yearmon(as.Date("2022-12-01"))))
check <- dplyr::inner_join(dplyr::select(rebuilt, alderman, rebuilt = uncertainty_index),
  dplyr::select(published, alderman, published = uncertainty_index), by = "alderman", relationship = "one-to-one")
stopifnot(nrow(check) == nrow(published), nrow(check) == nrow(rebuilt), max(abs(check$rebuilt - check$published)) < 1e-10)

through_2022 <- dplyr::filter(permits, month <= zoo::as.yearmon(as.Date("2022-12-01")))
versions <- list(
  through_2022 = rebuilt,
  through_2022_no_self_cert = fit_score(dplyr::filter(through_2022, review_type_clean != "SELF CERT")),
  through_2026 = fit_score(permits),
  through_2026_no_self_cert = fit_score(dplyr::filter(permits, review_type_clean != "SELF CERT"))
)
scores <- dplyr::bind_rows(versions, .id = "score_version")
readr::write_csv(scores, "../output/scores_through_2026.csv")

# Each alderman's score in every version, with the months in which their permits were filed.
first_permit <- permits |>
  dplyr::summarise(first_month = as.character(min(month)), last_month = as.character(max(month)), .by = alderman)
comparison <- scores |>
  dplyr::select(score_version, alderman, uncertainty_index, n_permits) |>
  tidyr::pivot_wider(names_from = score_version, values_from = c(uncertainty_index, n_permits)) |>
  dplyr::left_join(first_permit, by = "alderman", relationship = "one-to-one")
readr::write_csv(comparison, "../output/score_comparison.csv")
