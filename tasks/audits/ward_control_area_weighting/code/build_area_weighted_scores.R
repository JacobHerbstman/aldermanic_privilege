# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/ward_control_area_weighting/code")

source("../../../_lib/alderman_uncertainty_helpers.R")

permits <- load_uncertainty_permits(
  "../input/permits_for_uncertainty_index_centroid.csv"
)
ward_controls <- read_csv(
  "../output/ward_controls_area_weighted_2000_2023.csv",
  show_col_types = FALSE
)

if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Area-weighted controls are not unique by ward and year.", call. = FALSE)
}

max_permit_year <- max(permits$year, na.rm = TRUE)
max_control_year <- max(ward_controls$year, na.rm = TRUE)
if (max_permit_year > max_control_year) {
  ward_controls <- bind_rows(
    ward_controls,
    ward_controls %>%
      filter(year == max_control_year) %>%
      select(-year) %>%
      tidyr::crossing(year = (max_control_year + 1):max_permit_year)
  )
}

control_columns <- c(
  "pop_total", "median_hh_income", "share_black", "share_hisp",
  "share_white", "homeownership_rate", "share_bach_plus"
)

permits_area_weighted <- permits %>%
  select(-all_of(control_columns)) %>%
  left_join(
    ward_controls %>% select(ward, year, all_of(control_columns)),
    by = c("ward", "year"),
    relationship = "many-to-one"
  )

if (nrow(permits_area_weighted) != nrow(permits) ||
    anyDuplicated(permits_area_weighted$id) > 0) {
  stop("Replacing ward controls changed the permit observation unit.", call. = FALSE)
}
if (any(!is.finite(as.matrix(permits_area_weighted %>% select(all_of(control_columns)))))) {
  stop("Area-weighted permit data contain missing controls.", call. = FALSE)
}

write_csv(
  permits_area_weighted,
  "../output/permits_for_uncertainty_index_area_weighted.csv"
)

config <- list(
  permit_type_fe = TRUE,
  review_type_fe = TRUE,
  include_porch = TRUE,
  ca_fe = FALSE,
  two_stage = TRUE,
  stage2_weight = "N_PERMITS",
  volume_ctrl = "LAG1",
  volume_stage = "BOTH"
)

score_rows <- list()
stage1_rows <- list()

for (cutoff in c(2014L, 2022L)) {
  score_result <- build_residualized_uncertainty_index(
    permits = permits_area_weighted %>% filter(year <= cutoff),
    config = config,
    variant_id = "area_weighted",
    stage1_outcome = "log_processing_time",
    drop_covariates = character(),
    construction_rule = "Area-weighted block-group controls"
  )

  write_csv(
    score_result$alderman_index,
    sprintf("../output/alderman_scores_area_weighted_through%d.csv", cutoff)
  )

  score_rows[[as.character(cutoff)]] <- score_result$alderman_index %>%
    transmute(
      cutoff,
      alderman,
      n_permits,
      uncertainty_index,
      rank = rank(-uncertainty_index, ties.method = "average")
    )

  stage1_rows[[as.character(cutoff)]] <- score_result$stage1_terms %>%
    mutate(cutoff, .before = 1)
}

write_csv(bind_rows(score_rows), "../output/area_weighted_scores.csv")
write_csv(bind_rows(stage1_rows), "../output/area_weighted_stage1_terms.csv")
