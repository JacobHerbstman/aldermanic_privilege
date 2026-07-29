# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../_lib/alderman_uncertainty_helpers.R")

permits <- read_csv(
  "../input/permit_nonnegative_score_sample.csv",
  show_col_types = FALSE,
  col_types = cols(id = col_character(), pin = col_character(), .default = col_guess())
) %>%
  mutate(month = as.yearmon(month))

if (anyDuplicated(permits$id) > 0) {
  stop("The nonnegative permit sample has duplicate permit IDs.", call. = FALSE)
}
if (any(!is.finite(permits$processing_time)) || any(permits$processing_time < 0)) {
  stop("The permit sample contains an invalid processing time.", call. = FALSE)
}
if (any(permits$same_day != as.integer(permits$processing_time == 0))) {
  stop("The same-day indicator does not match processing time.", call. = FALSE)
}

base_spec <- tibble(
  spec_id = "baseline_log_all_workload",
  family = "Baseline",
  label = "Baseline: log days; all-permit workload",
  stage1_outcome = "log_processing_time",
  workload_universe = "all_nonnegative",
  permit_type_fe = TRUE,
  review_type_fe = TRUE,
  include_porch = TRUE,
  ca_fe = FALSE,
  two_stage = TRUE,
  stage2_weight = "N_PERMITS",
  volume_ctrl = "LAG1",
  volume_stage = "BOTH",
  drop_covariates = "share_bach_plus",
  score_transform = "standard"
)

specifications <- bind_rows(
  base_spec,
  base_spec %>% mutate(
    spec_id = "outcome_unlogged_positive",
    family = "Outcome",
    label = "Unlogged positive days; all-permit workload",
    stage1_outcome = "positive_processing_time"
  ),
  base_spec %>% mutate(
    spec_id = "outcome_unlogged_nonnegative",
    family = "Outcome",
    label = "Unlogged nonnegative days",
    stage1_outcome = "processing_time"
  ),
  base_spec %>% mutate(
    spec_id = "outcome_unlogged_positive_p99",
    family = "Outcome",
    label = "Unlogged positive days; winsorized p99",
    stage1_outcome = "positive_processing_time_p99"
  ),
  base_spec %>% mutate(
    spec_id = "outcome_unlogged_nonnegative_p99",
    family = "Outcome",
    label = "Unlogged nonnegative days; winsorized p99",
    stage1_outcome = "processing_time_p99"
  ),
  base_spec %>% mutate(
    spec_id = "outcome_same_day_rate",
    family = "Outcome",
    label = "Adjusted same-day issuance rate",
    stage1_outcome = "same_day",
    score_transform = "negate"
  ),
  base_spec %>% mutate(
    spec_id = "outcome_raw_mean_positive_rank",
    family = "Outcome",
    label = "Raw mean positive-day rank",
    workload_universe = "positive_only",
    score_transform = "raw_positive_rank"
  ),
  base_spec %>% mutate(
    spec_id = "outcome_raw_mean_nonnegative_rank",
    family = "Outcome",
    label = "Raw mean nonnegative-day rank",
    score_transform = "raw_nonnegative_rank"
  ),
  base_spec %>% mutate(
    spec_id = "workload_positive_only",
    family = "Workload",
    label = "Log days; positive-duration workload",
    workload_universe = "positive_only"
  ),
  base_spec %>% mutate(
    spec_id = "volume_none",
    family = "Workload",
    label = "No permit-volume control",
    volume_ctrl = "NONE"
  ),
  base_spec %>% mutate(
    spec_id = "volume_current_both",
    family = "Workload",
    label = "Current permit volume in both stages",
    volume_ctrl = "CURRENT"
  ),
  base_spec %>% mutate(
    spec_id = "volume_lag_stage1",
    family = "Workload",
    label = "Lagged permit volume in stage 1 only",
    volume_stage = "STAGE1"
  ),
  base_spec %>% mutate(
    spec_id = "volume_lag_stage2",
    family = "Workload",
    label = "Lagged permit volume in stage 2 only",
    volume_stage = "STAGE2"
  ),
  base_spec %>% mutate(
    spec_id = "stage_one",
    family = "Aggregation",
    label = "One-stage mean residual",
    two_stage = FALSE
  ),
  base_spec %>% mutate(
    spec_id = "stage2_unweighted",
    family = "Aggregation",
    label = "Two-stage; unweighted ward-months",
    stage2_weight = "NONE"
  ),
  base_spec %>% mutate(
    spec_id = "stage2_sqrt_weight",
    family = "Aggregation",
    label = "Two-stage; square-root permit weights",
    stage2_weight = "SQRT_N_PERMITS"
  ),
  base_spec %>% mutate(
    spec_id = "controls_full",
    family = "Controls",
    label = "Full controls including bachelor's share",
    drop_covariates = ""
  ),
  base_spec %>% mutate(
    spec_id = "controls_drop_income",
    family = "Controls",
    label = "Drop bachelor's share and income",
    drop_covariates = "share_bach_plus;median_hh_income_10k"
  ),
  base_spec %>% mutate(
    spec_id = "controls_drop_population",
    family = "Controls",
    label = "Drop bachelor's share and population",
    drop_covariates = "share_bach_plus;pop_total_10k"
  ),
  base_spec %>% mutate(
    spec_id = "controls_no_demographics",
    family = "Controls",
    label = "No ward demographic controls",
    drop_covariates = paste(
      c(
        "median_hh_income_10k", "share_black", "share_hisp", "share_white",
        "homeownership_rate", "share_bach_plus", "pop_total_10k"
      ),
      collapse = ";"
    )
  ),
  base_spec %>% mutate(
    spec_id = "controls_no_place",
    family = "Controls",
    label = "No location controls",
    drop_covariates = paste(
      c(
        "share_bach_plus", "dist_cbd_km", "dist_lake_km",
        "n_rail_stations_800m"
      ),
      collapse = ";"
    )
  ),
  base_spec %>% mutate(
    spec_id = "score_no_shrinkage",
    family = "Score scaling",
    label = "No empirical-Bayes shrinkage",
    score_transform = "unshrunk"
  ),
  base_spec %>% mutate(
    spec_id = "score_rank_residualized",
    family = "Score scaling",
    label = "Rank of residualized score",
    score_transform = "rank"
  )
)

fe_grid <- tidyr::crossing(
  permit_type_fe = c(FALSE, TRUE),
  review_type_fe = c(FALSE, TRUE),
  include_porch = c(FALSE, TRUE),
  ca_fe = c(FALSE, TRUE)
) %>%
  filter(!(
    permit_type_fe & review_type_fe & include_porch & !ca_fe
  )) %>%
  mutate(
    spec_id = paste0(
      "fe_pt", as.integer(permit_type_fe),
      "_rt", as.integer(review_type_fe),
      "_porch", as.integer(include_porch),
      "_ca", as.integer(ca_fe)
    ),
    family = "Fixed effects",
    label = paste0(
      "FE: permit type ", if_else(permit_type_fe, "yes", "no"),
      "; review type ", if_else(review_type_fe, "yes", "no"),
      "; porch ", if_else(include_porch, "yes", "no"),
      "; community area ", if_else(ca_fe, "yes", "no")
    ),
    stage1_outcome = "log_processing_time",
    workload_universe = "all_nonnegative",
    two_stage = TRUE,
    stage2_weight = "N_PERMITS",
    volume_ctrl = "LAG1",
    volume_stage = "BOTH",
    drop_covariates = "share_bach_plus",
    score_transform = "standard"
  ) %>%
  select(all_of(names(base_spec)))

specifications <- bind_rows(specifications, fe_grid) %>%
  mutate(
    spec_order = row_number(),
    is_baseline = spec_id == "baseline_log_all_workload"
  )

if (anyDuplicated(specifications$spec_id) > 0) {
  stop("The score grid contains duplicate specification IDs.", call. = FALSE)
}

score_rows <- list()
metadata_rows <- list()
stage1_rows <- list()

for (cutoff in c(2014L, 2022L)) {
  cutoff_permits <- permits %>%
    filter(month <= as.yearmon(sprintf("%d-12", cutoff)))

  positive_p99 <- quantile(
    cutoff_permits$processing_time[cutoff_permits$processing_time > 0],
    0.99,
    names = FALSE
  )
  nonnegative_p99 <- quantile(
    cutoff_permits$processing_time,
    0.99,
    names = FALSE
  )
  cutoff_permits <- cutoff_permits %>%
    mutate(
      positive_processing_time_p99 = if_else(
        processing_time > 0,
        pmin(processing_time, positive_p99),
        NA_real_
      ),
      processing_time_p99 = pmin(processing_time, nonnegative_p99)
    )

  for (spec_i in seq_len(nrow(specifications))) {
    spec <- specifications[spec_i, ]
    score_permits <- cutoff_permits
    if (spec$workload_universe == "positive_only") {
      score_permits <- score_permits %>% filter(processing_time > 0)
    }

    config <- default_uncertainty_config()
    config$permit_type_fe <- spec$permit_type_fe
    config$review_type_fe <- spec$review_type_fe
    config$include_porch <- spec$include_porch
    config$ca_fe <- spec$ca_fe
    config$two_stage <- spec$two_stage
    config$stage2_weight <- spec$stage2_weight
    config$volume_ctrl <- spec$volume_ctrl
    config$volume_stage <- spec$volume_stage

    if (spec$score_transform %in% c("raw_positive_rank", "raw_nonnegative_rank")) {
      prepared <- prepare_uncertainty_sample(
        score_permits,
        include_porch = config$include_porch,
        volume_ctrl = config$volume_ctrl,
        volume_stage = config$volume_stage
      )
      raw_sample <- prepared$permits %>%
        filter(!is.na(alderman), is.finite(processing_time))
      if (spec$score_transform == "raw_positive_rank") {
        raw_sample <- raw_sample %>% filter(processing_time > 0)
      }
      alderman_index <- raw_sample %>%
        group_by(alderman) %>%
        summarise(
          n_permits = n(),
          raw_mean_days = mean(processing_time),
          .groups = "drop"
        ) %>%
        mutate(score = standardize_uncertainty(rank(raw_mean_days, ties.method = "average")))

      metadata <- tibble(
        n_aldermen = nrow(alderman_index),
        n_permits_used = nrow(raw_sample),
        stage1_nobs = NA_real_,
        stage1_r2 = NA_real_,
        stage2_nobs = NA_real_,
        stage2_r2 = NA_real_
      )
    } else {
      dropped <- if (nzchar(spec$drop_covariates)) {
        str_split_1(spec$drop_covariates, ";")
      } else {
        character()
      }
      result <- build_residualized_uncertainty_index(
        permits = score_permits,
        config = config,
        variant_id = spec$spec_id,
        stage1_outcome = spec$stage1_outcome,
        drop_covariates = dropped,
        construction_rule = spec$label
      )

      alderman_index <- result$alderman_index
      if (spec$score_transform == "negate") {
        alderman_index <- alderman_index %>% mutate(score = -uncertainty_index)
      } else if (spec$score_transform == "unshrunk") {
        alderman_index <- alderman_index %>%
          mutate(score = standardize_uncertainty(alderman_fe_raw))
      } else if (spec$score_transform == "rank") {
        alderman_index <- alderman_index %>%
          mutate(score = standardize_uncertainty(
            rank(uncertainty_index, ties.method = "average")
          ))
      } else {
        alderman_index <- alderman_index %>% mutate(score = uncertainty_index)
      }
      metadata <- result$metadata %>%
        select(
          n_aldermen, n_permits_used, stage1_nobs, stage1_r2,
          stage2_nobs, stage2_r2
        )
      stage1_rows[[length(stage1_rows) + 1L]] <- result$stage1_terms %>%
        mutate(cutoff = cutoff, spec_id = spec$spec_id, .before = 1)
    }

    score_rows[[length(score_rows) + 1L]] <- alderman_index %>%
      transmute(
        cutoff = cutoff,
        spec_id = spec$spec_id,
        alderman,
        n_permits,
        score,
        strictness_rank = min_rank(desc(score))
      )
    metadata_rows[[length(metadata_rows) + 1L]] <- metadata %>%
      mutate(
        cutoff = cutoff,
        spec_id = spec$spec_id,
        positive_duration_p99 = positive_p99,
        nonnegative_duration_p99 = nonnegative_p99,
        .before = 1
      )
  }
}

scores <- bind_rows(score_rows)
if (anyDuplicated(scores[, c("cutoff", "spec_id", "alderman")]) > 0) {
  stop("A score specification has duplicate aldermen.", call. = FALSE)
}
if (any(!is.finite(scores$score))) {
  stop("A score specification produced a non-finite score.", call. = FALSE)
}

write_csv(specifications, "../output/score_specifications.csv")
write_csv(scores, "../output/alderman_scores.csv")
write_csv(bind_rows(metadata_rows), "../output/score_metadata.csv")
write_csv(bind_rows(stage1_rows), "../output/score_stage1_terms.csv")
