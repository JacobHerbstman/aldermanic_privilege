# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

baseline <- readr::read_csv(
  "../output/final_density_model_input.csv",
  show_col_types = FALSE
)
final_input <- readr::read_csv(
  "../output/multicard_adjudicated_density_model_input.csv",
  show_col_types = FALSE
)
adjudication <- readr::read_csv(
  "../output/multicard_final_adjudication.csv",
  show_col_types = FALSE
)
suppressions <- readr::read_csv(
  "../output/multicard_successor_suppressions.csv",
  show_col_types = FALSE
)
scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2022L,
    variant == "all_covariates"
  ) |>
  dplyr::select(alderman, score) |>
  tibble::deframe()

if (
  anyDuplicated(baseline$project_id) ||
    anyDuplicated(final_input$project_id) ||
    anyDuplicated(adjudication$project_id) ||
    anyDuplicated(suppressions$project_id)
) {
  stop("An attenuation-diagnostic input has duplicate project IDs.", call. = FALSE)
}

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

apply_rule_values <- function(data) {
  data |>
    dplyr::left_join(
      adjudication |>
        dplyr::select(
          project_id,
          rule_units,
          rule_building_sqft
        ),
      by = "project_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      dwelling_units = dplyr::coalesce(
        rule_units,
        dwelling_units
      ),
      building_sqft = dplyr::coalesce(
        rule_building_sqft,
        building_sqft
      ),
      density_far = building_sqft / land_sqft,
      density_dupac = 43560 * dwelling_units / land_sqft
    ) |>
    dplyr::select(-rule_units, -rule_building_sqft)
}

prepare_common_multifamily <- function(data) {
  data |>
    dplyr::mutate(
      score_own = unname(scores[alderman_own]),
      score_neighbor = unname(scores[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      dist_to_boundary_m = distance_to_boundary_ft * 0.3048,
      lenient_dist = abs(dist_to_boundary_m) * as.integer(side == 0L),
      strict_dist = abs(dist_to_boundary_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    ) |>
    dplyr::filter(
      construction_year >= 2006,
      construction_year <= 2022,
      distance_to_boundary_ft <= 500,
      dwelling_units > 1,
      allow_far,
      allow_dupac,
      is.finite(density_far),
      density_far > 0,
      is.finite(density_dupac),
      density_dupac > 0,
      is.finite(score_own),
      is.finite(score_neighbor),
      is.finite(pair_average_score),
      dplyr::if_all(
        dplyr::all_of(demographic_controls),
        is.finite
      ),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != ""
    )
}

fit_stage <- function(
  data,
  stage_id,
  omitted_pair = NA_character_,
  omitted_project = NA_character_
) {
  model_data <- data
  if (!is.na(omitted_pair)) {
    model_data <- model_data |>
      dplyr::filter(ward_pair != omitted_pair)
  }
  if (!is.na(omitted_project)) {
    model_data <- model_data |>
      dplyr::filter(project_id != omitted_project)
  }

  rows <- list()
  for (outcome in c("density_far", "density_dupac")) {
    for (treatment in c("continuous", "binary")) {
      treatment_term <- if (treatment == "continuous") {
        "continuous_score_difference"
      } else {
        "side"
      }
      model <- fixest::feols(
        stats::as.formula(paste0(
          "log(",
          outcome,
          ") ~ ",
          paste(
            c(
              treatment_term,
              "pair_average_score",
              "lenient_dist",
              "strict_dist",
              demographic_controls
            ),
            collapse = " + "
          ),
          " | zone_group + segment_id + construction_year"
        )),
        data = model_data,
        cluster = ~ward_pair
      )
      coefficient <- fixest::coeftable(model)[treatment_term, ]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        stage = stage_id,
        omitted_pair,
        outcome,
        treatment,
        estimate = unname(coefficient["Estimate"]),
        se = unname(coefficient["Std. Error"]),
        p_value = unname(coefficient["Pr(>|t|)"]),
        n_obs = stats::nobs(model),
        n_ward_pairs = dplyr::n_distinct(model_data$ward_pair)
      )
    }
  }
  dplyr::bind_rows(rows)
}

baseline_common <- prepare_common_multifamily(baseline)
rule_values <- apply_rule_values(baseline)
rule_common <- prepare_common_multifamily(rule_values)
single_unit_card_entrants <- setdiff(
  rule_common$project_id,
  baseline_common$project_id
)

stages <- list(
  baseline = baseline_common,
  revised_values_original_sample = rule_common |>
    dplyr::filter(project_id %in% baseline_common$project_id),
  revised_values_with_entrants = rule_common,
  revised_values_deduplicated = prepare_common_multifamily(
    rule_values |>
      dplyr::filter(!project_id %in% suppressions$project_id)
  ),
  final_adjudicated = prepare_common_multifamily(final_input),
  final_building_multifamily = prepare_common_multifamily(final_input) |>
    dplyr::filter(!project_id %in% single_unit_card_entrants),
  final_building_multifamily_selected_card_500_w_66th =
    final_input |>
    dplyr::mutate(
      dwelling_units = dplyr::if_else(
        project_id == "residential_multicard_20211200290000",
        2,
        dwelling_units
      ),
      building_sqft = dplyr::if_else(
        project_id == "residential_multicard_20211200290000",
        2210,
        building_sqft
      ),
      density_far = building_sqft / land_sqft,
      density_dupac = 43560 * dwelling_units / land_sqft
    ) |>
    prepare_common_multifamily() |>
    dplyr::filter(!project_id %in% single_unit_card_entrants),
  final_building_multifamily_without_500_w_66th =
    prepare_common_multifamily(final_input) |>
    dplyr::filter(
      !project_id %in% single_unit_card_entrants,
      project_id != "residential_multicard_20211200290000"
    )
)

stage_results <- purrr::imap_dfr(
  stages,
  ~ fit_stage(.x, .y)
)

stage_membership <- purrr::imap_dfr(
  stages,
  ~ tibble::tibble(
    project_id = .x$project_id,
    stage = .y
  )
) |>
  dplyr::mutate(in_stage = TRUE) |>
  tidyr::pivot_wider(
    names_from = stage,
    values_from = in_stage,
    values_fill = FALSE,
    names_prefix = "in_"
  )

project_changes <- dplyr::full_join(
  baseline_common |>
    dplyr::select(
      project_id,
      baseline_ward_pair = ward_pair,
      baseline_side = side,
      baseline_units = dwelling_units,
      baseline_far = density_far,
      baseline_dupac = density_dupac
    ),
  rule_common |>
    dplyr::select(
      project_id,
      rule_ward_pair = ward_pair,
      rule_side = side,
      rule_units = dwelling_units,
      rule_far = density_far,
      rule_dupac = density_dupac
    ),
  by = "project_id",
  relationship = "one-to-one"
) |>
  dplyr::full_join(
    stages$final_adjudicated |>
      dplyr::select(
        project_id,
        final_ward_pair = ward_pair,
        final_side = side,
        final_units = dwelling_units,
        final_far = density_far,
        final_dupac = density_dupac
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    stage_membership,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::starts_with("in_"),
      ~ dplyr::coalesce(.x, FALSE)
    ),
    newly_multifamily_after_card_revision =
      !in_baseline & in_revised_values_with_entrants,
    removed_as_duplicate =
      in_revised_values_with_entrants &
        !in_revised_values_deduplicated,
    added_by_final_overrides =
      !in_revised_values_deduplicated &
        in_final_adjudicated,
    removed_by_final_overrides =
      in_revised_values_deduplicated &
        !in_final_adjudicated,
    existing_far_log_change = dplyr::if_else(
      in_baseline & in_revised_values_with_entrants,
      log(rule_far) - log(baseline_far),
      NA_real_
    ),
    existing_dupac_log_change = dplyr::if_else(
      in_baseline & in_revised_values_with_entrants,
      log(rule_dupac) - log(baseline_dupac),
      NA_real_
    )
  ) |>
  dplyr::arrange(project_id)

stage_summary <- dplyr::bind_rows(
  purrr::imap_dfr(
    stages,
    ~ tibble::tibble(
      section = "stage",
      metric = .y,
      value = nrow(.x)
    )
  ),
  tibble::tibble(
    section = "project_changes",
    metric = c(
      "newly_multifamily_after_card_revision",
      "removed_as_duplicate",
      "added_by_final_overrides",
      "removed_by_final_overrides",
      "existing_projects_with_far_change",
      "existing_projects_with_dupac_change"
    ),
    value = c(
      sum(project_changes$newly_multifamily_after_card_revision),
      sum(project_changes$removed_as_duplicate),
      sum(project_changes$added_by_final_overrides),
      sum(project_changes$removed_by_final_overrides),
      sum(
        abs(project_changes$existing_far_log_change) > 1e-10,
        na.rm = TRUE
      ),
      sum(
        abs(project_changes$existing_dupac_log_change) > 1e-10,
        na.rm = TRUE
      )
    )
  )
)

all_pairs <- sort(unique(unlist(
  purrr::map(stages, ~ unique(.x$ward_pair))
)))
omitted_results <- purrr::map_dfr(
  all_pairs,
  function(pair_id) {
    purrr::imap_dfr(
      stages,
      ~ fit_stage(.x, .y, pair_id)
    )
  }
)

transitions <- tibble::tribble(
  ~transition, ~from_stage, ~to_stage,
  "existing_project_value_revisions",
  "baseline",
  "revised_values_original_sample",
  "new_multifamily_entrants",
  "revised_values_original_sample",
  "revised_values_with_entrants",
  "duplicate_suppression",
  "revised_values_with_entrants",
  "revised_values_deduplicated",
  "manual_and_year_overrides",
  "revised_values_deduplicated",
  "final_adjudicated",
  "total_change",
  "baseline",
  "final_adjudicated"
)

pair_influence <- transitions |>
  tidyr::crossing(
    outcome = c("density_far", "density_dupac"),
    treatment = c("continuous", "binary"),
    omitted_pair = all_pairs
  ) |>
  dplyr::left_join(
    stage_results |>
      dplyr::select(
        from_stage = stage,
        outcome,
        treatment,
        full_from_estimate = estimate
      ),
    by = c("from_stage", "outcome", "treatment"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    stage_results |>
      dplyr::select(
        to_stage = stage,
        outcome,
        treatment,
        full_to_estimate = estimate
      ),
    by = c("to_stage", "outcome", "treatment"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    omitted_results |>
      dplyr::select(
        from_stage = stage,
        omitted_pair,
        outcome,
        treatment,
        omitted_from_estimate = estimate
      ),
    by = c(
      "from_stage",
      "omitted_pair",
      "outcome",
      "treatment"
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    omitted_results |>
      dplyr::select(
        to_stage = stage,
        omitted_pair,
        outcome,
        treatment,
        omitted_to_estimate = estimate
      ),
    by = c(
      "to_stage",
      "omitted_pair",
      "outcome",
      "treatment"
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    full_change = full_to_estimate - full_from_estimate,
    omitted_change = omitted_to_estimate - omitted_from_estimate,
    pair_contribution = full_change - omitted_change
  ) |>
  dplyr::arrange(
    transition,
    outcome,
    treatment,
    dplyr::desc(abs(pair_contribution))
  )

project_transition_candidates <- dplyr::bind_rows(
  project_changes |>
    dplyr::filter(
      abs(existing_far_log_change) > 1e-10 |
        abs(existing_dupac_log_change) > 1e-10 |
        (in_baseline & !in_revised_values_original_sample)
    ) |>
    dplyr::transmute(
      transition = "existing_project_value_revisions",
      from_stage = "baseline",
      to_stage = "revised_values_original_sample",
      project_id
    ),
  project_changes |>
    dplyr::filter(newly_multifamily_after_card_revision) |>
    dplyr::transmute(
      transition = "new_multifamily_entrants",
      from_stage = "revised_values_original_sample",
      to_stage = "revised_values_with_entrants",
      project_id
    )
)

project_omitted_results <- purrr::pmap_dfr(
  project_transition_candidates,
  function(transition, from_stage, to_stage, project_id) {
    dplyr::bind_rows(
      fit_stage(
        stages[[from_stage]],
        from_stage,
        omitted_project = project_id
      ),
      fit_stage(
        stages[[to_stage]],
        to_stage,
        omitted_project = project_id
      )
    ) |>
      dplyr::mutate(
        transition = transition,
        project_id = project_id
      )
  }
)

project_influence <- project_transition_candidates |>
  tidyr::crossing(
    outcome = c("density_far", "density_dupac"),
    treatment = c("continuous", "binary")
  ) |>
  dplyr::left_join(
    stage_results |>
      dplyr::select(
        from_stage = stage,
        outcome,
        treatment,
        full_from_estimate = estimate
      ),
    by = c("from_stage", "outcome", "treatment"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    stage_results |>
      dplyr::select(
        to_stage = stage,
        outcome,
        treatment,
        full_to_estimate = estimate
      ),
    by = c("to_stage", "outcome", "treatment"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    project_omitted_results |>
      dplyr::select(
        transition,
        from_stage = stage,
        project_id,
        outcome,
        treatment,
        omitted_from_estimate = estimate
      ),
    by = c(
      "transition",
      "from_stage",
      "project_id",
      "outcome",
      "treatment"
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    project_omitted_results |>
      dplyr::select(
        transition,
        to_stage = stage,
        project_id,
        outcome,
        treatment,
        omitted_to_estimate = estimate
      ),
    by = c(
      "transition",
      "to_stage",
      "project_id",
      "outcome",
      "treatment"
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    full_change = full_to_estimate - full_from_estimate,
    omitted_change = omitted_to_estimate - omitted_from_estimate,
    project_contribution = full_change - omitted_change
  ) |>
  dplyr::arrange(
    transition,
    outcome,
    treatment,
    dplyr::desc(abs(project_contribution))
  )

if (
  nrow(stage_results) != length(stages) * 4L ||
    anyDuplicated(stage_results[c("stage", "outcome", "treatment")]) ||
    anyDuplicated(project_changes$project_id) ||
    any(!is.finite(pair_influence$pair_contribution)) ||
    any(!is.finite(project_influence$project_contribution))
) {
  stop("The multicard attenuation diagnostic failed validation.", call. = FALSE)
}

readr::write_csv(
  stage_results,
  "../output/multicard_attenuation_stage_results.csv",
  na = ""
)
readr::write_csv(
  stage_summary,
  "../output/multicard_attenuation_stage_summary.csv",
  na = ""
)
readr::write_csv(
  project_changes,
  "../output/multicard_attenuation_project_changes.csv",
  na = ""
)
readr::write_csv(
  pair_influence,
  "../output/multicard_attenuation_pair_influence.csv",
  na = ""
)
readr::write_csv(
  project_influence,
  "../output/multicard_attenuation_project_influence.csv",
  na = ""
)
