# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

density <- readr::read_csv(
  "../output/preferred_density_model_production_card_input.csv",
  show_col_types = FALSE
)

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(
    cutoff == 2022L,
    variant %in% c("all_covariates", "current_no_income")
  ) |>
  dplyr::select(variant, alderman, score)

if (anyDuplicated(scores[c("variant", "alderman")])) {
  stop("Score variants are not unique by variant and alderman.", call. = FALSE)
}

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

common_sample <- density |>
  dplyr::filter(
    construction_year >= 2006L,
    construction_year <= 2022L,
    distance_to_boundary_ft <= 500,
    dwelling_units > 1,
    allow_far,
    allow_dupac,
    is.finite(density_far),
    density_far > 0,
    is.finite(density_dupac),
    density_dupac > 0,
    dplyr::if_all(
      dplyr::all_of(demographic_controls),
      is.finite
    ),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != ""
  )

variant_rows <- list()
for (variant_id in c("all_covariates", "current_no_income")) {
  score_map <- scores |>
    dplyr::filter(variant == variant_id) |>
    dplyr::select(alderman, score) |>
    tibble::deframe()

  variant_rows[[variant_id]] <- common_sample |>
    dplyr::mutate(
      variant = variant_id,
      score_own = unname(score_map[alderman_own]),
      score_neighbor = unname(score_map[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      lenient_dist = abs(dist_to_boundary_m) * as.integer(side == 0L),
      strict_dist = abs(dist_to_boundary_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    ) |>
    dplyr::filter(
      is.finite(score_own),
      is.finite(score_neighbor),
      is.finite(pair_average_score)
    )
}

if (nrow(variant_rows$all_covariates) != 903L ||
    nrow(variant_rows$current_no_income) != 903L ||
    !setequal(
      variant_rows$all_covariates$project_id,
      variant_rows$current_no_income$project_id
    )) {
  stop("The two score variants do not use the expected common sample.", call. = FALSE)
}

estimate_density <- function(data, outcome, treatment, omitted_pair = NA_character_) {
  model_data <- if (is.na(omitted_pair)) {
    data
  } else {
    dplyr::filter(data, ward_pair != omitted_pair)
  }
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
    data = model_data
  )
  unname(stats::coef(model)[treatment_term])
}

influence_rows <- list()
for (outcome in c("density_far", "density_dupac")) {
  for (treatment in c("continuous", "binary")) {
    baseline_full <- estimate_density(
      variant_rows$all_covariates,
      outcome,
      treatment
    )
    baseline_neither <- estimate_density(
      variant_rows$current_no_income,
      outcome,
      treatment
    )
    baseline_gap <- baseline_neither - baseline_full

    for (pair_id in sort(unique(common_sample$ward_pair))) {
      leave_full <- estimate_density(
        variant_rows$all_covariates,
        outcome,
        treatment,
        pair_id
      )
      leave_neither <- estimate_density(
        variant_rows$current_no_income,
        outcome,
        treatment,
        pair_id
      )
      leave_gap <- leave_neither - leave_full

      influence_rows[[length(influence_rows) + 1L]] <- tibble::tibble(
        ward_pair = pair_id,
        outcome,
        treatment,
        baseline_full,
        baseline_neither,
        baseline_gap,
        leave_full,
        leave_neither,
        leave_gap,
        neither_negative_support = leave_neither - baseline_neither,
        more_negative_gap_support = leave_gap - baseline_gap
      )
    }
  }
}

influence <- dplyr::bind_rows(influence_rows)
if (nrow(influence) != 360L ||
    anyDuplicated(influence[c("ward_pair", "outcome", "treatment")])) {
  stop("The ward-pair influence grid is incomplete.", call. = FALSE)
}

treatment_changes <- variant_rows$all_covariates |>
  dplyr::select(
    project_id,
    ward_pair,
    construction_year,
    alderman_own,
    alderman_neighbor,
    full_side = side,
    full_difference = continuous_score_difference
  ) |>
  dplyr::inner_join(
    variant_rows$current_no_income |>
      dplyr::select(
        project_id,
        neither_side = side,
        neither_difference = continuous_score_difference
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    side_flip = full_side != neither_side,
    difference_change = neither_difference - full_difference
  ) |>
  dplyr::group_by(ward_pair) |>
  dplyr::summarise(
    n_projects = dplyr::n(),
    n_side_flips = sum(side_flip),
    mean_absolute_difference_change = mean(abs(difference_change)),
    max_absolute_difference_change = max(abs(difference_change)),
    alderman_comparisons = paste(
      sort(unique(paste(alderman_own, alderman_neighbor, sep = " / "))),
      collapse = "; "
    ),
    .groups = "drop"
  )

readr::write_csv(
  influence,
  "../output/neither_score_pair_leave_one_out.csv",
  na = ""
)
readr::write_csv(
  treatment_changes,
  "../output/neither_score_pair_treatment_changes.csv",
  na = ""
)
