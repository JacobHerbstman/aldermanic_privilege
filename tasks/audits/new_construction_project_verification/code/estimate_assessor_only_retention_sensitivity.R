# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

density <- readr::read_csv(
  "../input/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    construction_year,
    within_500ft,
    allow_far,
    allow_dupac,
    segment_id,
    ward_pair,
    dwelling_units,
    density_far,
    density_dupac,
    zone_group,
    alderman_own,
    alderman_neighbor,
    distance_to_boundary_ft,
    share_white_own,
    share_black_own,
    median_hh_income_own,
    share_bach_plus_own,
    homeownership_rate_own,
    external_multifamily
  ),
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_double(),
    within_500ft = readr::col_logical(),
    allow_far = readr::col_logical(),
    allow_dupac = readr::col_logical(),
    segment_id = readr::col_character(),
    ward_pair = readr::col_character(),
    dwelling_units = readr::col_double(),
    density_far = readr::col_double(),
    density_dupac = readr::col_double(),
    zone_group = readr::col_character(),
    alderman_own = readr::col_character(),
    alderman_neighbor = readr::col_character(),
    distance_to_boundary_ft = readr::col_double(),
    share_white_own = readr::col_double(),
    share_black_own = readr::col_double(),
    median_hh_income_own = readr::col_double(),
    share_bach_plus_own = readr::col_double(),
    homeownership_rate_own = readr::col_double(),
    external_multifamily = readr::col_logical(),
    .default = readr::col_skip()
  )
)

reviews <- readr::read_csv(
  "../output/reviewed_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    audit_decision = readr::col_character(),
    .default = readr::col_skip()
  )
)

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    cutoff = readr::col_double(),
    variant = readr::col_character(),
    alderman = readr::col_character(),
    score = readr::col_double(),
    .default = readr::col_skip()
  )
) |>
  dplyr::filter(
    cutoff == 2022,
    variant == "income_added_back"
  ) |>
  dplyr::select(alderman, score)

if (
  anyDuplicated(density$project_id) ||
    anyDuplicated(reviews$project_id) ||
    anyDuplicated(scores$alderman)
) {
  stop("A sensitivity input is not uniquely keyed.")
}

score_map <- stats::setNames(scores$score, scores$alderman)

data <- density |>
  dplyr::left_join(
    reviews,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    audit_decision = dplyr::coalesce(
      audit_decision,
      "outside_project_verification_scope"
    ),
    score_own = unname(score_map[alderman_own]),
    score_neighbor = unname(score_map[alderman_neighbor]),
    side = as.integer(score_own > score_neighbor),
    continuous_score_difference = (score_own - score_neighbor) / 2,
    pair_average_score = (score_own + score_neighbor) / 2,
    distance_m = distance_to_boundary_ft * 0.3048,
    lenient_dist = abs(distance_m) * as.integer(side == 0L),
    strict_dist = abs(distance_m) * as.integer(side == 1L)
  )

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

results <- list()

for (retention_rule in c("retain_assessor_only", "drop_assessor_only")) {
  analysis_data <- data |>
    dplyr::filter(
      audit_decision != "exclude_after_manual_review",
      retention_rule == "retain_assessor_only" |
        audit_decision != "retain_assessor_only_pending_review"
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- analysis_data |>
      dplyr::filter(
        construction_year >= 2006,
        construction_year <= 2022,
        within_500ft,
        dwelling_units > 0,
        allow_far,
        allow_dupac,
        is.finite(density_far),
        density_far > 0,
        is.finite(density_dupac),
        density_dupac > 0,
        sample_name == "all" | external_multifamily,
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
          data = sample_data,
          cluster = ~ward_pair
        )

        coefficient <- fixest::coeftable(model)[treatment_term, ]
        results[[length(results) + 1L]] <- tibble::tibble(
          retention_rule,
          sample = sample_name,
          outcome,
          treatment,
          estimate = unname(coefficient["Estimate"]),
          se = unname(coefficient["Std. Error"]),
          p_value = unname(coefficient["Pr(>|t|)"]),
          n_obs = stats::nobs(model),
          ward_pairs = dplyr::n_distinct(sample_data$ward_pair)
        )
      }
    }
  }
}

readr::write_csv(
  dplyr::bind_rows(results),
  "../output/assessor_only_retention_sensitivity.csv",
  na = ""
)
