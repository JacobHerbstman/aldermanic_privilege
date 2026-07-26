# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

density <- readr::read_csv(
  "../output/final_density_model_input.csv",
  show_col_types = FALSE
)

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(cutoff == 2022L) |>
  dplyr::select(variant, alderman, score)

expected_variants <- c(
  "current_no_income",
  "education_added_back",
  "income_added_back",
  "all_covariates"
)
if (!setequal(scores$variant, expected_variants)) {
  stop("The score file does not contain all four control variants.", call. = FALSE)
}
if (anyDuplicated(scores[c("variant", "alderman")])) {
  stop("Scores are not unique by variant and alderman.", call. = FALSE)
}

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

rows <- list()
for (variant_id in expected_variants) {
  score_map <- scores |>
    dplyr::filter(variant == variant_id) |>
    dplyr::select(alderman, score) |>
    tibble::deframe()

  variant_data <- density |>
    dplyr::mutate(
      score_own = unname(score_map[alderman_own]),
      score_neighbor = unname(score_map[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      lenient_dist = distance_to_boundary_ft * 0.3048 *
        as.integer(side == 0L),
      strict_dist = distance_to_boundary_ft * 0.3048 *
        as.integer(side == 1L),
      continuous_score_difference =
        (score_own - score_neighbor) / 2,
      pair_average_score =
        (score_own + score_neighbor) / 2
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- variant_data |>
      dplyr::filter(
        construction_year >= 2006,
        construction_year <= 2022,
        distance_to_boundary_ft <= 500,
        dwelling_units > if (sample_name == "all") 0 else 1
      )

    for (eligibility in c("outcome_specific", "common_far_dupac")) {
      for (outcome in c("density_far", "density_dupac")) {
        eligible <- if (eligibility == "common_far_dupac") {
          sample_data$allow_far & sample_data$allow_dupac
        } else if (outcome == "density_far") {
          sample_data$allow_far
        } else {
          sample_data$allow_dupac
        }

        model_data <- sample_data |>
          dplyr::filter(
            eligible,
            is.finite(.data[[outcome]]),
            .data[[outcome]] > 0,
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
            variant = variant_id,
            sample = sample_name,
            eligibility,
            outcome,
            treatment,
            estimate = unname(coefficient["Estimate"]),
            se = unname(coefficient["Std. Error"]),
            p_value = unname(coefficient["Pr(>|t|)"]),
            n_obs = stats::nobs(model),
            ward_pairs = dplyr::n_distinct(model_data$ward_pair)
          )
        }
      }
    }
  }
}

results <- dplyr::bind_rows(rows)
if (
  nrow(results) != 64L ||
    anyDuplicated(
      results[
        c(
          "variant",
          "sample",
          "eligibility",
          "outcome",
          "treatment"
        )
      ]
    )
) {
  stop("The final four-score result grid is incomplete.", call. = FALSE)
}

reference <- readr::read_csv(
  "../output/final_density_model_results.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(sample_version == "final") |>
  dplyr::select(
    sample,
    eligibility,
    outcome,
    treatment,
    reference_estimate = estimate,
    reference_se = se
  )

validation <- results |>
  dplyr::filter(variant == "all_covariates") |>
  dplyr::left_join(
    reference,
    by = c("sample", "eligibility", "outcome", "treatment"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    estimate_difference = estimate - reference_estimate,
    se_difference = se - reference_se
  )

if (
  anyNA(validation$reference_estimate) ||
    max(abs(validation$estimate_difference)) > 1e-8 ||
    max(abs(validation$se_difference)) > 1e-8
) {
  stop("The full-control score does not reproduce the final results.", call. = FALSE)
}

readr::write_csv(
  results,
  "../output/final_density_score_control_results.csv",
  na = ""
)
readr::write_csv(
  validation,
  "../output/final_density_score_control_validation.csv",
  na = ""
)
