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
  dplyr::filter(cutoff == 2022L) |>
  dplyr::select(variant, alderman, score)

expected_variants <- c(
  "all_covariates",
  "education_added_back",
  "income_added_back",
  "current_no_income"
)
if (!setequal(scores$variant, expected_variants)) {
  stop("The score-control sensitivity file does not contain all four variants.", call. = FALSE)
}
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

result_rows <- list()
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
      lenient_dist = abs(dist_to_boundary_m) * as.integer(side == 0L),
      strict_dist = abs(dist_to_boundary_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- variant_data |>
      dplyr::filter(
        construction_year >= 2006,
        construction_year <= 2022,
        distance_to_boundary_ft <= 500,
        dwelling_units > if (sample_name == "all") 0 else 1
      )

    for (sample_rule in c("outcome_specific", "common_density")) {
      for (outcome in c("density_far", "density_dupac")) {
        eligibility_field <- if (outcome == "density_far") "allow_far" else "allow_dupac"
        model_data <- sample_data |>
          dplyr::filter(
            .data[[eligibility_field]],
            is.finite(.data[[outcome]]),
            .data[[outcome]] > 0,
            sample_rule == "outcome_specific" | (
              allow_far &
                allow_dupac &
                is.finite(density_far) &
                density_far > 0 &
                is.finite(density_dupac) &
                density_dupac > 0
            ),
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

          for (cluster_level in c("ward_pair", "segment")) {
            cluster_field <- if (cluster_level == "ward_pair") {
              "ward_pair"
            } else {
              "segment_id"
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
              cluster = stats::as.formula(paste0("~", cluster_field))
            )
            coefficient <- fixest::coeftable(model)[treatment_term, ]

            result_rows[[length(result_rows) + 1L]] <- tibble::tibble(
              variant = variant_id,
              sample = sample_name,
              sample_rule,
              outcome,
              treatment,
              cluster_level,
              estimate = unname(coefficient["Estimate"]),
              se = unname(coefficient["Std. Error"]),
              p_value = unname(coefficient["Pr(>|t|)"]),
              n_obs = stats::nobs(model),
              n_clusters = dplyr::n_distinct(model_data[[cluster_field]])
            )
          }
        }
      }
    }
  }
}

results <- dplyr::bind_rows(result_rows)
if (nrow(results) != 128L || anyDuplicated(
  results[c(
    "variant",
    "sample",
    "sample_rule",
    "outcome",
    "treatment",
    "cluster_level"
  )]
)) {
  stop("The reconstructed-sample score-control result grid is incomplete.", call. = FALSE)
}

reference <- readr::read_csv(
  "../output/preferred_density_model_results.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(sample_version == "preferred_card_rule_common_year") |>
  dplyr::select(sample, outcome, treatment, reference_estimate = estimate)

validation <- results |>
  dplyr::filter(
    variant == "all_covariates",
    sample_rule == "outcome_specific",
    cluster_level == "ward_pair"
  ) |>
  dplyr::left_join(
    reference,
    by = c("sample", "outcome", "treatment"),
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(difference = estimate - reference_estimate)

if (anyNA(validation$reference_estimate) ||
    max(abs(validation$difference)) > 1e-8) {
  stop("The full-control reconstructed-sample estimates do not match their reference.", call. = FALSE)
}

readr::write_csv(
  results |>
    dplyr::filter(
      sample_rule == "outcome_specific",
      cluster_level == "ward_pair"
    ) |>
    dplyr::select(-sample_rule),
  "../output/new_sample_score_control_density_results.csv",
  na = ""
)
readr::write_csv(
  results |>
    dplyr::filter(sample_rule == "outcome_specific") |>
    dplyr::select(-sample_rule),
  "../output/new_sample_score_control_density_cluster_results.csv",
  na = ""
)
readr::write_csv(
  results |>
    dplyr::filter(sample_rule == "common_density") |>
    dplyr::select(-sample_rule),
  "../output/new_sample_score_control_density_common_sample_results.csv",
  na = ""
)
readr::write_csv(
  validation,
  "../output/new_sample_score_control_density_validation.csv",
  na = ""
)
