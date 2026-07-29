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
  stop("A decomposition input has duplicate project IDs.", call. = FALSE)
}

apply_card_values <- function(data, use_manual_overrides) {
  value_table <- adjudication |>
    dplyr::transmute(
      project_id,
      revised_units = if (use_manual_overrides) {
        final_units
      } else {
        rule_units
      },
      revised_building_sqft = if (use_manual_overrides) {
        final_building_sqft
      } else {
        rule_building_sqft
      }
    )

  data |>
    dplyr::left_join(
      value_table,
      by = "project_id",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      dwelling_units = dplyr::coalesce(
        revised_units,
        dwelling_units
      ),
      building_sqft = dplyr::coalesce(
        revised_building_sqft,
        building_sqft
      ),
      density_far = building_sqft / land_sqft,
      density_dupac = 43560 * dwelling_units / land_sqft
    ) |>
    dplyr::select(-revised_units, -revised_building_sqft)
}

sample_versions <- list(
  baseline = baseline,
  card_values_only = apply_card_values(baseline, TRUE),
  successor_suppression_only = baseline |>
    dplyr::filter(!project_id %in% suppressions$project_id),
  final_without_manual_overrides = apply_card_values(
    baseline |>
      dplyr::filter(!project_id %in% suppressions$project_id),
    FALSE
  ),
  final_adjudicated = final_input
)

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

result_rows <- list()
for (version_id in names(sample_versions)) {
  model_input <- sample_versions[[version_id]] |>
    dplyr::mutate(
      score_own = unname(scores[alderman_own]),
      score_neighbor = unname(scores[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      dist_to_boundary_m = distance_to_boundary_ft * 0.3048,
      lenient_dist = abs(dist_to_boundary_m) * as.integer(side == 0L),
      strict_dist = abs(dist_to_boundary_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- model_input |>
      dplyr::filter(
        construction_year >= 2006,
        construction_year <= 2022,
        distance_to_boundary_ft <= 500,
        dwelling_units > if (sample_name == "all") 0 else 1,
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

        result_rows[[length(result_rows) + 1L]] <- tibble::tibble(
          sample_version = version_id,
          sample = sample_name,
          outcome,
          treatment,
          estimate = unname(coefficient["Estimate"]),
          se = unname(coefficient["Std. Error"]),
          p_value = unname(coefficient["Pr(>|t|)"]),
          n_obs = stats::nobs(model),
          n_ward_pairs = dplyr::n_distinct(sample_data$ward_pair)
        )
      }
    }
  }
}

results <- dplyr::bind_rows(result_rows)
if (
  nrow(results) != 40L ||
    anyDuplicated(
      results[c(
        "sample_version",
        "sample",
        "outcome",
        "treatment"
      )]
    )
) {
  stop("The multicard decomposition result grid is incomplete.", call. = FALSE)
}

readr::write_csv(
  results,
  "../output/multicard_sample_decomposition_results.csv",
  na = ""
)
