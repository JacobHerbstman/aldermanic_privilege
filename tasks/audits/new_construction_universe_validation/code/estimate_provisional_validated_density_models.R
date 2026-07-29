# setwd("tasks/audits/new_construction_universe_validation/code")

source("../../../setup_environment/code/packages.R")

data <- readr::read_csv(
  "../output/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_select = c(
    project_id,
    construction_year,
    ward_pair,
    distance_to_boundary_ft,
    within_500ft,
    allow_far,
    allow_dupac,
    segment_id,
    dwelling_units,
    density_far,
    density_dupac,
    zone_group,
    alderman_own,
    alderman_neighbor,
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
    ward_pair = readr::col_character(),
    distance_to_boundary_ft = readr::col_double(),
    within_500ft = readr::col_logical(),
    allow_far = readr::col_logical(),
    allow_dupac = readr::col_logical(),
    segment_id = readr::col_character(),
    dwelling_units = readr::col_double(),
    density_far = readr::col_double(),
    density_dupac = readr::col_double(),
    zone_group = readr::col_character(),
    alderman_own = readr::col_character(),
    alderman_neighbor = readr::col_character(),
    share_white_own = readr::col_double(),
    share_black_own = readr::col_double(),
    median_hh_income_own = readr::col_double(),
    share_bach_plus_own = readr::col_double(),
    homeownership_rate_own = readr::col_double(),
    external_multifamily = readr::col_logical()
  )
)

scores <- readr::read_csv(
  "../input/score_control_variants.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(cutoff == 2022)

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

results <- list()

for (variant_id in sort(unique(scores$variant))) {
  score_map <- scores |>
    dplyr::filter(variant == variant_id) |>
    dplyr::select(alderman, score) |>
    tibble::deframe()

  variant_data <- data |>
    dplyr::mutate(
      score_own = unname(score_map[alderman_own]),
      score_neighbor = unname(score_map[alderman_neighbor]),
      side = as.integer(score_own > score_neighbor),
      distance_m = distance_to_boundary_ft * 0.3048,
      lenient_dist = abs(distance_m) * as.integer(side == 0L),
      strict_dist = abs(distance_m) * as.integer(side == 1L),
      continuous_score_difference = (score_own - score_neighbor) / 2,
      pair_average_score = (score_own + score_neighbor) / 2
    )

  for (sample_name in c("all", "multifamily")) {
    sample_data <- variant_data |>
      dplyr::filter(
        construction_year >= 2006,
        construction_year <= 2022,
        within_500ft,
        dwelling_units > 0,
        sample_name == "all" | external_multifamily
      )

    for (sample_rule in c("outcome_specific", "common_density")) {
      for (outcome in c("density_far", "density_dupac")) {
        eligibility_field <- if (outcome == "density_far") {
          "allow_far"
        } else {
          "allow_dupac"
        }

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
            results[[length(results) + 1L]] <- tibble::tibble(
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

readr::write_csv(
  dplyr::bind_rows(results),
  "../output/provisional_validated_density_results.csv",
  na = ""
)
