# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

data <- readr::read_csv(
  "../output/final_verified_density_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    ward_pair = readr::col_character(),
    segment_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)
results <- list()

for (sample_name in c("all", "multifamily")) {
  sample_data <- data |>
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

readr::write_csv(
  dplyr::bind_rows(results),
  "../output/final_verified_density_results.csv",
  na = ""
)
