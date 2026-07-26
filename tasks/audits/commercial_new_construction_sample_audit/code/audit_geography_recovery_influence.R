# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

production <- readr::read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    segment_id = readr::col_character(),
    ward_pair = readr::col_character(),
    construction_zone_group = readr::col_character(),
    .default = readr::col_double()
  ),
  col_select = c(
    pin,
    unitscount,
    areabuilding,
    arealotsf,
    dist_to_boundary_m,
    construction_zone_group,
    signed_distance_m,
    strictness_own,
    strictness_neighbor,
    ward_pair,
    segment_id,
    construction_year,
    density_far,
    density_dupac,
    share_white_own,
    share_black_own,
    median_hh_income_own,
    share_bach_plus_own,
    homeownership_rate_own
  )
) |>
  dplyr::mutate(
    project_id = pin,
    dwelling_units = unitscount,
    distance_to_boundary_ft = dist_to_boundary_m / 0.3048,
    zone_group = construction_zone_group,
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2,
    allow_far = arealotsf > 1 & areabuilding > 1,
    allow_dupac = arealotsf > 1 & areabuilding > 1
  )

preferred <- readr::read_csv(
  "../output/preferred_density_model_production_card_input.csv",
  show_col_types = FALSE
)

components <- readr::read_csv(
  "../output/preferred_residential_project_components_final.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
if (anyDuplicated(components$component_pin)) {
  stop("A residential component PIN belongs to more than one project.", call. = FALSE)
}

ledger <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_guess())
) |>
  dplyr::select(project_id, source_family, project_kind)
if (anyDuplicated(ledger$project_id)) {
  stop("Preferred project ledger is not unique by project.", call. = FALSE)
}

year_mismatches <- readr::read_csv(
  "../output/preferred_density_multicard_year_mismatches.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
)

production_500ft_pins <- production |>
  dplyr::filter(
    construction_year >= 2006,
    construction_year <= 2022,
    distance_to_boundary_ft <= 500
  ) |>
  dplyr::pull(project_id)

geography_recovery_projects <- components |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    represented_in_production_500ft = any(component_pin %in% production_500ft_pins),
    .groups = "drop"
  ) |>
  dplyr::filter(!represented_in_production_500ft) |>
  dplyr::inner_join(
    ledger,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(
    source_family == "residential",
    project_kind %in% c(
      "single_pin_single_card",
      "same_pin_multiple_cards"
    )
  )

additions <- preferred |>
  dplyr::filter(
    project_id %in% geography_recovery_projects$project_id,
    construction_year >= 2006,
    construction_year <= 2022,
    distance_to_boundary_ft <= 500,
    dwelling_units > 1
  )

production_common_year <- production |>
  dplyr::filter(!project_id %in% year_mismatches$pin)

combined <- dplyr::bind_rows(
  production_common_year,
  additions
)

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

estimate_model <- function(data, outcome, treatment) {
  eligibility_field <- if (outcome == "density_far") "allow_far" else "allow_dupac"
  treatment_var <- if (treatment == "continuous") {
    "continuous_score_difference"
  } else {
    "side"
  }

  model_data <- data |>
    dplyr::filter(
      construction_year >= 2006,
      construction_year <= 2022,
      distance_to_boundary_ft <= 500,
      dwelling_units > 1,
      .data[[eligibility_field]],
      is.finite(.data[[outcome]]),
      .data[[outcome]] > 0,
      is.finite(signed_distance_m),
      is.finite(pair_average_score),
      dplyr::if_all(
        dplyr::all_of(demographic_controls),
        is.finite
      ),
      !is.na(zone_group),
      !is.na(segment_id),
      segment_id != ""
    )

  model <- fixest::feols(
    stats::as.formula(paste0(
      "log(",
      outcome,
      ") ~ ",
      paste(
        c(
          treatment_var,
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

  coefficient <- fixest::coeftable(model)[treatment_var, ]
  tibble::tibble(
    estimate = unname(coefficient["Estimate"]),
    se = unname(coefficient["Std. Error"]),
    p_value = unname(coefficient["Pr(>|t|)"]),
    n_obs = stats::nobs(model)
  )
}

influence_rows <- list()
alderman_influence_rows <- list()
for (outcome in c("density_far", "density_dupac")) {
  for (treatment in c("continuous", "binary")) {
    production_result <- estimate_model(
      production_common_year,
      outcome,
      treatment
    )
    full_result <- estimate_model(
      combined,
      outcome,
      treatment
    )

    for (pair in sort(unique(additions$ward_pair))) {
      pair_project_ids <- additions |>
        dplyr::filter(ward_pair == pair) |>
        dplyr::pull(project_id)
      leave_pair_out <- estimate_model(
        combined |>
          dplyr::filter(!project_id %in% pair_project_ids),
        outcome,
        treatment
      )

      influence_rows[[length(influence_rows) + 1L]] <- tibble::tibble(
        outcome,
        treatment,
        ward_pair = pair,
        added_projects = length(pair_project_ids),
        production_estimate = production_result$estimate,
        full_geography_estimate = full_result$estimate,
        leave_pair_out_estimate = leave_pair_out$estimate,
        pair_contribution = full_result$estimate - leave_pair_out$estimate
      )
    }

    for (alderman in sort(unique(stats::na.omit(additions$alderman_own)))) {
      alderman_projects <- additions |>
        dplyr::filter(alderman_own == alderman) |>
        dplyr::pull(project_id)
      leave_alderman_out <- estimate_model(
        combined |>
          dplyr::filter(!project_id %in% alderman_projects),
        outcome,
        treatment
      )

      alderman_influence_rows[[length(alderman_influence_rows) + 1L]] <-
        tibble::tibble(
          outcome,
          treatment,
          alderman,
          wards = paste(
            sort(unique(additions$ward[additions$alderman_own == alderman])),
            collapse = ", "
          ),
          added_projects = length(alderman_projects),
          production_estimate = production_result$estimate,
          full_geography_estimate = full_result$estimate,
          leave_alderman_out_estimate = leave_alderman_out$estimate,
          alderman_contribution =
            full_result$estimate - leave_alderman_out$estimate
        )
    }
  }
}

project_details <- additions |>
  dplyr::mutate(
    project_side = dplyr::if_else(
      side == 1,
      "more_stringent",
      "less_stringent"
    )
  ) |>
  dplyr::select(
    project_id,
    project_kind,
    construction_year,
    ward,
    neighbor_ward,
    ward_pair,
    alderman_own,
    alderman_neighbor,
    strictness_own,
    strictness_neighbor,
    project_side,
    dwelling_units,
    density_far,
    density_dupac,
    distance_to_boundary_ft
  ) |>
  dplyr::arrange(ward_pair, ward, project_id)

ward_pair_summary <- project_details |>
  dplyr::group_by(ward_pair) |>
  dplyr::summarise(
    added_multifamily_projects = dplyr::n(),
    more_stringent_projects = sum(project_side == "more_stringent"),
    less_stringent_projects = sum(project_side == "less_stringent"),
    project_wards = paste(sort(unique(ward)), collapse = ", "),
    project_aldermen = paste(sort(unique(alderman_own)), collapse = "; "),
    neighboring_aldermen = paste(sort(unique(alderman_neighbor)), collapse = "; "),
    mean_far = mean(density_far, na.rm = TRUE),
    mean_dupac = mean(density_dupac, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(added_multifamily_projects), ward_pair)

readr::write_csv(
  project_details,
  "../output/geography_recovery_multifamily_projects.csv",
  na = ""
)
readr::write_csv(
  ward_pair_summary,
  "../output/geography_recovery_ward_pair_summary.csv",
  na = ""
)
readr::write_csv(
  dplyr::bind_rows(influence_rows) |>
    dplyr::arrange(
      outcome,
      treatment,
      dplyr::desc(pair_contribution)
    ),
  "../output/geography_recovery_pair_influence.csv",
  na = ""
)
readr::write_csv(
  dplyr::bind_rows(alderman_influence_rows) |>
    dplyr::arrange(
      outcome,
      treatment,
      dplyr::desc(alderman_contribution)
    ),
  "../output/geography_recovery_alderman_influence.csv",
  na = ""
)
