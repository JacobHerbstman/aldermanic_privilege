# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

base <- readr::read_csv(
  "../input/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_skip(),
    project_id = readr::col_character(),
    construction_year = readr::col_integer(),
    ward_pair = readr::col_character(),
    within_500ft = readr::col_logical(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    density_far = readr::col_double(),
    density_dupac = readr::col_double(),
    external_multifamily = readr::col_logical(),
    allow_far = readr::col_logical(),
    allow_dupac = readr::col_logical(),
    alderman_own = readr::col_character(),
    alderman_neighbor = readr::col_character(),
    distance_to_boundary_ft = readr::col_double(),
    share_white_own = readr::col_double(),
    share_black_own = readr::col_double(),
    median_hh_income_own = readr::col_double(),
    share_bach_plus_own = readr::col_double(),
    homeownership_rate_own = readr::col_double(),
    zone_group = readr::col_character(),
    segment_id = readr::col_character()
  )
)
review <- readr::read_csv(
  "../output/final_project_verification_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character())
) |>
  dplyr::select(
    project_id,
    final_include,
    audit_dwelling_units,
    audit_building_sqft,
    audit_land_sqft,
    audit_current_multifamily,
    valid_far,
    valid_dupac
  )
scores <- readr::read_csv(
  "../input/alderman_uncertainty_index.csv",
  show_col_types = FALSE
) |>
  dplyr::select(alderman, score = uncertainty_index)
final <- readr::read_csv(
  "../output/final_verified_density_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character())
)

base <- base |>
  dplyr::select(
    -dplyr::any_of(c(
      "strictness_own",
      "strictness_neighbor",
      "side",
      "continuous_score_difference",
      "pair_average_score",
      "signed_distance_m",
      "lenient_dist",
      "strict_dist"
    ))
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(alderman_own = alderman, strictness_own = score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores |>
      dplyr::rename(
        alderman_neighbor = alderman,
        strictness_neighbor = score
      ),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    side = as.integer(strictness_own > strictness_neighbor),
    pair_average_score =
      (strictness_own + strictness_neighbor) / 2,
    lenient_dist = distance_to_boundary_ft * 0.3048 *
      as.integer(side == 0L),
    strict_dist = distance_to_boundary_ft * 0.3048 *
      as.integer(side == 1L)
  )

exclusions_only <- base |>
  dplyr::left_join(review, by = "project_id", relationship = "one-to-one") |>
  dplyr::filter(is.na(final_include) | final_include) |>
  dplyr::select(-dplyr::all_of(names(review)[-1]))

fields_corrected <- base |>
  dplyr::left_join(review, by = "project_id", relationship = "one-to-one") |>
  dplyr::filter(is.na(final_include) | final_include) |>
  dplyr::mutate(
    reviewed_project = !is.na(final_include),
    dwelling_units = dplyr::if_else(
      reviewed_project,
      audit_dwelling_units,
      dwelling_units
    ),
    building_sqft = dplyr::if_else(
      reviewed_project,
      audit_building_sqft,
      building_sqft
    ),
    land_sqft = dplyr::if_else(
      reviewed_project,
      audit_land_sqft,
      land_sqft
    ),
    external_multifamily = dplyr::if_else(
      reviewed_project,
      audit_current_multifamily,
      external_multifamily
    ),
    allow_far = dplyr::if_else(reviewed_project, valid_far, allow_far),
    allow_dupac = dplyr::if_else(reviewed_project, valid_dupac, allow_dupac),
    density_far = dplyr::if_else(
      allow_far,
      building_sqft / land_sqft,
      NA_real_
    ),
    density_dupac = dplyr::if_else(
      allow_dupac,
      43560 * dwelling_units / land_sqft,
      NA_real_
    )
  ) |>
  dplyr::select(-dplyr::all_of(names(review)[-1]), -reviewed_project)

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)
versions <- list(
  current = base,
  exclusions_only = exclusions_only,
  fields_corrected = fields_corrected,
  final = final
)
results <- list()

for (version_name in names(versions)) {
  version_data <- versions[[version_name]]

  for (sample_name in c("all", "multifamily")) {
    model_data <- version_data |>
      dplyr::filter(
        construction_year >= 2006,
        construction_year <= 2022,
        within_500ft,
        dwelling_units > 0,
        sample_name == "all" | external_multifamily,
        allow_far,
        allow_dupac,
        is.finite(density_far),
        density_far > 0,
        is.finite(density_dupac),
        density_dupac > 0,
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
      model <- fixest::feols(
        stats::as.formula(paste0(
          "log(",
          outcome,
          ") ~ side + pair_average_score + lenient_dist + strict_dist + ",
          paste(demographic_controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = model_data,
        cluster = ~ward_pair
      )
      coefficient <- fixest::coeftable(model)["side", ]

      results[[length(results) + 1L]] <- tibble::tibble(
        version = version_name,
        sample = sample_name,
        outcome,
        estimate = unname(coefficient["Estimate"]),
        se = unname(coefficient["Std. Error"]),
        p_value = unname(coefficient["Pr(>|t|)"]),
        n_obs = stats::nobs(model)
      )
    }
  }
}

readr::write_csv(
  dplyr::bind_rows(results),
  "../output/final_review_decomposition.csv",
  na = ""
)
