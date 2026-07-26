# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

library(sf)

scope <- readr::read_csv(
  "../output/final_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(within_1500ft)

ledger <- readr::read_csv(
  "../output/final_new_construction_audit_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

zoning <- readr::read_csv(
  "../output/final_new_construction_zoning.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_zone_group = readr::col_character(),
    zoning_assignment_source = readr::col_character(),
    .default = readr::col_skip()
  )
)

previous <- readr::read_csv(
  "../output/preferred_density_model_production_card_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (
  anyDuplicated(scope$project_id) ||
    anyDuplicated(ledger$project_id) ||
    anyDuplicated(zoning$project_id) ||
    anyDuplicated(previous$project_id)
) {
  stop("A final density input is not unique by project.", call. = FALSE)
}

retained <- previous |>
  dplyr::semi_join(
    scope |>
      dplyr::filter(ledger_action == "retain_existing"),
    by = "project_id"
  ) |>
  dplyr::mutate(ledger_action = "retain_existing")

additions <- scope |>
  dplyr::filter(ledger_action == "add_recovered_project") |>
  dplyr::left_join(
    ledger |>
      dplyr::select(
        project_id,
        source_addresses,
        allow_far,
        allow_dupac
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

addition_points <- sf::st_as_sf(
  additions,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
segments_by_era <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(addition_points$era))
)
additions$segment_id <- assign_points_to_nearest_segments(
  addition_points,
  addition_points$era,
  addition_points$ward_pair,
  segments_by_era,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 1000L
)

aldermen <- readr::read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) |>
  dplyr::transmute(
    ward = as.integer(ward),
    yearmon_key = as.character(zoo::as.yearmon(month, format = "%b %Y")),
    alderman
  )
if (anyDuplicated(aldermen[c("ward", "yearmon_key")])) {
  stop("Alderman panel has duplicate ward-month rows.", call. = FALSE)
}

controls <- readr::read_csv(
  "../input/ward_controls_2000_2023.csv",
  show_col_types = FALSE
)
if (anyDuplicated(controls[c("ward", "year")])) {
  stop("Ward controls have duplicate ward-year rows.", call. = FALSE)
}

scores <- readr::read_csv(
  "../input/alderman_uncertainty_index.csv",
  show_col_types = FALSE
) |>
  dplyr::select(alderman, score = uncertainty_index)
if (anyDuplicated(scores$alderman)) {
  stop("Alderman scores are not unique by alderman.", call. = FALSE)
}

additions <- additions |>
  dplyr::left_join(
    zoning,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    yearmon_key = as.character(zoo::as.yearmon(construction_date)),
    dist_to_boundary_m = distance_to_boundary_ft * 0.3048,
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft
  ) |>
  dplyr::left_join(
    aldermen,
    by = c("ward", "yearmon_key"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(alderman_own = alderman) |>
  dplyr::left_join(
    aldermen |>
      dplyr::rename(alderman_neighbor = alderman),
    by = c("neighbor_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    scores,
    by = c("alderman_own" = "alderman"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(strictness_own = score) |>
  dplyr::left_join(
    scores,
    by = c("alderman_neighbor" = "alderman"),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(strictness_neighbor = score) |>
  dplyr::left_join(
    controls,
    by = c("ward", "construction_year" = "year"),
    relationship = "many-to-one"
  ) |>
  dplyr::left_join(
    controls,
    by = c("neighbor_ward" = "ward", "construction_year" = "year"),
    suffix = c("_own", "_neighbor"),
    relationship = "many-to-one"
  ) |>
  dplyr::mutate(
    signed_distance_m = dist_to_boundary_m * dplyr::case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    lenient_dist = abs(signed_distance_m) *
      as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) *
      as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference =
      (strictness_own - strictness_neighbor) / 2,
    pair_average_score =
      (strictness_own + strictness_neighbor) / 2,
    zone_group = construction_zone_group
  )

required_fields <- c(
  "segment_id",
  "zone_group",
  "alderman_own",
  "alderman_neighbor",
  "strictness_own",
  "strictness_neighbor",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)
missing_addition_fields <- additions |>
  dplyr::filter(within_500ft) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(required_fields),
      ~ sum(is.na(.x) | as.character(.x) == "")
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "field",
    values_to = "missing_rows"
  )

if (any(missing_addition_fields$missing_rows > 0)) {
  print(missing_addition_fields)
  stop("A recovered 500-foot project lacks a model field.", call. = FALSE)
}

model_columns <- c(
  "project_id",
  "source_family",
  "ledger_action",
  "project_kind",
  "construction_year",
  "construction_date",
  "ward",
  "neighbor_ward",
  "ward_pair",
  "distance_to_boundary_ft",
  "within_500ft",
  "within_1500ft",
  "allow_far",
  "allow_dupac",
  "segment_id",
  "dwelling_units",
  "building_sqft",
  "land_sqft",
  "density_far",
  "density_dupac",
  "zone_group",
  "alderman_own",
  "alderman_neighbor",
  "strictness_own",
  "strictness_neighbor",
  "signed_distance_m",
  "lenient_dist",
  "strict_dist",
  "side",
  "continuous_score_difference",
  "pair_average_score",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

previous <- previous |>
  dplyr::mutate(ledger_action = "previous_preferred") |>
  dplyr::select(dplyr::all_of(model_columns))
retained <- retained |>
  dplyr::select(dplyr::all_of(model_columns))
additions <- additions |>
  dplyr::select(dplyr::all_of(model_columns))

final <- dplyr::bind_rows(retained, additions)

if (
  anyDuplicated(final$project_id) ||
    nrow(additions) != 35L ||
    any(!additions$allow_far) ||
    any(!additions$allow_dupac)
) {
  stop("Final density model input failed validation.", call. = FALSE)
}

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

estimate_models <- function(data, sample_version) {
  rows <- list()

  for (sample_name in c("all", "multifamily")) {
    sample_data <- data |>
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

        for (treatment in c("continuous", "binary")) {
          treatment_var <- if (treatment == "continuous") {
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

          rows[[length(rows) + 1L]] <- tibble::tibble(
            sample_version,
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

  dplyr::bind_rows(rows)
}

results <- dplyr::bind_rows(
  estimate_models(previous, "previous_preferred"),
  estimate_models(retained, "deduplicated_existing"),
  estimate_models(final, "final")
)

if (
  nrow(results) != 48L ||
    anyDuplicated(
      results[
        c(
          "sample_version",
          "sample",
          "eligibility",
          "outcome",
          "treatment"
        )
      ]
    )
) {
  stop("Final density result grid is incomplete or duplicated.", call. = FALSE)
}

input_summary <- dplyr::bind_rows(
  previous |>
    dplyr::summarise(
      sample_version = "previous_preferred",
      projects = dplyr::n(),
      projects_500ft = sum(within_500ft),
      multifamily_500ft = sum(
        within_500ft & dwelling_units > 1,
        na.rm = TRUE
      )
    ),
  retained |>
    dplyr::summarise(
      sample_version = "deduplicated_existing",
      projects = dplyr::n(),
      projects_500ft = sum(within_500ft),
      multifamily_500ft = sum(
        within_500ft & dwelling_units > 1,
        na.rm = TRUE
      )
    ),
  final |>
    dplyr::summarise(
      sample_version = "final",
      projects = dplyr::n(),
      projects_500ft = sum(within_500ft),
      multifamily_500ft = sum(
        within_500ft & dwelling_units > 1,
        na.rm = TRUE
      )
    )
)

readr::write_csv(
  final,
  "../output/final_density_model_input.csv",
  na = ""
)
readr::write_csv(
  results,
  "../output/final_density_model_results.csv",
  na = ""
)
readr::write_csv(
  input_summary,
  "../output/final_density_model_input_summary.csv",
  na = ""
)
