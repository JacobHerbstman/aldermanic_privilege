# setwd("tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

library(sf)

scope <- readr::read_csv(
  "../output/preferred_new_construction_boundary_scope.csv",
  show_col_types = FALSE
) |>
  dplyr::filter(within_1500ft)

ledger <- readr::read_csv(
  "../output/preferred_new_construction_project_ledger.csv",
  show_col_types = FALSE
)

zoning <- readr::read_csv(
  "../output/preferred_new_construction_zoning.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character()),
  col_select = c(
    project_id,
    construction_zone_group,
    zoning_assignment_source
  )
)

points <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    scope,
    by = c("project_id", "source_family"),
    relationship = "one-to-one"
  )

if (sf::st_crs(points)$epsg != 3435) {
  stop("Preferred project centroids must use EPSG:3435.", call. = FALSE)
}

segments_by_era <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(points$era))
)
points$segment_id <- assign_points_to_nearest_segments(
  points,
  points$era,
  points$ward_pair,
  segments_by_era,
  max_distance = units::set_units(457.2, "m"),
  chunk_n = 5000L
)

preferred <- points |>
  sf::st_drop_geometry() |>
  dplyr::inner_join(
    ledger |>
      dplyr::select(
        project_id,
        project_kind,
        dwelling_units,
        building_sqft,
        land_sqft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::inner_join(
    zoning,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    construction_date = as.Date(construction_date),
    yearmon_key = as.character(zoo::as.yearmon(construction_date)),
    dist_to_boundary_m = distance_to_boundary_ft * 0.3048,
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft
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

preferred <- preferred |>
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
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2
  )

required_preferred <- c(
  "segment_id",
  "construction_zone_group",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)
missing_preferred <- preferred |>
  dplyr::filter(within_500ft) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(required_preferred),
      ~ sum(is.na(.x) | as.character(.x) == "")
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "field",
    values_to = "missing_rows"
  )

if (any(missing_preferred$missing_rows > 0)) {
  print(missing_preferred)
  stop("Preferred 500-foot model input has missing fixed effects or controls.", call. = FALSE)
}
if (anyDuplicated(preferred$project_id)) {
  stop("Preferred model input is not unique by project.", call. = FALSE)
}

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
    source_family = "production",
    project_kind = "production",
    dwelling_units = unitscount,
    building_sqft = areabuilding,
    land_sqft = arealotsf,
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

preferred <- preferred |>
  dplyr::mutate(zone_group = construction_zone_group)

production_commercial_pins <- readr::read_csv(
  "../output/commercial_500ft_sample_review.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character()),
  col_select = "pin"
) |>
  dplyr::distinct(pin) |>
  dplyr::pull(pin)

production_multicard_pins <- readr::read_csv(
  "../output/residential_multicard_cards.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_skip()),
  col_select = "pin"
) |>
  dplyr::distinct(pin) |>
  dplyr::pull(pin)

production_multicard_values <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::filter(pin %in% production_multicard_pins) |>
  dplyr::mutate(
    is_single_family =
      (
        !is.na(single_v_multi_family) &
          stringr::str_detect(
            single_v_multi_family,
            stringr::regex("^single", ignore_case = TRUE)
          )
      ) |
      (
        !is.na(type_of_residence) &
          type_of_residence %in% c(
            "1 Story",
            "1.5 Story",
            "2 Story",
            "3 Story +",
            "Split Level"
          )
      ),
    production_units = dplyr::if_else(
      is_single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  ) |>
  dplyr::transmute(
    pin,
    production_year = as.integer(year_built),
    production_units,
    production_building_sqft = as.numeric(building_sqft),
    production_land_sqft = as.numeric(land_sqft)
  )

preferred_multicard_values <- ledger |>
  dplyr::filter(
    source_family == "residential",
    project_kind == "same_pin_multiple_cards"
  ) |>
  dplyr::transmute(
    project_id,
    pin = component_pins,
    preferred_year = as.integer(construction_year)
  ) |>
  dplyr::inner_join(
    production_multicard_values,
    by = "pin",
    relationship = "one-to-one"
  )

preferred_multicard_projects <- preferred |>
  dplyr::filter(project_kind == "same_pin_multiple_cards") |>
  dplyr::pull(project_id)
preferred_multicard_values <- preferred_multicard_values |>
  dplyr::filter(project_id %in% preferred_multicard_projects)
if (!setequal(preferred_multicard_values$project_id, preferred_multicard_projects)) {
  stop("A preferred same-PIN multicard project lacks its production-selected card.", call. = FALSE)
}

multicard_year_mismatches <- preferred_multicard_values |>
  dplyr::filter(preferred_year != production_year)

preferred_production_card_rule <- preferred |>
  dplyr::left_join(
    preferred_multicard_values |>
      dplyr::select(
        project_id,
        production_units,
        production_building_sqft,
        production_land_sqft
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    replace_multicard = project_id %in% preferred_multicard_values$project_id,
    dwelling_units = dplyr::if_else(
      replace_multicard,
      production_units,
      dwelling_units
    ),
    building_sqft = dplyr::if_else(
      replace_multicard,
      production_building_sqft,
      building_sqft
    ),
    land_sqft = dplyr::if_else(
      replace_multicard,
      production_land_sqft,
      land_sqft
    ),
    density_far = building_sqft / land_sqft,
    density_dupac = 43560 * dwelling_units / land_sqft,
    allow_far = dplyr::if_else(
      replace_multicard,
      land_sqft > 1 & building_sqft > 1,
      allow_far
    ),
    allow_dupac = dplyr::if_else(
      replace_multicard,
      land_sqft > 1 & building_sqft > 1,
      allow_dupac
    )
  ) |>
  dplyr::filter(!project_id %in% multicard_year_mismatches$project_id) |>
  dplyr::select(
    -production_units,
    -production_building_sqft,
    -production_land_sqft,
    -replace_multicard
  )

preferred_residential_components <- readr::read_csv(
  "../output/preferred_residential_project_components_final.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    component_pin = readr::col_character(),
    .default = readr::col_guess()
  )
)
if (anyDuplicated(preferred_residential_components$component_pin)) {
  stop("A residential component PIN belongs to more than one preferred project.", call. = FALSE)
}

production_500ft_pins <- production |>
  dplyr::filter(
    construction_year >= 2006,
    construction_year <= 2022,
    distance_to_boundary_ft <= 500
  ) |>
  dplyr::pull(project_id)

geography_recovery_project_ids <- preferred_residential_components |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    represented_in_production_500ft = any(component_pin %in% production_500ft_pins),
    .groups = "drop"
  ) |>
  dplyr::filter(!represented_in_production_500ft) |>
  dplyr::inner_join(
    ledger |>
      dplyr::select(project_id, source_family, project_kind),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::filter(
    source_family == "residential",
    project_kind %in% c(
      "single_pin_single_card",
      "same_pin_multiple_cards"
    )
  ) |>
  dplyr::pull(project_id)

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

    for (outcome in c("density_far", "density_dupac")) {
      eligibility_field <- if (outcome == "density_far") "allow_far" else "allow_dupac"
      model_data <- sample_data |>
        dplyr::filter(
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

      for (treatment in c("continuous", "binary")) {
        treatment_var <- if (treatment == "continuous") {
          "continuous_score_difference"
        } else {
          "side"
        }
        controls_formula <- c(
          treatment_var,
          "pair_average_score",
          "lenient_dist",
          "strict_dist",
          demographic_controls
        )
        model <- fixest::feols(
          stats::as.formula(paste0(
            "log(",
            outcome,
            ") ~ ",
            paste(controls_formula, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = model_data,
          cluster = ~ward_pair
        )
        coefficient <- fixest::coeftable(model)[treatment_var, ]

        rows[[length(rows) + 1L]] <- tibble::tibble(
          sample_version,
          sample = sample_name,
          outcome,
          treatment,
          estimate = unname(coefficient["Estimate"]),
          se = unname(coefficient["Std. Error"]),
          p_value = unname(coefficient["Pr(>|t|)"]),
          n_obs = stats::nobs(model),
          ward_pairs = dplyr::n_distinct(model_data$ward_pair),
          projects_before_model = nrow(sample_data)
        )
      }
    }
  }

  dplyr::bind_rows(rows)
}

results <- dplyr::bind_rows(
  estimate_models(production, "production"),
  estimate_models(
    production |>
      dplyr::filter(!project_id %in% production_multicard_pins),
    "production_no_multicard"
  ),
  estimate_models(
    production |>
      dplyr::filter(!project_id %in% multicard_year_mismatches$pin),
    "production_card_rule_common_year"
  ),
  estimate_models(
    dplyr::bind_rows(
      production |>
        dplyr::filter(!project_id %in% multicard_year_mismatches$pin),
      preferred_production_card_rule |>
        dplyr::filter(project_id %in% geography_recovery_project_ids)
    ),
    "production_plus_geography_recovery"
  ),
  estimate_models(
    dplyr::bind_rows(
      production |>
        dplyr::filter(!project_id %in% production_commercial_pins),
      preferred |>
        dplyr::filter(source_family == "commercial")
    ),
    "preferred_commercial_only"
  ),
  estimate_models(
    dplyr::bind_rows(
      production |>
        dplyr::filter(project_id %in% production_commercial_pins),
      preferred |>
        dplyr::filter(source_family == "residential")
    ),
    "preferred_residential_only"
  ),
  estimate_models(
    dplyr::bind_rows(
      production |>
        dplyr::filter(
          project_id %in% production_commercial_pins,
          !project_id %in% multicard_year_mismatches$pin
        ),
      preferred_production_card_rule |>
        dplyr::filter(source_family == "residential")
    ),
    "preferred_residential_card_rule_only"
  ),
  estimate_models(
    preferred |>
      dplyr::filter(project_kind != "same_pin_multiple_cards"),
    "preferred_no_multicard"
  ),
  estimate_models(
    preferred_production_card_rule,
    "preferred_card_rule_common_year"
  ),
  estimate_models(preferred, "preferred")
)

if (nrow(results) != 80 || anyDuplicated(
  results[c("sample_version", "sample", "outcome", "treatment")]
)) {
  stop("Density model result grid is incomplete or duplicated.", call. = FALSE)
}

readr::write_csv(
  preferred,
  "../output/preferred_density_model_input.csv",
  na = ""
)
readr::write_csv(
  preferred_production_card_rule,
  "../output/preferred_density_model_production_card_input.csv",
  na = ""
)
readr::write_csv(
  multicard_year_mismatches,
  "../output/preferred_density_multicard_year_mismatches.csv",
  na = ""
)
readr::write_csv(
  results,
  "../output/preferred_density_model_results.csv",
  na = ""
)
readr::write_csv(
  missing_preferred,
  "../output/preferred_density_model_input_validation.csv",
  na = ""
)
readr::write_csv(
  preferred |>
    dplyr::filter(
      within_500ft,
      dwelling_units > 0,
      allow_far | allow_dupac,
      !is.finite(strictness_own) | !is.finite(strictness_neighbor)
    ) |>
    dplyr::select(
      project_id,
      source_family,
      construction_year,
      ward,
      neighbor_ward,
      ward_pair,
      alderman_own,
      alderman_neighbor,
      dwelling_units,
      density_far,
      density_dupac
    ),
  "../output/preferred_density_model_score_attrition.csv",
  na = ""
)
