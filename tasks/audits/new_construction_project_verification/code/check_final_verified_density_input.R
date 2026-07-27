# setwd("tasks/audits/new_construction_project_verification/code")

source("../../../setup_environment/code/packages.R")

base <- readr::read_csv(
  "../input/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_skip(),
    project_id = readr::col_character()
  )
)
review <- readr::read_csv(
  "../output/final_project_verification_ledger.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
)
final <- readr::read_csv(
  "../output/final_verified_density_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character())
)
geography_changes <- readr::read_csv(
  "../output/final_verified_density_geography_changes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character())
)
results <- readr::read_csv(
  "../output/final_verified_density_results.csv",
  show_col_types = FALSE
)

retained_review <- review |>
  dplyr::filter(final_include)
excluded_review <- review |>
  dplyr::filter(!final_include)
retained_in_scope <- retained_review |>
  dplyr::inner_join(
    final,
    by = "project_id",
    relationship = "one-to-one",
    suffix = c("_review", "_final")
  )

component_pins <- retained_review |>
  dplyr::select(project_id, component_pins) |>
  tidyr::separate_longer_delim(component_pins, delim = "/") |>
  dplyr::mutate(
    component_pins = stringr::str_pad(component_pins, 14, pad = "0")
  )

model_results <- results |>
  dplyr::filter(
    sample_rule == "common_density",
    treatment == "binary",
    cluster_level == "ward_pair"
  )

checks <- tibble::tribble(
  ~check,                                      ~observed, ~expected,
  "base_projects",                            nrow(base), 8705,
  "review_candidates",                        nrow(review), 795,
  "retained_review_projects",                 nrow(retained_review), 740,
  "excluded_review_projects",                 nrow(excluded_review), 55,
  "final_projects_within_1500ft",             nrow(final), 8648,
  "final_projects_within_500ft",              sum(final$within_500ft), 3710,
  "retained_review_projects_within_1500ft",   nrow(retained_in_scope), 738,
  "review_projects_leaving_1500ft",           nrow(retained_review) -
    nrow(retained_in_scope), 2,
  "projects_leaving_500ft_after_year_change", sum(
    geography_changes$prior_within_500ft &
      !geography_changes$within_500ft
  ), 5,
  "projects_with_changed_boundary_assignment", sum(
    geography_changes$prior_ward_pair != geography_changes$ward_pair |
      geography_changes$prior_ward != geography_changes$ward |
      geography_changes$prior_neighbor_ward !=
        geography_changes$neighbor_ward
  ), 5,
  "duplicate_final_project_ids",               anyDuplicated(final$project_id), 0,
  "duplicate_retained_component_pins",         anyDuplicated(
    component_pins$component_pins
  ), 0,
  "excluded_projects_in_final_data",           nrow(dplyr::semi_join(
    final,
    excluded_review,
    by = "project_id"
  )), 0,
  "binary_common_sample_models",               nrow(model_results), 4
)

field_mismatches <- retained_in_scope |>
  dplyr::summarise(
    construction_year = sum(
      final_construction_year != construction_year_final,
      na.rm = TRUE
    ),
    dwelling_units = sum(
      audit_dwelling_units != dwelling_units_final,
      na.rm = TRUE
    ),
    building_sqft = sum(
      dplyr::coalesce(audit_building_sqft, -1) !=
        dplyr::coalesce(building_sqft_final, -1)
    ),
    land_sqft = sum(
      dplyr::coalesce(audit_land_sqft, -1) !=
        dplyr::coalesce(land_sqft_final, -1)
    ),
    multifamily = sum(
      audit_current_multifamily != external_multifamily,
      na.rm = TRUE
    ),
    far_eligibility = sum(valid_far != allow_far, na.rm = TRUE),
    dupac_eligibility = sum(valid_dupac != allow_dupac, na.rm = TRUE)
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "field",
    values_to = "mismatches"
  )

checks <- dplyr::bind_rows(
  checks,
  field_mismatches |>
    dplyr::transmute(
      check = paste0("review_field_mismatches_", field),
      observed = mismatches,
      expected = 0
    )
) |>
  dplyr::mutate(passed = observed == expected)

if (any(!checks$passed)) {
  print(checks |> dplyr::filter(!passed))
  stop("The final verified density input failed an audit check.")
}

readr::write_csv(
  checks,
  "../output/final_verified_density_checks.csv",
  na = ""
)
