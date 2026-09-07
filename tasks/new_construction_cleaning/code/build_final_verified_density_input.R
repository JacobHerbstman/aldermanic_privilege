# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

source("../../shared/code/canonical_geometry_helpers.R")

base <- readr::read_csv(
  "../output/provisional_validated_density_input.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_skip(),
    project_id = readr::col_character(),
    source_family = readr::col_character(),
    project_kind = readr::col_character(),
    construction_year = readr::col_integer(),
    construction_date = readr::col_date(),
    ward = readr::col_integer(),
    neighbor_ward = readr::col_integer(),
    ward_pair = readr::col_character(),
    distance_to_boundary_ft = readr::col_double(),
    within_500ft = readr::col_logical(),
    within_1500ft = readr::col_logical(),
    segment_id = readr::col_character(),
    zone_group = readr::col_character(),
    allow_far = readr::col_logical(),
    allow_dupac = readr::col_logical(),
    dwelling_units = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    external_multifamily = readr::col_logical()
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
) |>
  dplyr::select(
    project_id,
    final_include,
    final_construction_year,
    audit_dwelling_units,
    audit_building_sqft,
    audit_land_sqft,
    audit_current_multifamily,
    valid_far,
    valid_dupac,
    final_evidence_tier
  )

scope <- readr::read_csv(
  "../output/final_new_construction_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character())
) |>
  dplyr::select(project_id, x_3435, y_3435)

zoning <- readr::read_csv(
  "../output/final_new_construction_zoning.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_skip(),
    project_id = readr::col_character(),
    zone_group_2006 = readr::col_character(),
    zone_group_2012 = readr::col_character(),
    zone_group_2014 = readr::col_character(),
    zone_group_2016 = readr::col_character(),
    zone_group_2025 = readr::col_character()
  )
) |>
  dplyr::select(
    project_id,
    zone_group_2006,
    zone_group_2012,
    zone_group_2014,
    zone_group_2016,
    zone_group_2025
  )

zoning_decisions <- readr::read_csv(
  "../adjudication/corrected_year_zoning_decisions.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    construction_year = readr::col_integer()
  )
)

if (
  anyDuplicated(base$project_id) ||
    anyDuplicated(review$project_id) ||
    anyDuplicated(scope$project_id) ||
    anyDuplicated(zoning$project_id) ||
    anyDuplicated(zoning_decisions$project_id)
) {
  stop("A density input is not uniquely keyed by project_id.")
}

if (nrow(review) != 795L || sum(review$final_include) != 740L) {
  stop("The final project review does not contain 740 of 795 retained projects.")
}

if (nrow(dplyr::anti_join(base, scope, by = "project_id")) > 0L) {
  stop("At least one provisional project lacks projected coordinates.")
}

data <- base |>
  dplyr::rename(
    prior_construction_year = construction_year,
    prior_ward = ward,
    prior_neighbor_ward = neighbor_ward,
    prior_ward_pair = ward_pair,
    prior_distance_to_boundary_ft = distance_to_boundary_ft,
    prior_within_500ft = within_500ft,
    prior_within_1500ft = within_1500ft,
    prior_segment_id = segment_id,
    prior_zone_group = zone_group
  ) |>
  dplyr::left_join(review, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(scope, by = "project_id", relationship = "many-to-one") |>
  dplyr::left_join(zoning, by = "project_id", relationship = "many-to-one") |>
  dplyr::mutate(
    reviewed_project = !is.na(final_include),
    final_include = dplyr::coalesce(final_include, TRUE)
  ) |>
  dplyr::filter(final_include) |>
  dplyr::mutate(
    construction_year = dplyr::if_else(
      reviewed_project,
      as.integer(final_construction_year),
      as.integer(prior_construction_year)
    ),
    dwelling_units = dplyr::if_else(
      reviewed_project,
      as.numeric(audit_dwelling_units),
      as.numeric(dwelling_units)
    ),
    building_sqft = dplyr::if_else(
      reviewed_project,
      as.numeric(audit_building_sqft),
      as.numeric(building_sqft)
    ),
    land_sqft = dplyr::if_else(
      reviewed_project,
      as.numeric(audit_land_sqft),
      as.numeric(land_sqft)
    ),
    allow_far = dplyr::if_else(reviewed_project, valid_far, allow_far),
    allow_dupac = dplyr::if_else(reviewed_project, valid_dupac, allow_dupac),
    external_multifamily = dplyr::if_else(
      reviewed_project,
      audit_current_multifamily,
      external_multifamily
    ),
    construction_year_changed = construction_year != prior_construction_year,
    stable_zone_group = dplyr::case_when(
      construction_year <= 2006 ~ zone_group_2006,
      construction_year <= 2012 &
        zone_group_2006 == zone_group_2012 ~ zone_group_2006,
      construction_year <= 2014 &
        zone_group_2012 == zone_group_2014 ~ zone_group_2012,
      construction_year == 2015 &
        zone_group_2014 == zone_group_2016 ~ zone_group_2014,
      construction_year >= 2016 &
        zone_group_2016 == zone_group_2025 ~ zone_group_2016,
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::left_join(
    zoning_decisions |>
      dplyr::rename(
        decision_construction_year = construction_year,
        decision_zone_group = construction_zone_group
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    zone_group = dplyr::case_when(
      !construction_year_changed ~ prior_zone_group,
      !is.na(decision_zone_group) ~ decision_zone_group,
      !is.na(stable_zone_group) ~ stable_zone_group,
      TRUE ~ NA_character_
    )
  )

changed_zoning <- data |>
  dplyr::filter(construction_year_changed)

if (
  nrow(changed_zoning) != 101L ||
    any(is.na(changed_zoning$zone_group)) ||
    any(
      !is.na(changed_zoning$decision_construction_year) &
        changed_zoning$construction_year !=
          changed_zoning$decision_construction_year
    )
) {
  stop("Corrected-year zoning is incomplete or inconsistent.")
}

if (
  nrow(dplyr::anti_join(
    zoning_decisions,
    changed_zoning,
    by = "project_id"
  )) > 0L
) {
  stop("A corrected-year zoning decision is not used.")
}

data <- data |>
  dplyr::mutate(
    construction_date = as.Date(sprintf("%d-06-15", construction_year)),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year)
  )

points <- sf::st_as_sf(
  data,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)

ward_panel <- sf::st_read("../input/ward_panel.gpkg", quiet = TRUE)

ward_maps <- load_canonical_ward_maps(ward_panel, eras = sort(unique(data$era)))

boundary_lines <- load_boundary_layers(
  "../input/ward_pair_boundaries.gpkg",
  eras = sort(unique(data$era))
)

assignments <- assign_points_to_boundaries(
  points,
  data$era,
  ward_maps,
  boundary_lines,
  chunk_n = 2000L
)

data <- data |>
  dplyr::mutate(
    ward = assignments$ward,
    neighbor_ward = assignments$neighbor_ward,
    ward_pair = assignments$ward_pair_id,
    distance_to_boundary_ft = assignments$dist_ft,
    within_500ft = distance_to_boundary_ft <= 500,
    within_1500ft = distance_to_boundary_ft <= 1500
  )

unchanged_geography <- data |>
  dplyr::filter(!construction_year_changed)

if (
  any(unchanged_geography$ward != unchanged_geography$prior_ward) ||
    any(
      unchanged_geography$neighbor_ward !=
        unchanged_geography$prior_neighbor_ward
    ) ||
    any(unchanged_geography$ward_pair != unchanged_geography$prior_ward_pair) ||
    max(
      abs(
        unchanged_geography$distance_to_boundary_ft -
          unchanged_geography$prior_distance_to_boundary_ft
      ),
      na.rm = TRUE
    ) > 0.01
) {
  stop("Reconstructed geography does not reproduce unchanged projects.")
}

segment_layers <- load_segment_line_layers(
  "../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(data$era))
)

data$segment_id <- assign_points_to_nearest_segments(
  points,
  data$era,
  data$ward_pair,
  segment_layers,
  max_distance = units::set_units(1500, "ft"),
  chunk_n = 2000L
)

aldermen <- readr::read_csv(
  "../input/chicago_alderman_terms.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    ward = readr::col_integer(),
    alderman = readr::col_character(),
    start_date = readr::col_date(),
    end_date = readr::col_date()
  )
)

term_overlaps <- aldermen |>
  dplyr::arrange(ward, start_date) |>
  dplyr::group_by(ward) |>
  dplyr::mutate(next_start = dplyr::lead(start_date)) |>
  dplyr::ungroup() |>
  dplyr::filter(!is.na(next_start), next_start <= end_date)

term_coverage <- aldermen |>
  dplyr::group_by(ward) |>
  dplyr::summarise(first_date = min(start_date), last_date = max(end_date))

if (
  anyNA(aldermen) || any(aldermen$start_date > aldermen$end_date) ||
    nrow(term_overlaps) > 0L ||
    !all(c(data$ward, data$neighbor_ward) %in% term_coverage$ward) ||
    any(term_coverage$first_date > min(data$construction_date)) ||
    any(term_coverage$last_date < max(data$construction_date))
) {
  stop("Alderman terms are invalid, overlap, or do not cover the construction period.")
}

controls <- readr::read_csv(
  "../input/ward_controls_2000_2023.csv",
  show_col_types = FALSE
)

scores <- readr::read_csv(
  "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) |>
  dplyr::select(alderman, score = uncertainty_index)

if (
  anyDuplicated(controls[c("ward", "year")]) ||
    anyDuplicated(scores$alderman)
) {
  stop("An alderman, control, or score input is not uniquely keyed.")
}

# Recorded exclusions concern density measurement; retain the construction rows.
denominator_decisions <- readr::read_csv(
  "../adjudication/density_denominator_decisions.csv",
  col_types = readr::cols(.default = readr::col_character())
)
stopifnot(!anyNA(denominator_decisions$project_id),
  !anyDuplicated(denominator_decisions$project_id),
  all(denominator_decisions$decision == "exclude_density"),
  all(denominator_decisions$project_id %in% data$project_id))
data <- data |>
  dplyr::mutate(
    allow_far = allow_far & !project_id %in% denominator_decisions$project_id,
    allow_dupac = allow_dupac & !project_id %in% denominator_decisions$project_id
  )

data <- data |>
  dplyr::mutate(
    density_far = dplyr::if_else(
      allow_far & building_sqft > 0 & land_sqft > 0,
      building_sqft / land_sqft,
      NA_real_
    ),
    density_dupac = dplyr::if_else(
      allow_dupac & dwelling_units > 0 & land_sqft > 0,
      43560 * dwelling_units / land_sqft,
      NA_real_
    )
  ) |>
  dplyr::left_join(
    aldermen,
    by = dplyr::join_by(
      ward, construction_date >= start_date, construction_date <= end_date
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::rename(alderman_own = alderman) |>
  dplyr::select(-start_date, -end_date) |>
  dplyr::left_join(
    aldermen |>
      dplyr::rename(alderman_neighbor = alderman),
    by = dplyr::join_by(
      neighbor_ward == ward,
      construction_date >= start_date,
      construction_date <= end_date
    ),
    relationship = "many-to-one"
  ) |>
  dplyr::select(-start_date, -end_date) |>
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
    alderman_own_assignment_status = dplyr::case_when(
      is.na(alderman_own) ~ "no_recorded_term_on_date",
      is.na(strictness_own) ~ "serving_alderman_without_score",
      TRUE ~ "assigned"
    ),
    alderman_neighbor_assignment_status = dplyr::case_when(
      is.na(alderman_neighbor) ~ "no_recorded_term_on_date",
      is.na(strictness_neighbor) ~ "serving_alderman_without_score",
      TRUE ~ "assigned"
    ),
    signed_distance_m = distance_to_boundary_ft * 0.3048 *
      dplyr::case_when(
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
      (strictness_own + strictness_neighbor) / 2
  )

data <- data |>
  dplyr::filter(within_1500ft)

required_500ft <- c(
  "segment_id",
  "zone_group",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

missing_500ft <- data |>
  dplyr::filter(within_500ft) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(required_500ft),
      ~ sum(is.na(.x) | as.character(.x) == "")
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::everything(),
    names_to = "field",
    values_to = "missing_rows"
  ) |>
  dplyr::filter(missing_rows > 0)

if (
  anyDuplicated(data$project_id) ||
    nrow(data) == 0L ||
    nrow(missing_500ft) > 0L ||
    any(data$within_500ft & (
      data$alderman_own_assignment_status == "serving_alderman_without_score" |
        data$alderman_neighbor_assignment_status == "serving_alderman_without_score"
    ))
) {
  print(missing_500ft)
  print(
    data |>
      dplyr::filter(
        within_500ft,
        is.na(alderman_own) |
          is.na(alderman_neighbor) |
          is.na(strictness_own) |
          is.na(strictness_neighbor)
      ) |>
      dplyr::select(
        project_id,
        construction_year,
        ward,
        neighbor_ward,
        ward_pair,
        alderman_own,
        alderman_neighbor
      )
  )
  stop("The final verified density input failed validation.")
}

readr::write_csv(
  data |>
    dplyr::select(
      project_id,
      source_family,
      project_kind,
      construction_year,
      construction_date,
      ward,
      neighbor_ward,
      ward_pair,
      distance_to_boundary_ft,
      within_500ft,
      within_1500ft,
      allow_far,
      allow_dupac,
      segment_id,
      dwelling_units,
      building_sqft,
      land_sqft,
      density_far,
      density_dupac,
      zone_group,
      alderman_own,
      alderman_neighbor,
      strictness_own,
      strictness_neighbor,
      signed_distance_m,
      lenient_dist,
      strict_dist,
      side,
      continuous_score_difference,
      pair_average_score,
      share_white_own,
      share_black_own,
      median_hh_income_own,
      share_bach_plus_own,
      homeownership_rate_own,
      external_multifamily,
      reviewed_project,
      final_evidence_tier,
      construction_year_changed,
      alderman_own_assignment_status,
      alderman_neighbor_assignment_status
    ) |>
    dplyr::arrange(construction_year, project_id),
  "../output/final_verified_density_input.csv",
  na = ""
)
