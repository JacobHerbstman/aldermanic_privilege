# setwd("tasks/new_construction_analysis_data/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/canonical_geometry_helpers.R")
source("../../shared/code/assessor_classification.R")

# Finished buildings supply every measurement. This task only attaches regressors.
projects <- readr::read_csv("../input/preferred_new_construction_project_ledger.csv",
  col_types = readr::cols(component_pins = "c", .default = readr::col_guess()))
boundaries <- readr::read_csv("../input/preferred_new_construction_boundary_scope.csv",
  col_types = readr::cols(ward_pair = "c", .default = readr::col_guess()))
residential <- readr::read_csv("../input/residential_selected_buildings.csv",
  col_types = readr::cols(class_values = "c", component_pins = "c", .default = readr::col_guess()))
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(boundaries$project_id),
  !anyDuplicated(residential$project_id),
  setequal(projects$project_id, boundaries$project_id))
for (field in c("construction_year", "allow_far", "allow_dupac", "location_resolved")) {
  stopifnot(identical(projects[[field]], boundaries[[field]][match(projects$project_id, boundaries$project_id)]))
}
projects <- projects |>
  left_join(boundaries |> select(-source_family, -construction_year, -allow_far, -allow_dupac,
    -location_resolved), by = "project_id", relationship = "one-to-one") |>
  filter(within_1500ft) |>
  left_join(residential |> select(project_id, class_values), by = "project_id", relationship = "one-to-one") |>
  rename(density_far = far, density_dupac = dupac)
stopifnot(all(projects$construction_date == as.Date(sprintf("%d-06-15", projects$construction_year))))

# Preserve the established class-first distinction: a group of townhouses is
# not an apartment building simply because the project has several homes.
projects <- projects |> mutate(
  single_family_class = stringr::str_detect(coalesce(class_values, ""),
    paste0("(^|/)(", paste(single_family_assessor_classes, collapse = "|"), ")($|/)")),
  multifamily_class = stringr::str_detect(coalesce(class_values, ""), "(^|/)(211|212|297)($|/)"),
  external_multifamily = case_when(
    dwelling_units <= 1 ~ FALSE,
    source_family == "commercial" ~ coalesce(dwelling_units > 1, FALSE),
    multifamily_class ~ TRUE,
    single_family_class ~ FALSE,
    class_values %in% c("EX", "OA2") ~ dwelling_units > 1,
    TRUE ~ NA),
  multifamily_source = "selected_assessor_class_and_finished_home_count")

# These existing reviews concern building type only; never copy their old areas or years.
type_reviews <- readr::read_csv("../input/multicard_external_web_reviews.csv", show_col_types = FALSE) |>
  filter(classification_override %in% c("include", "exclude"), supports_building_type) |>
  transmute(project_id, reviewed_multifamily = classification_override == "include",
    classification_source = "recorded_external_building_type_review")
manual_types <- readr::read_csv("../input/project_manual_reviews.csv", show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())) |>
  filter(review_status == "retain", reviewed_multifamily %in% c("true", "false")) |>
  transmute(project_id, manual_multifamily = reviewed_multifamily == "true")
stopifnot(!anyDuplicated(type_reviews$project_id), !anyDuplicated(manual_types$project_id))
projects <- projects |>
  left_join(type_reviews, by = "project_id", relationship = "one-to-one") |>
  left_join(manual_types, by = "project_id", relationship = "one-to-one") |>
  mutate(external_multifamily = case_when(
    dwelling_units <= 1 ~ FALSE,
    !is.na(manual_multifamily) ~ manual_multifamily,
    !is.na(reviewed_multifamily) ~ reviewed_multifamily,
    TRUE ~ external_multifamily),
    multifamily_source = case_when(
      dwelling_units <= 1 ~ "finished_building_has_one_home",
      !is.na(manual_multifamily) ~ "recorded_project_building_type_review",
      !is.na(reviewed_multifamily) ~ classification_source,
      TRUE ~ multifamily_source)) |>
  select(-single_family_class, -multifamily_class, -reviewed_multifamily, -manual_multifamily,
    -classification_source)
stopifnot(!any(is.na(projects$external_multifamily) & (projects$allow_far | projects$allow_dupac)))

points <- sf::st_as_sf(projects, coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)
segments <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg",
  eras = sort(unique(projects$era)))
projects$segment_id <- assign_points_to_nearest_segments(points, projects$era, projects$ward_pair,
  segments, max_distance = units::set_units(1500, "ft"))
assert_event_segment_contract(points, projects$era, projects$ward_pair, segments,
  projects$segment_id, projects$distance_to_boundary_ft * 0.3048,
  max_distance_m = 457.2, analysis_window_m = 457.2, context = "new construction")

# June 15 is the existing within-year date proxy. Use the actual daily term table.
terms <- readr::read_csv("../input/chicago_alderman_terms.csv", show_col_types = FALSE,
  col_types = readr::cols(ward = "i", alderman = "c", start_date = "D", end_date = "D"))
term_overlap <- terms |> arrange(ward, start_date) |> group_by(ward) |>
  mutate(next_start = lead(start_date)) |> ungroup() |>
  filter(!is.na(next_start), next_start <= end_date)
stopifnot(!anyNA(terms), all(terms$start_date <= terms$end_date), nrow(term_overlap) == 0)
controls <- readr::read_csv("../input/ward_controls_2006_2022.csv", show_col_types = FALSE) |>
  select(ward, year, share_white, share_black, median_hh_income, share_bach_plus, homeownership_rate)
scores <- readr::read_csv("../input/alderman_uncertainty_index_through2022.csv", show_col_types = FALSE) |>
  select(alderman, score = uncertainty_index)
stopifnot(!anyDuplicated(controls[c("ward", "year")]), !anyDuplicated(scores$alderman))
projects <- projects |>
  left_join(terms, by = join_by(ward, construction_date >= start_date, construction_date <= end_date),
    relationship = "many-to-one") |>
  rename(alderman_own = alderman) |> select(-start_date, -end_date) |>
  left_join(terms |> rename(alderman_neighbor = alderman),
    by = join_by(neighbor_ward == ward, construction_date >= start_date, construction_date <= end_date),
    relationship = "many-to-one") |> select(-start_date, -end_date) |>
  left_join(scores |> rename(alderman_own = alderman, strictness_own = score),
    by = "alderman_own", relationship = "many-to-one") |>
  left_join(scores |> rename(alderman_neighbor = alderman, strictness_neighbor = score),
    by = "alderman_neighbor", relationship = "many-to-one") |>
  left_join(controls, by = c("ward", "construction_year" = "year"), relationship = "many-to-one") |>
  left_join(controls, by = c("neighbor_ward" = "ward", "construction_year" = "year"),
    suffix = c("_own", "_neighbor"), relationship = "many-to-one") |>
  mutate(alderman_assignment_status = case_when(
    is.na(alderman_own) | is.na(alderman_neighbor) ~ "no_recorded_term_on_proxy_date",
    !is.finite(strictness_own) | !is.finite(strictness_neighbor) ~ "serving_alderman_without_score",
    strictness_own == strictness_neighbor ~ "equal_scores",
    TRUE ~ "assigned"),
    signed_distance_m = if_else(alderman_assignment_status == "assigned",
      distance_to_boundary_ft * 0.3048 * sign(strictness_own - strictness_neighbor), NA_real_),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2)

# Missing covariates remain visible; they cannot change a building's measurements.
stopifnot(!anyDuplicated(projects$project_id), nrow(projects) == sum(boundaries$within_1500ft, na.rm = TRUE))
for (field in c("share_white_own", "share_black_own", "median_hh_income_own",
  "share_bach_plus_own", "homeownership_rate_own")) {
  stopifnot(all(is.finite(projects[[field]])))
}
stopifnot(all(!projects$allow_far | is.finite(projects$density_far)),
  all(!projects$allow_dupac | is.finite(projects$density_dupac)))
readr::write_csv(projects |> arrange(construction_year, project_id),
  "../output/construction_regressors.csv", na = "")
