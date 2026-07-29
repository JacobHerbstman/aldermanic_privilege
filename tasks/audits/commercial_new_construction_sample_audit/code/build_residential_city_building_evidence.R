# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

review_scope <- readr::read_csv(
  "../output/preferred_adjudication_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) %>%
  filter(source_family == "residential", review_scope == "review_within_1500ft") %>%
  select(project_id)

projects <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(source_family == "residential") %>%
  inner_join(review_scope, by = "project_id", relationship = "one-to-one")

footprints <- sf::st_read(
  "../output/residential_review_city_building_footprints.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435)

intersection_index <- sf::st_intersects(projects, footprints)
footprint_links <- purrr::map2_dfr(
  seq_len(nrow(projects)),
  intersection_index,
  function(project_row, footprint_rows) {
    if (length(footprint_rows) == 0) return(tibble::tibble())
    project <- projects[project_row, ]
    candidates <- footprints[footprint_rows, ]
    footprint_centers <- suppressWarnings(sf::st_point_on_surface(candidates))
    center_within_project <- lengths(sf::st_within(
      footprint_centers,
      sf::st_geometry(project)
    )) > 0
    intersections <- suppressWarnings(sf::st_intersection(
      candidates,
      sf::st_geometry(project)
    ))
    if (nrow(intersections) == 0) return(tibble::tibble())
    sf::st_drop_geometry(intersections) %>%
      left_join(
        tibble::tibble(
          footprint_id = candidates$footprint_id,
          footprint_center_within_project = center_within_project
        ),
        by = "footprint_id",
        relationship = "many-to-one"
      ) %>%
      transmute(
        project_id = project$project_id,
        target_year = project$target_year,
        footprint_id,
        city_address,
        harris_pin,
        city_year_built,
        city_units,
        no_stories,
        city_building_sqft,
        footprint_area_sqft = city_shape_area_sqft,
        overlap_area_sqft = as.numeric(sf::st_area(intersections)),
        footprint_overlap_share = overlap_area_sqft / footprint_area_sqft,
        footprint_center_within_project
      )
  }
) %>%
  filter(footprint_overlap_share >= 0.5 | footprint_center_within_project) %>%
  distinct(project_id, footprint_id, .keep_all = TRUE) %>%
  arrange(project_id, desc(overlap_area_sqft), footprint_id)

if (anyDuplicated(footprint_links[c("project_id", "footprint_id")]) > 0) {
  stop("Residential City footprint links are not unique by project and footprint.", call. = FALSE)
}

evidence <- footprint_links %>%
  group_by(project_id, target_year) %>%
  summarise(
    matched_city_footprints = n_distinct(footprint_id),
    city_footprint_ids = paste(footprint_id, collapse = "/"),
    city_footprint_addresses = paste(city_address, collapse = " / "),
    city_harris_pins = paste(sort(unique(harris_pin[harris_pin != ""])), collapse = "/"),
    city_year_built_values = paste(
      sort(unique(city_year_built[is.finite(city_year_built) & city_year_built > 0])),
      collapse = "/"
    ),
    city_unit_values = paste(
      sort(unique(city_units[is.finite(city_units) & city_units > 0])),
      collapse = "/"
    ),
    city_building_sqft_values = paste(
      sort(unique(city_building_sqft[
        is.finite(city_building_sqft) & city_building_sqft > 0
      ])),
      collapse = "/"
    ),
    city_building_sqft_sum = sum(
      city_building_sqft[is.finite(city_building_sqft) & city_building_sqft > 0],
      na.rm = TRUE
    ),
    city_footprint_area_sqft = sum(footprint_area_sqft),
    city_near_target_footprints = sum(
      is.finite(city_year_built) & abs(city_year_built - target_year) <= 2
    ),
    city_old_footprints = sum(
      is.finite(city_year_built) & city_year_built > 0 & city_year_built < target_year - 5
    ),
    city_footprint_evidence = paste0(
      footprint_id,
      " address=", city_address,
      "; year=", coalesce(as.character(city_year_built), "missing"),
      "; units=", coalesce(as.character(city_units), "missing"),
      "; building_sqft=", coalesce(as.character(city_building_sqft), "missing"),
      "; overlap_share=", round(footprint_overlap_share, 3),
      "; center_inside=", footprint_center_within_project,
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    city_building_sqft_sum = if_else(city_building_sqft_sum > 0, city_building_sqft_sum, NA_real_)
  )

evidence <- review_scope %>%
  left_join(evidence, by = "project_id", relationship = "one-to-one") %>%
  mutate(matched_city_footprints = coalesce(matched_city_footprints, 0L))

if (anyDuplicated(evidence$project_id) > 0 || nrow(evidence) != nrow(review_scope)) {
  stop("Residential City evidence is not one row per scoped project.", call. = FALSE)
}

summary <- evidence %>%
  count(matched_city_footprints, name = "value") %>%
  transmute(metric = paste0("matched_footprints:", matched_city_footprints), value)

readr::write_csv(footprint_links, "../output/residential_review_city_footprint_links.csv")
readr::write_csv(evidence, "../output/residential_review_city_building_evidence.csv")
readr::write_csv(summary, "../output/residential_review_city_building_summary.csv")
