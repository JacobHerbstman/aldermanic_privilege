# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

projects <- sf::st_read(
  "../output/multicard_project_query_geometries.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::select(project_id, construction_year, query_geometry_source)

current_footprints <- sf::st_read(
  "../output/multicard_city_building_footprints.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435)

historical_footprints <- sf::st_read(
  "../output/cook_building_footprints_2006_2008.gpkg",
  layer = "cook_building_footprints_2006_2008",
  quiet = TRUE
) |>
  sf::st_transform(3435)

current_intersections <- sf::st_intersects(projects, current_footprints)
current_links <- purrr::map2_dfr(
  seq_len(nrow(projects)),
  current_intersections,
  function(project_row, footprint_rows) {
    if (length(footprint_rows) == 0) return(tibble::tibble())
    project <- projects[project_row, ]
    candidates <- current_footprints[footprint_rows, ]
    centers <- suppressWarnings(sf::st_point_on_surface(candidates))
    center_inside <- lengths(sf::st_within(centers, sf::st_geometry(project))) > 0
    intersections <- suppressWarnings(sf::st_intersection(
      candidates,
      sf::st_geometry(project)
    ))
    if (nrow(intersections) == 0) return(tibble::tibble())
    sf::st_drop_geometry(intersections) |>
      dplyr::left_join(
        tibble::tibble(
          footprint_id = candidates$footprint_id,
          footprint_center_inside = center_inside
        ),
        by = "footprint_id",
        relationship = "many-to-one"
      ) |>
      dplyr::transmute(
        project_id = project$project_id,
        construction_year = project$construction_year,
        query_geometry_source = project$query_geometry_source,
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
        footprint_center_inside
      )
  }
) |>
  dplyr::filter(
    footprint_overlap_share >= 0.5 |
      footprint_center_inside |
      query_geometry_source == "100ft_reference_point_buffer"
  ) |>
  dplyr::distinct(project_id, footprint_id, .keep_all = TRUE) |>
  dplyr::arrange(project_id, dplyr::desc(overlap_area_sqft), footprint_id)

historical_projects <- projects |>
  dplyr::filter(construction_year <= 2010L)
historical_intersections <- sf::st_intersects(historical_projects, historical_footprints)
historical_links <- purrr::map2_dfr(
  seq_len(nrow(historical_projects)),
  historical_intersections,
  function(project_row, footprint_rows) {
    if (length(footprint_rows) == 0) return(tibble::tibble())
    project <- historical_projects[project_row, ]
    candidates <- historical_footprints[footprint_rows, ]
    centers <- suppressWarnings(sf::st_point_on_surface(candidates))
    center_inside <- lengths(sf::st_within(centers, sf::st_geometry(project))) > 0
    intersections <- suppressWarnings(sf::st_intersection(
      candidates,
      sf::st_geometry(project)
    ))
    if (nrow(intersections) == 0) return(tibble::tibble())
    sf::st_drop_geometry(intersections) |>
      dplyr::left_join(
        tibble::tibble(
          footprint_2008_id = candidates$footprint_2008_id,
          footprint_center_inside = center_inside
        ),
        by = "footprint_2008_id",
        relationship = "many-to-one"
      ) |>
      dplyr::transmute(
        project_id = project$project_id,
        construction_year = project$construction_year,
        query_geometry_source = project$query_geometry_source,
        footprint_2008_id,
        address = stringr::str_squish(
          paste(address_from, address_to, street_direction, street_name, street_type)
        ),
        harris_pin,
        footprint_year_built = year_built,
        footprint_units = units,
        footprint_stories = stories,
        footprint_building_sqft = building_sqft,
        footprint_area_sqft = geometry_area_sqft,
        overlap_area_sqft = as.numeric(sf::st_area(intersections)),
        footprint_overlap_share = overlap_area_sqft / footprint_area_sqft,
        footprint_center_inside
      )
  }
) |>
  dplyr::filter(
    footprint_overlap_share >= 0.5 |
      footprint_center_inside |
      query_geometry_source == "100ft_reference_point_buffer"
  ) |>
  dplyr::distinct(project_id, footprint_2008_id, .keep_all = TRUE) |>
  dplyr::arrange(project_id, dplyr::desc(overlap_area_sqft), footprint_2008_id)

current_summary <- current_links |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    current_footprints = dplyr::n_distinct(footprint_id),
    current_footprints_near_construction = sum(
      is.finite(city_year_built) &
        abs(city_year_built - construction_year) <= 2
    ),
    current_footprints_with_exact_pin = sum(
      harris_pin == stringr::str_remove(project_id, "^residential_multicard_")
    ),
    current_city_year_values = paste(
      sort(unique(city_year_built[is.finite(city_year_built)])),
      collapse = "/"
    ),
    current_city_unit_values = paste(
      sort(unique(city_units[is.finite(city_units) & city_units > 0])),
      collapse = "/"
    ),
    current_city_units_sum = sum(
      city_units[is.finite(city_units) & city_units > 0],
      na.rm = TRUE
    ),
    current_city_building_sqft_sum = sum(
      city_building_sqft[
        is.finite(city_building_sqft) & city_building_sqft > 0
      ],
      na.rm = TRUE
    ),
    current_footprint_area_sum = sum(footprint_area_sqft, na.rm = TRUE),
    current_footprint_evidence = paste0(
      footprint_id,
      " address=", city_address,
      "; year=", dplyr::coalesce(as.character(city_year_built), "missing"),
      "; units=", dplyr::coalesce(as.character(city_units), "missing"),
      "; building_sqft=",
      dplyr::coalesce(as.character(city_building_sqft), "missing"),
      "; overlap=", round(footprint_overlap_share, 3),
      collapse = " || "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    current_city_units_sum = dplyr::if_else(
      current_city_units_sum > 0,
      current_city_units_sum,
      NA_real_
    ),
    current_city_building_sqft_sum = dplyr::if_else(
      current_city_building_sqft_sum > 0,
      current_city_building_sqft_sum,
      NA_real_
    )
  )

historical_summary <- historical_links |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(
    historical_2008_footprints = dplyr::n_distinct(footprint_2008_id),
    historical_2008_footprints_near_construction = sum(
      is.finite(footprint_year_built) &
        abs(footprint_year_built - construction_year) <= 2
    ),
    historical_2008_year_values = paste(
      sort(unique(footprint_year_built[is.finite(footprint_year_built)])),
      collapse = "/"
    ),
    historical_2008_footprint_area_sum = sum(footprint_area_sqft, na.rm = TRUE),
    historical_2008_evidence = paste0(
      footprint_2008_id,
      " address=", address,
      "; year=", dplyr::coalesce(as.character(footprint_year_built), "missing"),
      "; overlap=", round(footprint_overlap_share, 3),
      collapse = " || "
    ),
    .groups = "drop"
  )

evidence <- projects |>
  sf::st_drop_geometry() |>
  dplyr::left_join(current_summary, by = "project_id", relationship = "one-to-one") |>
  dplyr::left_join(historical_summary, by = "project_id", relationship = "one-to-one") |>
  dplyr::mutate(
    current_footprints = dplyr::coalesce(current_footprints, 0L),
    current_footprints_near_construction =
      dplyr::coalesce(current_footprints_near_construction, 0L),
    historical_2008_footprints =
      dplyr::coalesce(historical_2008_footprints, 0L),
    historical_2008_footprints_near_construction =
      dplyr::coalesce(historical_2008_footprints_near_construction, 0L)
  ) |>
  dplyr::arrange(project_id)

if (
  nrow(evidence) != nrow(projects) ||
    anyDuplicated(evidence$project_id) ||
    anyDuplicated(current_links[c("project_id", "footprint_id")]) ||
    anyDuplicated(historical_links[c("project_id", "footprint_2008_id")])
) {
  stop("Multicard footprint evidence fails completeness or uniqueness checks.", call. = FALSE)
}

summary <- dplyr::bind_rows(
  tibble::tibble(metric = "multicard_projects", value = nrow(evidence)),
  tibble::tibble(
    metric = "projects_with_current_footprint",
    value = sum(evidence$current_footprints > 0)
  ),
  tibble::tibble(
    metric = "projects_with_current_footprint_near_construction",
    value = sum(evidence$current_footprints_near_construction > 0)
  ),
  tibble::tibble(
    metric = "early_projects_with_2008_footprint",
    value = sum(evidence$historical_2008_footprints > 0)
  ),
  tibble::tibble(
    metric = "early_projects_with_2008_footprint_near_construction",
    value = sum(evidence$historical_2008_footprints_near_construction > 0)
  )
)

readr::write_csv(current_links, "../output/multicard_current_footprint_links.csv")
readr::write_csv(historical_links, "../output/multicard_2008_footprint_links.csv")
readr::write_csv(evidence, "../output/multicard_footprint_evidence.csv")
readr::write_csv(summary, "../output/multicard_footprint_evidence_summary.csv")
