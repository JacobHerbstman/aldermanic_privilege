# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

projects <- readr::read_csv(
  "../output/multicard_project_evidence_base.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    geometry_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(project_id, geometry_project_id, construction_year)

construction_year_geometry <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    projects,
    by = c("project_id" = "geometry_project_id"),
    relationship = "one-to-one"
  ) |>
  dplyr::transmute(
    project_id = project_id.y,
    construction_year,
    search_geometry_source = "construction_year_parcel"
  )

adjacent_year_geometry <- sf::st_read(
  "../output/residential_unresolved_adjacent_year_parcels.gpkg",
  quiet = TRUE
) |>
  dplyr::filter(project_id %in% projects$project_id) |>
  dplyr::group_by(project_id) |>
  dplyr::summarise(geom = sf::st_union(geom), .groups = "drop") |>
  dplyr::inner_join(
    projects |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(search_geometry_source = "accepted_adjacent_year_parcel")

missing_after_parcels <- projects |>
  dplyr::anti_join(
    dplyr::bind_rows(
      sf::st_drop_geometry(construction_year_geometry),
      sf::st_drop_geometry(adjacent_year_geometry)
    ) |>
      dplyr::distinct(project_id),
    by = "project_id"
  )

buffer_geometry <- sf::st_read(
  "../output/preferred_new_construction_project_centroids.gpkg",
  quiet = TRUE
) |>
  dplyr::inner_join(
    missing_after_parcels |>
      dplyr::select(project_id, construction_year),
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  dplyr::mutate(
    geom = sf::st_buffer(geom, 150),
    search_geometry_source = "centroid_150ft_candidate_search"
  )

project_geometry <- dplyr::bind_rows(
  construction_year_geometry,
  adjacent_year_geometry,
  buffer_geometry
) |>
  dplyr::select(project_id, construction_year, search_geometry_source)

if (nrow(project_geometry) != nrow(projects) ||
    anyDuplicated(project_geometry$project_id)) {
  stop("Multicard project search geometry is not complete and unique.", call. = FALSE)
}

current_condos <- data.table::fread(
  "../input/parcel_universe_2025_city.csv",
  select = c(
    "pin", "pin10", "tax_year", "class",
    "centroid_x_crs_3435", "centroid_y_crs_3435"
  ),
  colClasses = "character"
) |>
  tibble::as_tibble() |>
  dplyr::transmute(
    pin = stringr::str_pad(
      stringr::str_replace_all(pin, "[^0-9]", ""),
      14,
      pad = "0"
    ),
    pin10 = stringr::str_pad(
      stringr::str_replace_all(pin10, "[^0-9]", ""),
      10,
      pad = "0"
    ),
    tax_year = as.integer(tax_year),
    class = stringr::str_squish(class),
    x_3435 = as.numeric(centroid_x_crs_3435),
    y_3435 = as.numeric(centroid_y_crs_3435)
  ) |>
  dplyr::filter(
    class == "299",
    is.finite(x_3435),
    is.finite(y_3435)
  )

if (anyDuplicated(current_condos$pin)) {
  stop("Current condominium PINs are not unique.", call. = FALSE)
}

current_condos_sf <- current_condos |>
  sf::st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE)

intersections <- sf::st_intersects(current_condos_sf, project_geometry)
linked_rows <- which(lengths(intersections) > 0)

links <- tibble::tibble(
  condo_row = rep(linked_rows, lengths(intersections[linked_rows])),
  project_row = unlist(intersections[linked_rows], use.names = FALSE)
) |>
  dplyr::transmute(
    project_id = project_geometry$project_id[project_row],
    construction_year = project_geometry$construction_year[project_row],
    search_geometry_source =
      project_geometry$search_geometry_source[project_row],
    pin = current_condos$pin[condo_row],
    pin10 = current_condos$pin10[condo_row],
    tax_year = current_condos$tax_year[condo_row]
  ) |>
  dplyr::distinct() |>
  dplyr::group_by(pin) |>
  dplyr::mutate(projects_per_current_condo_pin = dplyr::n_distinct(project_id)) |>
  dplyr::ungroup() |>
  dplyr::arrange(project_id, pin)

requests <- links |>
  dplyr::distinct(project_id, construction_year, pin10) |>
  dplyr::group_by(pin10) |>
  dplyr::mutate(projects_per_condo_base = dplyr::n_distinct(project_id)) |>
  dplyr::ungroup() |>
  dplyr::arrange(pin10, project_id)

summary <- tibble::tibble(
  metric = c(
    "multicard_projects",
    "projects_with_current_condo_successors",
    "current_condo_pin_links",
    "projects_using_centroid_candidate_search",
    "current_condo_pins_linked_to_multiple_projects",
    "distinct_condo_bases_requested",
    "condo_bases_linked_to_multiple_projects"
  ),
  value = c(
    nrow(projects),
    dplyr::n_distinct(links$project_id),
    nrow(links),
    sum(
      project_geometry$search_geometry_source ==
        "centroid_150ft_candidate_search"
    ),
    dplyr::n_distinct(
      links$pin[links$projects_per_current_condo_pin > 1]
    ),
    dplyr::n_distinct(requests$pin10),
    dplyr::n_distinct(
      requests$pin10[requests$projects_per_condo_base > 1]
    )
  )
)

readr::write_csv(
  links,
  "../output/multicard_successor_condo_links.csv"
)
readr::write_csv(
  requests,
  "../output/multicard_successor_condo_requests.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_successor_condo_request_summary.csv"
)
