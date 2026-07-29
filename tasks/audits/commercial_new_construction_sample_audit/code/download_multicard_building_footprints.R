# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

request_socrata <- function(where_clause) {
  httr2::request("https://data.cityofchicago.org/resource/syp8-uezg.geojson") |>
    httr2::req_url_query(
      `$select` = paste(
        "bldg_id,orig_bldg_,bldg_statu,f_add1,t_add1,pre_dir1,",
        "st_name1,st_type1,bldg_name1,comments,footprint_,",
        "bldg_creat,bldg_activ,harris_str,no_of_unit,no_stories,",
        "year_built,bldg_sq_fo,shape_area,the_geom",
        sep = ""
      ),
      `$where` = where_clause,
      `$limit` = 50000
    ) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_perform()
}

scope <- readr::read_csv(
  "../output/multicard_project_evidence_base.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    geometry_project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) |>
  dplyr::select(project_id, geometry_project_id, construction_year)

project_geometries <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) |>
  sf::st_transform(3435) |>
  dplyr::select(geometry_project_id = project_id)

projects_with_polygons <- scope |>
  dplyr::inner_join(
    project_geometries,
    by = "geometry_project_id",
    relationship = "one-to-one"
  ) |>
  sf::st_as_sf() |>
  dplyr::mutate(query_geometry_source = "construction_year_parcel_polygon")
sf::st_geometry(projects_with_polygons) <- "geometry"

fallback_coordinates <- readr::read_csv(
  "../output/final_new_construction_boundary_scope.csv",
  show_col_types = FALSE,
  col_types = readr::cols(project_id = readr::col_character(), .default = readr::col_guess())
) |>
  dplyr::select(project_id, x_3435, y_3435)

projects_with_buffers <- scope |>
  dplyr::anti_join(
    projects_with_polygons |>
      sf::st_drop_geometry() |>
      dplyr::select(project_id),
    by = "project_id"
  ) |>
  dplyr::left_join(
    fallback_coordinates,
    by = "project_id",
    relationship = "one-to-one"
  ) |>
  sf::st_as_sf(coords = c("x_3435", "y_3435"), crs = 3435, remove = FALSE) |>
  sf::st_buffer(100) |>
  dplyr::mutate(query_geometry_source = "100ft_reference_point_buffer")
sf::st_geometry(projects_with_buffers) <- "geometry"

projects <- dplyr::bind_rows(projects_with_polygons, projects_with_buffers) |>
  dplyr::arrange(project_id)

if (
  nrow(projects) != nrow(scope) ||
    anyDuplicated(projects$project_id) ||
    any(sf::st_is_empty(projects)) ||
    any(!sf::st_is_valid(projects))
) {
  stop("Multicard project geometries are incomplete or invalid.", call. = FALSE)
}

project_centers <- suppressWarnings(sf::st_point_on_surface(projects))
query_index <- purrr::map_dfr(seq_len(nrow(projects)), function(i) {
  project_bbox <- sf::st_bbox(projects[i, ])
  center_4326 <- sf::st_transform(project_centers[i, ], 4326)
  coordinates <- sf::st_coordinates(center_4326)
  radius_ft <- sqrt(
    (project_bbox[["xmax"]] - project_bbox[["xmin"]])^2 +
      (project_bbox[["ymax"]] - project_bbox[["ymin"]])^2
  ) / 2 + 200
  tibble::tibble(
    project_id = projects$project_id[i],
    construction_year = projects$construction_year[i],
    query_geometry_source = projects$query_geometry_source[i],
    longitude = coordinates[1, 1],
    latitude = coordinates[1, 2],
    query_radius_m = max(radius_ft * 0.3048, 75)
  )
})

query_batches <- split(seq_len(nrow(query_index)), ceiling(seq_len(nrow(query_index)) / 10))
downloaded <- list()
manifest <- list()

for (batch_number in seq_along(query_batches)) {
  rows <- query_batches[[batch_number]]
  where_clause <- paste(
    sprintf(
      "within_circle(the_geom,%.7f,%.7f,%.2f)",
      query_index$latitude[rows],
      query_index$longitude[rows],
      query_index$query_radius_m[rows]
    ),
    collapse = " OR "
  )
  response <- request_socrata(where_clause)
  body <- httr2::resp_body_string(response)
  result <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  feature_count <- length(result$features)
  if (feature_count >= 50000) {
    stop("A multicard footprint request reached the Socrata row limit.", call. = FALSE)
  }
  if (feature_count > 0) {
    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    downloaded[[batch_number]] <- sf::st_read(temporary_geojson, quiet = TRUE)
    unlink(temporary_geojson)
  }
  manifest[[batch_number]] <- tibble::tibble(
    batch_number,
    first_project_id = query_index$project_id[min(rows)],
    last_project_id = query_index$project_id[max(rows)],
    requested_projects = length(rows),
    returned_features = feature_count,
    response_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

if (length(downloaded) == 0) {
  stop("The multicard footprint request returned no features.", call. = FALSE)
}

footprints <- do.call(rbind, downloaded) |>
  sf::st_transform(3435) |>
  dplyr::mutate(
    bldg_id = as.character(bldg_id),
    footprint_id = dplyr::if_else(
      !is.na(bldg_id) & bldg_id != "",
      paste0("city_building_", bldg_id),
      paste0(
        "city_geometry_",
        vapply(
          sf::st_as_binary(geometry),
          digest::digest,
          character(1),
          algo = "sha256",
          serialize = FALSE
        )
      )
    ),
    harris_pin = stringr::str_replace_all(
      dplyr::coalesce(as.character(harris_str), ""),
      "[^0-9]",
      ""
    ),
    city_year_built = suppressWarnings(as.integer(year_built)),
    city_units = suppressWarnings(as.numeric(no_of_unit)),
    city_building_sqft = suppressWarnings(as.numeric(bldg_sq_fo)),
    city_shape_area_sqft = as.numeric(sf::st_area(geometry)),
    city_address = stringr::str_squish(
      paste(f_add1, t_add1, pre_dir1, st_name1, st_type1)
    )
  ) |>
  dplyr::arrange(footprint_id) |>
  dplyr::distinct(footprint_id, .keep_all = TRUE) |>
  dplyr::select(
    footprint_id,
    bldg_id,
    orig_bldg_,
    bldg_statu,
    city_address,
    bldg_name1,
    comments,
    footprint_,
    bldg_creat,
    bldg_activ,
    harris_pin,
    city_year_built,
    city_units,
    no_stories,
    city_building_sqft,
    city_shape_area_sqft
  )
sf::st_geometry(footprints) <- "geometry"

if (anyDuplicated(footprints$footprint_id) || any(!sf::st_is_valid(footprints))) {
  stop("Downloaded multicard footprints fail uniqueness or geometry checks.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "multicard_projects_requested",
    "request_batches",
    "unique_building_footprints",
    "footprints_with_positive_year_built"
  ),
  value = c(
    nrow(query_index),
    length(query_batches),
    nrow(footprints),
    sum(is.finite(footprints$city_year_built) & footprints$city_year_built > 0)
  )
)

sf::st_write(
  footprints,
  "../output/multicard_city_building_footprints.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
sf::st_write(
  projects,
  "../output/multicard_project_query_geometries.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(query_index, "../output/multicard_city_footprint_requests.csv")
readr::write_csv(dplyr::bind_rows(manifest), "../output/multicard_city_footprint_manifest.csv")
readr::write_csv(summary, "../output/multicard_city_footprint_summary.csv")
