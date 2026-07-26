# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

request_socrata <- function(where_clause) {
  httr2::request("https://data.cityofchicago.org/resource/syp8-uezg.geojson") %>%
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
    ) %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_timeout(seconds = 180) %>%
    httr2::req_perform()
}

projects <- sf::st_read(
  "../output/preferred_project_year_geometry.gpkg",
  quiet = TRUE
) %>%
  sf::st_transform(3435) %>%
  filter(
    source_family == "commercial",
    between(target_year, 2006L, 2022L)
  )

if (anyDuplicated(projects[c("project_id", "target_year")]) > 0) {
  stop("Commercial project geometries are not unique by project and year.", call. = FALSE)
}
if (any(!sf::st_is_valid(projects))) {
  stop("Commercial project geometries contain invalid polygons.", call. = FALSE)
}

project_centers <- sf::st_centroid(projects)

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
    target_year = projects$target_year[i],
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
    stop("A building-footprint request reached the Socrata row limit.", call. = FALSE)
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
  stop("The City building-footprint request returned no features.", call. = FALSE)
}

footprints <- do.call(rbind, downloaded) %>%
  sf::st_transform(3435) %>%
  mutate(
    bldg_id = as.character(bldg_id),
    footprint_id = if_else(
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
    harris_pin = str_replace_all(coalesce(as.character(harris_str), ""), "[^0-9]", ""),
    city_year_built = suppressWarnings(as.integer(year_built)),
    city_units = suppressWarnings(as.numeric(no_of_unit)),
    city_building_sqft = suppressWarnings(as.numeric(bldg_sq_fo)),
    city_shape_area_sqft = as.numeric(sf::st_area(geometry)),
    city_address = str_squish(paste(f_add1, t_add1, pre_dir1, st_name1, st_type1))
  ) %>%
  arrange(footprint_id) %>%
  distinct(footprint_id, .keep_all = TRUE) %>%
  select(
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

if (anyDuplicated(footprints$footprint_id) > 0) {
  stop("Downloaded City building footprints are not uniquely identified.", call. = FALSE)
}
if (any(!sf::st_is_valid(footprints))) {
  stop("Downloaded City building footprints contain invalid geometry.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "commercial_project_years_requested",
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
  "../output/commercial_city_building_footprints.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  query_index,
  "../output/commercial_city_building_footprint_requests.csv"
)
readr::write_csv(
  dplyr::bind_rows(manifest),
  "../output/commercial_city_building_footprint_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/commercial_city_building_footprint_summary.csv"
)
