# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

base_url <- paste0(
  "https://gis.cookcountyil.gov/traditional/rest/services/",
  "buildingFootprint_2008/MapServer/1/query"
)
where_clause <- "YEAR_BUILT BETWEEN 2006 AND 2008"
page_size <- 2000L

count_response <- httr2::request(base_url) |>
  httr2::req_url_query(
    where = where_clause,
    returnCountOnly = "true",
    f = "json"
  ) |>
  httr2::req_retry(max_tries = 5) |>
  httr2::req_timeout(seconds = 180) |>
  httr2::req_perform()

expected_features <- httr2::resp_body_json(count_response)$count
if (!is.numeric(expected_features) || expected_features <= 0) {
  stop("The official 2008 footprint query returned no features.", call. = FALSE)
}

offsets <- seq.int(0L, expected_features - 1L, by = page_size)
pages <- vector("list", length(offsets))
manifest <- vector("list", length(offsets))

for (i in seq_along(offsets)) {
  response <- httr2::request(base_url) |>
    httr2::req_url_query(
      where = where_clause,
      outFields = paste(c(
        "OBJECTID", "BLDG_ID", "BLDG_STATU", "F_ADD1", "T_ADD1",
        "PRE_DIR1", "ST_NAME1", "ST_TYPE1", "HARRIS_STR",
        "NO_OF_UNIT", "NO_STORIES", "YEAR_BUILT", "BLDG_SQ_FO",
        "Shape_Area"
      ), collapse = ","),
      returnGeometry = "true",
      outSR = "3435",
      orderByFields = "OBJECTID",
      resultOffset = offsets[i],
      resultRecordCount = page_size,
      f = "geojson"
    ) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_perform()

  body <- httr2::resp_body_string(response)
  temporary_geojson <- tempfile(fileext = ".geojson")
  writeLines(body, temporary_geojson, useBytes = TRUE)
  pages[[i]] <- sf::st_read(temporary_geojson, quiet = TRUE)
  unlink(temporary_geojson)

  manifest[[i]] <- tibble::tibble(
    page = i,
    result_offset = offsets[i],
    returned_features = nrow(pages[[i]]),
    response_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

footprints <- do.call(rbind, pages) |>
  sf::st_transform(3435) |>
  dplyr::transmute(
    footprint_2008_id = paste0("cook_2008_", OBJECTID),
    object_id = as.integer(OBJECTID),
    building_id = as.character(BLDG_ID),
    building_status = as.character(BLDG_STATU),
    address_from = suppressWarnings(as.integer(F_ADD1)),
    address_to = suppressWarnings(as.integer(T_ADD1)),
    street_direction = as.character(PRE_DIR1),
    street_name = as.character(ST_NAME1),
    street_type = as.character(ST_TYPE1),
    harris_pin = stringr::str_replace_all(
      dplyr::coalesce(as.character(HARRIS_STR), ""),
      "[^0-9]",
      ""
    ),
    units = suppressWarnings(as.numeric(NO_OF_UNIT)),
    stories = suppressWarnings(as.numeric(NO_STORIES)),
    year_built = suppressWarnings(as.integer(YEAR_BUILT)),
    building_sqft = suppressWarnings(as.numeric(BLDG_SQ_FO)),
    source_shape_area_sqft = suppressWarnings(as.numeric(Shape_Area)),
    geometry_area_sqft = as.numeric(sf::st_area(geometry))
  ) |>
  dplyr::arrange(object_id)

invalid_geometry_before_repair <- sum(!sf::st_is_valid(footprints))
if (invalid_geometry_before_repair > 0) {
  sf::st_geometry(footprints) <- sf::st_make_valid(sf::st_geometry(footprints))
}

validation_failures <- c(
  feature_count = nrow(footprints) != expected_features,
  duplicate_ids = anyDuplicated(footprints$footprint_2008_id) > 0,
  invalid_year = any(!dplyr::between(footprints$year_built, 2006L, 2008L)),
  invalid_geometry = any(!sf::st_is_valid(footprints))
)
if (any(validation_failures)) {
  stop(
    "The official 2008 footprint extract failed validation: ",
    paste(names(validation_failures)[validation_failures], collapse = ", "),
    call. = FALSE
  )
}

manifest <- dplyr::bind_rows(manifest)
if (sum(manifest$returned_features) != expected_features) {
  stop("The official 2008 footprint page manifest does not reconcile.", call. = FALSE)
}

summary <- tibble::tibble(
  metric = c(
    "official_query_features",
    "downloaded_features",
    "features_with_harris_pin",
    "features_with_positive_units",
    "features_with_positive_building_sqft",
    "invalid_geometries_repaired",
    "download_pages"
  ),
  value = c(
    expected_features,
    nrow(footprints),
    sum(stringr::str_length(footprints$harris_pin) == 14L),
    sum(is.finite(footprints$units) & footprints$units > 0),
    sum(is.finite(footprints$building_sqft) & footprints$building_sqft > 0),
    invalid_geometry_before_repair,
    nrow(manifest)
  )
)

sf::st_write(
  footprints,
  "../output/cook_building_footprints_2006_2008.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(
  manifest,
  "../output/cook_building_footprints_2006_2008_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/cook_building_footprints_2006_2008_summary.csv"
)
