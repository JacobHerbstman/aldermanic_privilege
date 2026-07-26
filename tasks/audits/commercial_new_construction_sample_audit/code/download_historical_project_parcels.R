# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

layer_ids <- c(
  `2006` = 6, `2007` = 7, `2008` = 8, `2009` = 9,
  `2010` = 10, `2011` = 11, `2012` = 12, `2013` = 14,
  `2014` = 15, `2015` = 16, `2016` = 17, `2017` = 18,
  `2018` = 20, `2019` = 21, `2020` = 22, `2021` = 23,
  `2022` = 2022
)
service_url <- "https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer"

request_arcgis <- function(url, query) {
  request <- httr2::request(url)
  request <- do.call(httr2::req_url_query, c(list(request), query))
  response <- request %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_timeout(seconds = 180) %>%
    httr2::req_perform()
  if (httr2::resp_status(response) != 200) {
    stop("Cook County parcel request failed: ", httr2::resp_status(response), call. = FALSE)
  }
  response
}

project_components <- readr::read_csv(
  "../output/new_construction_project_components.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(within_1500ft)

needed_project_parcels <- purrr::pmap_dfr(
  project_components,
  function(source_family, project_id, component_pin, candidate_year_min,
           candidate_year_max, candidate_units_min, candidate_units_max,
           candidate_building_sqft_min, candidate_building_sqft_max,
           candidate_land_sqft_min, candidate_land_sqft_max, review_category,
           within_1500ft, pin10) {
    if (!is.finite(candidate_year_min) || !is.finite(candidate_year_max)) {
      return(tibble::tibble())
    }
    first_year <- max(2006L, as.integer(candidate_year_min), na.rm = TRUE)
    last_year <- min(2022L, as.integer(candidate_year_max), na.rm = TRUE)
    if (!is.finite(first_year) || !is.finite(last_year) || first_year > last_year) {
      return(tibble::tibble())
    }
    tibble::tibble(
      source_family,
      project_id,
      component_pin,
      pin10,
      target_year = seq.int(first_year, last_year)
    )
  }
) %>%
  distinct(source_family, project_id, component_pin, pin10, target_year) %>%
  arrange(target_year, pin10, source_family, project_id, component_pin)

if (nrow(needed_project_parcels) == 0) {
  stop("No project-year parcels fall in the 2006-2022 audit universe.", call. = FALSE)
}
if (anyDuplicated(needed_project_parcels[c(
  "source_family", "project_id", "component_pin", "target_year"
)]) > 0) {
  stop("Requested project-year parcel keys are not unique.", call. = FALSE)
}

annual_parcels <- list()
layer_manifest <- list()

for (target_year in as.integer(names(layer_ids))) {
  year_pin10 <- needed_project_parcels %>%
    filter(target_year == .env$target_year) %>%
    distinct(pin10) %>%
    pull(pin10)

  if (length(year_pin10) == 0) {
    next
  }

  layer_id <- unname(layer_ids[as.character(target_year)])
  layer_url <- paste0(service_url, "/", layer_id)
  metadata_response <- request_arcgis(layer_url, list(f = "json"))
  metadata <- jsonlite::fromJSON(httr2::resp_body_string(metadata_response))
  if (!is.null(metadata$error)) {
    stop("Layer metadata error for ", target_year, ": ", metadata$error$message, call. = FALSE)
  }
  object_id_field <- metadata$fields$name[
    metadata$fields$type == "esriFieldTypeOID"
  ][1]

  object_ids <- integer()
  for (pin_chunk in split(year_pin10, ceiling(seq_along(year_pin10) / 75))) {
    where <- paste0("PIN10 IN ('", paste(pin_chunk, collapse = "','"), "')")
    id_response <- request_arcgis(
      paste0(layer_url, "/query"),
      list(
        where = where,
        returnIdsOnly = "true",
        returnGeometry = "false",
        f = "json"
      )
    )
    id_result <- jsonlite::fromJSON(httr2::resp_body_string(id_response))
    if (!is.null(id_result$error)) {
      stop("Object-ID query error for ", target_year, ": ", id_result$error$message, call. = FALSE)
    }
    object_ids <- c(object_ids, as.integer(id_result$objectIds))
  }
  object_ids <- sort(unique(object_ids[is.finite(object_ids)]))

  year_parcels <- list()
  if (length(object_ids) > 0) {
    for (id_chunk in split(object_ids, ceiling(seq_along(object_ids) / 300))) {
      geometry_response <- request_arcgis(
        paste0(layer_url, "/query"),
        list(
          objectIds = paste(id_chunk, collapse = ","),
          outFields = "*",
          returnGeometry = "true",
          outSR = "3435",
          f = "geojson"
        )
      )
      body <- httr2::resp_body_string(geometry_response)
      if (str_detect(body, '"error"\\s*:')) {
        error_body <- jsonlite::fromJSON(body)
        stop("Geometry query error for ", target_year, ": ", error_body$error$message, call. = FALSE)
      }
      temporary_geojson <- tempfile(fileext = ".geojson")
      writeLines(body, temporary_geojson, useBytes = TRUE)
      chunk_parcels <- sf::st_read(temporary_geojson, quiet = TRUE)
      unlink(temporary_geojson)
      year_parcels[[length(year_parcels) + 1]] <- chunk_parcels
    }
  }

  if (length(year_parcels) > 0) {
    year_parcels <- do.call(rbind, year_parcels) %>%
      sf::st_transform(3435)
    pin14_field <- intersect(c("PIN14", "Name", "NAME"), names(year_parcels))[1]
    pin10_field <- intersect(c("PIN10", "Pin10", "pin10"), names(year_parcels))[1]
    if (is.na(pin14_field) || is.na(pin10_field) || is.na(object_id_field)) {
      stop("Expected parcel fields are missing for layer year ", target_year, call. = FALSE)
    }
    year_parcels <- year_parcels %>%
      transmute(
        target_year = target_year,
        layer_id = layer_id,
        object_id = as.integer(.data[[object_id_field]]),
        pin14 = str_replace_all(as.character(.data[[pin14_field]]), "[^0-9]", ""),
        pin10 = str_replace_all(as.character(.data[[pin10_field]]), "[^0-9]", ""),
        geometry_valid = sf::st_is_valid(geometry),
        geometry = geometry
      )
    if (anyDuplicated(year_parcels$object_id) > 0) {
      stop("Duplicate parcel object IDs returned for layer year ", target_year, call. = FALSE)
    }
    if (!all(year_parcels$pin10 %in% year_pin10)) {
      stop("Parcel service returned an unrequested PIN10 for year ", target_year, call. = FALSE)
    }
    annual_parcels[[as.character(target_year)]] <- year_parcels
  }

  layer_manifest[[as.character(target_year)]] <- tibble::tibble(
    target_year,
    layer_id,
    layer_name = metadata$name,
    service_spatial_reference = metadata$extent$spatialReference$latestWkid,
    object_id_field = object_id_field,
    requested_pin10 = length(year_pin10),
    returned_object_ids = length(object_ids),
    service_url = layer_url,
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

historical_parcels <- do.call(rbind, annual_parcels) %>%
  arrange(target_year, pin10, pin14, object_id)

parcel_counts <- historical_parcels %>%
  sf::st_drop_geometry() %>%
  count(target_year, pin10, name = "polygon_count_pin10")

exact_pin14 <- historical_parcels %>%
  sf::st_drop_geometry() %>%
  distinct(target_year, pin10, pin14) %>%
  mutate(exact_pin14_available = TRUE)

coverage <- needed_project_parcels %>%
  left_join(
    parcel_counts,
    by = c("target_year", "pin10"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    exact_pin14,
    by = c("target_year", "pin10", "component_pin" = "pin14"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    polygon_count_pin10 = coalesce(polygon_count_pin10, 0L),
    exact_pin14_available = coalesce(exact_pin14_available, FALSE),
    coverage_status = case_when(
      exact_pin14_available ~ "exact_pin14",
      polygon_count_pin10 == 1 ~ "unique_pin10_only",
      polygon_count_pin10 > 1 ~ "ambiguous_pin10",
      TRUE ~ "missing"
    )
  ) %>%
  arrange(target_year, source_family, project_id, component_pin)

summary <- bind_rows(
  coverage %>%
    count(coverage_status, name = "value") %>%
    transmute(metric = paste0("project_component_year_", coverage_status), value),
  tibble::tibble(metric = "requested_project_component_years", value = nrow(coverage)),
  tibble::tibble(metric = "returned_annual_parcel_polygons", value = nrow(historical_parcels)),
  tibble::tibble(metric = "invalid_returned_geometries", value = sum(!historical_parcels$geometry_valid))
)

sf::st_write(
  historical_parcels,
  "../output/historical_project_parcels.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(coverage, "../output/historical_project_parcel_coverage.csv")
readr::write_csv(bind_rows(layer_manifest), "../output/historical_parcel_layer_manifest.csv")
readr::write_csv(summary, "../output/historical_project_parcel_summary.csv")
