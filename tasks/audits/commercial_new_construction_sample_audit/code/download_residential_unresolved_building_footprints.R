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

requests <- readr::read_csv(
  "../output/residential_unresolved_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    request_id = readr::col_character(),
    project_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(geocode_status == "accepted_reference_point") %>%
  distinct(
    request_id,
    project_id,
    address,
    address_normalized,
    query_house_number,
    matched_address,
    x_3435,
    y_3435
  ) %>%
  mutate(
    request_street = address_normalized %>%
      str_remove("^[0-9-]+\\s+") %>%
      str_replace_all("\\bAVENUE\\b", "AVE") %>%
      str_replace_all("\\bSTREET\\b", "ST") %>%
      str_replace_all("\\bTERRACE\\b", "TER") %>%
      str_replace_all("\\bCOURT\\b", "CT") %>%
      str_replace_all("\\bPLACE\\b", "PL") %>%
      str_replace_all("\\bROAD\\b", "RD") %>%
      str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
      str_replace_all("\\bPARKWAY\\b", "PKWY") %>%
      str_squish()
  )

request_points <- sf::st_as_sf(
  requests,
  coords = c("x_3435", "y_3435"),
  crs = 3435,
  remove = FALSE
)
request_coordinates <- request_points %>%
  sf::st_transform(4326) %>%
  sf::st_coordinates()
requests$longitude <- request_coordinates[, 1]
requests$latitude <- request_coordinates[, 2]

query_batches <- split(seq_len(nrow(requests)), ceiling(seq_len(nrow(requests)) / 10))
downloaded <- list()
manifest <- list()

for (batch_number in seq_along(query_batches)) {
  rows <- query_batches[[batch_number]]
  where_clause <- paste(
    sprintf(
      "within_circle(the_geom,%.7f,%.7f,75)",
      requests$latitude[rows],
      requests$longitude[rows]
    ),
    collapse = " OR "
  )
  response <- request_socrata(where_clause)
  body <- httr2::resp_body_string(response)
  result <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  feature_count <- length(result$features)
  if (feature_count >= 50000) {
    stop("An unresolved residential footprint request reached the Socrata row limit.", call. = FALSE)
  }
  if (feature_count > 0) {
    temporary_geojson <- tempfile(fileext = ".geojson")
    writeLines(body, temporary_geojson, useBytes = TRUE)
    downloaded[[batch_number]] <- sf::st_read(temporary_geojson, quiet = TRUE)
    unlink(temporary_geojson)
  }
  manifest[[batch_number]] <- tibble::tibble(
    batch_number,
    first_request_id = requests$request_id[min(rows)],
    last_request_id = requests$request_id[max(rows)],
    requested_points = length(rows),
    returned_features = feature_count,
    response_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    retrieved_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
}

if (length(downloaded) == 0) {
  stop("The unresolved residential footprint request returned no features.", call. = FALSE)
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
    city_address = str_squish(paste(f_add1, t_add1, pre_dir1, st_name1, st_type1)),
    city_from_address = suppressWarnings(as.integer(f_add1)),
    city_to_address = suppressWarnings(as.integer(t_add1)),
    city_street = str_squish(paste(pre_dir1, st_name1, st_type1)) %>%
      str_to_upper() %>%
      str_replace_all("\\bAVENUE\\b", "AVE") %>%
      str_replace_all("\\bSTREET\\b", "ST") %>%
      str_replace_all("\\bTERRACE\\b", "TER") %>%
      str_replace_all("\\bCOURT\\b", "CT") %>%
      str_replace_all("\\bPLACE\\b", "PL") %>%
      str_replace_all("\\bROAD\\b", "RD") %>%
      str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
      str_replace_all("\\bPARKWAY\\b", "PKWY") %>%
      str_squish()
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
    city_from_address,
    city_to_address,
    city_street,
    city_year_built,
    city_units,
    no_stories,
    city_building_sqft,
    city_shape_area_sqft
  )
sf::st_geometry(footprints) <- "geometry"

if (anyDuplicated(footprints$footprint_id) > 0 || any(!sf::st_is_valid(footprints))) {
  stop("Unresolved residential footprints fail uniqueness or geometry checks.", call. = FALSE)
}

distances <- sf::st_distance(request_points, footprints)
link_index <- which(distances <= units::set_units(200, "ft"), arr.ind = TRUE) %>%
  as.data.frame() %>%
  transmute(
    request_row = row,
    footprint_row = col,
    distance_ft = as.numeric(units::set_units(distances[cbind(row, col)], "ft"))
  )
links <- link_index %>%
  bind_cols(
    requests[
      link_index$request_row,
      c(
        "request_id", "project_id", "address", "matched_address",
        "query_house_number", "request_street"
      )
    ],
    sf::st_drop_geometry(footprints[link_index$footprint_row, ])
  ) %>%
  select(-request_row, -footprint_row) %>%
  distinct(project_id, request_id, footprint_id, .keep_all = TRUE) %>%
  mutate(
    exact_address_range_match = request_street == city_street &
      is.finite(query_house_number) &
      is.finite(city_from_address) &
      is.finite(city_to_address) &
      query_house_number >= pmin(city_from_address, city_to_address) &
      query_house_number <= pmax(city_from_address, city_to_address)
  ) %>%
  group_by(project_id, request_id) %>%
  arrange(desc(exact_address_range_match), distance_ft, footprint_id, .by_group = TRUE) %>%
  mutate(
    exact_address_matches = sum(exact_address_range_match),
    nearest_footprint = row_number() == 1,
    selected_exact_footprint = exact_address_range_match & exact_address_matches == 1,
    address_footprint_status = case_when(
      exact_address_matches == 1 ~ "unique_address_range_match",
      exact_address_matches > 1 ~ "multiple_address_range_matches_review",
      TRUE ~ "nearest_only_review"
    )
  ) %>%
  ungroup() %>%
  arrange(project_id, request_id, desc(selected_exact_footprint), distance_ft, footprint_id)

linked_request_evidence <- links %>%
  group_by(project_id, request_id) %>%
  summarise(
    address = dplyr::first(address),
    matched_address = dplyr::first(matched_address),
    address_footprint_status = dplyr::first(address_footprint_status),
    exact_address_matches = dplyr::first(exact_address_matches),
    selected_footprint_id = dplyr::first(
      footprint_id[selected_exact_footprint],
      default = NA_character_
    ),
    selected_city_address = dplyr::first(
      city_address[selected_exact_footprint],
      default = NA_character_
    ),
    selected_city_year_built = dplyr::first(
      city_year_built[selected_exact_footprint],
      default = NA_integer_
    ),
    selected_city_units = dplyr::first(
      city_units[selected_exact_footprint],
      default = NA_real_
    ),
    selected_city_building_sqft = dplyr::first(
      city_building_sqft[selected_exact_footprint],
      default = NA_real_
    ),
    nearest_footprint_id = dplyr::first(footprint_id[nearest_footprint]),
    nearest_city_address = dplyr::first(city_address[nearest_footprint]),
    nearest_distance_ft = dplyr::first(distance_ft[nearest_footprint]),
    .groups = "drop"
  )

request_evidence <- requests %>%
  select(request_id, project_id, address, matched_address) %>%
  left_join(
    linked_request_evidence %>%
      select(-address, -matched_address),
    by = c("request_id", "project_id"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    address_footprint_status = coalesce(
      address_footprint_status,
      "no_footprint_within_200ft"
    ),
    exact_address_matches = coalesce(exact_address_matches, 0L)
  )

project_evidence <- request_evidence %>%
  group_by(project_id) %>%
  summarise(
    reference_addresses = n_distinct(request_id),
    uniquely_matched_addresses = sum(address_footprint_status == "unique_address_range_match"),
    ambiguous_address_matches = sum(
      address_footprint_status == "multiple_address_range_matches_review"
    ),
    nearest_only_addresses = sum(address_footprint_status == "nearest_only_review"),
    addresses_without_nearby_footprint = sum(
      address_footprint_status == "no_footprint_within_200ft"
    ),
    selected_city_footprints = n_distinct(selected_footprint_id, na.rm = TRUE),
    selected_city_year_built_values = paste(
      sort(unique(selected_city_year_built[
        is.finite(selected_city_year_built) & selected_city_year_built > 0
      ])),
      collapse = "/"
    ),
    selected_city_unit_values = paste(
      sort(unique(selected_city_units[is.finite(selected_city_units) & selected_city_units > 0])),
      collapse = "/"
    ),
    selected_city_building_sqft_sum = sum(
      selected_city_building_sqft[
        is.finite(selected_city_building_sqft) & selected_city_building_sqft > 0
      ]
    ),
    selected_city_footprint_evidence = paste0(
      request_id,
      " status=", address_footprint_status,
      "; requested_address=", address,
      "; footprint=", coalesce(selected_footprint_id, "unresolved"),
      "; city_address=", coalesce(selected_city_address, "unresolved"),
      "; year=", coalesce(as.character(selected_city_year_built), "missing"),
      "; units=", coalesce(as.character(selected_city_units), "missing"),
      "; building_sqft=", coalesce(as.character(selected_city_building_sqft), "missing"),
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  mutate(
    selected_city_building_sqft_sum = if_else(
      selected_city_building_sqft_sum > 0,
      selected_city_building_sqft_sum,
      NA_real_
    )
  )

summary <- tibble::tibble(
  metric = c(
    "accepted_address_points",
    "request_batches",
    "unique_building_footprints",
      "projects_with_nearby_footprint",
      "unique_project_footprint_links",
      "address_requests_with_unique_range_match",
      "address_requests_with_ambiguous_range_match",
      "address_requests_with_nearest_only_evidence",
      "address_requests_without_footprint_within_200ft"
  ),
  value = c(
    nrow(requests),
    length(query_batches),
    nrow(footprints),
    n_distinct(links$project_id),
    n_distinct(paste(links$project_id, links$footprint_id)),
    sum(request_evidence$address_footprint_status == "unique_address_range_match"),
    sum(request_evidence$address_footprint_status == "multiple_address_range_matches_review"),
    sum(request_evidence$address_footprint_status == "nearest_only_review"),
    sum(request_evidence$address_footprint_status == "no_footprint_within_200ft")
  )
)

sf::st_write(
  footprints,
  "../output/residential_unresolved_city_building_footprints.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)
readr::write_csv(links, "../output/residential_unresolved_city_footprint_links.csv")
readr::write_csv(project_evidence, "../output/residential_unresolved_city_building_evidence.csv")
readr::write_csv(bind_rows(manifest), "../output/residential_unresolved_city_footprint_manifest.csv")
readr::write_csv(summary, "../output/residential_unresolved_city_footprint_summary.csv")
