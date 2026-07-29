# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

normalize_address <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_squish()
}

initial_geocoded_addresses <- readr::read_csv(
  "../output/residential_unresolved_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    request_id = readr::col_character(),
    project_id = readr::col_character(),
    address = readr::col_character(),
    address_normalized = readr::col_character(),
    geocode_status = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    request_id,
    project_id,
    address,
    address_key = normalize_address(address_normalized),
    geocode_status,
    x_3435 = as.numeric(x_3435),
    y_3435 = as.numeric(y_3435)
  )

permit_geocoded_addresses <- readr::read_csv(
  "../output/residential_unresolved_permit_address_geocodes.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    request_id = readr::col_character(),
    project_id = readr::col_character(),
    address = readr::col_character(),
    address_normalized = readr::col_character(),
    geocode_status = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  transmute(
    request_id,
    project_id,
    address,
    address_key = normalize_address(address_normalized),
    geocode_status,
    x_3435 = as.numeric(x_3435),
    y_3435 = as.numeric(y_3435)
  )

geocoded_addresses <- bind_rows(initial_geocoded_addresses, permit_geocoded_addresses)

if (anyDuplicated(geocoded_addresses$request_id) > 0 ||
    anyDuplicated(geocoded_addresses[c("project_id", "address_key")]) > 0) {
  stop("Unresolved residential address requests are not unique.", call. = FALSE)
}

permit_addresses <- readr::read_csv(
  "../output/residential_unresolved_address_permit_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    permit_number = readr::col_character(),
    permit_address = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(!is.na(permit_address), str_squish(permit_address) != "") %>%
  transmute(
    project_id,
    address = str_squish(permit_address),
    address_key = normalize_address(permit_address),
    permit_number
  ) %>%
  distinct(project_id, address_key, .keep_all = TRUE) %>%
  anti_join(
    geocoded_addresses %>% select(project_id, address_key),
    by = c("project_id", "address_key")
  ) %>%
  arrange(project_id, address_key) %>%
  mutate(
    request_id = paste0("exact_permit_address_", row_number()),
    geocode_status = "exact_project_permit_address",
    x_3435 = NA_real_,
    y_3435 = NA_real_
  ) %>%
  select(names(geocoded_addresses))

addresses <- bind_rows(geocoded_addresses, permit_addresses)

if (anyDuplicated(addresses$request_id) > 0 ||
    anyDuplicated(addresses[c("project_id", "address_key")]) > 0) {
  stop("Combined successor address requests violate their declared keys.", call. = FALSE)
}

connection <- DBI::dbConnect(duckdb::duckdb())
on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)

duckdb::duckdb_register(connection, "request_addresses", addresses)

exact_address_candidates <- DBI::dbGetQuery(
  connection,
  paste0(
    "WITH parcel_addresses AS (",
    "SELECT trim(pin) AS pin, trim(prop_address_full) AS parcel_address, ",
    "upper(trim(regexp_replace(prop_address_full, '[^A-Za-z0-9 ]', ' ', 'g'))) AS address_key ",
    "FROM read_csv('../input/parcel_addresses_2025_chicago.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000)",
    "), current_parcels AS (",
    "SELECT trim(pin) AS pin, trim(pin10) AS pin10, trim(class) AS class, ",
    "try_cast(centroid_x_crs_3435 AS DOUBLE) AS parcel_x_3435, ",
    "try_cast(centroid_y_crs_3435 AS DOUBLE) AS parcel_y_3435 ",
    "FROM read_csv('../input/parcel_universe_2025_city.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000)",
    ") ",
    "SELECT r.request_id, r.project_id, r.address, r.address_key, r.geocode_status, ",
    "r.x_3435, r.y_3435, p.pin, p.pin10, p.class, a.parcel_address, ",
    "p.parcel_x_3435, p.parcel_y_3435, ",
    "CASE WHEN r.x_3435 IS NOT NULL AND r.y_3435 IS NOT NULL ",
    "THEN sqrt(pow(p.parcel_x_3435 - r.x_3435, 2) + pow(p.parcel_y_3435 - r.y_3435, 2)) ",
    "ELSE NULL END AS point_distance_ft ",
    "FROM request_addresses r ",
    "JOIN parcel_addresses a ON r.address_key = a.address_key ",
    "JOIN current_parcels p ON a.pin = p.pin"
  )
) %>%
  as_tibble() %>%
  mutate(candidate_method = "exact_current_parcel_address")

accepted_points <- addresses %>%
  filter(
    geocode_status == "accepted_reference_point",
    is.finite(x_3435),
    is.finite(y_3435)
  )

duckdb::duckdb_register(connection, "accepted_points", accepted_points)

nearby_point_candidates <- DBI::dbGetQuery(
  connection,
  paste0(
    "WITH parcel_addresses AS (",
    "SELECT trim(pin) AS pin, string_agg(DISTINCT trim(prop_address_full), '/') AS parcel_address ",
    "FROM read_csv('../input/parcel_addresses_2025_chicago.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) ",
    "GROUP BY trim(pin)",
    "), current_parcels AS (",
    "SELECT trim(u.pin) AS pin, trim(u.pin10) AS pin10, trim(u.class) AS class, ",
    "a.parcel_address, ",
    "try_cast(u.centroid_x_crs_3435 AS DOUBLE) AS parcel_x_3435, ",
    "try_cast(u.centroid_y_crs_3435 AS DOUBLE) AS parcel_y_3435 ",
    "FROM read_csv('../input/parcel_universe_2025_city.csv', ",
    "all_varchar = true, header = true, ignore_errors = true, max_line_size = 10000000) u ",
    "LEFT JOIN parcel_addresses a ON trim(u.pin) = a.pin ",
    "WHERE try_cast(u.centroid_x_crs_3435 AS DOUBLE) IS NOT NULL ",
    "AND try_cast(u.centroid_y_crs_3435 AS DOUBLE) IS NOT NULL",
    ") ",
    "SELECT r.request_id, r.project_id, r.address, r.address_key, r.geocode_status, ",
    "r.x_3435, r.y_3435, p.pin, p.pin10, p.class, p.parcel_address, ",
    "p.parcel_x_3435, p.parcel_y_3435, ",
    "sqrt(pow(p.parcel_x_3435 - r.x_3435, 2) + pow(p.parcel_y_3435 - r.y_3435, 2)) ",
    "AS point_distance_ft ",
    "FROM accepted_points r ",
    "JOIN current_parcels p ",
    "ON abs(p.parcel_x_3435 - r.x_3435) <= 200 ",
    "AND abs(p.parcel_y_3435 - r.y_3435) <= 200 ",
    "WHERE sqrt(pow(p.parcel_x_3435 - r.x_3435, 2) + pow(p.parcel_y_3435 - r.y_3435, 2)) <= 200"
  )
) %>%
  as_tibble() %>%
  mutate(candidate_method = "current_parcel_centroid_within_200ft")

candidates <- bind_rows(exact_address_candidates, nearby_point_candidates) %>%
  distinct(
    request_id,
    pin,
    candidate_method,
    .keep_all = TRUE
  ) %>%
  group_by(project_id, pin) %>%
  summarise(
    pin10 = first(pin10),
    class = first(class),
    exact_address_match = any(candidate_method == "exact_current_parcel_address"),
    accepted_point_match = any(candidate_method == "current_parcel_centroid_within_200ft"),
    minimum_point_distance_ft = {
      finite_distances <- point_distance_ft[is.finite(point_distance_ft)]
      if (length(finite_distances) > 0) min(finite_distances) else NA_real_
    },
    addresses = paste(sort(unique(address)), collapse = "/"),
    current_parcel_addresses = paste(sort(unique(na.omit(parcel_address))), collapse = "/"),
    address_keys = paste(sort(unique(address_key)), collapse = "/"),
    request_ids = paste(sort(unique(request_id)), collapse = "/"),
    candidate_methods = paste(sort(unique(candidate_method)), collapse = "/"),
    .groups = "drop"
  ) %>%
  arrange(project_id, desc(exact_address_match), minimum_point_distance_ft, pin)

project_summary <- addresses %>%
  distinct(project_id) %>%
  left_join(
    candidates %>%
      group_by(project_id) %>%
      summarise(
        current_candidate_pins = n_distinct(pin),
        exact_address_candidate_pins = n_distinct(pin[exact_address_match]),
        candidates_within_25ft = n_distinct(pin[is.finite(minimum_point_distance_ft) & minimum_point_distance_ft <= 25]),
        candidates_within_50ft = n_distinct(pin[is.finite(minimum_point_distance_ft) & minimum_point_distance_ft <= 50]),
        condo_bases = n_distinct(
          pin10[
            class == "299" &
              (exact_address_match |
                (is.finite(minimum_point_distance_ft) & minimum_point_distance_ft <= 50))
          ]
        ),
        candidate_classes = paste(sort(unique(class)), collapse = "/"),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) %>%
  arrange(project_id)

summary <- tibble::tibble(
  metric = c(
    "projects_requested",
    "address_requests",
    "accepted_address_points",
    "projects_with_current_candidates",
    "projects_with_exact_address_candidates",
    "projects_with_candidate_within_25ft",
    "projects_with_candidate_within_50ft",
    "projects_with_condo_base_candidate",
    "distinct_current_candidate_pins"
  ),
  value = c(
    n_distinct(addresses$project_id),
    nrow(addresses),
    nrow(accepted_points),
    sum(project_summary$current_candidate_pins > 0),
    sum(project_summary$exact_address_candidate_pins > 0),
    sum(project_summary$candidates_within_25ft > 0),
    sum(project_summary$candidates_within_50ft > 0),
    sum(project_summary$condo_bases > 0),
    n_distinct(candidates$pin)
  )
)

readr::write_csv(
  candidates,
  "../output/residential_unresolved_successor_candidates.csv"
)
readr::write_csv(
  project_summary,
  "../output/residential_unresolved_successor_candidate_projects.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_successor_candidate_summary.csv"
)
