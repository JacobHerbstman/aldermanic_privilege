# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

spatial_candidates <- readr::read_csv(
  "../output/residential_unresolved_successor_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    pin10 = readr::col_character(),
    class = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(
    class == "299",
    exact_address_match |
      (is.finite(minimum_point_distance_ft) & minimum_point_distance_ft <= 50)
  ) %>%
  distinct(project_id, pin10, .keep_all = TRUE)

manual_candidates <- readr::read_csv(
  "../adjudication/residential_unresolved_successor_condo_overrides.csv",
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) %>%
  transmute(
    project_id,
    pin = NA_character_,
    pin10,
    class = "299",
    exact_address_match = FALSE,
    accepted_point_match = FALSE,
    minimum_point_distance_ft = NA_real_,
    addresses = NA_character_,
    current_parcel_addresses = NA_character_,
    address_keys = NA_character_,
    request_ids = NA_character_,
    candidate_methods = "manual_successor_condo_override",
    override_reason = reason,
    override_evidence = evidence
  )

candidates <- bind_rows(
  spatial_candidates %>%
    mutate(override_reason = NA_character_, override_evidence = NA_character_),
  manual_candidates
) %>%
  arrange(project_id, pin10, desc(candidate_methods == "manual_successor_condo_override")) %>%
  distinct(project_id, pin10, .keep_all = TRUE)

if (anyDuplicated(candidates[c("project_id", "pin10")]) > 0) {
  stop("Condo successor candidates are not unique by project and base.", call. = FALSE)
}

requested_bases <- candidates %>%
  distinct(pin10) %>%
  arrange(pin10)

if (nrow(requested_bases) == 0) {
  stop("No unresolved residential condo successor bases were identified.", call. = FALSE)
}

batches <- split(requested_bases$pin10, ceiling(seq_len(nrow(requested_bases)) / 40))
responses <- vector("list", length(batches))
manifests <- vector("list", length(batches))

for (i in seq_along(batches)) {
  where_clause <- paste0(
    "pin10 in ('",
    paste(batches[[i]], collapse = "','"),
    "')"
  )

  response <- httr2::request(
    "https://datacatalog.cookcountyil.gov/resource/3r7i-mrz4.json"
  ) %>%
    httr2::req_url_query(
      `$where` = where_clause,
      `$limit` = 50000
    ) %>%
    httr2::req_retry(max_tries = 5) %>%
    httr2::req_timeout(seconds = 180) %>%
    httr2::req_perform()

  if (httr2::resp_status(response) != 200) {
    stop("Cook County condo request failed: ", httr2::resp_status(response), call. = FALSE)
  }

  body <- httr2::resp_body_string(response)
  parsed <- jsonlite::fromJSON(body, simplifyDataFrame = TRUE)
  if (length(parsed) == 0) {
    parsed <- tibble::tibble()
  }

  responses[[i]] <- as_tibble(parsed)
  manifests[[i]] <- tibble::tibble(
    batch = i,
    requested_pin10_count = length(batches[[i]]),
    response_rows = nrow(responses[[i]]),
    response_sha256 = digest::digest(body, algo = "sha256", serialize = FALSE),
    retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source_url = "https://datacatalog.cookcountyil.gov/resource/3r7i-mrz4.json",
    where_clause = where_clause
  )
}

raw_evidence <- bind_rows(responses)

if (nrow(raw_evidence) == 0) {
  stop("Cook County returned no unresolved residential condo evidence.", call. = FALSE)
}

raw_evidence <- raw_evidence %>%
  mutate(
    pin = str_pad(str_replace_all(as.character(pin), "[^0-9]", ""), 14, pad = "0"),
    pin10 = str_pad(str_replace_all(as.character(pin10), "[^0-9]", ""), 10, pad = "0"),
    year = as.integer(year),
    is_parking_space = as.logical(is_parking_space),
    is_common_area = as.logical(is_common_area)
  ) %>%
  arrange(pin10, year, pin)

if (anyDuplicated(raw_evidence$row_id) > 0) {
  stop("Cook County condo response contains duplicate row IDs.", call. = FALSE)
}

unexpected_bases <- raw_evidence %>%
  distinct(pin10) %>%
  anti_join(requested_bases, by = "pin10")

if (nrow(unexpected_bases) > 0) {
  stop("Cook County returned unrequested condo bases.", call. = FALSE)
}

base_year_summary <- raw_evidence %>%
  group_by(pin10, year) %>%
  summarise(
    condo_pin_records = n_distinct(pin),
    residential_pin_records = n_distinct(pin[is_parking_space %in% FALSE & is_common_area %in% FALSE]),
    parking_pin_records = n_distinct(pin[is_parking_space %in% TRUE]),
    common_area_pin_records = n_distinct(pin[is_common_area %in% TRUE]),
    year_built_values = paste(sort(unique(na.omit(as.character(char_yrblt)))), collapse = "/"),
    building_sqft_values = paste(sort(unique(na.omit(as.character(char_building_sf)))), collapse = "/"),
    land_sqft_values = paste(sort(unique(na.omit(as.character(char_land_sf)))), collapse = "/"),
    reported_nonunit_values = paste(sort(unique(na.omit(as.character(char_building_non_units)))), collapse = "/"),
    reported_building_pin_values = paste(sort(unique(na.omit(as.character(char_building_pins)))), collapse = "/"),
    .groups = "drop"
  ) %>%
  arrange(pin10, year)

project_base_summary <- candidates %>%
  select(
    project_id,
    pin10,
    exact_address_match,
    minimum_point_distance_ft,
    addresses,
    candidate_methods,
    override_reason,
    override_evidence
  ) %>%
  left_join(
    base_year_summary %>% tidyr::nest(condo_history = -pin10),
    by = "pin10",
    relationship = "many-to-one"
  ) %>%
  tidyr::unnest(condo_history, keep_empty = TRUE) %>%
  arrange(project_id, pin10, year)

summary <- tibble::tibble(
  metric = c(
    "projects_with_condo_successor_candidates",
    "condo_bases_requested",
    "condo_bases_returned",
    "raw_condo_history_rows",
    "distinct_condo_pins"
  ),
  value = c(
    n_distinct(candidates$project_id),
    nrow(requested_bases),
    n_distinct(raw_evidence$pin10),
    nrow(raw_evidence),
    n_distinct(raw_evidence$pin)
  )
)

readr::write_csv(
  raw_evidence,
  "../output/residential_unresolved_successor_condo_evidence.csv"
)
readr::write_csv(
  base_year_summary,
  "../output/residential_unresolved_successor_condo_base_year_summary.csv"
)
readr::write_csv(
  project_base_summary,
  "../output/residential_unresolved_successor_condo_projects.csv"
)
readr::write_csv(
  bind_rows(manifests),
  "../output/residential_unresolved_successor_condo_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/residential_unresolved_successor_condo_summary.csv"
)
