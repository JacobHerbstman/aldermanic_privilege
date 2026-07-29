# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

requests <- readr::read_csv(
  "../output/multicard_successor_condo_requests.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(requests[c("project_id", "pin10")])) {
  stop("Multicard condo requests are not unique.", call. = FALSE)
}

requested_bases <- requests |>
  dplyr::distinct(pin10) |>
  dplyr::arrange(pin10)

if (nrow(requested_bases) == 0L) {
  stop("No multicard condo bases were requested.", call. = FALSE)
}

batches <- split(
  requested_bases$pin10,
  ceiling(seq_len(nrow(requested_bases)) / 40)
)
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
  ) |>
    httr2::req_url_query(
      `$where` = where_clause,
      `$limit` = 50000
    ) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_perform()

  if (httr2::resp_status(response) != 200) {
    stop(
      "Cook County condo request failed: ",
      httr2::resp_status(response),
      call. = FALSE
    )
  }

  body <- httr2::resp_body_string(response)
  parsed <- jsonlite::fromJSON(body, simplifyDataFrame = TRUE)
  if (length(parsed) == 0) {
    parsed <- tibble::tibble()
  }

  responses[[i]] <- tibble::as_tibble(parsed)
  manifests[[i]] <- tibble::tibble(
    batch = i,
    requested_pin10_count = length(batches[[i]]),
    response_rows = nrow(responses[[i]]),
    response_sha256 = digest::digest(
      body,
      algo = "sha256",
      serialize = FALSE
    ),
    retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    source_url =
      "https://datacatalog.cookcountyil.gov/resource/3r7i-mrz4.json",
    where_clause
  )
}

raw_evidence <- dplyr::bind_rows(responses) |>
  dplyr::mutate(
    pin = stringr::str_pad(
      stringr::str_replace_all(as.character(pin), "[^0-9]", ""),
      14,
      pad = "0"
    ),
    pin10 = stringr::str_pad(
      stringr::str_replace_all(as.character(pin10), "[^0-9]", ""),
      10,
      pad = "0"
    ),
    year = as.integer(year),
    is_parking_space = as.logical(is_parking_space),
    is_common_area = as.logical(is_common_area)
  ) |>
  dplyr::arrange(pin10, year, pin)

if (nrow(raw_evidence) == 0L || anyDuplicated(raw_evidence$row_id)) {
  stop(
    "Cook County condo response is empty or has duplicate row IDs.",
    call. = FALSE
  )
}

unexpected_bases <- raw_evidence |>
  dplyr::distinct(pin10) |>
  dplyr::anti_join(requested_bases, by = "pin10")

if (nrow(unexpected_bases) > 0L) {
  stop("Cook County returned unrequested condo bases.", call. = FALSE)
}

base_year_summary <- raw_evidence |>
  dplyr::group_by(pin10, year) |>
  dplyr::summarise(
    condo_pin_records = dplyr::n_distinct(pin),
    residential_pin_records = dplyr::n_distinct(
      pin[is_parking_space %in% FALSE & is_common_area %in% FALSE]
    ),
    parking_pin_records =
      dplyr::n_distinct(pin[is_parking_space %in% TRUE]),
    common_area_pin_records =
      dplyr::n_distinct(pin[is_common_area %in% TRUE]),
    year_built_values = paste(
      sort(unique(stats::na.omit(as.character(char_yrblt)))),
      collapse = "/"
    ),
    building_sqft_values = paste(
      sort(unique(stats::na.omit(as.character(char_building_sf)))),
      collapse = "/"
    ),
    unit_sqft_values = paste(
      sort(unique(stats::na.omit(as.character(char_unit_sf)))),
      collapse = "/"
    ),
    .groups = "drop"
  ) |>
  dplyr::arrange(pin10, year)

missing_bases <- requested_bases |>
  dplyr::anti_join(
    raw_evidence |>
      dplyr::distinct(pin10),
    by = "pin10"
  )

summary <- tibble::tibble(
  metric = c(
    "requested_condo_bases",
    "condo_bases_returned",
    "condo_bases_missing",
    "raw_condo_history_rows",
    "distinct_condo_pins"
  ),
  value = c(
    nrow(requested_bases),
    dplyr::n_distinct(raw_evidence$pin10),
    nrow(missing_bases),
    nrow(raw_evidence),
    dplyr::n_distinct(raw_evidence$pin)
  )
)

readr::write_csv(
  raw_evidence,
  "../output/multicard_successor_condo_evidence.csv"
)
readr::write_csv(
  base_year_summary,
  "../output/multicard_successor_condo_base_year_summary.csv"
)
readr::write_csv(
  missing_bases,
  "../output/multicard_successor_condo_missing_bases.csv"
)
readr::write_csv(
  dplyr::bind_rows(manifests),
  "../output/multicard_successor_condo_download_manifest.csv"
)
readr::write_csv(
  summary,
  "../output/multicard_successor_condo_evidence_summary.csv"
)
