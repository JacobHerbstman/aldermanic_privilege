# setwd("tasks/assign_zoning_amendment_wards/code")
# A deliberate refresh of the geocodes: each distinct address query (build_address_queries.R) sent to the U.S. Census
# Bureau's batch geocoder (public, no key; https://geocoding.geo.census.gov/geocoder/locations/addressbatch,
# Public_AR_Current benchmark), in batches of batch_size. The batch responses are kept as received, in
# ../temp/census_geocoder_responses_current.csv, for comparison with the preserved snapshot the build uses
# (../sources/census_geocoder_responses_20260928.csv.gz); adopting a refresh means preserving the new file there and
# recording its checksum in geocoder_snapshot.sha256. Run with `make refresh-geocodes`.
batch_size <- 2000L
geocoder_url <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

source("../../setup_environment/code/packages.R")

queries <- read_csv("../output/zoning_amendment_address_queries.csv", show_col_types = FALSE) |>
  filter(!is.na(address_id)) |>
  distinct(address_id, address_query)

responses <- character()
for (batch in split(queries, ceiling(queries$address_id / batch_size))) {
  request_file <- "../temp/geocoder_batch.csv"
  write_csv(transmute(batch, address_id, street = address_query, city = "Chicago", state = "IL", zip = ""),
    request_file, col_names = FALSE, na = "")
  response <- httr2::request(geocoder_url) |>
    httr2::req_body_multipart(addressFile = curl::form_file(request_file), benchmark = "Public_AR_Current") |>
    httr2::req_timeout(600) |>
    httr2::req_retry(max_tries = 5, backoff = function(attempt) 60) |>
    httr2::req_perform()
  body <- httr2::resp_body_string(response)
  if (!endsWith(body, "\n")) body <- paste0(body, "\n")
  stopifnot(length(strsplit(trimws(body), "\n", fixed = TRUE)[[1]]) == nrow(batch))
  responses <- c(responses, body)
}
writeLines(responses, "../temp/census_geocoder_responses_current.tmp", sep = "", useBytes = TRUE)
stopifnot(file.rename("../temp/census_geocoder_responses_current.tmp", "../temp/census_geocoder_responses_current.csv"))
