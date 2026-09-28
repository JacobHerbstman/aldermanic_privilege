# setwd("tasks/assign_zoning_amendment_wards/code")
# Geocode the address in each zoning map amendment's title with the U.S. Census Bureau's batch geocoder (public, no
# key; https://geocoding.geo.census.gov/geocoder/locations/addressbatch, Public_AR_Current benchmark). The address is
# the first one after "at" in the title, with an address range reduced to its first number ("3939-3935 W Devon Ave"
# becomes 3939 W Devon Ave). Titles naming only an intersection or no address are not sent. Each batch response is
# kept as received; the source is live, so a rerun is a deliberate refresh.
batch_size <- 2000L
geocoder_url <- "https://geocoding.geo.census.gov/geocoder/locations/addressbatch"

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

amendments <- read_csv("../input/zoning_map_amendments.csv", show_col_types = FALSE)
addresses <- amendments |>
  transmute(matter_id,
    title_address = str_squish(str_match(title, regex("\\bat\\s+(.+?)\\s*(?:-\\s*App|,|;|\\s+and\\s+|$)",
      ignore_case = TRUE))[, 2]),
    address_query = str_replace(str_to_upper(title_address), "^([0-9]+)\\s*-\\s*[0-9]+\\b", "\\1")) |>
  mutate(address_query = if_else(str_detect(address_query, "^[0-9]+\\s+[NSEW]\\b"), address_query, NA_character_))

queries <- addresses |>
  filter(!is.na(address_query)) |>
  distinct(address_query) |>
  arrange(address_query) |>
  mutate(address_id = row_number())
SaveData(left_join(addresses, queries, by = "address_query", relationship = "many-to-one"), "matter_id",
  "../output/zoning_amendment_address_queries.csv")

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
writeLines(responses, "../temp/census_geocoder_responses.csv", sep = "", useBytes = TRUE)
stopifnot(file.rename("../temp/census_geocoder_responses.csv", "../output/census_geocoder_responses.csv"))
