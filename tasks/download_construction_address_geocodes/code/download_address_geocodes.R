# setwd("tasks/download_construction_address_geocodes/code")
# provider <- "census"

library(readr)

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(provider)
stopifnot(length(args) == 1L, args[1] %in% c("census", "chicago"))
provider <- args[1]
queries <- read_csv("../output/address_geocode_queries.csv", col_types = cols(.default = col_character()))
stopifnot(!anyNA(queries), !anyDuplicated(queries$selected_address))
requests <- vector("list", nrow(queries))
for (i in seq_len(nrow(queries))) {
  if (provider == "census") {
    request <- httr2::request("https://geocoding.geo.census.gov/geocoder/locations/onelineaddress") |>
      httr2::req_url_query(address = queries$address_query[i], benchmark = "Public_AR_Current", format = "json")
  } else {
    request <- httr2::request("https://gisapps.cityofchicago.org/arcgis/rest/services/Chicago_Addresses/GeocodeServer/findAddressCandidates") |>
      httr2::req_url_query(SingleLine = queries$selected_address[i], outFields = "*", outSR = 3435, f = "json")
  }
  requests[[i]] <- request |> httr2::req_retry(max_tries = 5) |> httr2::req_timeout(60)
}
responses <- httr2::req_perform_parallel(requests, max_active = 3, on_error = "stop", progress = FALSE)
queries$response_json <- vapply(responses, httr2::resp_body_string, character(1))
for (body in queries$response_json) {
  response <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  stopifnot(is.null(response$error), is.null(response$errors))
  if (provider == "census") {
    stopifnot("addressMatches" %in% names(response$result))
  } else {
    stopifnot("candidates" %in% names(response), !isTRUE(response$exceededTransferLimit))
  }
}
write_csv(queries, paste0("../temp/address_geocodes_", provider, ".csv"))
stopifnot(file.rename(paste0("../temp/address_geocodes_", provider, ".csv"),
                      paste0("../output/address_geocodes_", provider, "_download.csv")))
