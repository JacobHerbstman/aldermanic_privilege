# setwd("tasks/download_construction_historical_parcels/code")
# history_start_year <- 1999L
# history_end_year <- 2025L
# scope <- "initial"

source("../../shared/code/save_data.R")

library(dplyr)
library(readr)

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(history_start_year, history_end_year, scope)
stopifnot(length(args) == 3L, args[3] %in% c("initial", "geocoding", "reviewed"))
scope <- args[3]
history_start_year <- as.integer(args[1])
history_end_year <- as.integer(args[2])
stopifnot(!anyNA(c(history_start_year, history_end_year)), history_start_year <= history_end_year)
if (scope == "initial") {
  queries <- read_csv("../output/predecessor_history_queries_download.csv", col_types = cols(pin = col_character()))
} else if (scope == "reviewed") {
  queries <- read_csv("reviewed_history_queries.csv", col_types = cols(pin = col_character()))
} else {
  queries <- read_csv("../output/geocoding_history_queries_download.csv", col_types = cols(pin = col_character()))
}
stopifnot(!anyNA(queries), !anyDuplicated(queries), all(grepl("^[0-9]{14}$", queries$pin)))

history <- tibble(pin = character(), pin10 = character(), year = character(),
  lon = character(), lat = character(), x_3435 = character(), y_3435 = character(), row_id = character())
for (pins in split(queries$pin, ceiling(seq_len(nrow(queries)) / 50L))) {
  response <- httr2::request("https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json") |>
    httr2::req_url_query(`$select` = "pin,pin10,year,lon,lat,x_3435,y_3435,row_id",
      `$where` = sprintf("year between %d and %d and pin in(%s)", history_start_year,
        history_end_year, paste(sprintf("'%s'", pins), collapse = ",")),
      `$order` = "pin,year,row_id", `$limit` = 50000L) |>
    httr2::req_retry(max_tries = 5) |>
    httr2::req_timeout(180) |>
    httr2::req_perform() |>
    httr2::resp_body_string()
  records <- jsonlite::fromJSON(response, simplifyVector = FALSE)
  stopifnot(is.list(records), is.null(names(records)), length(records) < 50000L)
  batch <- bind_rows(records)
  if (nrow(batch) > 0L) {
    stopifnot(all(c("pin", "year", "row_id") %in% names(batch)),
              all(batch$pin %in% pins), !anyNA(batch$year),
              all(as.integer(batch$year) %in% history_start_year:history_end_year))
    history <- bind_rows(history, batch)
  }
}
# Preserve every source row. Key conflicts are checked before location selection.
stopifnot(!anyNA(history$row_id), !anyDuplicated(history$row_id))
history <- arrange(history, pin, year, row_id)
if (scope == "initial") {
  write_csv(history, "../temp/predecessor_parcel_history_download.csv")
  stopifnot(file.rename("../temp/predecessor_parcel_history_download.csv", "../output/predecessor_parcel_history_download.csv"))
  ReportData("../output/predecessor_parcel_history_download.csv", "row_id")
} else if (scope == "reviewed") {
  write_csv(history, "../temp/reviewed_parcel_history.csv")
  stopifnot(file.rename("../temp/reviewed_parcel_history.csv", "../output/reviewed_parcel_history_current.csv"))
  ReportData("../output/reviewed_parcel_history_current.csv", c("pin", "year"))
} else {
  write_csv(history, "../temp/geocoding_parcel_history_download.csv")
  stopifnot(file.rename("../temp/geocoding_parcel_history_download.csv", "../output/geocoding_parcel_history_download.csv"))
  ReportData("../output/geocoding_parcel_history_download.csv", c("pin", "year"))
}
