# setwd("tasks/download_construction_historical_coordinates/code")
# history_start_year <- 1999
# history_end_year <- 2025

source("../../setup_environment/code/packages.R")

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical parcel audit.", call. = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(history_start_year, history_end_year)
if (length(args) != 2L) stop("Expected the first and last historical assessment years.")
history_start_year <- as.integer(args[1])
history_end_year <- as.integer(args[2])
if (anyNA(c(history_start_year, history_end_year)) || history_start_year > history_end_year) {
  stop("Invalid historical source window.")
}
buildings <- read_csv(
  "../output/historical_parcel_requests.csv", show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)
if (anyNA(buildings$pin) || anyDuplicated(buildings$pin)) {
  stop("Historical parcel requests must have unique PINs.")
}
pin_chunks <- split(buildings$pin, ceiling(seq_len(nrow(buildings)) / 50L))

base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"
records <- list()
request_number <- 0L
total_requests <- length(pin_chunks)

for (pin_chunk in pin_chunks) {
    request_number <- request_number + 1L
    parameters <- c(
      "$select" = "pin,pin10,year,class,lon,lat,x_3435,y_3435,misc_subdivision_id,row_id",
      "$where" = sprintf(
        "year between %d and %d and pin in(%s)",
        history_start_year, history_end_year,
        paste(sprintf("'%s'", pin_chunk), collapse = ",")
      ),
      "$order" = "pin,year",
      "$limit" = "50000"
    )
    query <- paste0(
      base_url,
      "?",
      paste(
        paste0(
          URLencode(names(parameters), reserved = TRUE),
          "=",
          URLencode(unname(parameters), reserved = TRUE)
        ),
        collapse = "&"
      )
    )

    response <- NULL
    request_error <- NULL
    for (attempt in 1:5) {
      response <- tryCatch(
        curl::curl_fetch_memory(query, handle = curl::new_handle(timeout = 180)),
        error = function(e) {
          request_error <<- conditionMessage(e)
          NULL
        }
      )
      if (!is.null(response) && response$status_code == 200L) {
        break
      }
      Sys.sleep(attempt)
    }
    if (is.null(response) || response$status_code != 200L) {
      stop(
        sprintf(
          "Historical parcel query %d of %d failed: %s",
          request_number, total_requests,
          if (is.null(response)) request_error else paste(response$status_code, rawToChar(response$content))
        ),
        call. = FALSE
      )
    }

    payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
    if (is.data.frame(payload) && nrow(payload) >= 50000L) stop("Historical source query hit its row limit.")
    if (is.data.frame(payload) && nrow(payload) > 0) {
      records[[length(records) + 1L]] <- as_tibble(payload)
    }

    if (request_number %% 10L == 0L || request_number == total_requests) {
      message(sprintf("Historical parcel requests: %d of %d", request_number, total_requests))
    }
}

historical <- bind_rows(records) %>%
  transmute(
    pin = as.character(pin),
    pin10 = as.character(pin10),
    year = suppressWarnings(as.integer(year)),
    parcel_class = as.character(class),
    longitude = suppressWarnings(as.numeric(lon)),
    latitude = suppressWarnings(as.numeric(lat)),
    centroid_x_crs_3435 = suppressWarnings(as.numeric(x_3435)),
    centroid_y_crs_3435 = suppressWarnings(as.numeric(y_3435)),
    subdivision_id = as.character(misc_subdivision_id),
    row_id = as.character(row_id)
  )

duplicate_records <- historical %>%
  count(pin, year, name = "rows") %>%
  filter(rows > 1)
if (nrow(duplicate_records) > 0) {
  stop("Historical Parcel Universe returned duplicate PIN-year records.", call. = FALSE)
}

unexpected_records <- historical %>%
  anti_join(buildings, by = "pin")
if (anyNA(historical$pin) || anyNA(historical$year) ||
    any(historical$year < history_start_year | historical$year > history_end_year) ||
    nrow(unexpected_records) > 0) {
  stop("Historical Parcel Universe returned unrequested PIN-year records.", call. = FALSE)
}

historical %>%
  arrange(pin, year) %>%
  write_csv("../output/density_historical_parcel_records.csv")
