# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_historical_sale_parcel_coordinates/code")
# start_year <- 1999
# end_year <- 2025

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_year, end_year)
}
if (length(cli_args) != 2) {
  stop("Script requires 2 arguments: <start_year> <end_year>.", call. = FALSE)
}

start_year <- suppressWarnings(as.integer(cli_args[1]))
end_year <- suppressWarnings(as.integer(cli_args[2]))
if (!is.finite(start_year) || !is.finite(end_year) || start_year > end_year) {
  stop("start_year and end_year must be valid integers with start_year <= end_year.", call. = FALSE)
}
if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required to download historical parcel coordinates.", call. = FALSE)
}

sales <- fread(
  "../input/parcel_sales.csv",
  select = c("pin", "year", "class"),
  colClasses = list(character = "pin")
)
sales[, `:=`(
  pin = gsub("[^0-9]", "", trimws(pin)),
  year = suppressWarnings(as.integer(year)),
  class = suppressWarnings(as.integer(class))
)]
sales[nchar(pin) == 13L, pin := paste0("0", pin)]
sales <- sales[
  year >= start_year & year <= end_year &
    class %in% 202:211
]
if (any(nchar(sales$pin) != 14L)) {
  stop("Residential sales contain an invalid full PIN.", call. = FALSE)
}

current_parcels <- fread(
  "../input/parcel_universe_2025_city.csv",
  select = "pin",
  colClasses = list(character = "pin")
)
current_parcels[, pin := gsub("[^0-9]", "", trimws(pin))]
current_parcels[nchar(pin) == 13L, pin := paste0("0", pin)]
if (any(nchar(current_parcels$pin) != 14L)) {
  stop("Current parcel universe contains an invalid full PIN.", call. = FALSE)
}
if (anyDuplicated(current_parcels$pin) > 0) {
  stop("Current parcel universe must be unique by full PIN.", call. = FALSE)
}

missing_pins <- sort(unique(sales[!pin %in% current_parcels$pin, pin]))
if (length(missing_pins) == 0) {
  stop("No residential sale PINs require historical coordinates.", call. = FALSE)
}

chunks <- split(missing_pins, ceiling(seq_along(missing_pins) / 100L))
records <- vector("list", length(chunks))
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"

for (i in seq_along(chunks)) {
  parameters <- c(
    "$select" = "pin,year,lon,lat,x_3435,y_3435",
    "$where" = sprintf(
      "year between %d and %d and pin in(%s)",
      start_year,
      end_year,
      paste(sprintf("'%s'", chunks[[i]]), collapse = ",")
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
  for (attempt in 1:3) {
    response <- tryCatch(curl::curl_fetch_memory(query), error = function(e) NULL)
    if (!is.null(response) && response$status_code == 200L) {
      break
    }
    Sys.sleep(attempt)
  }
  if (is.null(response) || response$status_code != 200L) {
    stop(sprintf("Historical parcel download failed for chunk %d of %d.", i, length(chunks)), call. = FALSE)
  }

  payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
  if (is.data.frame(payload) && nrow(payload) > 0) {
    records[[i]] <- as.data.table(payload)
  }
}

historical_parcels <- rbindlist(records, use.names = TRUE, fill = TRUE)
if (nrow(historical_parcels) == 0) {
  stop("Historical parcel download returned no coordinates.", call. = FALSE)
}
historical_parcels[, `:=`(
  pin = as.character(pin),
  year = suppressWarnings(as.integer(year)),
  longitude = suppressWarnings(as.numeric(lon)),
  latitude = suppressWarnings(as.numeric(lat)),
  centroid_x_crs_3435 = suppressWarnings(as.numeric(x_3435)),
  centroid_y_crs_3435 = suppressWarnings(as.numeric(y_3435))
)]
historical_parcels[, c("lon", "lat", "x_3435", "y_3435") := NULL]
historical_parcels <- historical_parcels[
  nchar(pin) == 14L &
    year >= start_year & year <= end_year &
    is.finite(longitude) & is.finite(latitude)
]
if (anyDuplicated(historical_parcels[, .(pin, year)]) > 0) {
  stop("Historical parcel coordinates must be unique by full PIN and year.", call. = FALSE)
}

setorder(historical_parcels, pin, year)
fwrite(
  historical_parcels,
  sprintf("../output/historical_sale_parcel_coordinates_%d_%d.csv", start_year, end_year)
)
