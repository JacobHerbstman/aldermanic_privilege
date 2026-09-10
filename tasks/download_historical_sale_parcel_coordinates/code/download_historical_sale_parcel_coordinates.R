# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_historical_sale_parcel_coordinates/code")
# start_year <- 2006
# end_year <- 2022

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

sales <- read_parquet(
  "../input/residential_sales_clean.parquet",
  col_select = c("pin", "year")
)
setDT(sales)
sale_keys <- unique(sales[year %between% c(start_year, end_year), .(pin, year)])
if (any(nchar(sale_keys$pin) != 14L)) {
  stop("Clean residential sales contain an invalid full PIN.", call. = FALSE)
}
if (nrow(sale_keys) == 0) {
  stop("No residential sale PIN-years require historical coordinates.", call. = FALSE)
}

setorder(sale_keys, year, pin)
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"
request_plan <- list()

for (year_i in sort(unique(sale_keys$year))) {
  year_pins <- sale_keys[year == year_i, pin]
  chunks <- split(year_pins, ceiling(seq_along(year_pins) / 500L))

  for (chunk_i in seq_along(chunks)) {
    parameters <- c(
      "$select" = "pin,year,lon,lat,x_3435,y_3435",
      "$where" = sprintf(
        "year=%d and pin in(%s)",
        year_i,
        paste(sprintf("'%s'", chunks[[chunk_i]]), collapse = ",")
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
    request_plan[[length(request_plan) + 1L]] <- data.table(
      year = year_i,
      chunk = chunk_i,
      query = query,
      destination = tempfile(
        sprintf("historical_parcels_%d_%04d_", year_i, chunk_i),
        fileext = ".json"
      )
    )
  }
}

request_plan <- rbindlist(request_plan)
downloaded <- rep(FALSE, nrow(request_plan))

# Retry failed requests after the other batches, allowing the service to recover.
for (attempt in 1:3) {
  pending <- which(!downloaded)
  if (length(pending) == 0) {
    break
  }
  if (attempt > 1) {
    Sys.sleep(10 * (attempt - 1))
  }
  for (batch_start in seq(1L, length(pending), by = 24L)) {
    batch <- pending[batch_start:min(batch_start + 23L, length(pending))]
    results <- curl::multi_download(
      request_plan$query[batch],
      request_plan$destination[batch],
      progress = FALSE,
      connecttimeout = 30,
      timeout = 120
    )
    downloaded[batch] <- results$success & results$status_code == 200L
    message(sprintf(
      "Historical parcel requests: %d/%d successful (attempt %d).",
      sum(downloaded), nrow(request_plan), attempt
    ))
  }
}
if (any(!downloaded)) {
  failed <- request_plan[!downloaded, sprintf("%d/%d", year, chunk)]
  stop(
    sprintf(
      "Historical parcel download failed for %d requests, including %s.",
      length(failed),
      paste(head(failed, 3), collapse = ", ")
    ),
    call. = FALSE
  )
}

records <- lapply(
  request_plan$destination,
  jsonlite::fromJSON,
  simplifyDataFrame = TRUE
)
unlink(request_plan$destination)
records <- Filter(\(x) is.data.frame(x) && nrow(x) > 0, records)
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
    paste(pin, year, sep = "\r") %in% paste(sale_keys$pin, sale_keys$year, sep = "\r") &
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
