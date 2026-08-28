# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/sales_sample_definition_audit/code")

source("../../setup_environment/code/packages.R")

sale_keys <- fread(
  "../output/sales_sample_definition_coordinate_keys.csv",
  colClasses = list(character = "pin")
)
sale_keys[, year := suppressWarnings(as.integer(year))]
if (
  nrow(sale_keys) == 0L ||
    any(nchar(sale_keys$pin) != 14L) ||
    anyDuplicated(sale_keys[, .(pin, year)]) > 0L
) {
  stop("Candidate sale coordinate keys are invalid.", call. = FALSE)
}

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
    request_plan[[length(request_plan) + 1L]] <- data.table(
      year = year_i,
      chunk = chunk_i,
      query = paste0(
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
      ),
      destination = tempfile(
        sprintf("sample_definition_coordinates_%d_%04d_", year_i, chunk_i),
        fileext = ".json"
      )
    )
  }
}

request_plan <- rbindlist(request_plan)
downloaded <- rep(FALSE, nrow(request_plan))

for (batch_start in seq(1L, nrow(request_plan), by = 24L)) {
  batch <- batch_start:min(batch_start + 23L, nrow(request_plan))

  for (attempt in 1:3) {
    pending <- batch[!downloaded[batch]]
    if (length(pending) == 0L) {
      break
    }
    results <- curl::multi_download(
      request_plan$query[pending],
      request_plan$destination[pending],
      progress = FALSE,
      connecttimeout = 30,
      timeout = 120
    )
    downloaded[pending] <- results$success & results$status_code == 200L
    if (any(!downloaded[batch])) {
      Sys.sleep(attempt)
    }
  }
}
if (any(!downloaded)) {
  failed <- request_plan[!downloaded, sprintf("%d/%d", year, chunk)]
  stop(sprintf(
    "Historical parcel download failed for %d requests, including %s.",
    length(failed),
    paste(head(failed, 3), collapse = ", ")
  ), call. = FALSE)
}

records <- lapply(
  request_plan$destination,
  jsonlite::fromJSON,
  simplifyDataFrame = TRUE
)
unlink(request_plan$destination)
records <- Filter(\(x) is.data.frame(x) && nrow(x) > 0L, records)
historical_parcels <- rbindlist(records, use.names = TRUE, fill = TRUE)
if (nrow(historical_parcels) == 0L) {
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
  paste(pin, year, sep = "\r") %in%
    paste(sale_keys$pin, sale_keys$year, sep = "\r") &
    is.finite(longitude) & is.finite(latitude)
]
if (anyDuplicated(historical_parcels[, .(pin, year)]) > 0L) {
  stop("Historical parcel coordinates must be unique by PIN-year.", call. = FALSE)
}

setorder(historical_parcels, pin, year)
fwrite(
  historical_parcels,
  "../output/sales_sample_definition_historical_coordinates.csv"
)
