# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_location_assessor_join_audit/code")

source("../../../setup_environment/code/packages.R")

fallback_cases <- fread(
  "../output/sales_coordinate_fallback_cases.csv",
  colClasses = list(character = c("pin_norm"))
)
missing_pins <- sort(unique(fallback_cases[!is.na(pin_norm) & pin_norm != "", pin_norm]))
if (length(missing_pins) == 0) {
  stop("No missing full PINs were available for the historical parcel query.", call. = FALSE)
}
if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical parcel audit.", call. = FALSE)
}

chunks <- split(missing_pins, ceiling(seq_along(missing_pins) / 50L))
records <- vector("list", length(chunks))
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"

for (i in seq_along(chunks)) {
  pin_list <- paste(sprintf("'%s'", chunks[[i]]), collapse = ",")
  parameters <- c(
    "$select" = "pin,year,lon,lat,x_3435,y_3435",
    "$where" = sprintf("year between 1999 and 2025 and pin in(%s)", pin_list),
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
    stop(sprintf("Historical parcel query failed for chunk %d of %d.", i, length(chunks)), call. = FALSE)
  }

  payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
  if (is.data.frame(payload) && nrow(payload) > 0) {
    records[[i]] <- as.data.table(payload)
  }
  if (i %% 10L == 0L || i == length(chunks)) {
    message(sprintf("Historical parcel query: %d of %d chunks", i, length(chunks)))
  }
}

historical_parcels <- rbindlist(records, use.names = TRUE, fill = TRUE)
if (nrow(historical_parcels) == 0) {
  stop("Historical parcel query returned no records for the missing PINs.", call. = FALSE)
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

if (anyDuplicated(historical_parcels[, .(pin, year)]) > 0) {
  stop("Historical parcel endpoint returned duplicate PIN-year rows.", call. = FALSE)
}
setorder(historical_parcels, pin, year)
fwrite(historical_parcels, "../output/historical_parcel_records.csv")
