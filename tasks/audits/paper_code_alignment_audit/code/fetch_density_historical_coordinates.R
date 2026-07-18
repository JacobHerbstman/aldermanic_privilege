# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

cases <- read_csv(
  "../output/density_historical_coordinate_cases.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)
missing_pins <- sort(unique(cases$pin))
if (length(missing_pins) == 0) {
  stop("No density PINs require historical coordinate recovery.", call. = FALSE)
}
if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical parcel audit.", call. = FALSE)
}

chunks <- split(missing_pins, ceiling(seq_along(missing_pins) / 50L))
records <- vector("list", length(chunks))
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"

for (i in seq_along(chunks)) {
  parameters <- c(
    "$select" = "pin,pin10,year,class,lon,lat,x_3435,y_3435,misc_subdivision_id",
    "$where" = sprintf(
      "year between 1999 and 2025 and pin in(%s)",
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
    stop(sprintf("Historical parcel query failed for chunk %d of %d.", i, length(chunks)), call. = FALSE)
  }

  payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
  if (is.data.frame(payload) && nrow(payload) > 0) {
    records[[i]] <- as_tibble(payload)
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
    subdivision_id = as.character(misc_subdivision_id)
  ) %>%
  filter(is.finite(longitude), is.finite(latitude)) %>%
  distinct(pin, year, .keep_all = TRUE) %>%
  arrange(pin, year)

if (anyDuplicated(historical[c("pin", "year")]) > 0) {
  stop("Historical parcel endpoint returned duplicate PIN-year coordinates.", call. = FALSE)
}
write_csv(historical, "../output/density_historical_parcel_records.csv")
