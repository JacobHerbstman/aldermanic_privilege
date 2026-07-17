# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical parcel audit.", call. = FALSE)
}

buildings <- read_csv(
  "../output/density_historical_building_universe.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  select(pin, construction_year)

if (anyDuplicated(buildings[c("pin", "construction_year")]) > 0) {
  stop("Historical parcel requests are not unique by PIN and construction year.", call. = FALSE)
}

request_groups <- buildings %>%
  arrange(construction_year, pin) %>%
  group_by(construction_year) %>%
  group_split()

base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"
records <- list()
request_number <- 0L
total_requests <- sum(vapply(request_groups, function(x) ceiling(nrow(x) / 100), numeric(1)))

for (year_rows in request_groups) {
  construction_year <- unique(year_rows$construction_year)
  pin_chunks <- split(year_rows$pin, ceiling(seq_len(nrow(year_rows)) / 100L))

  for (pin_chunk in pin_chunks) {
    request_number <- request_number + 1L
    parameters <- c(
      "$select" = "pin,pin10,year,class,lon,lat,x_3435,y_3435,misc_subdivision_id,row_id",
      "$where" = sprintf(
        "year=%d and pin in(%s)",
        construction_year,
        paste(sprintf("'%s'", pin_chunk), collapse = ",")
      ),
      "$order" = "pin",
      "$limit" = "5000"
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
    for (attempt in 1:5) {
      response <- tryCatch(curl::curl_fetch_memory(query), error = function(e) NULL)
      if (!is.null(response) && response$status_code == 200L) {
        break
      }
      Sys.sleep(attempt)
    }
    if (is.null(response) || response$status_code != 200L) {
      stop(
        sprintf("Historical parcel query failed for request %d of %d.", request_number, total_requests),
        call. = FALSE
      )
    }

    payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
    if (is.data.frame(payload) && nrow(payload) > 0) {
      records[[length(records) + 1L]] <- as_tibble(payload)
    }

    if (request_number %% 10L == 0L || request_number == total_requests) {
      message(sprintf("Historical parcel requests: %d of %d", request_number, total_requests))
    }
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
  anti_join(buildings, by = c("pin", "year" = "construction_year"))
if (nrow(unexpected_records) > 0) {
  stop("Historical Parcel Universe returned unrequested PIN-year records.", call. = FALSE)
}

historical %>%
  arrange(pin, year) %>%
  write_csv("../output/density_historical_exact_parcel_records.csv")
