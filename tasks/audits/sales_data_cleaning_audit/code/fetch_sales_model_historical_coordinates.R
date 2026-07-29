# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_data_cleaning_audit/code")

source("../../../setup_environment/code/packages.R")

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical coordinate audit.", call. = FALSE)
}

sale_keys <- read_parquet("../input/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  mutate(
    pin = gsub("[^0-9]", "", trimws(as.character(pin))),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin)
  ) %>%
  filter(
    year >= 2006,
    year <= 2022,
    nchar(pin) == 14L
  ) %>%
  distinct(pin, year)

if (any(nchar(sale_keys$pin) != 14L)) {
  stop("Historical coordinate audit contains an invalid full PIN.", call. = FALSE)
}

records <- list()
base_url <- "https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"

for (year_i in sort(unique(sale_keys$year))) {
  year_pins <- sale_keys %>%
    filter(year == year_i) %>%
    pull(pin)
  chunks <- split(year_pins, ceiling(seq_along(year_pins) / 200L))

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

    response <- NULL
    for (attempt in 1:3) {
      response <- tryCatch(curl::curl_fetch_memory(query), error = function(e) NULL)
      if (!is.null(response) && response$status_code == 200L) {
        break
      }
      Sys.sleep(attempt)
    }
    if (is.null(response) || response$status_code != 200L) {
      stop(sprintf(
        "Historical coordinate request failed for year %d, chunk %d of %d.",
        year_i,
        chunk_i,
        length(chunks)
      ), call. = FALSE)
    }

    payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
    if (is.data.frame(payload) && nrow(payload) > 0) {
      records[[length(records) + 1L]] <- as_tibble(payload)
    }
  }
}

historical <- bind_rows(records) %>%
  transmute(
    pin = as.character(pin),
    year = suppressWarnings(as.integer(year)),
    historical_longitude = suppressWarnings(as.numeric(lon)),
    historical_latitude = suppressWarnings(as.numeric(lat)),
    historical_x_3435 = suppressWarnings(as.numeric(x_3435)),
    historical_y_3435 = suppressWarnings(as.numeric(y_3435))
  ) %>%
  filter(
    paste(pin, year, sep = "\r") %in% paste(sale_keys$pin, sale_keys$year, sep = "\r")
  )

if (anyDuplicated(historical[c("pin", "year")]) > 0) {
  stop("Historical coordinates are not unique by full PIN and year.", call. = FALSE)
}

write_csv(historical, "../output/sales_model_historical_coordinates.csv")
