# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")
# start_year <- 2006
# end_year <- 2022
# candidate_bandwidth_ft <- 1000

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_year, end_year, candidate_bandwidth_ft)
}
if (length(cli_args) != 3) {
  stop("Script requires 3 arguments: <start_year> <end_year> <candidate_bandwidth_ft>.", call. = FALSE)
}

start_year <- suppressWarnings(as.integer(cli_args[1]))
end_year <- suppressWarnings(as.integer(cli_args[2]))
candidate_bandwidth_ft <- suppressWarnings(as.numeric(cli_args[3]))
if (!is.finite(start_year) || !is.finite(end_year) || start_year > end_year) {
  stop("start_year and end_year must be valid integers with start_year <= end_year.", call. = FALSE)
}
if (!is.finite(candidate_bandwidth_ft) || candidate_bandwidth_ft <= 500) {
  stop("candidate_bandwidth_ft must exceed the 500-foot estimation bandwidth.", call. = FALSE)
}
if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical coordinate audit.", call. = FALSE)
}

sales <- read_parquet("../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet") %>%
  as_tibble() %>%
  transmute(
    pin = as.character(pin),
    year = suppressWarnings(as.integer(year)),
    dist_ft_current = as.numeric(dist_m) / 0.3048
  ) %>%
  filter(
    year >= start_year,
    year <= end_year,
    is.finite(dist_ft_current),
    dist_ft_current <= candidate_bandwidth_ft
  )

current_pins <- fread(
  "../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv",
  select = "pin",
  colClasses = list(character = "pin")
) %>%
  as_tibble() %>%
  transmute(
    pin = gsub("[^0-9]", "", trimws(pin)),
    pin = if_else(nchar(pin) == 13L, paste0("0", pin), pin)
  ) %>%
  distinct(pin)

candidate_keys <- sales %>%
  semi_join(current_pins, by = "pin") %>%
  distinct(pin, year)
candidate_pins <- sort(unique(candidate_keys$pin))
if (length(candidate_pins) == 0) {
  stop("No current parcel PINs were found in the sales audit screening sample.", call. = FALSE)
}

chunks <- split(candidate_pins, ceiling(seq_along(candidate_pins) / 200L))
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
  for (attempt in 1:5) {
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
    records[[i]] <- as_tibble(payload)
  }
  if (i %% 25L == 0L || i == length(chunks)) {
    message(sprintf("Downloaded chunk %d of %d.", i, length(chunks)))
  }
  Sys.sleep(0.05)
}

historical <- bind_rows(records) %>%
  transmute(
    pin = as.character(pin),
    year = suppressWarnings(as.integer(year)),
    longitude_historical = suppressWarnings(as.numeric(lon)),
    latitude_historical = suppressWarnings(as.numeric(lat)),
    x_3435_historical = suppressWarnings(as.numeric(x_3435)),
    y_3435_historical = suppressWarnings(as.numeric(y_3435))
  ) %>%
  filter(
    nchar(pin) == 14L,
    year >= start_year,
    year <= end_year,
    is.finite(longitude_historical),
    is.finite(latitude_historical),
    is.finite(x_3435_historical),
    is.finite(y_3435_historical)
  ) %>%
  arrange(pin, year)

if (nrow(historical) == 0) {
  stop("The historical parcel endpoint returned no usable coordinates.", call. = FALSE)
}
if (anyDuplicated(historical[c("pin", "year")]) > 0) {
  stop("Historical parcel coordinates are not unique by PIN-year.", call. = FALSE)
}

candidate_coverage <- candidate_keys %>%
  left_join(
    historical %>% select(pin, year, longitude_historical),
    by = c("pin", "year"),
    relationship = "one-to-one"
  )

write_parquet(
  historical,
  sprintf(
    "../output/sales_historical_coordinates_bw%s_%d_%d.parquet",
    format(candidate_bandwidth_ft, scientific = FALSE, trim = TRUE),
    start_year,
    end_year
  )
)

write_csv(
  tibble(
    extraction_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    start_year = start_year,
    end_year = end_year,
    candidate_bandwidth_ft = candidate_bandwidth_ft,
    screened_sale_rows = nrow(sales),
    candidate_current_pin_years = nrow(candidate_keys),
    candidate_current_pins = length(candidate_pins),
    request_chunks = length(chunks),
    fetched_pin_years = nrow(historical),
    fetched_pins = n_distinct(historical$pin),
    candidate_pin_years_with_exact_historical_coordinates = sum(!is.na(candidate_coverage$longitude_historical)),
    candidate_pin_years_missing_exact_historical_coordinates = sum(is.na(candidate_coverage$longitude_historical))
  ),
  "../output/sales_historical_coordinate_fetch_summary.csv"
)
