# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("The curl R package is required for the historical address audit.", call. = FALSE)
}

targets <- read_csv(
  "../output/density_parcel_address_targets.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
)

pin_chunks <- split(targets$pin, ceiling(seq_len(nrow(targets)) / 50L))
base_url <- "https://datacatalog.cookcountyil.gov/resource/3723-97qp.json"
records <- list()

for (i in seq_along(pin_chunks)) {
  parameters <- c(
    "$select" = paste(
      "pin,pin10,year,prop_address_full,prop_address_city_name,",
      "prop_address_state,prop_address_zipcode_1,row_id",
      sep = ""
    ),
    "$where" = sprintf(
      "year between 1999 and 2025 and pin in(%s)",
      paste(sprintf("'%s'", pin_chunks[[i]]), collapse = ",")
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
    stop(
      sprintf("Historical address query failed for chunk %d of %d.", i, length(pin_chunks)),
      call. = FALSE
    )
  }

  payload <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
  if (is.data.frame(payload) && nrow(payload) > 0) {
    records[[length(records) + 1L]] <- as_tibble(payload)
  }
  message(sprintf("Historical address chunks: %d of %d", i, length(pin_chunks)))
}

addresses <- bind_rows(records) %>%
  transmute(
    pin = as.character(pin),
    pin10 = as.character(pin10),
    year = suppressWarnings(as.integer(year)),
    property_address = as.character(prop_address_full),
    property_city = as.character(prop_address_city_name),
    property_state = as.character(prop_address_state),
    property_zip = as.character(prop_address_zipcode_1),
    row_id = as.character(row_id)
  )

if (anyDuplicated(addresses[c("pin", "year")]) > 0) {
  stop("Historical parcel addresses are not unique by PIN-year.", call. = FALSE)
}
if (nrow(anti_join(addresses, targets, by = "pin")) > 0) {
  stop("Historical address endpoint returned unrequested PINs.", call. = FALSE)
}

addresses %>%
  arrange(pin, year) %>%
  write_csv("../output/density_historical_address_records.csv")
