# Download FRED CPI series for explicit downstream price deflation.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_fred_cpi/code")
# series_id <- "CUURA207SA0"
source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(series_id)
}
if (length(cli_args) != 1) {
  stop("Expected argument: series_id.", call. = FALSE)
}

series_id <- cli_args[1]
output_csv <- "../output/fred_cpi_cuura207sa0.csv"
metadata_csv <- "../output/fred_cpi_cuura207sa0_metadata.csv"

fred_url <- sprintf("https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s", series_id)
message(sprintf("Downloading FRED series %s", series_id))

old_http_ua <- getOption("HTTPUserAgent")
on.exit(options(HTTPUserAgent = old_http_ua), add = TRUE)
options(HTTPUserAgent = "curl/8.0.0")

cpi_raw <- readr::read_csv(fred_url, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
if (!all(c("observation_date", series_id) %in% names(cpi_raw))) {
  stop(sprintf("FRED response missing expected columns for series %s.", series_id), call. = FALSE)
}

cpi <- cpi_raw %>%
  dplyr::transmute(
    observation_date = as.Date(observation_date),
    value = suppressWarnings(as.numeric(.data[[series_id]]))
  ) %>%
  dplyr::filter(!is.na(observation_date)) %>%
  dplyr::arrange(observation_date)

if (nrow(cpi) == 0 || !any(is.finite(cpi$value))) {
  stop(sprintf("FRED series %s has no usable numeric observations.", series_id), call. = FALSE)
}

names(cpi)[names(cpi) == "value"] <- series_id

readr::write_csv(cpi, output_csv)

readr::write_csv(
  tibble::tibble(
        series_id = series_id,
        source_url = fred_url,
        downloaded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        rows = nrow(cpi),
        nonmissing_rows = sum(is.finite(cpi[[series_id]])),
        missing_rows = sum(!is.finite(cpi[[series_id]])),
        min_observation_date = min(cpi$observation_date),
        max_observation_date = max(cpi$observation_date),
        min_nonmissing_observation_date = min(cpi$observation_date[is.finite(cpi[[series_id]])]),
        max_nonmissing_observation_date = max(cpi$observation_date[is.finite(cpi[[series_id]])])
      ),
      metadata_csv
    )
