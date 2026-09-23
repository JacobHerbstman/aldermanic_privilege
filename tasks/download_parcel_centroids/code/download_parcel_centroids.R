# setwd("tasks/download_parcel_centroids/code")
# first_year <- 1999
# last_year <- 2025
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(first_year, last_year)
stopifnot(length(args) == 2L)
first_year <- as.integer(args[1])
last_year <- as.integer(args[2])

# Cook County Assessor parcel universe (dataset nj4t-kc8j), Chicago triad: one row per 10-digit parcel number ever
# assessed in the years, with its mean centroid, so parcels retired by later condominium declarations or
# subdivisions keep a location. One query per 3-digit parcel-number prefix keeps each response small.
centroids <- map(sprintf("%03d", 0:339), \(prefix) {
  httr2::request("https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.csv") |>
    httr2::req_url_query(
      `$select` = "pin10, min(year) AS first_year, max(year) AS last_year, avg(x_3435::number) AS x_3435, avg(y_3435::number) AS y_3435",
      `$where` = sprintf("triad_name = 'City' AND year BETWEEN '%d' AND '%d' AND starts_with(pin10, '%s')", first_year, last_year, prefix),
      `$group` = "pin10", `$order` = "pin10", `$limit` = 500000L) |>
    httr2::req_retry(max_tries = 5) |> httr2::req_timeout(600) |> httr2::req_perform() |> httr2::resp_body_string() |>
    read_csv(col_types = cols(pin10 = "c", first_year = "i", last_year = "i", x_3435 = "d", y_3435 = "d"))
}) |> bind_rows()
stopifnot(nrow(centroids) > 500000L, !anyDuplicated(centroids$pin10), all(grepl("^[0-9]{10}$", centroids$pin10)),
  all(between(centroids$first_year, first_year, last_year)), all(between(centroids$last_year, first_year, last_year)))
SaveData(centroids, "pin10", "../output/parcel_centroids.csv")
