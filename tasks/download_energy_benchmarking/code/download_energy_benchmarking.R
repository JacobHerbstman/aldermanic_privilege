# setwd("tasks/download_energy_benchmarking/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

# City of Chicago Energy Benchmarking (dataset xq83-jr8c): one row per building of 50,000 square feet or more and
# reporting year, with the reported year built, property type, floor area and location.
query <- function(format, ...) {
  httr2::request(paste0("https://data.cityofchicago.org/resource/xq83-jr8c.", format)) |> httr2::req_url_query(...) |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |> httr2::req_timeout(300) |> httr2::req_perform()
}
fields <- paste("row_id, data_year, id, property_name, reporting_status, address, zip_code, community_area,",
  "primary_property_type, gross_floor_area_buildings_sq_ft, year_built, of_buildings, latitude, longitude")
n <- as.integer(httr2::resp_body_json(query("json", `$select` = "count(*) as n"))[[1]]$n)
buildings <- query("csv", `$select` = fields, `$limit` = 1000000L) |> httr2::resp_body_string() |>
  read_csv(col_types = cols(.default = col_character())) |> arrange(data_year, id)
stopifnot(n < 1000000L, nrow(buildings) == n, buildings$row_id == paste0(buildings$data_year, "-", buildings$id))
SaveData(buildings, "row_id", "../output/energy_benchmarking.csv")
