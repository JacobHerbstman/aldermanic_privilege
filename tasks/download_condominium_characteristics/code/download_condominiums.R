# setwd("tasks/download_condominium_characteristics/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

first_year_built <- 2000L  # condominium buildings reported built from this year on

# Cook County Assessor condominium characteristics (dataset 3r7i-mrz4), Chicago townships 70-77.
query <- function(...) {
  httr2::request("https://datacatalog.cookcountyil.gov/resource/3r7i-mrz4.csv") |>
    httr2::req_url_query(...) |> httr2::req_retry(max_tries = 5) |> httr2::req_timeout(600) |>
    httr2::req_perform() |> httr2::resp_body_string() |>
    read_csv(col_types = cols(.default = col_character()))
}
chicago <- "township_code in ('70','71','72','73','74','75','76','77')"
fields <- paste("row_id, pin, pin10, year, class, char_yrblt, char_building_sf, char_unit_sf, char_land_sf,",
  "char_building_pins, char_building_non_units, is_parking_space, is_common_area, bldg_is_mixed_use")

# Buildings with any record reporting construction in or after the first year, then all of their records.
buildings <- query(`$select` = "distinct pin10", `$where` = sprintf("%s and char_yrblt >= %d", chicago, first_year_built),
  `$order` = "pin10", `$limit` = 50000L)
stopifnot(nrow(buildings) > 0L, nrow(buildings) < 50000L, all(grepl("^[0-9]{10}$", buildings$pin10)))

records <- list()
for (bases in split(buildings$pin10, ceiling(seq_len(nrow(buildings)) / 200L))) {
  offset <- 0L
  repeat {
    page <- query(`$select` = fields, `$where` = sprintf("pin10 in ('%s')", paste(bases, collapse = "','")),
      `$order` = "row_id", `$limit` = 50000L, `$offset` = offset)
    records[[length(records) + 1L]] <- page
    if (nrow(page) < 50000L) break
    offset <- offset + 50000L
  }
}
records <- bind_rows(records) |> arrange(pin, year, row_id)
stopifnot(all(records$pin10 %in% buildings$pin10), !anyDuplicated(records[c("pin", "year")]))
SaveData(records, "row_id", "../output/condominium_characteristics.csv")
