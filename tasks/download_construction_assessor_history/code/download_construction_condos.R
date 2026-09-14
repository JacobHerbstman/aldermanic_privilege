# setwd("tasks/download_construction_assessor_history/code")
library(dplyr)
library(readr)
queries <- read_csv("construction_condominium_queries.csv",
  col_types = cols(pin10 = col_character(), .default = col_skip())) |> distinct() |> arrange(pin10)
stopifnot(!anyNA(queries), all(grepl("^[0-9]{10}$", queries$pin10)))
records <- list()
for (bases in split(queries$pin10, ceiling(seq_len(nrow(queries)) / 20L))) {
  body <- httr2::request("https://datacatalog.cookcountyil.gov/resource/3r7i-mrz4.json") |>
    httr2::req_url_query(`$where` = paste0("year between 2021 and 2025 and pin10 in('",
      paste(bases, collapse = "','"), "')"), `$order` = "pin,year,row_id", `$limit` = 50000L) |>
    httr2::req_retry(max_tries = 5) |> httr2::req_timeout(180) |>
    httr2::req_perform() |> httr2::resp_body_string()
  rows <- jsonlite::fromJSON(body, simplifyVector = FALSE)
  stopifnot(is.list(rows), is.null(names(rows)), length(rows) < 50000L)
  batch <- bind_rows(rows)
  if (nrow(batch)) stopifnot(all(batch$pin10 %in% bases), all(as.integer(batch$year) %in% 2021:2025))
  records <- c(records, list(batch))
}
records <- bind_rows(records) |> arrange(pin, year, row_id)
stopifnot(nrow(records) > 0L, !anyNA(records$row_id), !anyDuplicated(records$row_id),
  !anyDuplicated(records[c("pin", "year")]))
write_csv(records, "../temp/construction_condominium_history_current.csv")
stopifnot(file.rename("../temp/construction_condominium_history_current.csv", "../output/construction_condominium_history_current.csv"))
