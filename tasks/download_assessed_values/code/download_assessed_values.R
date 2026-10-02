# setwd("tasks/download_assessed_values/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

townships <- as.character(70:77)  # the eight Chicago townships

# Cook County Assessor assessed values (dataset uzyt-m557): one row per parcel and year, every class.
resource <- "https://datacatalog.cookcountyil.gov/resource/uzyt-m557"
query <- function(format, ...) {
  httr2::request(paste0(resource, ".", format)) |> httr2::req_url_query(...) |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |> httr2::req_timeout(300) |> httr2::req_perform()
}
updated <- function() {
  httr2::request("https://datacatalog.cookcountyil.gov/api/views/uzyt-m557.json") |> httr2::req_perform() |>
    httr2::resp_body_json() |> purrr::pluck("rowsUpdatedAt")
}
updated_before <- updated()

# One request per township and year returns all of its rows; its row count must equal the API's count, and
# each township's rows must add up to its total. The source writes some years as "2026.0" and others as "2026".
value_columns <- paste0(rep(c("mailed", "certified", "board"), each = 4), "_", c("bldg", "land", "tot", "hie"))
count_rows <- function(where) as.integer(httr2::resp_body_json(query("json", `$select` = "count(*) as n", `$where` = where))[[1]]$n)
pages <- list()
for (t in townships) {
  township_rows <- count_rows(sprintf("township_code = '%s'", t))
  downloaded <- 0L
  year_values <- query("json", `$select` = "distinct year", `$where` = sprintf("township_code = '%s'", t)) |>
    httr2::resp_body_json() |> purrr::map_chr("year")
  for (y in sort(year_values)) {
    where <- sprintf("township_code = '%s' and year = '%s'", t, y)
    n <- count_rows(where)
    page <- query("csv", `$where` = where, `$limit` = 1000000L) |> httr2::resp_body_string() |>
      read_csv(col_types = cols(.default = col_character(), !!!setNames(rep(list(col_double()), 12), value_columns)))
    stopifnot(n < 1000000L, nrow(page) == n, nrow(problems(page)) == 0L, as.numeric(page$year) == as.numeric(y))
    pages[[length(pages) + 1L]] <- page
    downloaded <- downloaded + nrow(page)
    message(sprintf("township %s, %s: %d rows", t, y, n))
  }
  stopifnot(downloaded == township_rows)
}
stopifnot(updated() == updated_before)  # the source did not change during the download

values <- bind_rows(pages)
stopifnot(grepl("^[0-9]{4}([.]0)?$", values$year))
values <- values |> mutate(year = as.integer(sub("[.]0$", "", year))) |> arrange(pin, year)
stopifnot(all(grepl("^[0-9]{14}$", values$pin)), substr(values$row_id, 1, 14) == values$pin,
  !anyDuplicated(values[c("pin", "year")]), all(values$township_code %in% townships))
SaveData(values, c("pin", "year"), "../output/assessed_values.parquet")
