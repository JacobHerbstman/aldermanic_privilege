# setwd("tasks/download_census_building_permits/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

years <- 1990:2025

# Census Bureau Building Permits Survey, annual Midwest place files: Chicago's permitted buildings, units and
# value by building size. The leading geographic columns differ across years; the last 24 fields are always the
# imputed and then the reported-only counts for 1-unit, 2-unit, 3-4-unit and 5+-unit buildings, after the place
# name. Chicago is the Illinois (state 17) place named "Chicago"; place IDs were reassigned in the early 1990s.
groups <- c("units_1", "units_2", "units_3_4", "units_5_plus")
value_names <- c(paste0(rep(groups, each = 3), "_", c("bldgs", "units", "value")), paste0(rep(groups, each = 3), "_", c("bldgs", "units", "value"), "_reported"))
permits <- map(years, \(y) {
  lines <- httr2::request(sprintf("https://www2.census.gov/econ/bps/Place/Midwest%%20Region/mw%da.txt", y)) |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE) |> httr2::req_timeout(300) |> httr2::req_perform() |>
    httr2::resp_body_string() |> strsplit("\r?\n") |> unlist()
  header1 <- strsplit(lines[1], ",")[[1]]; header2 <- strsplit(lines[2], ",")[[1]]
  stopifnot(trimws(tail(header1[header1 != ""], 8)) == c("1-unit", "2-units", "3-4 units", "5+ units", "1-unit rep", "2-units rep", "3-4 units rep", "5+ units rep"),
    tail(header2, 24) == rep(c("Bldgs", "Units", "Value"), 8))
  rows <- strsplit(lines[-(1:2)], ",")
  chicago <- rows[vapply(rows, \(r) length(r) > 25 && r[2] == "17" && trimws(r[length(r) - 24]) == "Chicago", TRUE)]
  stopifnot(length(chicago) == 1L)
  r <- chicago[[1]]
  tibble(year = y, months_reported = as.integer(r[which(trimws(header2) == "Months Rep")]),
    !!!setNames(as.list(as.numeric(tail(r, 24))), value_names))
}) |> bind_rows()
stopifnot(!anyNA(permits))
SaveData(permits, "year", "../output/chicago_building_permits_survey.csv")
