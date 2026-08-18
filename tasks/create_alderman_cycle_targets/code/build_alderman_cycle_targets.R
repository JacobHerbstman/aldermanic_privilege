# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/create_alderman_cycle_targets/code")

source("../../setup_environment/code/packages.R")

make_alderman_id <- function(name) {
  out <- tolower(iconv(name, to = "ASCII//TRANSLIT"))
  out <- gsub("[^a-z0-9]+", "_", out)
  out <- gsub("_+", "_", out)
  gsub("^_|_$", "", out)
}

cycle_targets <- data.table::data.table(
  cycle_year = c(2003L, 2007L, 2011L, 2015L, 2019L, 2023L),
  cycle_target_month = c("Jun 2003", "Jun 2007", "Jun 2011", "Jun 2015", "Jun 2019", "Jun 2023"),
  cycle_target_date = as.Date(c(
    "2003-06-01", "2007-06-01", "2011-06-01",
    "2015-06-01", "2019-06-01", "2023-06-01"
  )),
  cycle_start_date = as.Date(c(
    "1999-06-01", "2003-06-01", "2007-06-01",
    "2011-06-01", "2015-06-01", "2019-06-01"
  )),
  cycle_end_date = as.Date(c(
    "2003-05-31", "2007-05-31", "2011-05-31",
    "2015-05-31", "2019-05-31", "2023-05-31"
  ))
)

panel <- data.table::fread("../input/chicago_alderman_panel.csv")
required_columns <- c("month", "ward", "alderman")
if (!all(required_columns %in% names(panel))) {
  stop("chicago_alderman_panel.csv must contain month, ward, and alderman.", call. = FALSE)
}
if (anyDuplicated(cycle_targets, by = "cycle_target_month") > 0) {
  stop("Cycle target months must be unique.", call. = FALSE)
}
if (anyDuplicated(panel, by = c("month", "ward")) > 0) {
  stop("Alderman panel must be unique by month and ward.", call. = FALSE)
}

targets <- panel[month %in% cycle_targets$cycle_target_month]
targets <- merge(targets, cycle_targets, by.x = "month", by.y = "cycle_target_month", all.x = TRUE, sort = FALSE)
data.table::setnames(targets, "month", "cycle_target_month")
targets[, ward_number := suppressWarnings(as.integer(ward))]
targets[, full_name := trimws(as.character(alderman))]
targets[, alderman_id := make_alderman_id(full_name)]

if (any(is.na(targets$ward_number)) || any(targets$full_name == "") || any(targets$alderman_id == "")) {
  stop("Cycle target panel has missing ward, name, or alderman_id values.", call. = FALSE)
}

target_counts <- targets[, .(n_targets = .N, n_wards = data.table::uniqueN(ward_number)), by = cycle_year]
bad_counts <- target_counts[n_targets != 50L | n_wards != 50L]
if (nrow(bad_counts) > 0) {
  stop(
    sprintf("Each cycle must have 50 unique wards; bad cycles: %s", paste(bad_counts$cycle_year, collapse = ", ")),
    call. = FALSE
  )
}
if (anyDuplicated(targets, by = c("cycle_year", "ward_number")) > 0) {
  stop("Cycle targets must be unique by cycle_year and ward_number.", call. = FALSE)
}

targets <- targets[
  order(cycle_year, ward_number),
  .(
    cycle_year,
    cycle_start_date,
    cycle_end_date,
    cycle_target_date,
    ward_number,
    alderman_id,
    full_name
  )
]

data.table::fwrite(targets, "../output/alderman_cycle_targets.csv")
