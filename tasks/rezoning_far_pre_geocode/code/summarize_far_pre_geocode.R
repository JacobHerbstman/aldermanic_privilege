## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_far_pre_geocode/code")
#
## interactive argument examples (mirror Makefile inputs)
# date_tag <- "19990101_20260212"

source("../../setup_environment/code/packages.R")

parse_args <- function(args) {
  out <- list(date_tag = "19990101_20260212")
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (startsWith(arg, "--date-tag=")) {
      out$date_tag <- sub("^--date-tag=", "", arg)
    } else if (arg == "--date-tag" && i < length(args)) {
      i <- i + 1
      out$date_tag <- args[[i]]
    }
    i <- i + 1
  }
  out
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  temp_summary <- paste0("../temp/rezoning_far_pre_geocode_summary_", opts$date_tag, ".json")
  out_summary <- paste0("../output/rezoning_far_pre_geocode_summary_", opts$date_tag, ".json")

  if (file.exists(temp_summary)) {
    payload <- jsonlite::fromJSON(temp_summary, simplifyVector = FALSE)
  } else {
    unresolved_csv <- paste0("../output/far_unresolved_rows_", opts$date_tag, ".csv")
    far_csv <- paste0("../output/zoning_matters_far_", opts$date_tag, ".csv")
    unresolved <- readr::read_csv(unresolved_csv, show_col_types = FALSE)
    far_rows <- readr::read_csv(far_csv, show_col_types = FALSE)
    gate_rows <- unresolved |>
      dplyr::filter(.data$blocks_geocode_gate %in% c(TRUE, "TRUE", "True", "true"))
    payload <- list(
      rows_total = as.integer(nrow(far_rows)),
      unresolved_row_count = as.integer(nrow(unresolved)),
      parseable_non_structural_missing_count = as.integer(nrow(gate_rows)),
      generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  }

  jsonlite::write_json(payload, out_summary, pretty = TRUE, auto_unbox = TRUE, na = "null")
}

main()
