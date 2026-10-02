# setwd("tasks/download_rent_data/code")
# start_date <- "2014-01-01"
# end_date <- "2022-12-31"
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) cli_args <- c(start_date, end_date)
stopifnot(length(cli_args) == 2L)
start_date <- as.Date(cli_args[1])
end_date <- as.Date(cli_args[2])
stopifnot(!anyNA(c(start_date, end_date)), start_date <= end_date)

recorded <- read_csv("../input/renthub_manifest.csv", show_col_types = FALSE,
  col_types = cols(source_date = col_date(), file_name = col_character(),
    file_size_bytes = col_double(), source_modified_utc = col_character(), md5 = col_character()))
stopifnot(nrow(recorded) > 0, !anyDuplicated(recorded$file_name),
  all(recorded$source_date >= start_date & recorded$source_date <= end_date))

# Resuming an acquisition may reuse only files that match the recorded bytes.
recorded_files <- file.path("../output", recorded$file_name)
present <- file.exists(recorded_files)
if (any(present)) {
  stopifnot(all(unname(tools::md5sum(recorded_files[present])) == recorded$md5[present]))
}
pending <- recorded[!present, ]
if (nrow(pending) > 0) {
  dewey_api_key <- Sys.getenv("DEWEY_API_KEY")
  if (!nzchar(dewey_api_key)) stop("DEWEY_API_KEY is required to retrieve the missing RentHub files.")
  available <- deweydatar::get_file_list(
    apikey = dewey_api_key,
    product_path = "https://api.deweydata.io/api/v1/external/data/prj_4ufibhzc__cdst_wuxt87zew9ektcpt",
    start_date = start_date, end_date = end_date
  )
  stopifnot(!anyDuplicated(available$file_name))
  matches <- match(pending$file_name, available$file_name)
  if (anyNA(matches)) stop("Dewey no longer lists some recorded RentHub files; restore the recorded archive.")
  available <- available[matches, , drop = FALSE]
  stopifnot(all(as.numeric(available$file_size_bytes) == pending$file_size_bytes))

  # One request per file; Dewey's download server returns occasional transient errors, which are retried.
  for (i in seq_len(nrow(pending))) {
    received <- file.path("../temp", pending$file_name[i])
    httr2::request(available$link[i]) |>
      httr2::req_retry(
        max_tries = 8,
        is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504),
        backoff = function(attempt) 15 * attempt
      ) |>
      httr2::req_perform(path = received)
    stopifnot(file.size(received) == pending$file_size_bytes[i], unname(tools::md5sum(received)) == pending$md5[i])
    stopifnot(file.rename(received, file.path("../output", pending$file_name[i])))
  }
}
stopifnot(all(unname(tools::md5sum(recorded_files)) == recorded$md5))

stopifnot(file.copy("../input/renthub_manifest.csv", "../output/renthub_manifest.csv", overwrite = TRUE))
ReportData("../output/renthub_manifest.csv", "file_name")
