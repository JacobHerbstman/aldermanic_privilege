# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_rent_data/code")
# start_date <- "2014-01-01"
# end_date <- "2022-12-31"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) {
  cli_args <- c(start_date, end_date)
}
if (length(cli_args) != 2) {
  stop("Usage: Rscript download_rent_data.R <start_date> <end_date>", call. = FALSE)
}

start_date <- as.Date(cli_args[[1]])
end_date <- as.Date(cli_args[[2]])
if (is.na(start_date) || is.na(end_date) || end_date < start_date) {
  stop("The RentHub date range is invalid.", call. = FALSE)
}

product_id <- "prj_4ufibhzc__cdst_wuxt87zew9ektcpt"
product_path <- sprintf("https://api.deweydata.io/api/v1/external/data/%s", product_id)

dewey_api_key <- Sys.getenv("DEWEY_API_KEY")
if (!nzchar(dewey_api_key)) {
  stop("DEWEY_API_KEY is not set; cannot download RentHub data.", call. = FALSE)
}

if (!requireNamespace("deweydatar", quietly = TRUE)) {
  stop("Package deweydatar is required to download RentHub data.", call. = FALSE)
}

message("Reading Dewey RentHub file list...")
file_list <- tryCatch(
  deweydatar::get_file_list(
    apikey = dewey_api_key,
    product_path = product_path,
    start_date = start_date,
    end_date = end_date
  ),
  error = function(e) {
    stop(sprintf("Dewey file list request failed: %s", conditionMessage(e)), call. = FALSE)
  }
)
if (!is.data.frame(file_list) || nrow(file_list) == 0) {
  stop("Dewey returned no RentHub files for the requested dates.", call. = FALSE)
}

message("Downloading RentHub data...")
download_error <- NULL
for (attempt in seq_len(5L)) {
  download_error <- tryCatch(
    {
      deweydatar::download_files(
        files_df = file_list,
        dest_folder = "../output/",
        skip_exists = TRUE
      )
      NULL
    },
    error = function(e) e
  )
  if (is.null(download_error)) {
    break
  }
  if (attempt < 5L) {
    message(sprintf(
      "Dewey download attempt %s failed: %s. Retrying without redownloading completed files...",
      attempt,
      conditionMessage(download_error)
    ))
    Sys.sleep(10)
  }
}
if (!is.null(download_error)) {
  stop(sprintf(
    "Dewey file download failed after 5 attempts: %s",
    conditionMessage(download_error)
  ), call. = FALSE)
}

if (anyDuplicated(file_list$file_name)) {
  stop("Dewey returned duplicate RentHub file names.", call. = FALSE)
}

expected_files <- file.path("../output", file_list$file_name)
missing_files <- !file.exists(expected_files)
if (any(missing_files)) {
  stop(
    sprintf(
      "RentHub download is incomplete. Missing %s requested files, including %s.",
      sum(missing_files),
      paste(head(file_list$file_name[missing_files], 3), collapse = ", ")
    ),
    call. = FALSE
  )
}

expected_sizes <- parse_double(as.character(file_list$file_size_bytes))
if (any(is.na(expected_sizes))) {
  stop("Dewey returned an invalid RentHub file size.", call. = FALSE)
}

downloaded_sizes <- file.info(expected_files)$size
wrong_sizes <- is.na(downloaded_sizes) |
  downloaded_sizes != expected_sizes
if (any(wrong_sizes)) {
  stop(
    sprintf(
      "RentHub download contains %s files with unexpected byte sizes, including %s.",
      sum(wrong_sizes),
      paste(head(file_list$file_name[wrong_sizes], 3), collapse = ", ")
    ),
    call. = FALSE
  )
}

manifest <- tibble(
  source_date = as.Date(file_list$partition_key),
  file_name = file_list$file_name,
  file_size_bytes = expected_sizes,
  source_modified_utc = file_list$modified_at,
  md5 = unname(tools::md5sum(expected_files))
) |>
  arrange(source_date, file_name)

if (any(is.na(manifest$source_date)) || any(is.na(manifest$md5) | manifest$md5 == "")) {
  stop("Failed to validate one or more RentHub files.", call. = FALSE)
}

write_csv(manifest, "../output/renthub_manifest.csv")
message(sprintf("RentHub data ready: %s verified parquet files.", nrow(manifest)))
