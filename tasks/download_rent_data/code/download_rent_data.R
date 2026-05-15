# Download RentHub files from Dewey.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_rent_data/code")

source("../../setup_environment/code/packages.R")

if (!requireNamespace("deweydatar", quietly = TRUE)) {
  stop("Package deweydatar is required to download RentHub data.", call. = FALSE)
}

dewey_api_key <- Sys.getenv("DEWEY_API_KEY")
if (!nzchar(dewey_api_key)) {
  stop("DEWEY_API_KEY is not set; cannot download RentHub data.", call. = FALSE)
}

product_path <- "https://api.deweydata.io/api/v1/external/data/cdst_uytkqpdft8tbhbhc"

file_list <- tryCatch(
  deweydatar::get_file_list(
    apikey = dewey_api_key,
    product_path = product_path
  ),
  error = function(e) {
    stop(sprintf("Dewey file list request failed: %s", conditionMessage(e)), call. = FALSE)
  }
)

if (!is.data.frame(file_list) || nrow(file_list) == 0) {
  stop("Dewey returned no RentHub files.", call. = FALSE)
}

manifest <- file_list %>%
  dplyr::mutate(dplyr::across(where(is.list), as.character))
readr::write_csv(manifest, "../output/renthub_file_manifest.csv")

tryCatch(
  deweydatar::download_files(
    files_df = file_list,
    dest_folder = "../output/"
  ),
  error = function(e) {
    stop(sprintf("Dewey file download failed: %s", conditionMessage(e)), call. = FALSE)
  }
)

parquet_files <- list.files("../output", pattern = "\\.parquet$", full.names = TRUE)
if (length(parquet_files) == 0) {
  stop("Dewey download completed but no parquet files were written.", call. = FALSE)
}

readr::write_csv(
  tibble::tibble(
    product_path = product_path,
    downloaded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    manifest_rows = nrow(file_list),
    parquet_files = length(parquet_files),
    total_bytes = sum(file.info(parquet_files)$size, na.rm = TRUE)
  ),
  "../output/renthub_download_metadata.csv"
)

file.create("../output/download.done")
