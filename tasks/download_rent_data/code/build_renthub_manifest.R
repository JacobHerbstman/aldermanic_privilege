# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_rent_data/code")

source("../../setup_environment/code/packages.R")

parquet_files <- sort(list.files(
  "../output",
  pattern = "\\.parquet$",
  full.names = TRUE
))
if (length(parquet_files) == 0) {
  stop("No RentHub parquet files found; run the download task first.", call. = FALSE)
}

file_details <- file.info(parquet_files)
if (any(is.na(file_details$size)) || any(file_details$size <= 0)) {
  stop("RentHub contains a missing or empty parquet file.", call. = FALSE)
}

manifest <- tibble(
  file_name = basename(parquet_files),
  file_size_bytes = as.numeric(file_details$size),
  modified_utc = format(
    file_details$mtime,
    format = "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  ),
  md5 = unname(tools::md5sum(parquet_files))
)
if (any(is.na(manifest$md5) | manifest$md5 == "")) {
  stop("Failed to hash one or more RentHub parquet files.", call. = FALSE)
}

write_csv(manifest, "../output/renthub_manifest.csv")
message(sprintf("Recorded %s RentHub parquet files.", nrow(manifest)))
