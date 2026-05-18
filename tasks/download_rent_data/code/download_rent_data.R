# Download the customized Illinois RentHub export from Dewey.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/download_rent_data/code")

source("../../setup_environment/code/packages.R")

dewey_api_key <- Sys.getenv("DEWEY_API_KEY")
if (!nzchar(dewey_api_key)) {
  stop("DEWEY_API_KEY is not set; cannot download RentHub data.", call. = FALSE)
}

product_id <- "prj_4ufibhzc__cdst_wuxt87zew9ektcpt"
product_path <- sprintf("https://api.deweydata.io/api/v1/external/data/%s", product_id)
download_method <- tolower(Sys.getenv("DEWEY_DOWNLOAD_METHOD", "auto"))
force_download <- identical(Sys.getenv("FORCE_RENT_DOWNLOAD"), "1")

if (!download_method %in% c("auto", "deweypy", "deweydatar")) {
  stop("DEWEY_DOWNLOAD_METHOD must be one of: auto, deweypy, deweydatar.", call. = FALSE)
}

uvx_path <- Sys.which("uvx")
use_deweypy <- download_method == "deweypy" || (download_method == "auto" && nzchar(uvx_path))
file_list <- tibble::tibble()
existing_parquet_files <- sort(list.files("../output", pattern = "\\.parquet$", full.names = TRUE))

if (use_deweypy) {
  if (!nzchar(uvx_path)) {
    stop("DEWEY_DOWNLOAD_METHOD=deweypy requires uvx on PATH.", call. = FALSE)
  }
  if (length(existing_parquet_files) > 0 && !force_download) {
    message(sprintf("Found %s existing RentHub parquet files; skipping DeweyPy download.", length(existing_parquet_files)))
  } else {
    message("Downloading RentHub data with DeweyPy speedy-download...")
    old_wd <- getwd()
    setwd("../output")
    on.exit(setwd(old_wd), add = TRUE)
    status <- system2(
      uvx_path,
      args = c(
        "--python", "3.13",
        "--from", "deweypy",
        "dewey",
        "--api-key", dewey_api_key,
        "speedy-download",
        product_id
      ),
      stdout = TRUE,
      stderr = TRUE
    )
    setwd(old_wd)
    exit_status <- attr(status, "status")
    if (!is.null(exit_status) && exit_status != 0) {
      stop(
        sprintf("DeweyPy speedy-download failed with exit status %s:\n%s", exit_status, paste(status, collapse = "\n")),
        call. = FALSE
      )
    }
  }
} else {
  if (!requireNamespace("deweydatar", quietly = TRUE)) {
    stop("Package deweydatar is required when uvx is unavailable or DEWEY_DOWNLOAD_METHOD=deweydatar.", call. = FALSE)
  }
  message("Reading Dewey RentHub file list...")
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

  if (length(existing_parquet_files) > 0 && !force_download) {
    message(sprintf("Found %s existing RentHub parquet files; skipping Dewey file download.", length(existing_parquet_files)))
  } else {
    message("Downloading RentHub data with deweydatar...")
    tryCatch(
      deweydatar::download_files(
        files_df = file_list,
        dest_folder = "../output/"
      ),
      error = function(e) {
        stop(sprintf("Dewey file download failed: %s", conditionMessage(e)), call. = FALSE)
      }
    )
  }
}

parquet_files <- sort(list.files("../output", pattern = "\\.parquet$", full.names = TRUE))
if (length(parquet_files) == 0) {
  stop("Dewey download completed but no parquet files were written.", call. = FALSE)
}
parquet_file_info <- file.info(parquet_files)

manifest <- if (nrow(file_list) > 0) {
  file_list %>%
    dplyr::mutate(dplyr::across(where(is.list), as.character))
} else {
  tibble::tibble(
    local_file = basename(parquet_files),
    source = "deweypy_speedy_download"
  )
}
readr::write_csv(manifest, "../output/renthub_file_manifest.csv")

local_file_audit <- tibble::tibble(
  local_file = basename(parquet_files),
  local_path = parquet_files,
  file_date = as.Date(substr(basename(parquet_files), 1, 10)),
  file_size_bytes = parquet_file_info$size,
  modified_time = as.character(parquet_file_info$mtime)
) %>%
  arrange(local_file)

read_parquet_schema <- function(path) {
  schema_names <- tryCatch(
    names(arrow::open_dataset(path, format = "parquet")$schema),
    error = function(e) character()
  )
  if (length(schema_names) == 0) {
    return(tibble::tibble(local_file = basename(path), column_name = NA_character_))
  }
  tibble::tibble(local_file = basename(path), column_name = schema_names)
}

expected_columns <- c(
  "ID", "PROPERTY_ID", "UNIT_ID", "ADDRESS", "CITY", "RENT_PRICE",
  "BUILDING_TYPE", "BEDS", "BATHS", "SQFT", "LATITUDE", "LONGITUDE",
  "SCRAPED_TIMESTAMP", "DATE_POSTED", "AVAILABLE_AT"
)

schema_long <- purrr::map_dfr(parquet_files, read_parquet_schema)
schema_audit <- schema_long %>%
  mutate(column_upper = toupper(column_name)) %>%
  summarise(
    n_columns = sum(!is.na(column_name)),
    missing_expected_columns = paste(setdiff(expected_columns, unique(column_upper)), collapse = "|"),
    extra_columns = paste(setdiff(unique(column_upper), expected_columns), collapse = "|"),
    .by = local_file
  ) %>%
  arrange(local_file)

readr::write_csv(local_file_audit, "../output/renthub_download_file_audit.csv")
readr::write_csv(schema_audit, "../output/renthub_schema_audit.csv")

readr::write_csv(
  tibble::tibble(
	    product_path = product_path,
	    product_id = product_id,
	    delivery_scope = "Illinois customized Dewey export; downstream tasks filter to Chicago listings",
	    download_method = ifelse(use_deweypy, "deweypy_speedy_download", "deweydatar"),
	    downloaded_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
	    manifest_rows = nrow(manifest),
	    local_parquet_files = length(parquet_files),
    total_bytes = sum(parquet_file_info$size, na.rm = TRUE),
    min_file_date = min(local_file_audit$file_date, na.rm = TRUE),
    max_file_date = max(local_file_audit$file_date, na.rm = TRUE),
    min_local_file = min(basename(parquet_files)),
    max_local_file = max(basename(parquet_files)),
    files_missing_expected_schema = sum(nzchar(schema_audit$missing_expected_columns))
  ),
  "../output/renthub_download_metadata.csv"
)

file.create("../output/download.done")
