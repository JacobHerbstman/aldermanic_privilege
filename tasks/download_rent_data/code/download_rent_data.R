# Download the customized Illinois RentHub export from Dewey.

# setwd("tasks/download_rent_data/code")

source("../../setup_environment/code/packages.R")

product_id <- "prj_4ufibhzc__cdst_wuxt87zew9ektcpt"
product_path <- sprintf("https://api.deweydata.io/api/v1/external/data/%s", product_id)
download_method <- tolower(Sys.getenv("DEWEY_DOWNLOAD_METHOD", "auto"))
force_download <- identical(Sys.getenv("FORCE_RENT_DOWNLOAD"), "1")

if (!download_method %in% c("auto", "deweypy", "deweydatar")) {
  stop("DEWEY_DOWNLOAD_METHOD must be one of: auto, deweypy, deweydatar.", call. = FALSE)
}

existing_parquet_files <- sort(list.files("../output", pattern = "\\.parquet$", full.names = TRUE))
needs_download <- length(existing_parquet_files) == 0 || force_download

if (needs_download) {
  dewey_api_key <- Sys.getenv("DEWEY_API_KEY")
  if (!nzchar(dewey_api_key)) {
    stop("DEWEY_API_KEY is not set; cannot download RentHub data.", call. = FALSE)
  }

  uvx_path <- Sys.which("uvx")
  use_deweypy <- download_method == "deweypy" || (download_method == "auto" && nzchar(uvx_path))

  if (use_deweypy) {
    if (!nzchar(uvx_path)) {
      stop("DEWEY_DOWNLOAD_METHOD=deweypy requires uvx on PATH.", call. = FALSE)
    }
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
} else {
  message(sprintf("Found %s existing RentHub parquet files; skipping download.", length(existing_parquet_files)))
}

parquet_files <- sort(list.files("../output", pattern = "\\.parquet$", full.names = TRUE))
if (length(parquet_files) == 0) {
  stop("Dewey download completed but no parquet files were written.", call. = FALSE)
}

invisible(file.create("../output/download.done"))
message(sprintf("RentHub data ready: %s parquet files.", length(parquet_files)))
