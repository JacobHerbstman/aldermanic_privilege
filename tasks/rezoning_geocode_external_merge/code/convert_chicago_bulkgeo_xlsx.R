## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_geocode_external_merge/code")
#
## interactive argument examples (mirror Makefile inputs)
# convert_chicago_bulkgeo_xlsx.R --date-tag 20101201_20260212

source("../../setup_environment/code/packages.R")

parse_args <- function(args) {
  out <- list(date_tag = "20101201_20260212")
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

choose_column <- function(columns, options, contains = NULL) {
  lower <- tolower(columns)
  for (opt in options) {
    idx <- match(tolower(opt), lower)
    if (!is.na(idx)) return(columns[[idx]])
  }
  if (!is.null(contains)) {
    for (col in columns) {
      c <- tolower(col)
      if (all(vapply(contains, function(fragment) stringr::str_detect(c, stringr::fixed(fragment)), logical(1)))) {
        return(col)
      }
    }
  }
  NULL
}

safe_float <- function(value) {
  if (is.na(value)) return(NA_real_)
  out <- suppressWarnings(as.numeric(trimws(as.character(value))))
  if (is.na(out) || is.infinite(out)) return(NA_real_)
  out
}

parse_lat_lon_text <- function(value) {
  if (is.na(value) || trimws(as.character(value)) == "") {
    return(c(NA_real_, NA_real_))
  }
  text <- trimws(as.character(value))
  if (text %in% c("---", "nan", "None")) {
    return(c(NA_real_, NA_real_))
  }

  point_match <- stringr::str_match(text, "POINT\\s*\\(\\s*([-0-9.]+)\\s+([-0-9.]+)\\s*\\)")
  if (!is.na(point_match[[1, 2]])) {
    lon <- safe_float(point_match[[1, 2]])
    lat <- safe_float(point_match[[1, 3]])
    return(c(lat, lon))
  }

  pair_match <- stringr::str_match(text, "\\(?\\s*([-0-9.]+)\\s*[,;]\\s*([-0-9.]+)\\s*\\)?")
  if (!is.na(pair_match[[1, 2]])) {
    first <- safe_float(pair_match[[1, 2]])
    second <- safe_float(pair_match[[1, 3]])
    if (is.na(first) || is.na(second)) return(c(NA_real_, NA_real_))
    if (first >= -90 && first <= 90 && second >= -180 && second <= 180) return(c(first, second))
    if (second >= -90 && second <= 90 && first >= -180 && first <= 180) return(c(second, first))
  }

  c(NA_real_, NA_real_)
}

file_sha256 <- function(path) {
  out <- suppressWarnings(system2("shasum", c("-a", "256", path), stdout = TRUE, stderr = FALSE))
  if (length(out) == 0) return(NA_character_)
  trimws(strsplit(out[[1]], " ", fixed = TRUE)[[1]][[1]])
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  in_xlsx <- paste0("../input/chicago_geocoder_results_", opts$date_tag, ".xlsx")
  out_csv <- paste0("../input/chicago_geocoder_results_", opts$date_tag, ".csv")
  out_meta_json <- paste0("../input/chicago_geocoder_results_", opts$date_tag, "_import_meta.json")

  df <- readxl::read_xlsx(in_xlsx, col_types = "text")
  cols <- names(df)

  row_col <- choose_column(cols, c("ROW_NUMBER", "row_number", "rownum"), contains = c("row", "number"))
  address_col <- choose_column(cols, c("address", "input_address"), contains = c("address"))
  full_address_col <- choose_column(cols, c("full address", "full_address", "matched_address"), contains = c("full", "address"))
  status_col <- choose_column(cols, c("status", "match_status"), contains = c("status"))
  lat_col <- choose_column(cols, c("latitude", "lat"), contains = c("lat"))
  lon_col <- choose_column(cols, c("longitude", "lon", "lng"), contains = c("lon"))
  latlon_col <- choose_column(cols, c("lat/long coordinates", "coordinates", "location", "point"), contains = c("coord"))

  out <- tibble::tibble()
  if (!is.null(row_col)) {
    out$upload_row_number <- suppressWarnings(as.integer(df[[row_col]]))
  } else {
    out$upload_row_number <- seq_len(nrow(df))
  }

  out$address <- if (!is.null(address_col)) as.character(df[[address_col]]) else NA_character_
  out$matched_address <- if (!is.null(full_address_col)) as.character(df[[full_address_col]]) else NA_character_
  out$status <- if (!is.null(status_col)) as.character(df[[status_col]]) else NA_character_

  out$latitude <- if (!is.null(lat_col)) vapply(df[[lat_col]], safe_float, numeric(1)) else NA_real_
  out$longitude <- if (!is.null(lon_col)) vapply(df[[lon_col]], safe_float, numeric(1)) else NA_real_

  if (!is.null(latlon_col)) {
    parsed <- t(vapply(df[[latlon_col]], parse_lat_lon_text, numeric(2)))
    parsed_lat <- parsed[, 1]
    parsed_lon <- parsed[, 2]
    out$latitude[is.na(out$latitude)] <- parsed_lat[is.na(out$latitude)]
    out$longitude[is.na(out$longitude)] <- parsed_lon[is.na(out$longitude)]
    out$lat_long_coordinates <- as.character(df[[latlon_col]])
  } else {
    out$lat_long_coordinates <- NA_character_
  }

  out <- out %>%
    dplyr::arrange(.data$upload_row_number)

  dir.create("../input", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out, out_csv)

  rows_with_valid_coords <- sum(
    !is.na(out$latitude) &
      !is.na(out$longitude) &
      out$latitude >= -90 & out$latitude <= 90 &
      out$longitude >= -180 & out$longitude <= 180
  )

  summary <- list(
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    source_xlsx = in_xlsx,
    source_xlsx_sha256 = file_sha256(in_xlsx),
    rows_in_xlsx = nrow(df),
    rows_exported = nrow(out),
    rows_with_valid_coords = as.integer(rows_with_valid_coords),
    detected_columns = list(
      row_col = row_col,
      address_col = address_col,
      full_address_col = full_address_col,
      status_col = status_col,
      lat_col = lat_col,
      lon_col = lon_col,
      latlon_col = latlon_col
    ),
    output_csv = out_csv
  )

  jsonlite::write_json(summary, out_meta_json, pretty = TRUE, auto_unbox = TRUE, na = "null")
}

main()
