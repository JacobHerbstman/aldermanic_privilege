## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_geocode_external_merge/code")
#
## interactive argument examples (mirror Makefile inputs)
# date_tag <- "19990101_20260212"

source("../../setup_environment/code/packages.R")

parse_args <- function(args) {
  out <- list(date_tag = "19990101_20260212", write_remaining_unmatched = FALSE)
  i <- 1
  while (i <= length(args)) {
    arg <- args[[i]]
    if (startsWith(arg, "--date-tag=")) {
      out$date_tag <- sub("^--date-tag=", "", arg)
    } else if (arg == "--date-tag" && i < length(args)) {
      i <- i + 1
      out$date_tag <- args[[i]]
    } else if (arg == "--write-remaining-unmatched") {
      out$write_remaining_unmatched <- TRUE
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

parse_bool <- function(value) {
  if (is.na(value)) return(FALSE)
  text <- tolower(trimws(as.character(value)))
  if (text %in% c("1", "true", "t", "yes", "y")) return(TRUE)
  if (text %in% c("0", "false", "f", "no", "n", "")) return(FALSE)
  TRUE
}

normalize_id <- function(value) {
  if (is.na(value)) return(NA_character_)
  out <- trimws(as.character(value))
  if (out %in% c("", "nan", "NaN", "none", "None", "NA", "N/A")) return(NA_character_)
  out
}

normalize_id_series <- function(values) {
  vapply(values, normalize_id, character(1))
}

parse_lat_lon_text <- function(value) {
  if (is.na(value)) return(c(NA_real_, NA_real_))
  text <- trimws(as.character(value))
  if (text == "") return(c(NA_real_, NA_real_))

  point_match <- stringr::str_match(text, "POINT\\s*\\(\\s*([-0-9.]+)\\s+([-0-9.]+)\\s*\\)")
  if (!is.na(point_match[[1, 2]])) {
    lon <- safe_float(point_match[[1, 2]])
    lat <- safe_float(point_match[[1, 3]])
    return(c(lat, lon))
  }

  pair_match <- stringr::str_match(text, "\\(?\\s*([-0-9.]+)\\s*,\\s*([-0-9.]+)\\s*\\)?")
  if (!is.na(pair_match[[1, 2]])) {
    first <- safe_float(pair_match[[1, 2]])
    second <- safe_float(pair_match[[1, 3]])
    if (is.na(first) || is.na(second)) return(c(NA_real_, NA_real_))
    if (first < 0 && second > 0 && second >= -90 && second <= 90 && first >= -180 && first <= 180) return(c(second, first))
    if (first >= -90 && first <= 90 && second >= -180 && second <= 180) return(c(first, second))
    if (second >= -90 && second <= 90 && first >= -180 && first <= 180) return(c(second, first))
  }

  c(NA_real_, NA_real_)
}

load_chicago_upload_mapping <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(upload_row_number = integer(), external_row_id = character()))
  }

  df <- readr::read_csv(path, show_col_types = FALSE)
  if (nrow(df) == 0) {
    return(tibble::tibble(upload_row_number = integer(), external_row_id = character()))
  }

  cols <- names(df)
  row_col <- choose_column(cols, c("upload_row_number", "row_number", "upload_row_num"), contains = c("row", "number"))
  id_col <- choose_column(cols, c("external_row_id", "matter_id", "row_id", "id", "unique_id"))
  include_col <- choose_column(cols, c("upload_included", "is_uploadable", "include_in_upload"), contains = c("included"))
  if (is.null(row_col) || is.null(id_col)) {
    return(tibble::tibble(upload_row_number = integer(), external_row_id = character()))
  }

  out <- tibble::tibble(
    upload_row_number = suppressWarnings(as.integer(df[[row_col]])),
    external_row_id = normalize_id_series(df[[id_col]])
  )

  if (!is.null(include_col)) {
    include_mask <- vapply(df[[include_col]], parse_bool, logical(1))
    out <- out[include_mask, ]
  }

  out %>%
    dplyr::filter(!is.na(.data$upload_row_number), !is.na(.data$external_row_id)) %>%
    dplyr::arrange(.data$upload_row_number) %>%
    dplyr::distinct(.data$upload_row_number, .keep_all = TRUE)
}

attach_row_order_ids <- function(out_df, mapping_df, fill_only_missing = FALSE) {
  if (nrow(mapping_df) == 0) return(out_df)
  with_ids <- out_df %>%
    dplyr::mutate(.upload_row_number = dplyr::row_number()) %>%
    dplyr::left_join(mapping_df, by = c(".upload_row_number" = "upload_row_number"))

  if (!("external_row_id.x" %in% names(with_ids))) {
    with_ids$external_row_id.x <- NA_character_
  }

  if (fill_only_missing) {
    base <- normalize_id_series(with_ids$external_row_id.x)
    with_ids$external_row_id <- dplyr::if_else(!is.na(base), base, with_ids$external_row_id.y)
  } else {
    with_ids$external_row_id <- with_ids$external_row_id.y
  }

  with_ids %>%
    dplyr::select(-dplyr::any_of(c(".upload_row_number", "external_row_id.x", "external_row_id.y")))
}

load_chicago_results <- function(path, mapping_path) {
  if (!file.exists(path)) {
    return(tibble::tibble(external_row_id = character(), latitude = numeric(), longitude = numeric(), provider = character()))
  }

  mapping_df <- load_chicago_upload_mapping(mapping_path)
  df <- readr::read_csv(path, show_col_types = FALSE)
  if (nrow(df) == 0) {
    return(tibble::tibble(external_row_id = character(), latitude = numeric(), longitude = numeric(), provider = character()))
  }

  cols <- names(df)
  id_col <- choose_column(cols, c("external_row_id", "row_id", "id", "unique_id", "matter_id"))
  lat_col <- choose_column(cols, c("latitude", "lat", "y", "ycoordinate", "y_coordinate"), contains = c("lat"))
  lon_col <- choose_column(cols, c("longitude", "lon", "lng", "x", "xcoordinate", "x_coordinate"), contains = c("lon"))
  if (is.null(lon_col)) lon_col <- choose_column(cols, c("x", "xcoordinate", "x_coordinate"))
  location_col <- choose_column(cols, c("location", "point", "coordinates"))

  out <- tibble::tibble(
    external_row_id = if (!is.null(id_col)) normalize_id_series(df[[id_col]]) else rep(NA_character_, nrow(df)),
    latitude = if (!is.null(lat_col)) vapply(df[[lat_col]], safe_float, numeric(1)) else rep(NA_real_, nrow(df)),
    longitude = if (!is.null(lon_col)) vapply(df[[lon_col]], safe_float, numeric(1)) else rep(NA_real_, nrow(df))
  )

  if (!is.null(location_col)) {
    parsed <- t(vapply(df[[location_col]], parse_lat_lon_text, numeric(2)))
    out$latitude[is.na(out$latitude)] <- parsed[is.na(out$latitude), 1]
    out$longitude[is.na(out$longitude)] <- parsed[is.na(out$longitude), 2]
  }

  if (is.null(id_col)) {
    out <- attach_row_order_ids(out, mapping_df, fill_only_missing = FALSE)
  } else if (any(is.na(out$external_row_id)) && nrow(mapping_df) > 0) {
    out <- attach_row_order_ids(out, mapping_df, fill_only_missing = TRUE)
  }

  out %>%
    dplyr::mutate(external_row_id = normalize_id_series(.data$external_row_id)) %>%
    dplyr::filter(
      !is.na(.data$external_row_id),
      !is.na(.data$latitude), !is.na(.data$longitude),
      .data$latitude >= -90, .data$latitude <= 90,
      .data$longitude >= -180, .data$longitude <= 180
    ) %>%
    dplyr::distinct(.data$external_row_id, .keep_all = TRUE) %>%
    dplyr::mutate(provider = "chicago_geocoder")
}

looks_like_header <- function(first_line) {
  if (is.na(first_line) || first_line == "") return(FALSE)
  joined <- tolower(first_line)
  stringr::str_detect(joined, "id") || stringr::str_detect(joined, "address") || stringr::str_detect(joined, "match")
}

load_census_results <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble(external_row_id = character(), latitude = numeric(), longitude = numeric(), provider = character()))
  }

  first_line <- readr::read_lines(path, n_max = 1)
  if (length(first_line) == 0) {
    return(tibble::tibble(external_row_id = character(), latitude = numeric(), longitude = numeric(), provider = character()))
  }

  if (looks_like_header(first_line[[1]])) {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    df <- readr::read_csv(path, col_names = FALSE, show_col_types = FALSE)
    expected <- c(
      "external_row_id",
      "input_address",
      "match_status",
      "match_type",
      "matched_address",
      "coordinates",
      "tiger_line_id",
      "tiger_line_side",
      "state_fips",
      "county_fips",
      "tract",
      "block"
    )
    for (idx in seq_along(expected)) {
      if (idx <= ncol(df)) names(df)[idx] <- expected[[idx]]
    }
  }

  cols <- names(df)
  id_col <- choose_column(cols, c("external_row_id", "id", "unique_id"), contains = c("id"))
  if (is.null(id_col)) id_col <- cols[[1]]
  lat_col <- choose_column(cols, c("latitude", "lat"), contains = c("lat"))
  lon_col <- choose_column(cols, c("longitude", "lon", "lng"), contains = c("lon"))
  coord_col <- choose_column(cols, c("coordinates", "matched_coordinates", "coordinate", "location"))
  status_col <- choose_column(cols, c("match", "match_status", "matched", "is_match"), contains = c("match"))

  out <- tibble::tibble(
    external_row_id = normalize_id_series(df[[id_col]]),
    latitude = if (!is.null(lat_col)) vapply(df[[lat_col]], safe_float, numeric(1)) else rep(NA_real_, nrow(df)),
    longitude = if (!is.null(lon_col)) vapply(df[[lon_col]], safe_float, numeric(1)) else rep(NA_real_, nrow(df))
  )

  if (!is.null(coord_col)) {
    parsed <- t(vapply(df[[coord_col]], parse_lat_lon_text, numeric(2)))
    out$latitude[is.na(out$latitude)] <- parsed[is.na(out$latitude), 1]
    out$longitude[is.na(out$longitude)] <- parsed[is.na(out$longitude), 2]
  }

  if (!is.null(status_col)) {
    keep <- stringr::str_detect(toupper(trimws(as.character(df[[status_col]]))), "^MATCH")
    keep[is.na(keep)] <- FALSE
    out <- out[keep, ]
  }

  out %>%
    dplyr::filter(
      !is.na(.data$external_row_id),
      !is.na(.data$latitude), !is.na(.data$longitude),
      .data$latitude >= -90, .data$latitude <= 90,
      .data$longitude >= -180, .data$longitude <= 180
    ) %>%
    dplyr::distinct(.data$external_row_id, .keep_all = TRUE) %>%
    dplyr::mutate(provider = "census_geocoder")
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  tag <- opts$date_tag

  in_stage1_csv <- paste0("../input/rezoning_geocode_stage1_", tag, ".csv")
  in_unmatched_csv <- paste0("../input/rezoning_geocode_stage1_unmatched_", tag, ".csv")
  in_chicago_results_csv <- paste0("../input/chicago_geocoder_results_", tag, ".csv")
  in_census_results_csv <- paste0("../input/census_geocoder_results_", tag, ".csv")
  in_chicago_upload_mapping_csv <- paste0("../input/chicago_geocoder_upload_mapping_", tag, ".csv")
  out_geocoded_csv <- paste0("../output/rezoning_geocode_with_external_", tag, ".csv")
  out_remaining_unmatched_csv <- paste0("../output/rezoning_geocode_remaining_unmatched_", tag, ".csv")

  external_inputs <- c(in_chicago_results_csv, in_census_results_csv)
  missing_external_inputs <- external_inputs[!file.exists(external_inputs)]
  if (length(missing_external_inputs) > 0) {
    stop(paste0(
      "Missing required external geocoder input(s): ",
      paste(missing_external_inputs, collapse = ", ")
    ))
  }

  stage1_df <- readr::read_csv(in_stage1_csv, show_col_types = FALSE)
  unmatched_df <- readr::read_csv(in_unmatched_csv, show_col_types = FALSE)

  required_cols <- c("external_row_id", "geocode_source", "geocode_confidence", "latitude", "longitude")
  missing <- setdiff(required_cols, names(stage1_df))
  if (length(missing) > 0) {
    stop(paste0("Stage1 file is missing required column(s): ", paste(missing, collapse = ", ")))
  }

  chicago_df <- load_chicago_results(in_chicago_results_csv, in_chicago_upload_mapping_csv)
  census_df <- load_census_results(in_census_results_csv)

  chicago_map <- split(chicago_df, chicago_df$external_row_id)
  census_map <- split(census_df, census_df$external_row_id)
  unmatched_ids <- unique(normalize_id_series(unmatched_df$external_row_id))
  unmatched_ids <- unmatched_ids[!is.na(unmatched_ids)]

  chicago_added <- 0L
  census_added <- 0L
  ids <- normalize_id_series(stage1_df$external_row_id)

  for (idx in seq_len(nrow(stage1_df))) {
    row_id <- ids[[idx]]
    if (is.na(row_id) || !(row_id %in% unmatched_ids)) next

    source <- tolower(trimws(as.character(stage1_df$geocode_source[[idx]])))
    lat <- safe_float(stage1_df$latitude[[idx]])
    lon <- safe_float(stage1_df$longitude[[idx]])
    if (source == "parcel_match" && !is.na(lat) && !is.na(lon)) next

    provider <- NA_character_
    update <- NULL
    if (!is.null(chicago_map[[row_id]]) && nrow(chicago_map[[row_id]]) > 0) {
      provider <- "chicago_geocoder"
      update <- chicago_map[[row_id]][1, ]
      chicago_added <- chicago_added + 1L
    } else if (!is.null(census_map[[row_id]]) && nrow(census_map[[row_id]]) > 0) {
      provider <- "census_geocoder"
      update <- census_map[[row_id]][1, ]
      census_added <- census_added + 1L
    }

    if (is.null(update)) next
    stage1_df$latitude[[idx]] <- as.character(update$latitude[[1]])
    stage1_df$longitude[[idx]] <- as.character(update$longitude[[1]])
    stage1_df$geocode_source[[idx]] <- provider
    stage1_df$geocode_confidence[[idx]] <- "external_geocoder"
  }

  lat_num <- vapply(stage1_df$latitude, safe_float, numeric(1))
  lon_num <- vapply(stage1_df$longitude, safe_float, numeric(1))
  src <- tolower(trimws(as.character(stage1_df$geocode_source)))
  remaining_unmatched <- stage1_df[is.na(lat_num) | is.na(lon_num) | src == "unmatched", ]

  dir.create("../output", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(stage1_df, out_geocoded_csv)
  if (opts$write_remaining_unmatched) {
    readr::write_csv(remaining_unmatched, out_remaining_unmatched_csv)
  }

  message("Chicago rows added: ", chicago_added)
  message("Census rows added: ", census_added)
}

main()
