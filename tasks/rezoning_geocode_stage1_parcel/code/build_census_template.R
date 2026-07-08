## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/alderman_data/tasks/rezoning_geocode_stage1_parcel/code")
#
## interactive argument examples (mirror Makefile inputs)
# build_census_template.R --date-tag 20101201_20260212

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

clean_street_address <- function(value) {
  if (is.na(value)) return("")
  stringr::str_squish(as.character(value))
}

derive_from_external <- function(address_for_external) {
  if (is.na(address_for_external)) return("")
  text <- stringr::str_squish(as.character(address_for_external))
  lowered <- tolower(text)
  bad_addr <- c(
    "",
    "nan",
    "na",
    "none",
    "nan, chicago, il",
    "na, chicago, il",
    "none, chicago, il",
    ", chicago, il",
    "chicago, il"
  )
  if (lowered %in% bad_addr) return("")
  text <- stringr::str_replace(text, ",?\\s+chicago\\s*(,\\s*)?il(?:linois)?\\s*$", "")
  text <- stringr::str_squish(stringr::str_trim(text, side = "both"))
  text <- stringr::str_replace_all(text, "^,+|,+$", "")
  stringr::str_trim(text)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))

  in_unmatched_csv <- paste0("../output/rezoning_geocode_stage1_unmatched_", opts$date_tag, ".csv")
  out_census_template_csv <- paste0("../output/census_geocoder_upload_template_", opts$date_tag, ".csv")

  unmatched <- readr::read_csv(in_unmatched_csv, show_col_types = FALSE)

  out <- unmatched %>%
    dplyr::transmute(
      external_row_id = .data$external_row_id,
      street_address = clean_street_address(.data$street_address),
      city = .data$city,
      state = .data$state,
      zip = .data$zip,
      address_for_external = .data$address_for_external
    )

  missing_street <- stringr::str_trim(out$street_address) == ""
  out$street_address[missing_street] <- vapply(
    out$address_for_external[missing_street],
    derive_from_external,
    FUN.VALUE = character(1)
  )

  out <- out %>%
    dplyr::mutate(
      city = dplyr::if_else(is.na(.data$city) | .data$city == "", "Chicago", .data$city),
      state = dplyr::if_else(is.na(.data$state) | .data$state == "", "IL", .data$state),
      zip = dplyr::if_else(is.na(.data$zip), "", .data$zip)
    ) %>%
    dplyr::select(.data$external_row_id, .data$street_address, .data$city, .data$state, .data$zip)

  dir.create("../output", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(out, out_census_template_csv, col_names = FALSE)
}

main()
