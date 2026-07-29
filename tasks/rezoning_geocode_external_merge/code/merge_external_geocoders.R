# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rezoning_geocode_external_merge/code")
# date_tag <- "20101101_20201231"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(date_tag)
}
if (length(cli_args) != 1) {
  stop("Script requires one argument: <date_tag>.", call. = FALSE)
}
date_tag <- cli_args[1]

stage1 <- read_csv(
  paste0("../input/rezoning_geocode_stage1_", date_tag, ".csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  mutate(external_row_id = as.character(external_row_id))

if (any(is.na(stage1$external_row_id) | stage1$external_row_id == "")) {
  stop("Stage-one geocodes contain missing external_row_id values.", call. = FALSE)
}
if (anyDuplicated(stage1$external_row_id) > 0) {
  stop("Stage-one geocodes are not unique by external_row_id.", call. = FALSE)
}

unmatched <- read_csv(
  paste0("../input/rezoning_geocode_stage1_unmatched_", date_tag, ".csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(external_row_id = as.character(external_row_id)) %>%
  filter(!is.na(external_row_id), external_row_id != "") %>%
  distinct()

external_geocodes <- read_csv(
  paste0("../input/rezoning_external_geocodes_", date_tag, ".csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  transmute(
    external_row_id = as.character(external_row_id),
    external_latitude = as.numeric(latitude),
    external_longitude = as.numeric(longitude),
    external_geocode_source = geocode_source
  )

if (any(is.na(external_geocodes$external_row_id) | external_geocodes$external_row_id == "")) {
  stop("Frozen external geocodes contain missing external_row_id values.", call. = FALSE)
}
if (anyDuplicated(external_geocodes$external_row_id) > 0) {
  stop("Frozen external geocodes are not unique by external_row_id.", call. = FALSE)
}
if (any(
  !is.finite(external_geocodes$external_latitude) |
    !is.finite(external_geocodes$external_longitude) |
    !between(external_geocodes$external_latitude, -90, 90) |
    !between(external_geocodes$external_longitude, -180, 180)
)) {
  stop("Frozen external geocodes contain invalid coordinates.", call. = FALSE)
}

stage1_ids <- stage1$external_row_id
unmatched_ids <- unmatched$external_row_id

merged <- stage1 %>%
  mutate(.row_order = row_number()) %>%
  left_join(external_geocodes, by = "external_row_id", relationship = "many-to-one") %>%
  mutate(
    existing_coordinate = is.finite(as.numeric(latitude)) & is.finite(as.numeric(longitude)),
    eligible_external = external_row_id %in% unmatched_ids,
    use_external = !existing_coordinate & eligible_external &
      is.finite(external_latitude) & is.finite(external_longitude),
    latitude = if_else(use_external, as.character(external_latitude), latitude),
    longitude = if_else(use_external, as.character(external_longitude), longitude),
    geocode_source = if_else(use_external, external_geocode_source, geocode_source),
    geocode_confidence = if_else(use_external, "external_geocoder", geocode_confidence)
  ) %>%
  arrange(.row_order) %>%
  select(
    -external_latitude, -external_longitude, -external_geocode_source,
    -existing_coordinate, -eligible_external, -use_external, -.row_order
  )

if (nrow(merged) != nrow(stage1) || !identical(merged$external_row_id, stage1_ids)) {
  stop("External geocoder merge changed the stage-one row set or order.", call. = FALSE)
}

write_csv(merged, paste0("../output/rezoning_geocode_with_external_", date_tag, ".csv"))

message("Rows filled from frozen external geocodes: ", sum(merged$geocode_confidence == "external_geocoder", na.rm = TRUE))
message("Rows remaining without coordinates: ", sum(!is.finite(as.numeric(merged$latitude)) | !is.finite(as.numeric(merged$longitude))))
