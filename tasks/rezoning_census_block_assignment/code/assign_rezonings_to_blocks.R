# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rezoning_census_block_assignment/code")
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

sf_use_s2(FALSE)

rezonings <- read_csv(
  paste0("../input/rezoning_geocode_with_external_", date_tag, ".csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  mutate(
    .row_order = row_number(),
    latitude_num = as.numeric(latitude),
    longitude_num = as.numeric(longitude),
    valid_coordinate = is.finite(latitude_num) & is.finite(longitude_num) &
      between(latitude_num, -90, 90) & between(longitude_num, -180, 180)
  )

if (any(is.na(rezonings$matter_id) | rezonings$matter_id == "")) {
  stop("Rezoning data contain missing matter_id values.", call. = FALSE)
}
if (anyDuplicated(rezonings$matter_id) > 0) {
  stop("Rezoning data are not unique by matter_id.", call. = FALSE)
}

blocks <- read_csv(
  "../input/census_blocks_2010.csv",
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
)

if (any(is.na(blocks$GEOID10) | nchar(blocks$GEOID10) != 15)) {
  stop("The 2010 census-block input contains missing or malformed GEOID10 values.", call. = FALSE)
}
conflicting_blocks <- blocks %>%
  distinct(GEOID10, the_geom) %>%
  count(GEOID10, name = "n_geometries") %>%
  filter(n_geometries > 1)
if (nrow(conflicting_blocks) > 0) {
  stop("The 2010 census-block input contains block IDs with conflicting geometries.", call. = FALSE)
}

blocks <- blocks %>%
  distinct(GEOID10, .keep_all = TRUE) %>%
  rename(geometry = the_geom) %>%
  st_as_sf(wkt = "geometry", crs = 4269) %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  transmute(census_block_id = GEOID10)

valid_points <- rezonings %>%
  filter(valid_coordinate) %>%
  st_as_sf(coords = c("longitude_num", "latitude_num"), crs = 4326, remove = FALSE) %>%
  st_transform(3435) %>%
  st_join(blocks, join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  select(.row_order, census_block_id)

if (anyDuplicated(valid_points$.row_order) > 0) {
  stop("At least one rezoning coordinate matched multiple 2010 census blocks.", call. = FALSE)
}
if (any(is.na(valid_points$census_block_id))) {
  stop("At least one valid rezoning coordinate did not fall within a 2010 census block.", call. = FALSE)
}

parcel_matches <- read_csv(
  paste0("../input/rezoning_parcel_matches_", date_tag, ".csv"),
  show_col_types = FALSE,
  col_types = cols(.default = col_character())
) %>%
  mutate(
    .parcel_row = row_number(),
    latitude_num = as.numeric(latitude),
    longitude_num = as.numeric(longitude)
  )

if (any(is.na(parcel_matches$matter_id) | parcel_matches$matter_id == "")) {
  stop("Rezoning parcel matches contain missing matter_id values.", call. = FALSE)
}
if (any(!parcel_matches$matter_id %in% rezonings$matter_id)) {
  stop("Rezoning parcel matches contain matter IDs absent from the rezoning data.", call. = FALSE)
}
if (anyDuplicated(parcel_matches[c("matter_id", "matched_pin")]) > 0) {
  stop("Rezoning parcel matches are not unique by matter_id and matched_pin.", call. = FALSE)
}
if (any(
  !is.finite(parcel_matches$latitude_num) |
    !is.finite(parcel_matches$longitude_num) |
    !between(parcel_matches$latitude_num, -90, 90) |
    !between(parcel_matches$longitude_num, -180, 180)
)) {
  stop("Rezoning parcel matches contain invalid coordinates.", call. = FALSE)
}

parcel_blocks <- parcel_matches %>%
  st_as_sf(coords = c("longitude_num", "latitude_num"), crs = 4326, remove = FALSE) %>%
  st_transform(3435) %>%
  st_join(blocks, join = st_within, left = TRUE) %>%
  st_drop_geometry()

if (anyDuplicated(parcel_blocks$.parcel_row) > 0) {
  stop("At least one matched parcel coordinate joined to multiple 2010 census blocks.", call. = FALSE)
}
if (any(is.na(parcel_blocks$census_block_id))) {
  stop("At least one matched parcel coordinate did not fall within a 2010 census block.", call. = FALSE)
}

output <- rezonings %>%
  left_join(valid_points, by = ".row_order", relationship = "one-to-one") %>%
  mutate(
    census_block_vintage = "2010",
    census_block_group_id = if_else(!is.na(census_block_id), substr(census_block_id, 1, 12), NA_character_),
    census_tract_id = if_else(!is.na(census_block_id), substr(census_block_id, 1, 11), NA_character_),
    block_assignment_status = if_else(valid_coordinate, "assigned_within", "missing_coordinate")
  ) %>%
  arrange(.row_order) %>%
  select(-.row_order, -latitude_num, -longitude_num, -valid_coordinate)

if (nrow(output) != nrow(rezonings) || !identical(output$matter_id, rezonings$matter_id)) {
  stop("Census-block assignment changed the rezoning row set or order.", call. = FALSE)
}

parcel_bridge <- parcel_blocks %>%
  count(matter_id, census_block_id, name = "matched_parcel_count") %>%
  mutate(block_assignment_method = "matched_parcels")

representative_bridge <- output %>%
  anti_join(parcel_bridge %>% distinct(matter_id), by = "matter_id") %>%
  transmute(
    matter_id,
    census_block_id,
    matched_parcel_count = NA_integer_,
    block_assignment_method = if_else(
      !is.na(census_block_id),
      "representative_point",
      "missing_coordinate"
    )
  )

matter_block_bridge <- bind_rows(parcel_bridge, representative_bridge) %>%
  mutate(
    census_block_vintage = "2010",
    census_block_group_id = if_else(!is.na(census_block_id), substr(census_block_id, 1, 12), NA_character_),
    census_tract_id = if_else(!is.na(census_block_id), substr(census_block_id, 1, 11), NA_character_)
  ) %>%
  arrange(matter_id, census_block_id)

if (anyDuplicated(matter_block_bridge[c("matter_id", "census_block_id")]) > 0) {
  stop("Matter-to-block bridge is not unique by matter_id and census_block_id.", call. = FALSE)
}
if (!setequal(matter_block_bridge$matter_id, output$matter_id)) {
  stop("Matter-to-block bridge does not represent every rezoning matter.", call. = FALSE)
}

write_csv(output, paste0("../output/rezoning_census_blocks_", date_tag, ".csv"))
write_csv(matter_block_bridge, paste0("../output/rezoning_matter_block_bridge_", date_tag, ".csv"))

message("Rows assigned to 2010 census blocks: ", sum(output$block_assignment_status == "assigned_within"))
message("Rows without coordinates: ", sum(output$block_assignment_status == "missing_coordinate"))
message("Matter-block rows: ", nrow(matter_block_bridge))
message("Matters spanning multiple blocks: ", sum(count(matter_block_bridge, matter_id)$n > 1))
