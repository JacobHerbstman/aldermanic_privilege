# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/permit_block_assignment_review/code")
# permit_start_year <- 2010
# permit_end_month <- "2020-12"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(permit_start_year, permit_end_month)
}
if (length(cli_args) != 2) {
  stop("Script requires two arguments: <permit_start_year> <permit_end_month>.", call. = FALSE)
}
permit_start_year <- as.integer(cli_args[1])
permit_end_month <- cli_args[2]

sf_use_s2(FALSE)

permits <- st_read(
  "../input/building_permits_clean.gpkg",
  query = paste(
    "SELECT id, application_start_date_ym, issue_date_ym, latitude, longitude, geom",
    "FROM building_permits_clean",
    "WHERE permit_issued = 1 OR permit_issued IS NULL"
  ),
  quiet = TRUE
) %>%
  mutate(
    id = as.character(id),
    application_month = as.yearmon(as.Date(application_start_date_ym)),
    issue_month = as.yearmon(as.Date(issue_date_ym))
  ) %>%
  filter(
    (!is.na(application_month) &
      as.integer(format(as.Date(application_month), "%Y")) >= permit_start_year &
      application_month <= as.yearmon(as.Date(paste0(permit_end_month, "-01")))) |
      (!is.na(issue_month) &
        as.integer(format(as.Date(issue_month), "%Y")) >= permit_start_year &
        issue_month <= as.yearmon(as.Date(paste0(permit_end_month, "-01"))))
  )

block_inputs <- tibble(
  block_vintage = c("2010", "2020"),
  path = c("../input/census_blocks_2010.csv", "../input/census_blocks_2020.csv"),
  block_column = c("GEOID10", "GEOID20")
)

review <- list()
for (i in seq_len(nrow(block_inputs))) {
  blocks <- read_csv(
    block_inputs$path[i],
    show_col_types = FALSE,
    col_types = cols(.default = col_character())
  ) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_make_valid() %>%
    st_transform(3435) %>%
    rename(block_id = all_of(block_inputs$block_column[i])) %>%
    distinct(block_id, .keep_all = TRUE)

  permit_points <- st_transform(permits, 3435)
  unmatched <- st_join(permit_points, blocks %>% select(block_id), join = st_within) %>%
    filter(is.na(block_id))

  empty_geometry <- st_is_empty(unmatched)
  nearest_index <- rep(NA_integer_, nrow(unmatched))
  nearest_distance <- rep(NA_real_, nrow(unmatched))
  nearest_block <- rep(NA_character_, nrow(unmatched))
  if (any(!empty_geometry)) {
    nearest_index[!empty_geometry] <- st_nearest_feature(unmatched[!empty_geometry, ], blocks)
    nearest_distance[!empty_geometry] <- as.numeric(st_distance(
      unmatched[!empty_geometry, ],
      blocks[nearest_index[!empty_geometry], ],
      by_element = TRUE
    ))
    nearest_block[!empty_geometry] <- blocks$block_id[nearest_index[!empty_geometry]]
  }

  review[[i]] <- unmatched %>%
    st_drop_geometry() %>%
    transmute(
      id,
      block_vintage = block_inputs$block_vintage[i],
      latitude,
      longitude,
      empty_geometry,
      nearest_block_id = nearest_block,
      nearest_block_distance_m = nearest_distance
    )
}

bind_rows(review) %>%
  arrange(block_vintage, id) %>%
  write_csv("../output/unmatched_permit_block_review.csv")
