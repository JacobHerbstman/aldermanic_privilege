# setwd("tasks/new_construction_cleaning/code")

source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
requests <- readr::read_csv(
  "../output/preferred_project_geography_requests.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pin = readr::col_character(),
    pin10 = readr::col_character(),
    .default = readr::col_guess()
  )
)

if (anyDuplicated(requests[c("project_id", "component_pin", "target_year")]) > 0) {
  stop("Preferred geography requests are not unique.", call. = FALSE)
}

requested_year_pin10 <- requests %>%
  distinct(target_year, pin10)

source_queries <- readr::read_csv(
  "../input/preferred_historical_parcel_source_queries.csv",
  col_types = readr::cols(target_year = readr::col_integer(), pin10 = readr::col_character())
)
if (anyDuplicated(source_queries[c("target_year", "pin10")]) ||
    nrow(anti_join(requested_year_pin10, source_queries, by = c("target_year", "pin10"))) > 0) {
  stop("Historical parcel requests exceed the pinned source query coverage; acquire and pin the missing source queries.")
}

historical_parcels <- sf::st_read("../input/preferred_historical_parcel_source.gpkg", quiet = TRUE, promote_to_multi = FALSE) %>%
  sf::st_transform(3435) %>%
  semi_join(requested_year_pin10, by = c("target_year", "pin10")) %>%
  arrange(target_year, pin10, pin14, object_id)
if (anyDuplicated(sf::st_drop_geometry(historical_parcels)[c("target_year", "object_id")]) ||
    any(!sf::st_is_valid(historical_parcels)) || any(sf::st_is_empty(historical_parcels))) {
  stop("Pinned historical parcel source identifiers or geometries are invalid.")
}

parcel_counts <- historical_parcels %>%
  sf::st_drop_geometry() %>%
  count(target_year, pin10, name = "polygon_count_pin10")

exact_pin14 <- historical_parcels %>%
  sf::st_drop_geometry() %>%
  distinct(target_year, pin10, pin14) %>%
  mutate(exact_pin14_available = TRUE)

coverage <- requests %>%
  left_join(
    parcel_counts,
    by = c("target_year", "pin10"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    exact_pin14,
    by = c("target_year", "pin10", "component_pin" = "pin14"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    polygon_count_pin10 = coalesce(polygon_count_pin10, 0L),
    exact_pin14_available = coalesce(exact_pin14_available, FALSE),
    coverage_status = case_when(
      exact_pin14_available ~ "exact_pin14",
      polygon_count_pin10 == 1 ~ "unique_pin10_predecessor",
      polygon_count_pin10 > 1 ~ "ambiguous_pin10",
      TRUE ~ "missing_pin10"
    )
  ) %>%
  arrange(target_year, project_id, component_pin)

SaveData(historical_parcels, c("target_year", "object_id"), "../output/preferred_historical_parcels.gpkg", delete_dsn = TRUE, quiet = TRUE)
SaveData(coverage, c("project_id", "component_pin", "target_year"), "../output/preferred_historical_parcel_coverage.csv")
