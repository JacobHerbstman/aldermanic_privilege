# setwd("tasks/construction_historical_sites/code")

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")


# Build preferred geography requests

residential <- readr::read_csv(
  "../input/preferred_residential_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    project_kind = readr::col_character(),
    candidate_status = readr::col_character(),
    construction_year = readr::col_double(),
    component_pins = readr::col_character(),
    .default = readr::col_skip()
  )
)

commercial <- readr::read_csv(
  "../input/preferred_commercial_project_candidates.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    source_family = readr::col_character(),
    project_id = readr::col_character(),
    project_kind = readr::col_character(),
    candidate_status = readr::col_character(),
    construction_year = readr::col_double(),
    component_pins = readr::col_character(),
    .default = readr::col_skip()
  )
)

project_columns <- c(
  "source_family", "project_id", "project_kind", "candidate_status",
  "construction_year", "component_pins"
)

all_candidates <- bind_rows(
  residential %>% select(all_of(project_columns)),
  commercial %>% select(all_of(project_columns))
)

projects <- all_candidates %>%
  filter(
    candidate_status != "exclude_outside_period",
    between(construction_year, 2006L, 2022L)
  )

if (anyDuplicated(projects$project_id) > 0) {
  stop("Preferred geography projects are not unique by project ID.", call. = FALSE)
}

requests <- projects %>%
  tidyr::separate_longer_delim(component_pins, delim = "/") %>%
  transmute(
    source_family,
    project_id,
    project_kind,
    candidate_status,
    target_year = as.integer(construction_year),
    component_pin = component_pins,
    pin10 = str_sub(component_pins, 1, 10)
  ) %>%
  distinct(project_id, component_pin, target_year, .keep_all = TRUE) %>%
  arrange(target_year, pin10, project_id, component_pin)

if (any(is.na(requests$component_pin) | str_length(requests$component_pin) != 14)) {
  stop("Preferred geography request contains an invalid component PIN.", call. = FALSE)
}
if (anyDuplicated(requests[c("project_id", "component_pin", "target_year")]) > 0) {
  stop("Preferred project-component-year requests are not unique.", call. = FALSE)
}

SaveData(requests, character(), "../output/preferred_project_geography_requests.csv")

# Build preferred historical parcels

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
