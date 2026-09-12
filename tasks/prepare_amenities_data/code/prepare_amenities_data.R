# Prepare Chicago amenities layers from raw local files and save EPSG:3435 GeoPackages.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_amenities_data/code")
source("../../setup_environment/code/packages.R")

source("../../shared/code/save_data.R")
cta <- st_read("../input/cta_stations.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names()

cta_open_dates <- read_csv("../input/cta_opening_dates.csv", show_col_types = FALSE,
  col_types = cols(station_id = col_character(), active_from_date = col_date(), active_date_source = col_character()))

cta <- cta %>%
  mutate(station_id = as.character(station_id)) %>%
  left_join(cta_open_dates, by = "station_id", relationship = "many-to-one") %>%
  mutate(source = "cta") %>%
  mutate(
    active_from_date = coalesce(active_from_date, as.Date("1900-01-01")),
    active_to_date = as.Date(NA),
    active_date_source = coalesce(active_date_source, "CTA current station file; treated as open before 2006 sample start")
  ) %>%
  select(
    station_id, longname, lines,
    point_x, point_y, legend,
    active_from_date, active_to_date, active_date_source,
    source, geometry
  )

cta_historical <- read_csv("../input/cta_historical_stations.csv", show_col_types = FALSE,
  col_types = cols(station_id = col_character(), longname = col_character(), lines = col_character(),
    longitude = col_double(), latitude = col_double(), legend = col_character(),
    active_from_date = col_date(), active_to_date = col_date(), active_date_source = col_character())) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(3435)

historical_coordinates <- st_coordinates(cta_historical)
cta_historical <- cta_historical %>%
  mutate(
    point_x = as.character(historical_coordinates[, 1]),
    point_y = as.character(historical_coordinates[, 2]),
    source = "cta_historical_station"
  ) %>%
  select(
    station_id, longname, lines,
    point_x, point_y, legend,
    active_from_date, active_to_date, active_date_source,
    source, geometry
  )

cta <- bind_rows(cta, cta_historical)

if (anyDuplicated(cta$station_id) > 0) {
  stop("CTA station IDs must be unique.", call. = FALSE)
}
if (any(is.na(cta$active_from_date))) {
  stop("CTA stations must have an opening date.", call. = FALSE)
}
if (any(!is.na(cta$active_to_date) & cta$active_to_date < cta$active_from_date)) {
  stop("CTA station closing dates cannot precede opening dates.", call. = FALSE)
}

SaveData(cta, c("station_id"), "../output/cta_stops.gpkg", delete_dsn = TRUE, quiet = TRUE)


major_streets <- st_read("../input/major_streets.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "major_streets") %>% 
  select(streetname, class, status, source, geometry)

SaveData(major_streets, c("fid"), "../output/major_streets.gpkg", delete_dsn = TRUE, quiet = TRUE)


parks <- st_read("../input/cpd_park_boundaries.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "cpd_park_boundaries") %>%
  select(park, park_no, park_class, acres, source, geometry)

SaveData(parks, c("fid"), "../output/parks.gpkg", delete_dsn = TRUE, quiet = TRUE)


schools <- st_read("../input/cps_school_locations_sy1415.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "cps_schools") %>% 
  select(school_id, school_nm, grade_cat, sch_type, source, geometry)

SaveData(schools, c("fid"), "../output/schools_2015.gpkg", delete_dsn = TRUE, quiet = TRUE)
