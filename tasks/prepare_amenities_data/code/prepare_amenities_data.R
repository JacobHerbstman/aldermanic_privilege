# Prepare Chicago amenities layers from raw local files and save EPSG:3435 GeoPackages.

# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_amenities_data/code")
source("../../setup_environment/code/packages.R")

cta <- st_read("../input/cta_stations.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names()

cta_open_dates <- tibble::tribble(
  ~station_id, ~active_from_date, ~active_date_source,
  "1510", as.Date("2012-05-18"), "CTA 2012 Annual Ridership Report",
  "1680", as.Date("2012-04-30"), "CTA Oakton-Skokie opening announcement",
  "1690", as.Date("2015-02-09"), "CTA Cermak-McCormick Place opening announcement",
  "1700", as.Date("2017-08-31"), "CTA Washington/Wabash opening announcement",
  "1710", as.Date("2024-08-06"), "CTA Damen Green Line opening announcement"
)

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

cta_historical <- tibble::tribble(
  ~station_id, ~longname, ~lines, ~longitude, ~latitude, ~legend, ~active_from_date, ~active_to_date, ~active_date_source,
  "historical_washington_state", "Washington/State", "Red Line", -87.627800, 41.883700, "Red Line", as.Date("1900-01-01"), as.Date("2006-10-22"), "CTA closure effective October 23, 2006",
  "historical_madison_wabash", "Madison/Wabash", "Brown, Orange, Pink, Purple (Express), Green", -87.626098, 41.882023, "Multiple Lines", as.Date("1900-01-01"), as.Date("2015-03-15"), "CTA closure effective March 16, 2015",
  "historical_randolph_wabash", "Randolph/Wabash", "Brown, Orange, Pink, Purple (Express), Green", -87.626149, 41.884431, "Multiple Lines", as.Date("1900-01-01"), as.Date("2017-09-02"), "CTA closure effective September 3, 2017"
) %>%
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

st_write(cta, "../output/cta_stops.gpkg", delete_dsn = TRUE, quiet = TRUE)


major_streets <- st_read("../input/major_streets.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  { if ("streetname" %in% names(.)) . else rename(., streetname = street_nam) } %>%
  mutate(source = "major_streets") %>% 
  select(streetname, class, status, source, geometry)

st_write(major_streets, "../output/major_streets.gpkg", delete_dsn = TRUE, quiet = TRUE)


parks <- st_read("../input/cpd_park_boundaries.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "cpd_park_boundaries") %>%
  select(park, park_no, park_class, acres, source, geometry)

st_write(parks, "../output/parks.gpkg", delete_dsn = TRUE, quiet = TRUE)


schools <- st_read("../input/cps_school_locations_sy1415.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "cps_schools") %>% 
  select(school_id, school_nm, grade_cat, sch_type, source, geometry)

st_write(schools, "../output/schools_2015.gpkg", delete_dsn = TRUE, quiet = TRUE)
