# Purpose: Download & clean Chicago amenities layers and save as EPSG:3435 GeoPackage.
# Layers: CTA stops, Major Streets, CPD Parks, CPS Schools.

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")
source("../../setup_environment/code/packages.R")

# ---------- CTA data ----------
cta <- st_read("../input/cta_stations.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%  # safety: City files are usually EPSG:4326
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names()

cta <- cta %>%
  mutate(source = "cta") %>%
  select(
    station_id, longname, lines,
    point_x, point_y, legend,
    source, geometry
  )

st_write(cta,"../output/cta_stops.gpkg", delete_dsn = TRUE, quiet = TRUE)


## major streets

major_streets <- st_read("../input/Major_Streets.shp", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "major_streets") %>% 
  select(streetname, class, status, source, geometry)

st_write(major_streets,"../output/major_streets.gpkg", delete_dsn = TRUE, quiet = TRUE)


parks <- st_read("../input/CPD_Facilities_20250925.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "cpd_parks") %>% 
  select(park, park_no, facility_n, facility_t, source, geometry)

st_write(parks,"../output/parks.gpkg", delete_dsn = TRUE, quiet = TRUE)


schools <- st_read("../input/CPS_School_Locations_SY1415_20250925.geojson", quiet = TRUE) %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  { if (is.na(st_crs(.))) st_set_crs(., 4326) else . } %>%
  st_make_valid() %>%
  st_transform(3435) %>%
  janitor::clean_names() %>%
  mutate(source = "cps_schools") %>% 
  select(school_id, school_nm, grade_cat, sch_type, source, geometry)

st_write(schools,"../output/schools_2015.gpkg", delete_dsn = TRUE, quiet = TRUE)




