source("../../shared/code/save_data.R")
# setwd("tasks/new_construction_cleaning/code")

library(sf)
library(dplyr)
library(stringr)

projects <- st_read("../output/preferred_project_year_geometry.gpkg", quiet = TRUE) |>
  st_transform(3435) |>
  filter(source_family == "commercial", between(target_year, 2006L, 2022L))
stopifnot(nrow(projects) > 0L, !anyDuplicated(st_drop_geometry(projects)[c("project_id", "target_year")]),
          all(st_is_valid(projects)), !any(st_is_empty(projects)))

# Read the complete pinned City source. The evidence consumer uses intersections
# with project polygons, so nearby footprints outside those polygons are unused.
footprints <- st_read(
  "/vsizip/../input/chicago_building_footprints_2015.zip/buildings.shp",
  query = "SELECT BLDG_ID, F_ADD1, T_ADD1, PRE_DIR1, ST_NAME1, ST_TYPE1, YEAR_BUILT, NO_OF_UNIT, BLDG_SQ_FO FROM buildings",
  quiet = TRUE
) |>
  st_transform(3435) |>
  st_filter(projects)
stopifnot(!anyNA(footprints$BLDG_ID), all(footprints$BLDG_ID > 0),
          !anyDuplicated(footprints$BLDG_ID))

footprints <- footprints |>
  transmute(
    footprint_id = paste0("city_building_", BLDG_ID),
    city_address = str_squish(paste(coalesce(as.character(F_ADD1), ""), coalesce(as.character(T_ADD1), ""),
                                  coalesce(PRE_DIR1, ""), coalesce(ST_NAME1, ""), coalesce(ST_TYPE1, ""))),
    city_year_built = as.integer(YEAR_BUILT),
    city_units = as.numeric(NO_OF_UNIT),
    city_building_sqft = as.numeric(BLDG_SQ_FO),
    city_shape_area_sqft = as.numeric(st_area(geometry))
  ) |>
  arrange(footprint_id)

# Never discard a conflicting City identifier or repair a project footprint silently.
stopifnot(nrow(footprints) > 0L, !anyDuplicated(footprints$footprint_id),
          !anyNA(footprints$footprint_id), !any(st_is_empty(footprints)),
          all(st_is_valid(footprints)), all(footprints$city_shape_area_sqft > 0))
SaveData(footprints, c("footprint_id"), "../output/commercial_city_building_footprints.gpkg", layer = "commercial_city_building_footprints", delete_dsn = TRUE, quiet = TRUE)
