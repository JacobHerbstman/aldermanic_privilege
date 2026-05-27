# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/census_block_2020_cleaning/code")

source("../../setup_environment/code/packages.R")

sf_use_s2(FALSE)

message("Loading official city boundary...")

city_boundary <- st_read("../input/city_boundary.geojson", quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(4326)

city_boundary_bbox <- st_bbox(city_boundary)
city_boundary_planar <- city_boundary %>%
    st_transform(3435) %>%
    st_union()

message("Loading 2020 census blocks (Illinois statewide)...")

blocks_2020_il <- st_read("../input/tl_2025_17_tabblock20.shp", quiet = TRUE)
message(sprintf("Loaded %s Illinois census blocks", format(nrow(blocks_2020_il), big.mark = ",")))

blocks_2020_il <- st_transform(blocks_2020_il, st_crs(city_boundary))

message("Filtering to Chicago area...")

blocks_in_bbox <- blocks_2020_il %>%
    filter(
        as.numeric(INTPTLON20) >= city_boundary_bbox["xmin"] - 0.01,
        as.numeric(INTPTLON20) <= city_boundary_bbox["xmax"] + 0.01,
        as.numeric(INTPTLAT20) >= city_boundary_bbox["ymin"] - 0.01,
        as.numeric(INTPTLAT20) <= city_boundary_bbox["ymax"] + 0.01
    )

message(sprintf("After bounding box filter: %s blocks", format(nrow(blocks_in_bbox), big.mark = ",")))

blocks_in_bbox_planar <- st_transform(blocks_in_bbox, 3435)
intersects_official_city_boundary <- lengths(
    st_intersects(blocks_in_bbox_planar, city_boundary_planar)
) > 0

blocks_chicago <- blocks_in_bbox[intersects_official_city_boundary, ]

message(sprintf("2020 census blocks in Chicago: %s", format(nrow(blocks_chicago), big.mark = ",")))

message("Formatting output to match 2010 format...")

blocks_output <- blocks_chicago %>%
    mutate(
        the_geom = st_as_text(geometry)
    ) %>%
    st_drop_geometry() %>%
    select(
        the_geom,
        GEOID20,
        STATEFP20,
        COUNTYFP20,
        TRACTCE20,
        BLOCKCE20,
        ALAND20,
        AWATER20,
        INTPTLAT20,
        INTPTLON20
    )

message("Saving output...")

write_csv(blocks_output, "../output/census_blocks_2020.csv")

message(sprintf(
    "Saved: ../output/census_blocks_2020.csv (%s rows)",
    format(nrow(blocks_output), big.mark = ",")
))

message("\nDone!")
