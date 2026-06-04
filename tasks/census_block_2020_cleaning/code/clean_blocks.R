# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/census_block_2020_cleaning/code")

source("../../setup_environment/code/packages.R")

suppressMessages(sf_use_s2(FALSE))

city_boundary <- st_read("../input/city_boundary.geojson", quiet = TRUE) %>%
    st_make_valid() %>%
    st_transform(4326)

city_boundary_bbox <- st_bbox(city_boundary)
city_boundary_planar <- city_boundary %>%
    st_transform(3435) %>%
    st_union()

blocks_2020_il <- st_read("../input/tl_2025_17_tabblock20.shp", quiet = TRUE)
blocks_2020_il <- st_transform(blocks_2020_il, st_crs(city_boundary))

blocks_in_bbox <- blocks_2020_il %>%
    filter(
        as.numeric(INTPTLON20) >= city_boundary_bbox["xmin"] - 0.01,
        as.numeric(INTPTLON20) <= city_boundary_bbox["xmax"] + 0.01,
        as.numeric(INTPTLAT20) >= city_boundary_bbox["ymin"] - 0.01,
        as.numeric(INTPTLAT20) <= city_boundary_bbox["ymax"] + 0.01
    )

blocks_in_bbox_planar <- st_transform(blocks_in_bbox, 3435)
intersects_official_city_boundary <- lengths(
    st_intersects(blocks_in_bbox_planar, city_boundary_planar)
) > 0

blocks_chicago <- blocks_in_bbox[intersects_official_city_boundary, ]

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

write_csv(blocks_output, "../output/census_blocks_2020.csv")
