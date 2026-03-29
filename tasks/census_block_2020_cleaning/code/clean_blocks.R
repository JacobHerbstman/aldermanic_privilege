source("../../setup_environment/code/packages.R")

# Clean 2020 census blocks: load Illinois shapefile, clip to Chicago, output CSV.
# The active clipping rule uses the official city boundary. The older
# 2010-block-union rule is kept only for diagnostics so we can see which
# 2020 blocks were previously dropped.

message("Loading 2010 census blocks for boundary diagnostics...")

sf_use_s2(FALSE)

blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269)

message(sprintf("Loaded %s 2010 census blocks", format(nrow(blocks_2010), big.mark = ",")))

blocks_2010_union_planar <- blocks_2010 %>%
    st_transform(3435) %>%
    st_union() %>%
    st_buffer(100)

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

blocks_2020_il <- st_transform(blocks_2020_il, st_crs(blocks_2010))

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
block_centroids <- st_centroid(blocks_in_bbox_planar)

centroid_within_old_2010_union <- st_within(
    block_centroids,
    blocks_2010_union_planar,
    sparse = FALSE
)[, 1]

intersects_official_city_boundary <- lengths(
    st_intersects(blocks_in_bbox_planar, city_boundary_planar)
) > 0

blocks_chicago <- blocks_in_bbox[intersects_official_city_boundary, ]

boundary_diagnostics <- blocks_in_bbox %>%
    mutate(
        centroid_within_old_2010_union = centroid_within_old_2010_union,
        intersects_official_city_boundary = intersects_official_city_boundary,
        newly_retained_by_boundary_fix =
            intersects_official_city_boundary & !centroid_within_old_2010_union
    ) %>%
    filter(newly_retained_by_boundary_fix) %>%
    st_drop_geometry() %>%
    transmute(
        GEOID20,
        STATEFP20,
        COUNTYFP20,
        TRACTCE20,
        BLOCKCE20,
        ALAND20,
        AWATER20,
        INTPTLAT20,
        INTPTLON20,
        centroid_within_old_2010_union,
        intersects_official_city_boundary,
        newly_retained_by_boundary_fix
    )

message(sprintf("2020 census blocks in Chicago: %s", format(nrow(blocks_chicago), big.mark = ",")))
message(sprintf(
    "Newly retained 2020 blocks under official city-boundary intersection: %s",
    format(nrow(boundary_diagnostics), big.mark = ",")
))

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
write_csv(boundary_diagnostics, "../output/census_blocks_2020_boundary_diagnostics.csv")

message(sprintf(
    "Saved: ../output/census_blocks_2020.csv (%s rows)",
    format(nrow(blocks_output), big.mark = ",")
))
message(sprintf(
    "Saved: ../output/census_blocks_2020_boundary_diagnostics.csv (%s rows)",
    format(nrow(boundary_diagnostics), big.mark = ",")
))

message("\nDone!")
