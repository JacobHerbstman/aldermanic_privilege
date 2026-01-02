# clean_blocks.R
# Clean 2020 census blocks: load Illinois shapefile, clip to Chicago, output CSV

source("../../setup_environment/code/packages.R")

# =============================================================================
# 1. LOAD 2010 CENSUS BLOCKS TO GET CHICAGO BOUNDARY
# =============================================================================
message("Loading 2010 census blocks to get Chicago boundary...")

# Disable s2 to avoid geometry errors with complex polygons
sf_use_s2(FALSE)

blocks_2010 <- read_csv("../input/census_blocks_2010.csv", show_col_types = FALSE) %>%
    rename(geometry = the_geom) %>%
    st_as_sf(wkt = "geometry", crs = 4269)

message(sprintf("Loaded %s 2010 census blocks", format(nrow(blocks_2010), big.mark = ",")))

# Get bounding box for fast filtering (before union to avoid expensive operation if possible)
chicago_bbox <- st_bbox(blocks_2010)

# Create Chicago boundary as union of all 2010 blocks
# Use planar CRS for union to avoid topology issues
blocks_2010_planar <- st_transform(blocks_2010, 3435) # Illinois StatePlane
chicago_boundary <- st_union(blocks_2010_planar) %>%
    st_buffer(100) # Small buffer to ensure edge blocks are included

message("Created Chicago boundary from 2010 census blocks")

# =============================================================================
# 2. LOAD 2020 CENSUS BLOCKS (ILLINOIS)
# =============================================================================
message("Loading 2020 census blocks (Illinois statewide)...")

blocks_2020_il <- st_read("../input/tl_2025_17_tabblock20.shp", quiet = TRUE)
message(sprintf("Loaded %s Illinois census blocks", format(nrow(blocks_2020_il), big.mark = ",")))

# Transform to match 2010 CRS
blocks_2020_il <- st_transform(blocks_2020_il, st_crs(blocks_2010))

# =============================================================================
# 3. CLIP TO CHICAGO
# =============================================================================
message("Filtering to Chicago area...")

# Fast pre-filter using bounding box
blocks_in_bbox <- blocks_2020_il %>%
    filter(
        as.numeric(INTPTLON20) >= chicago_bbox["xmin"] - 0.01,
        as.numeric(INTPTLON20) <= chicago_bbox["xmax"] + 0.01,
        as.numeric(INTPTLAT20) >= chicago_bbox["ymin"] - 0.01,
        as.numeric(INTPTLAT20) <= chicago_bbox["ymax"] + 0.01
    )

message(sprintf("After bounding box filter: %s blocks", format(nrow(blocks_in_bbox), big.mark = ",")))

# Precise filter: blocks whose centroid is within Chicago boundary
# Use same planar CRS as chicago_boundary
blocks_in_bbox_planar <- st_transform(blocks_in_bbox, 3435)
block_centroids <- st_centroid(blocks_in_bbox_planar)
in_chicago <- st_within(block_centroids, chicago_boundary, sparse = FALSE)[, 1]

blocks_chicago <- blocks_in_bbox[in_chicago, ]

message(sprintf("2020 census blocks in Chicago: %s", format(nrow(blocks_chicago), big.mark = ",")))

# =============================================================================
# 4. FORMAT OUTPUT TO MATCH 2010 CSV
# =============================================================================
message("Formatting output to match 2010 format...")

# Convert geometry to WKT
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

# =============================================================================
# 5. SAVE OUTPUT
# =============================================================================
message("Saving output...")

write_csv(blocks_output, "../output/census_blocks_2020.csv")

message(sprintf(
    "Saved: ../output/census_blocks_2020.csv (%s rows)",
    format(nrow(blocks_output), big.mark = ",")
))

message("\nDone!")
