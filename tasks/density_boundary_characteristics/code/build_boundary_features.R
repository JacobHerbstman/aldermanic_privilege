# setwd("tasks/density_boundary_characteristics/code")
source("../../setup_environment/code/packages.R")

# Reused geometric operations retain the original 30-metre feature corridor.
line_buffer_overlap_ft <- function(segment_sf, feature_sf, buffer_ft) {
  if (nrow(segment_sf) == 0 || is.null(feature_sf) || nrow(feature_sf) == 0) {
    return(rep(0, nrow(segment_sf)))
  }

  feature_sf <- feature_sf[!st_is_empty(feature_sf), ]
  if (nrow(feature_sf) == 0) {
    return(rep(0, nrow(segment_sf)))
  }

  seg_lines <- st_sf(segment_row = seq_len(nrow(segment_sf)), geometry = st_geometry(segment_sf))
  feature_union <- st_sf(geometry = st_union(st_buffer(st_geometry(feature_sf), buffer_ft)))
  inter <- suppressWarnings(st_intersection(seg_lines, feature_union))
  out <- rep(0, nrow(segment_sf))
  if (nrow(inter) == 0) {
    return(out)
  }

  inter$overlap_ft <- as.numeric(st_length(inter))
  sums <- tapply(inter$overlap_ft, inter$segment_row, sum, na.rm = TRUE)
  out[as.integer(names(sums))] <- as.numeric(sums)
  out
}

area_share <- function(segment_sf, polygon_sf, buffer_ft) {
  if (nrow(segment_sf) == 0 || is.null(polygon_sf) || nrow(polygon_sf) == 0) {
    return(rep(0, nrow(segment_sf)))
  }

  polygon_sf <- polygon_sf[!st_is_empty(polygon_sf), ]
  if (nrow(polygon_sf) == 0) {
    return(rep(0, nrow(segment_sf)))
  }

  corridors <- st_sf(
    segment_row = seq_len(nrow(segment_sf)),
    geometry = st_buffer(st_geometry(segment_sf), buffer_ft, endCapStyle = "FLAT")
  )
  corridor_area <- as.numeric(st_area(corridors))
  polygon_union <- st_sf(geometry = st_union(st_geometry(polygon_sf)))
  inter <- suppressWarnings(st_intersection(corridors, polygon_union))
  out <- rep(0, nrow(segment_sf))
  if (nrow(inter) == 0) {
    return(out)
  }

  inter$overlap_area <- as.numeric(st_area(inter))
  sums <- tapply(inter$overlap_area, inter$segment_row, sum, na.rm = TRUE)
  idx <- as.integer(names(sums))
  out[idx] <- pmin(1, as.numeric(sums) / corridor_area[idx])
  out[!is.finite(out)] <- 0
  out
}


segments <- do.call(rbind, lapply(c("2003_2014", "2015_2023"), function(era) {
  st_read("../input/boundary_segments_1320ft.gpkg", layer = era, quiet = TRUE) |> st_transform(3435)
}))
stopifnot(!anyDuplicated(segments$segment_id))
window <- st_buffer(st_as_sfc(st_bbox(segments)), 1000)
read_nearby <- function(path) {
  st_read(path, quiet = TRUE) |> st_zm() |> st_transform(3435) |> st_make_valid() |>
    st_filter(window)
}
major <- read_nearby("../input/major_streets.geojson")
roads <- read_nearby("../input/gis_osm_roads_free_1.shp")
water <- read_nearby("../input/gis_osm_water_a_free_1.shp")
waterways <- read_nearby("../input/gis_osm_waterways_free_1.shp")
land <- read_nearby("../input/gis_osm_landuse_a_free_1.shp")
stopifnot("CLASS" %in% names(major))
expressways <- st_sf(geometry = c(st_geometry(major[major$CLASS %in% c(1, 9), ]),
  st_geometry(roads[roads$fclass %in% c("motorway", "motorway_link", "trunk", "trunk_link"), ])))
features <- tibble(segment_id = segments$segment_id,
  segment_length_ft = as.numeric(st_length(segments)),
  expressway_overlap_ft = line_buffer_overlap_ft(segments, expressways, 30 / 0.3048),
  major_overlap_arterial_ft = line_buffer_overlap_ft(segments, major[major$CLASS %in% 2, ], 30 / 0.3048),
  waterway_overlap_ft = line_buffer_overlap_ft(segments, waterways, 30 / 0.3048),
  water_area_share = area_share(segments, water, 30 / 0.3048),
  park_area_share = area_share(segments, land[land$fclass %in% c("park", "recreation_ground", "grass", "forest", "nature_reserve", "meadow", "village_green", "greenfield"), ], 30 / 0.3048),
  cemetery_area_share = area_share(segments, land[land$fclass %in% "cemetery", ], 30 / 0.3048))
stopifnot(all(features$segment_length_ft > 0), !anyNA(features))
write_csv(features, "../output/boundary_feature_measurements.csv")
