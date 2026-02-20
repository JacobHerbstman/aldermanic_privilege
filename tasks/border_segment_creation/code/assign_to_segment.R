#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

infer_ward_year_for_era <- function(ward_panel, era) {
  years <- sort(unique(as.integer(ward_panel$year)))
  if (era == "1998_2002") return(2000)
  if (era == "2003_2014") return(2010)
  if (era == "2015_2023") return(2019)
  if (era == "post_2023") return(max(years))
  if (grepl("^[0-9]{4}_[0-9]{4}$", era)) {
    bounds <- as.integer(strsplit(era, "_")[[1]])
    return(floor(mean(bounds)))
  }
  median_year <- floor(median(years))
  median_year
}

load_segment_layer <- function(segments_gpkg, era) {
  layers <- st_layers(segments_gpkg)$name
  line_layers <- layers[!grepl("_bw(250|500|1000)$", layers)]
  if (!era %in% line_layers) {
    stop(sprintf("Era layer '%s' not found in %s. Available line layers: %s",
                 era, segments_gpkg, paste(line_layers, collapse = ", ")))
  }
  st_read(segments_gpkg, layer = era, quiet = TRUE)
}

load_wards_for_era <- function(ward_panel_gpkg, era, target_crs) {
  ward_layers <- st_layers(ward_panel_gpkg)$name
  ward <- st_read(ward_panel_gpkg, layer = ward_layers[1], quiet = TRUE)

  if ("year" %in% names(ward)) {
    pick_year <- infer_ward_year_for_era(ward, era)
    ward <- ward %>% mutate(year = as.integer(year), ward = as.integer(ward))
    if (!pick_year %in% ward$year) {
      pick_year <- max(ward$year)
    }
    ward <- ward %>% filter(year == pick_year)
  } else if ("era" %in% names(ward)) {
    ward <- ward %>% filter(.data$era == era)
  }

  st_transform(ward, target_crs)
}

coerce_points_sf <- function(points, lon_col = "lon", lat_col = "lat", point_crs = 4326) {
  if (inherits(points, "sf")) {
    return(points)
  }
  if (!all(c(lon_col, lat_col) %in% names(points))) {
    stop(sprintf("points must include columns '%s' and '%s'", lon_col, lat_col))
  }
  st_as_sf(points, coords = c(lon_col, lat_col), crs = point_crs, remove = FALSE)
}

assign_to_segment <- function(points,
                              segments_gpkg,
                              ward_panel_gpkg,
                              era,
                              lon_col = "lon",
                              lat_col = "lat",
                              point_crs = 4326) {
  segments <- load_segment_layer(segments_gpkg, era)
  points_sf <- coerce_points_sf(points, lon_col = lon_col, lat_col = lat_col, point_crs = point_crs)
  points_sf <- st_transform(points_sf, st_crs(segments))
  wards <- load_wards_for_era(ward_panel_gpkg, era, st_crs(segments))

  nearest_idx <- st_nearest_feature(points_sf, segments)
  nearest_seg <- segments[nearest_idx, ]
  dist_ft <- as.numeric(st_distance(points_sf, nearest_seg, by_element = TRUE))

  points_with_ward <- st_join(points_sf, wards %>% select(ward), join = st_within, left = TRUE)
  point_ward <- as.integer(points_with_ward$ward)

  out <- points_with_ward %>%
    mutate(
      nearest_segment_id = nearest_seg$segment_id,
      distance_to_segment_ft = dist_ft,
      nearest_ward_pair_id = nearest_seg$ward_pair_id,
      segment_type = nearest_seg$segment_type,
      ward_a = as.integer(nearest_seg$ward_a),
      ward_b = as.integer(nearest_seg$ward_b),
      side = case_when(
        point_ward == ward_a ~ "ward_a",
        point_ward == ward_b ~ "ward_b",
        TRUE ~ NA_character_
      ),
      era = era
    ) %>%
    select(
      everything(),
      nearest_segment_id,
      distance_to_segment_ft,
      nearest_ward_pair_id,
      segment_type,
      side
    )

  out
}

