read_amenity_layer <- function(path) {
  st_read(path, quiet = TRUE) %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_make_valid() %>%
    st_transform(3435)
}

nearest_distance_ft <- function(point_sf, target_sf, chunk_size = 100000L, label = "points") {
  if (nrow(point_sf) == 0) {
    return(numeric())
  }

  out <- numeric(nrow(point_sf))
  starts <- seq(1L, nrow(point_sf), by = chunk_size)

  for (s in starts) {
    e <- min(s + chunk_size - 1L, nrow(point_sf))
    idx <- s:e
    if (nrow(target_sf) == 1L) {
      out[idx] <- as.numeric(st_distance(point_sf[idx, ], target_sf))
    } else {
      nearest_idx <- st_nearest_feature(point_sf[idx, ], target_sf)
      nearest_geom <- target_sf[nearest_idx, ]
      out[idx] <- as.numeric(st_distance(point_sf[idx, ], nearest_geom, by_element = TRUE))
    }
    message(sprintf("  Processed %s / %s %s", format(e, big.mark = ","), format(nrow(point_sf), big.mark = ","), label))
  }

  out
}

nearest_distance_m <- function(point_sf, target_sf, chunk_size = 100000L, label = "points") {
  nearest_distance_ft(point_sf, target_sf, chunk_size, label) * 0.3048
}

lake_michigan_geom <- function(path) {
  water <- st_read(path, quiet = TRUE) %>%
    st_zm(drop = TRUE, what = "ZM") %>%
    st_make_valid() %>%
    st_transform(3435)

  if (!"name" %in% names(water)) {
    stop("Lake water shapefile is missing the `name` column.", call. = FALSE)
  }

  lake <- water %>%
    filter(!is.na(name), str_to_lower(name) == "lake michigan")

  if (nrow(lake) == 0) {
    stop("Could not find Lake Michigan in the water shapefile.", call. = FALSE)
  }

  lake_boundary <- lake %>%
    st_union() %>%
    st_boundary() %>%
    st_simplify(dTolerance = 50)

  st_as_sf(tibble(name = "Lake Michigan"), geometry = st_sfc(lake_boundary, crs = st_crs(lake)))
}

build_unique_coordinate_amenity_table <- function(df, lon_col, lat_col, schools_gpkg, parks_gpkg, major_streets_gpkg, water_shp, chunk_n = 100000L, distance_units = "feet") {
  stopifnot(lon_col %in% names(df), lat_col %in% names(df))
  if (!distance_units %in% c("feet", "meters")) {
    stop("distance_units must be one of: feet, meters.", call. = FALSE)
  }

  coords <- df %>%
    transmute(longitude = .data[[lon_col]], latitude = .data[[lat_col]]) %>%
    filter(is.finite(longitude), is.finite(latitude)) %>%
    distinct()

  coords_sf <- st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(3435)

  schools <- read_amenity_layer(schools_gpkg)
  parks <- read_amenity_layer(parks_gpkg)
  major_streets <- read_amenity_layer(major_streets_gpkg)
  lake <- lake_michigan_geom(water_shp)

  distance_fn <- if (distance_units == "meters") nearest_distance_m else nearest_distance_ft
  suffix <- if (distance_units == "meters") "m" else "ft"

  coords[[paste0("nearest_school_dist_", suffix)]] <- distance_fn(coords_sf, schools, chunk_n, "coordinates")
  coords[[paste0("nearest_park_dist_", suffix)]] <- distance_fn(coords_sf, parks, chunk_n, "coordinates")
  coords[[paste0("nearest_major_road_dist_", suffix)]] <- distance_fn(coords_sf, major_streets, chunk_n, "coordinates")
  coords[[paste0("lake_michigan_dist_", suffix)]] <- distance_fn(coords_sf, lake, chunk_n, "coordinates")

  coords
}

append_amenity_distances <- function(df, coords_tbl, lon_col, lat_col) {
  out <- as.data.table(as.data.frame(df))
  coords_dt <- as.data.table(as.data.frame(coords_tbl))

  setnames(coords_dt, c("longitude", "latitude"), c(lon_col, lat_col))
  setkeyv(out, c(lon_col, lat_col))
  setkeyv(coords_dt, c(lon_col, lat_col))

  coords_dt[out]
}

amenity_distance_diagnostics <- function(df_with_amenities, coords_tbl, dataset_label) {
  metric_cols <- c(
    "nearest_school_dist_m",
    "nearest_park_dist_m",
    "nearest_major_road_dist_m",
    "lake_michigan_dist_m"
  )
  stat_suffix <- "m"
  if (!all(metric_cols %in% names(df_with_amenities))) {
    metric_cols <- c(
      "nearest_school_dist_ft",
      "nearest_park_dist_ft",
      "nearest_major_road_dist_ft",
      "lake_michigan_dist_ft"
    )
    stat_suffix <- "ft"
  }

  bind_rows(
    tibble(
      metric = metric_cols
    ) %>%
      rowwise() %>%
      mutate(
        dataset = dataset_label,
        n_rows = nrow(df_with_amenities),
        n_unique_coords = nrow(coords_tbl),
        n_nonmissing = sum(!is.na(df_with_amenities[[metric]])),
        share_nonmissing = mean(!is.na(df_with_amenities[[metric]])),
        min_distance = min(df_with_amenities[[metric]], na.rm = TRUE),
        p50_distance = median(df_with_amenities[[metric]], na.rm = TRUE),
        p90_distance = quantile(df_with_amenities[[metric]], 0.90, na.rm = TRUE),
        mean_distance = mean(df_with_amenities[[metric]], na.rm = TRUE),
        max_distance = max(df_with_amenities[[metric]], na.rm = TRUE),
        distance_units = stat_suffix
      ) %>%
      ungroup(),
    tibble(
      dataset = dataset_label,
      metric = "metadata",
      n_rows = nrow(df_with_amenities),
      n_unique_coords = nrow(coords_tbl),
      n_nonmissing = NA_real_,
      share_nonmissing = NA_real_,
      min_distance = NA_real_,
      p50_distance = NA_real_,
      p90_distance = NA_real_,
      mean_distance = NA_real_,
      max_distance = NA_real_,
      distance_units = stat_suffix
    )
  )
}
