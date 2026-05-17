source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_segment_creation/code")
# Sys.setenv(REBUILD_MODE = "raw_features")
# source("build_boundary_segments.R")

st_agr("constant")

eras <- c("1998_2002", "2003_2014", "2015_2023", "post_2023")
segment_lengths_ft <- scan(text = Sys.getenv("SEGMENT_LENGTHS_FT", "1320 2640"), quiet = TRUE)
if (length(segment_lengths_ft) != 2 || any(!is.finite(segment_lengths_ft)) || any(segment_lengths_ft <= 0)) {
  stop("SEGMENT_LENGTHS_FT must contain exactly two positive numeric segment lengths in feet.", call. = FALSE)
}
segment_lengths_ft <- sort(unique(as.numeric(segment_lengths_ft)))
if (length(segment_lengths_ft) != 2) {
  stop("SEGMENT_LENGTHS_FT must contain two distinct segment lengths.", call. = FALSE)
}
segment_lengths_m <- segment_lengths_ft * 0.3048
primary_segment_length_ft <- segment_lengths_ft[1]
secondary_segment_length_ft <- segment_lengths_ft[2]
primary_segment_length_m <- segment_lengths_m[1]
secondary_segment_length_m <- segment_lengths_m[2]

segment_layer_bws_m <- scan(text = Sys.getenv("SEGMENT_LAYER_BWS_M", "100 250 400"), quiet = TRUE)
if (length(segment_layer_bws_m) == 0 || any(!is.finite(segment_layer_bws_m)) || any(segment_layer_bws_m <= 0)) {
  stop("SEGMENT_LAYER_BWS_M must contain positive numeric bandwidths in meters.", call. = FALSE)
}
bws_m <- sort(unique(as.integer(round(segment_layer_bws_m))))
bws_ft <- bws_m / 0.3048

out_primary <- sprintf("../output/boundary_segments_%dft.gpkg", as.integer(round(primary_segment_length_ft)))
out_secondary <- sprintf("../output/boundary_segments_%dft.gpkg", as.integer(round(secondary_segment_length_ft)))
out_class <- "../output/segment_classification.csv"
out_boundaries <- "../output/ward_pair_boundaries.gpkg"
out_summary <- "../output/ward_pair_boundary_summary.csv"
out_diag <- "../output/ward_pair_boundary_diagnostics.csv"
out_feature_audit <- "../output/segment_feature_source_audit.csv"

ward_panel_path <- "../input/ward_panel.gpkg"
major_streets_path <- "../input/Major_Streets.shp"
osm_roads_path <- "../input/gis_osm_roads_free_1.shp"
osm_landuse_path <- "../input/gis_osm_landuse_a_free_1.shp"
osm_water_path <- "../input/gis_osm_water_a_free_1.shp"
osm_waterways_path <- "../input/gis_osm_waterways_free_1.shp"
snapshot_primary_path <- "../input/canonical_boundary_segments_1320ft.gpkg"
snapshot_secondary_path <- "../input/canonical_boundary_segments_2640ft.gpkg"
snapshot_class_path <- "../input/canonical_segment_classification.csv"
snapshot_boundaries_path <- "../input/canonical_ward_pair_boundaries.gpkg"

rebuild_mode <- tolower(Sys.getenv("REBUILD_MODE", "raw"))
if (!rebuild_mode %in% c("snapshot", "reuse", "raw_features", "raw")) {
  stop("REBUILD_MODE must be 'snapshot', 'reuse', 'raw_features', or 'raw'.", call. = FALSE)
}

feature_buffer_m <- as.numeric(Sys.getenv("SEGMENT_FEATURE_BUFFER_M", "30"))
legacy_feature_buffer_ft <- Sys.getenv("SEGMENT_FEATURE_BUFFER_FT", "")
if (nzchar(legacy_feature_buffer_ft)) {
  feature_buffer_ft <- as.numeric(legacy_feature_buffer_ft)
  feature_buffer_m <- feature_buffer_ft * 0.3048
} else {
  feature_buffer_ft <- feature_buffer_m / 0.3048
}
if (!is.finite(feature_buffer_m) || feature_buffer_m <= 0) {
  stop("SEGMENT_FEATURE_BUFFER_M must be positive.", call. = FALSE)
}
if (!is.finite(feature_buffer_ft) || feature_buffer_ft <= 0) {
  stop("SEGMENT_FEATURE_BUFFER_FT must be positive when supplied.", call. = FALSE)
}

expected_layer_names <- c(
  eras,
  as.vector(outer(eras, paste0("bw", bws_m, "m"), paste, sep = "_"))
)

required_segment_cols <- c(
  "segment_id", "ward_pair_id", "ward_a", "ward_b", "era", "segment_number",
  "n_segments_in_pair", "segment_length_m", "segment_length_ft", "centroid_lat", "centroid_lon",
  "segment_type", "nearest_street_name", "nearest_street_class",
  "nearest_street_class_mapped", "distance_to_nearest_street_m", "distance_to_nearest_street_ft",
  "major_overlap_arterial_m", "major_overlap_arterial_ft",
  "major_overlap_collector_m", "major_overlap_collector_ft",
  "major_overlap_residential_m", "major_overlap_residential_ft",
  "water_area_share", "park_area_share",
  "cemetery_area_share", "park_cemetery_area_share",
  "waterway_overlap_m", "waterway_overlap_ft",
  "major_overlap_expressway_m", "major_overlap_expressway_ft",
  "major_overlap_ramp_m", "major_overlap_ramp_ft",
  "osm_overlap_expressway_m", "osm_overlap_expressway_ft",
  "expressway_overlap_m", "expressway_overlap_ft",
  "feature_buffer_m", "feature_buffer_ft"
)

segment_validity_cols <- c(
  "raw_segment_id", "analysis_segment_id", "valid_segment", "invalid_reason", "merge_reason",
  "component_type", "short_segment", "terminal_segment", "segment_lt500ft", "segment_lt1000ft",
  "nearest_same_pair_segment_id", "nearest_same_pair_segment_number", "nearest_same_pair_distance_ft",
  "n_touching_same_pair_1ft", "n_near_same_pair_10ft", "two_sided_all_offsets",
  "left_offset_ward_5ft", "right_offset_ward_5ft", "two_sided_pass_5ft",
  "left_offset_ward_20ft", "right_offset_ward_20ft", "two_sided_pass_20ft",
  "left_offset_ward_50ft", "right_offset_ward_50ft", "two_sided_pass_50ft",
  "left_offset_ward_100ft", "right_offset_ward_100ft", "two_sided_pass_100ft"
)

ensure_segment_cols <- function(x, target_len_ft, target_len_m = target_len_ft * 0.3048) {
  for (nm in required_segment_cols) {
    if (!nm %in% names(x)) {
      if (nm %in% c("segment_type", "nearest_street_name", "nearest_street_class_mapped")) {
        x[[nm]] <- if (nm == "segment_type") "no_feature" else NA_character_
      } else if (nm %in% c("nearest_street_class")) {
        x[[nm]] <- NA_real_
      } else if (nm %in% c("segment_id", "ward_pair_id", "era")) {
        x[[nm]] <- NA_character_
      } else {
        x[[nm]] <- NA_real_
      }
    }
  }

  x$ward_a <- as.integer(x$ward_a)
  x$ward_b <- as.integer(x$ward_b)
  x$segment_number <- as.integer(x$segment_number)
  x$n_segments_in_pair <- as.integer(x$n_segments_in_pair)
  x$segment_length_m <- as.numeric(x$segment_length_m)
  x$segment_length_ft <- as.numeric(x$segment_length_ft)
  if (all(!is.finite(x$segment_length_m)) && any(is.finite(x$segment_length_ft))) {
    x$segment_length_m <- x$segment_length_ft * 0.3048
  }
  if (all(!is.finite(x$segment_length_ft)) && any(is.finite(x$segment_length_m))) {
    x$segment_length_ft <- x$segment_length_m / 0.3048
  }
  x$centroid_lat <- as.numeric(x$centroid_lat)
  x$centroid_lon <- as.numeric(x$centroid_lon)
  x$nearest_street_class <- suppressWarnings(as.numeric(x$nearest_street_class))
  x$distance_to_nearest_street_m <- as.numeric(x$distance_to_nearest_street_m)
  x$distance_to_nearest_street_ft <- as.numeric(x$distance_to_nearest_street_ft)
  if (all(!is.finite(x$distance_to_nearest_street_m)) && any(is.finite(x$distance_to_nearest_street_ft))) {
    x$distance_to_nearest_street_m <- x$distance_to_nearest_street_ft * 0.3048
  }
  if (all(!is.finite(x$distance_to_nearest_street_ft)) && any(is.finite(x$distance_to_nearest_street_m))) {
    x$distance_to_nearest_street_ft <- x$distance_to_nearest_street_m / 0.3048
  }
  x$major_overlap_arterial_m <- as.numeric(x$major_overlap_arterial_m)
  x$major_overlap_arterial_ft <- as.numeric(x$major_overlap_arterial_ft)
  x$major_overlap_collector_m <- as.numeric(x$major_overlap_collector_m)
  x$major_overlap_collector_ft <- as.numeric(x$major_overlap_collector_ft)
  x$major_overlap_residential_m <- as.numeric(x$major_overlap_residential_m)
  x$major_overlap_residential_ft <- as.numeric(x$major_overlap_residential_ft)
  x$water_area_share <- as.numeric(x$water_area_share)
  x$park_area_share <- as.numeric(x$park_area_share)
  x$cemetery_area_share <- as.numeric(x$cemetery_area_share)
  x$park_cemetery_area_share <- as.numeric(x$park_cemetery_area_share)
  x$waterway_overlap_m <- as.numeric(x$waterway_overlap_m)
  x$waterway_overlap_ft <- as.numeric(x$waterway_overlap_ft)
  x$major_overlap_expressway_m <- as.numeric(x$major_overlap_expressway_m)
  x$major_overlap_expressway_ft <- as.numeric(x$major_overlap_expressway_ft)
  x$major_overlap_ramp_m <- as.numeric(x$major_overlap_ramp_m)
  x$major_overlap_ramp_ft <- as.numeric(x$major_overlap_ramp_ft)
  x$osm_overlap_expressway_m <- as.numeric(x$osm_overlap_expressway_m)
  x$osm_overlap_expressway_ft <- as.numeric(x$osm_overlap_expressway_ft)
  x$expressway_overlap_m <- as.numeric(x$expressway_overlap_m)
  x$expressway_overlap_ft <- as.numeric(x$expressway_overlap_ft)
  for (nm in c(
    "major_overlap_arterial", "major_overlap_collector", "major_overlap_residential",
    "waterway_overlap", "major_overlap_expressway", "major_overlap_ramp",
    "osm_overlap_expressway", "expressway_overlap"
  )) {
    m_nm <- paste0(nm, "_m")
    ft_nm <- paste0(nm, "_ft")
    if (all(!is.finite(x[[m_nm]])) && any(is.finite(x[[ft_nm]]))) {
      x[[m_nm]] <- x[[ft_nm]] * 0.3048
    }
    if (all(!is.finite(x[[ft_nm]])) && any(is.finite(x[[m_nm]]))) {
      x[[ft_nm]] <- x[[m_nm]] / 0.3048
    }
  }
  x$feature_buffer_m <- as.numeric(x$feature_buffer_m)
  x$feature_buffer_ft <- as.numeric(x$feature_buffer_ft)
  if (all(!is.finite(x$feature_buffer_m)) && any(is.finite(x$feature_buffer_ft))) {
    x$feature_buffer_m <- x$feature_buffer_ft * 0.3048
  }
  if (all(!is.finite(x$feature_buffer_ft)) && any(is.finite(x$feature_buffer_m))) {
    x$feature_buffer_ft <- x$feature_buffer_m / 0.3048
  }

  x$target_length_ft <- as.numeric(target_len_ft)
  x$target_length_m <- as.numeric(target_len_m)
  x
}

read_segment_layers <- function(path, target_len_ft, target_len_m = target_len_ft * 0.3048) {
  if (!file.exists(path)) {
    stop(sprintf("Missing segment GPKG: %s", path), call. = FALSE)
  }

  missing_layers <- setdiff(eras, st_layers(path)$name)
  if (length(missing_layers) > 0) {
    stop(
      sprintf("Segment GPKG %s missing layers: %s", path, paste(missing_layers, collapse = ", ")),
      call. = FALSE
    )
  }

  out <- list()
  for (era_i in eras) {
    out[[era_i]] <- ensure_segment_cols(st_read(path, layer = era_i, quiet = TRUE), target_len_ft, target_len_m)
  }
  do.call(rbind, out)
}

read_boundary_layers <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Missing ward-pair boundary GPKG: %s", path), call. = FALSE)
  }

  missing_layers <- setdiff(eras, st_layers(path)$name)
  if (length(missing_layers) > 0) {
    stop(
      sprintf("Boundary GPKG %s missing layers: %s", path, paste(missing_layers, collapse = ", ")),
      call. = FALSE
    )
  }

  out <- list()
  for (era_i in eras) {
    out[[era_i]] <- st_read(path, layer = era_i, quiet = TRUE)
  }
  out
}

validate_segment_features <- function(class_dt, target_segment_length_ft = primary_segment_length_ft) {
  setDT(class_dt)
  for (nm in c("target_length", "waterway_overlap", "major_overlap_arterial", "expressway_overlap")) {
    m_nm <- paste0(nm, "_m")
    ft_nm <- paste0(nm, "_ft")
    if (!m_nm %in% names(class_dt) && ft_nm %in% names(class_dt)) {
      class_dt[, (m_nm) := as.numeric(get(ft_nm)) * 0.3048]
    }
    if (!ft_nm %in% names(class_dt) && m_nm %in% names(class_dt)) {
      class_dt[, (ft_nm) := as.numeric(get(m_nm)) / 0.3048]
    }
  }
  required_cols <- c(
    "segment_type",
    "water_area_share",
    "park_area_share",
    "cemetery_area_share",
    "park_cemetery_area_share",
    "waterway_overlap_m",
    "major_overlap_arterial_m",
    "expressway_overlap_m",
    "target_length_ft",
    "target_length_m"
  )
  missing_cols <- setdiff(required_cols, names(class_dt))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("segment_classification.csv missing feature columns: %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  x <- copy(class_dt)
  x[, target_length_ft := as.numeric(target_length_ft)]
  x <- x[abs(target_length_ft - target_segment_length_ft) < 1e-8]
  if (nrow(x) == 0) {
    stop(sprintf("No %.0fft rows found in segment_classification.csv.", target_segment_length_ft), call. = FALSE)
  }

  x[, water_area_share := as.numeric(water_area_share)]
  x[, park_area_share := as.numeric(park_area_share)]
  x[, cemetery_area_share := as.numeric(cemetery_area_share)]
  x[, park_cemetery_area_share := as.numeric(park_cemetery_area_share)]
  x[, waterway_overlap_m := as.numeric(waterway_overlap_m)]
  x[, major_overlap_arterial_m := as.numeric(major_overlap_arterial_m)]
  x[, expressway_overlap_m := as.numeric(expressway_overlap_m)]
  x[!is.finite(water_area_share), water_area_share := 0]
  x[!is.finite(park_area_share), park_area_share := 0]
  x[!is.finite(cemetery_area_share), cemetery_area_share := 0]
  x[!is.finite(park_cemetery_area_share), park_cemetery_area_share := 0]
  x[!is.finite(waterway_overlap_m), waterway_overlap_m := 0]
  x[!is.finite(major_overlap_arterial_m), major_overlap_arterial_m := 0]
  x[!is.finite(expressway_overlap_m), expressway_overlap_m := 0]

  has_features <- any(
    !(as.character(x$segment_type) %in% c("no_feature", "", NA_character_)) |
      x$water_area_share > 0 |
      x$park_area_share > 0 |
      x$cemetery_area_share > 0 |
      x$waterway_overlap_m > 0 |
      x$expressway_overlap_m > 0 |
      x$major_overlap_arterial_m > 0,
    na.rm = TRUE
  )
  if (!has_features) {
    stop(
      sprintf("segment_classification.csv has no park/water/arterial feature metrics after the %.0fft filter.", target_segment_length_ft),
      call. = FALSE
    )
  }

  if (!any(x$major_overlap_arterial_m > 0 | x$expressway_overlap_m > 0, na.rm = TRUE)) {
    stop(
      sprintf("segment_classification.csv has no arterial or expressway overlap after the %.0fft filter. Check the road-buffer overlap construction.", target_segment_length_ft),
      call. = FALSE
    )
  }
}

split_linestring_into_segments <- function(line_geom, target_len, crs_obj) {
  pieces <- suppressWarnings(st_cast(st_sfc(line_geom, crs = crs_obj), "LINESTRING", warn = FALSE))
  if (length(pieces) == 0) {
    return(st_sfc(crs = crs_obj))
  }

  interpolate_point <- function(coords_xy, cumdist, target_dist, tol = 1e-7) {
    if (target_dist <= tol) {
      return(coords_xy[1, , drop = FALSE])
    }

    total_len <- cumdist[length(cumdist)]
    if (target_dist >= total_len - tol) {
      return(coords_xy[nrow(coords_xy), , drop = FALSE])
    }

    end_idx <- which(cumdist >= target_dist - tol)[1]
    if (abs(cumdist[end_idx] - target_dist) <= tol) {
      return(coords_xy[end_idx, , drop = FALSE])
    }

    start_idx <- max(1L, end_idx - 1L)
    seg_len <- cumdist[end_idx] - cumdist[start_idx]
    if (!is.finite(seg_len) || seg_len <= tol) {
      return(coords_xy[start_idx, , drop = FALSE])
    }

    weight <- (target_dist - cumdist[start_idx]) / seg_len
    pt <- coords_xy[start_idx, ] + weight * (coords_xy[end_idx, ] - coords_xy[start_idx, ])
    matrix(pt, nrow = 1L, dimnames = list(NULL, colnames(coords_xy)))
  }

  build_subline <- function(coords_xy, start_dist, end_dist, tol = 1e-7) {
    seglen <- sqrt(rowSums((coords_xy[-1, , drop = FALSE] - coords_xy[-nrow(coords_xy), , drop = FALSE])^2))
    cumdist <- c(0, cumsum(seglen))

    start_pt <- interpolate_point(coords_xy, cumdist, start_dist, tol = tol)
    end_pt <- interpolate_point(coords_xy, cumdist, end_dist, tol = tol)
    interior_idx <- which(cumdist > start_dist + tol & cumdist < end_dist - tol)

    pts <- rbind(start_pt, coords_xy[interior_idx, , drop = FALSE], end_pt)
    if (nrow(pts) < 2) {
      return(NULL)
    }

    keep <- c(TRUE, rowSums(abs(diff(pts)) > tol) > 0)
    pts <- pts[keep, , drop = FALSE]
    if (nrow(pts) < 2) {
      return(NULL)
    }
    if (all(abs(pts[1, ] - pts[nrow(pts), ]) < tol)) {
      return(NULL)
    }

    st_linestring(as.matrix(pts))
  }

  seg_out <- list()
  for (k in seq_along(pieces)) {
    ls <- pieces[k]
    len_k <- as.numeric(st_length(ls))
    if (!is.finite(len_k) || len_k <= 0) next

    nseg <- max(1L, as.integer(round(len_k / target_len)))
    if (nseg == 1L) {
      seg_out[[length(seg_out) + 1L]] <- ls[[1]]
      next
    }

    coords <- st_coordinates(ls)
    coords_xy <- coords[, c("X", "Y"), drop = FALSE]
    if (nrow(coords_xy) < 2) {
      seg_out[[length(seg_out) + 1L]] <- ls[[1]]
      next
    }

    break_dists <- seq(0, len_k, length.out = nseg + 1L)
    for (i in seq_len(length(break_dists) - 1L)) {
      seg_i <- build_subline(coords_xy, break_dists[i], break_dists[i + 1L])
      if (is.null(seg_i)) next
      seg_out[[length(seg_out) + 1L]] <- seg_i
    }
  }

  if (length(seg_out) == 0L) {
    return(st_sfc(crs = crs_obj))
  }
  st_sfc(seg_out, crs = crs_obj)
}

build_segments_raw <- function(boundary_list, target_len_ft, target_len_m = target_len_ft * 0.3048) {
  rows <- list()
  geoms <- list()

  for (era_i in eras) {
    b <- boundary_list[[era_i]]
    if (is.null(b) || nrow(b) == 0) next

    for (r in seq_len(nrow(b))) {
      segs <- split_linestring_into_segments(st_geometry(b[r, ])[[1]], target_len_ft, st_crs(b))
      if (length(segs) == 0) next

      n_pair <- length(segs)
      for (k in seq_along(segs)) {
        seg_geom <- segs[k]
        ctd <- st_centroid(st_transform(seg_geom, 4326))
        cc <- st_coordinates(ctd)

        rows[[length(rows) + 1L]] <- data.table(
          segment_id = sprintf("%d_%d_%s_%d", b$ward_a[r], b$ward_b[r], era_i, k),
          ward_pair_id = as.character(b$ward_pair_id[r]),
          ward_a = as.integer(b$ward_a[r]),
          ward_b = as.integer(b$ward_b[r]),
          era = era_i,
          segment_number = as.integer(k),
          n_segments_in_pair = as.integer(n_pair),
          segment_length_m = as.numeric(st_length(seg_geom)) * 0.3048,
          segment_length_ft = as.numeric(st_length(seg_geom)),
          centroid_lat = as.numeric(cc[1, "Y"]),
          centroid_lon = as.numeric(cc[1, "X"]),
          segment_type = "no_feature",
          nearest_street_name = NA_character_,
          nearest_street_class = NA_real_,
          nearest_street_class_mapped = NA_character_,
          distance_to_nearest_street_m = NA_real_,
          distance_to_nearest_street_ft = NA_real_,
          major_overlap_arterial_m = 0,
          major_overlap_arterial_ft = 0,
          major_overlap_collector_m = 0,
          major_overlap_collector_ft = 0,
          major_overlap_residential_m = 0,
          major_overlap_residential_ft = 0,
          water_area_share = 0,
          park_area_share = 0,
          cemetery_area_share = 0,
          park_cemetery_area_share = 0,
          waterway_overlap_m = 0,
          waterway_overlap_ft = 0,
          major_overlap_expressway_m = 0,
          major_overlap_expressway_ft = 0,
          major_overlap_ramp_m = 0,
          major_overlap_ramp_ft = 0,
          osm_overlap_expressway_m = 0,
          osm_overlap_expressway_ft = 0,
          expressway_overlap_m = 0,
          expressway_overlap_ft = 0,
          feature_buffer_m = feature_buffer_m,
          feature_buffer_ft = feature_buffer_ft,
          target_length_ft = as.numeric(target_len_ft),
          target_length_m = as.numeric(target_len_m)
        )
        geoms[[length(geoms) + 1L]] <- seg_geom[[1]]
      }
    }
  }

  if (length(rows) == 0) {
    return(st_sf(data.table(), geom = st_sfc(crs = st_crs(boundary_list[[eras[1]]]))))
  }

  dt <- rbindlist(rows)
  sf <- st_sf(dt, geom = st_sfc(geoms, crs = st_crs(boundary_list[[eras[1]]])))
  sf <- sf[order(sf$era, sf$ward_pair_id, sf$segment_number), ]
  sf
}

read_filtered_layer <- function(path, filter_geom_3435, label) {
  if (!file.exists(path)) {
    stop(sprintf("Missing %s layer: %s", label, path), call. = FALSE)
  }

  x <- st_read(path, quiet = TRUE)

  if (nrow(x) == 0) {
    return(st_sf(data.table(), geometry = st_sfc(crs = st_crs(filter_geom_3435))))
  }

  x <- st_zm(x, drop = TRUE, what = "ZM")
  if (is.na(st_crs(x))) {
    st_crs(x) <- st_crs(4326)
  }
  x <- st_make_valid(st_transform(x, st_crs(filter_geom_3435)))
  suppressWarnings(st_filter(x, filter_geom_3435, .predicate = st_intersects))
}

clean_feature_text <- function(x) {
  out <- tolower(trimws(as.character(x)))
  out[is.na(out)] <- ""
  out
}

load_feature_layers <- function(ward_panel) {
  city_geom <- ward_panel |>
    st_make_valid() |>
    st_union() |>
    st_buffer(1500)

  major_streets <- read_filtered_layer(major_streets_path, city_geom, "Major Streets")
  if (nrow(major_streets) > 0) {
    major_streets$source_layer <- "major_streets"
    major_streets$road_name <- as.character(major_streets$STREET_NAM)
    major_streets$road_class_code <- suppressWarnings(as.integer(major_streets$CLASS))
    major_streets$road_class_mapped <- fifelse(
      major_streets$road_class_code == 1L, "expressway",
      fifelse(
        major_streets$road_class_code == 2L, "arterial",
        fifelse(
          major_streets$road_class_code == 3L, "collector",
          fifelse(major_streets$road_class_code == 9L, "ramp", "other")
        )
      )
    )
  } else {
    major_streets$source_layer <- character()
    major_streets$road_name <- character()
    major_streets$road_class_code <- integer()
    major_streets$road_class_mapped <- character()
  }

  osm_roads <- read_filtered_layer(osm_roads_path, city_geom, "OSM roads")
  if (nrow(osm_roads) > 0) {
    osm_roads$source_layer <- "osm_roads"
    osm_roads$road_name <- as.character(osm_roads$name)
    osm_roads$road_class_code <- suppressWarnings(as.integer(osm_roads$code))
    osm_roads$fclass_clean <- clean_feature_text(osm_roads$fclass)
    osm_roads$road_class_mapped <- fifelse(
      osm_roads$fclass_clean %in% c("motorway", "motorway_link", "trunk", "trunk_link"), "expressway",
      fifelse(
        osm_roads$fclass_clean %in% c("primary", "primary_link", "secondary", "secondary_link"), "arterial",
        fifelse(
          osm_roads$fclass_clean %in% c("tertiary", "tertiary_link"), "collector",
          fifelse(osm_roads$fclass_clean %in% c("residential", "living_street", "unclassified", "service"), "residential", "other")
        )
      )
    )
  } else {
    osm_roads$source_layer <- character()
    osm_roads$road_name <- character()
    osm_roads$road_class_code <- integer()
    osm_roads$fclass_clean <- character()
    osm_roads$road_class_mapped <- character()
  }

  osm_major_roads <- osm_roads[osm_roads$road_class_mapped %in% c("expressway", "arterial", "collector"), ]
  roads_all <- rbind(
    major_streets[, c("source_layer", "road_name", "road_class_code", "road_class_mapped", "geometry")],
    osm_major_roads[, c("source_layer", "road_name", "road_class_code", "road_class_mapped", "geometry")]
  )
  roads_all <- roads_all[!st_is_empty(roads_all), ]

  osm_landuse <- read_filtered_layer(osm_landuse_path, city_geom, "OSM landuse polygons")
  if (nrow(osm_landuse) > 0) {
    osm_landuse$fclass_clean <- clean_feature_text(osm_landuse$fclass)
  } else {
    osm_landuse$fclass_clean <- character()
  }

  osm_water <- read_filtered_layer(osm_water_path, city_geom, "OSM water polygons")
  if (nrow(osm_water) > 0) {
    osm_water$fclass_clean <- clean_feature_text(osm_water$fclass)
  } else {
    osm_water$fclass_clean <- character()
  }

  osm_waterways <- read_filtered_layer(osm_waterways_path, city_geom, "OSM waterways")
  if (nrow(osm_waterways) > 0) {
    osm_waterways$fclass_clean <- clean_feature_text(osm_waterways$fclass)
  } else {
    osm_waterways$fclass_clean <- character()
  }

  park_fclasses <- c(
    "park", "recreation_ground", "grass", "forest", "nature_reserve",
    "meadow", "village_green", "greenfield"
  )
  cemetery_fclasses <- c("cemetery")

  list(
    roads_all = roads_all,
    major_expressway = major_streets[major_streets$road_class_mapped == "expressway", ],
    major_ramp = major_streets[major_streets$road_class_mapped == "ramp", ],
    major_arterial = major_streets[major_streets$road_class_mapped == "arterial", ],
    major_collector = major_streets[major_streets$road_class_mapped == "collector", ],
    osm_expressway = osm_roads[osm_roads$road_class_mapped == "expressway", ],
    water_polygons = osm_water,
    waterways = osm_waterways,
    park_polygons = osm_landuse[osm_landuse$fclass_clean %in% park_fclasses, ],
    cemetery_polygons = osm_landuse[osm_landuse$fclass_clean %in% cemetery_fclasses, ],
    audit = data.table(
      source_layer = c(
        "major_streets", "osm_roads", "osm_water_polygons", "osm_waterways",
        "osm_landuse_parks", "osm_landuse_cemeteries"
      ),
      rows_after_filter = c(
        nrow(major_streets),
        nrow(osm_roads),
        nrow(osm_water),
        nrow(osm_waterways),
        nrow(osm_landuse[osm_landuse$fclass_clean %in% park_fclasses, ]),
        nrow(osm_landuse[osm_landuse$fclass_clean %in% cemetery_fclasses, ])
      ),
      source_path = c(
        major_streets_path,
        osm_roads_path,
        osm_water_path,
        osm_waterways_path,
        osm_landuse_path,
        osm_landuse_path
      )
    )
  )
}

combine_feature_layers <- function(...) {
  layers <- list(...)
  layers <- layers[vapply(layers, function(x) !is.null(x) && nrow(x) > 0, logical(1))]
  if (length(layers) == 0) {
    return(NULL)
  }

  crs_obj <- st_crs(layers[[1]])
  geoms <- do.call(c, lapply(layers, st_geometry))
  st_sf(geometry = st_sfc(geoms, crs = crs_obj))
}

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

nearest_road_fields <- function(segment_sf, roads_all) {
  n <- nrow(segment_sf)
  out <- data.table(
    nearest_street_name = rep(NA_character_, n),
    nearest_street_class = rep(NA_real_, n),
    nearest_street_class_mapped = rep(NA_character_, n),
    distance_to_nearest_street_m = rep(NA_real_, n),
    distance_to_nearest_street_ft = rep(NA_real_, n)
  )

  if (n == 0 || is.null(roads_all) || nrow(roads_all) == 0) {
    return(out)
  }

  segment_midpoints <- st_centroid(st_geometry(segment_sf))
  idx <- st_nearest_feature(segment_midpoints, roads_all)
  out$nearest_street_name <- as.character(roads_all$road_name[idx])
  out$nearest_street_class <- suppressWarnings(as.numeric(roads_all$road_class_code[idx]))
  out$nearest_street_class_mapped <- as.character(roads_all$road_class_mapped[idx])
  out$distance_to_nearest_street_ft <- as.numeric(st_distance(segment_midpoints, roads_all[idx, ], by_element = TRUE))
  out$distance_to_nearest_street_m <- out$distance_to_nearest_street_ft * 0.3048
  out
}

classify_segments_from_features <- function(segment_sf, features) {
  if (nrow(segment_sf) == 0) {
    return(segment_sf)
  }

  nearest <- nearest_road_fields(segment_sf, features$roads_all)
  segment_sf$nearest_street_name <- nearest$nearest_street_name
  segment_sf$nearest_street_class <- nearest$nearest_street_class
  segment_sf$nearest_street_class_mapped <- nearest$nearest_street_class_mapped
  segment_sf$distance_to_nearest_street_m <- nearest$distance_to_nearest_street_m
  segment_sf$distance_to_nearest_street_ft <- nearest$distance_to_nearest_street_ft

  segment_sf$major_overlap_expressway_ft <- line_buffer_overlap_ft(segment_sf, features$major_expressway, feature_buffer_ft)
  segment_sf$major_overlap_ramp_ft <- line_buffer_overlap_ft(segment_sf, features$major_ramp, feature_buffer_ft)
  segment_sf$major_overlap_arterial_ft <- line_buffer_overlap_ft(segment_sf, features$major_arterial, feature_buffer_ft)
  segment_sf$major_overlap_collector_ft <- line_buffer_overlap_ft(segment_sf, features$major_collector, feature_buffer_ft)
  segment_sf$major_overlap_residential_ft <- 0
  segment_sf$osm_overlap_expressway_ft <- line_buffer_overlap_ft(segment_sf, features$osm_expressway, feature_buffer_ft)

  expressway_features <- combine_feature_layers(
    features$major_expressway,
    features$major_ramp,
    features$osm_expressway
  )
  segment_sf$expressway_overlap_ft <- line_buffer_overlap_ft(segment_sf, expressway_features, feature_buffer_ft)
  segment_sf$waterway_overlap_ft <- line_buffer_overlap_ft(segment_sf, features$waterways, feature_buffer_ft)

  segment_sf$water_area_share <- area_share(segment_sf, features$water_polygons, feature_buffer_ft)
  segment_sf$park_area_share <- area_share(segment_sf, features$park_polygons, feature_buffer_ft)
  segment_sf$cemetery_area_share <- area_share(segment_sf, features$cemetery_polygons, feature_buffer_ft)
  segment_sf$park_cemetery_area_share <- pmin(
    1,
    segment_sf$park_area_share + segment_sf$cemetery_area_share
  )
  segment_sf$major_overlap_expressway_m <- segment_sf$major_overlap_expressway_ft * 0.3048
  segment_sf$major_overlap_ramp_m <- segment_sf$major_overlap_ramp_ft * 0.3048
  segment_sf$major_overlap_arterial_m <- segment_sf$major_overlap_arterial_ft * 0.3048
  segment_sf$major_overlap_collector_m <- segment_sf$major_overlap_collector_ft * 0.3048
  segment_sf$major_overlap_residential_m <- segment_sf$major_overlap_residential_ft * 0.3048
  segment_sf$osm_overlap_expressway_m <- segment_sf$osm_overlap_expressway_ft * 0.3048
  segment_sf$expressway_overlap_m <- segment_sf$expressway_overlap_ft * 0.3048
  segment_sf$waterway_overlap_m <- segment_sf$waterway_overlap_ft * 0.3048
  segment_sf$feature_buffer_m <- feature_buffer_m
  segment_sf$feature_buffer_ft <- feature_buffer_ft

  feature_flags <- data.table(
    expressway = segment_sf$expressway_overlap_m > 0,
    arterial = segment_sf$major_overlap_arterial_m > 0,
    collector = segment_sf$major_overlap_collector_m > 0,
    residential = segment_sf$major_overlap_residential_m > 0,
    park_water = (
      segment_sf$water_area_share > 0 |
        segment_sf$park_area_share > 0 |
        segment_sf$waterway_overlap_m > 0
    ),
    cemetery = segment_sf$cemetery_area_share > 0
  )
  feature_count <- rowSums(feature_flags, na.rm = TRUE)
  segment_sf$segment_type <- "no_feature"
  segment_sf$segment_type[feature_count > 1] <- "mixed"
  for (nm in names(feature_flags)) {
    segment_sf$segment_type[feature_count == 1 & feature_flags[[nm]]] <- nm
  }

  segment_sf
}

# -----------------------------------------------------------------------------
# Build path
# -----------------------------------------------------------------------------
out_primary_layers_valid <- FALSE
if (file.exists(out_primary)) {
  out_primary_layers_valid <- setequal(st_layers(out_primary)$name, expected_layer_names)
}

out_secondary_layers_valid <- FALSE
if (file.exists(out_secondary)) {
  out_secondary_layers_valid <- setequal(st_layers(out_secondary)$name, expected_layer_names)
}

can_reuse <- rebuild_mode == "reuse" &&
  out_primary_layers_valid &&
  out_secondary_layers_valid &&
  file.exists(out_class) &&
  file.exists(out_boundaries)

if (rebuild_mode == "snapshot") {
  message("Rebuild mode: restore canonical segment artifacts from data_raw/boundaries/segments.")

  seg_primary <- read_segment_layers(snapshot_primary_path, primary_segment_length_ft, primary_segment_length_m)
  seg_secondary <- read_segment_layers(snapshot_secondary_path, secondary_segment_length_ft, secondary_segment_length_m)
  boundary_list <- read_boundary_layers(snapshot_boundaries_path)
  snapshot_class <- fread(snapshot_class_path)
  validate_segment_features(snapshot_class)
  feature_audit <- data.table(
    source_layer = "canonical_snapshot",
    rows_after_filter = nrow(snapshot_class),
    source_path = snapshot_class_path
  )
  build_mode <- "snapshot"
} else if (can_reuse) {
  message("Rebuild mode: reuse existing canonical segment artifacts with schema checks.")

  seg_primary_by_era <- list()
  for (era_i in eras) {
    seg_primary_by_era[[era_i]] <- ensure_segment_cols(
      st_read(out_primary, layer = era_i, quiet = TRUE),
      primary_segment_length_ft,
      primary_segment_length_m
    )
  }
  seg_primary <- do.call(rbind, seg_primary_by_era)

  seg_secondary_by_era <- list()
  for (era_i in eras) {
    seg_secondary_by_era[[era_i]] <- ensure_segment_cols(
      st_read(out_secondary, layer = era_i, quiet = TRUE),
      secondary_segment_length_ft,
      secondary_segment_length_m
    )
  }
  seg_secondary <- do.call(rbind, seg_secondary_by_era)

  boundaries <- seg_primary[, c("ward_a", "ward_b", "ward_pair_id", "era", "segment_length_ft")] |>
    dplyr::group_by(ward_a, ward_b, ward_pair_id, era) |>
    dplyr::summarise(length_ft = sum(segment_length_ft, na.rm = TRUE), .groups = "drop")
  boundaries <- boundaries[, c("ward_a", "ward_b", "ward_pair_id", "era", "length_ft")]
  boundaries <- boundaries[order(boundaries$era, boundaries$ward_pair_id), ]
  boundary_list <- split(boundaries, boundaries$era)
  boundary_list <- lapply(eras, function(ei) boundary_list[[ei]])
  names(boundary_list) <- eras
  feature_audit <- data.table(
    source_layer = "existing_output_reuse",
    rows_after_filter = nrow(st_drop_geometry(seg_primary)) + nrow(st_drop_geometry(seg_secondary)),
    source_path = "../output/boundary_segments_*.gpkg"
  )
  build_mode <- "reuse"
} else if (rebuild_mode == "raw_features") {
  message("Rebuild mode: canonical segment geometry with raw Major Streets and OSM feature overlays.")
  stopifnot(file.exists(ward_panel_path))

  ward_panel <- st_read(ward_panel_path, quiet = TRUE)
  ward_panel$year <- as.integer(ward_panel$year)
  ward_panel$ward <- as.integer(ward_panel$ward)
  ward_panel <- ward_panel[order(ward_panel$year, ward_panel$ward), ]

  boundary_list <- read_boundary_layers(snapshot_boundaries_path)
  feature_layers <- load_feature_layers(ward_panel)
  seg_primary <- build_segments_raw(boundary_list, primary_segment_length_ft, primary_segment_length_m)
  seg_secondary <- build_segments_raw(boundary_list, secondary_segment_length_ft, secondary_segment_length_m)
  seg_primary <- classify_segments_from_features(seg_primary, feature_layers)
  seg_secondary <- classify_segments_from_features(seg_secondary, feature_layers)
  feature_audit <- copy(feature_layers$audit)
  build_mode <- "raw_features"
} else {
  message("Rebuild mode: raw generation from ward panel, Major Streets, and OSM feature layers.")
  stopifnot(file.exists(ward_panel_path))

  ward_panel <- st_read(ward_panel_path, quiet = TRUE)
  ward_panel$year <- as.integer(ward_panel$year)
  ward_panel$ward <- as.integer(ward_panel$ward)
  ward_panel <- ward_panel[order(ward_panel$year, ward_panel$ward), ]

  boundary_list <- build_canonical_boundary_list(ward_panel)
  if (length(boundary_list) == 0 || all(vapply(boundary_list, is.null, logical(1)))) {
    stop("Failed to build ward-pair boundaries from ward panel.", call. = FALSE)
  }

  seg_primary <- build_segments_raw(boundary_list, primary_segment_length_ft, primary_segment_length_m)
  seg_secondary <- build_segments_raw(boundary_list, secondary_segment_length_ft, secondary_segment_length_m)
  feature_layers <- load_feature_layers(ward_panel)
  seg_primary <- classify_segments_from_features(seg_primary, feature_layers)
  seg_secondary <- classify_segments_from_features(seg_secondary, feature_layers)
  feature_audit <- copy(feature_layers$audit)
  build_mode <- "raw"
}

if (!exists("ward_panel", inherits = FALSE)) {
  stopifnot(file.exists(ward_panel_path))
  ward_panel <- st_read(ward_panel_path, quiet = TRUE)
  ward_panel$year <- as.integer(ward_panel$year)
  ward_panel$ward <- as.integer(ward_panel$ward)
  ward_panel <- ward_panel[order(ward_panel$year, ward_panel$ward), ]
}

ward_maps <- load_canonical_ward_maps(ward_panel)
seg_primary <- annotate_boundary_segment_validity(seg_primary, ward_maps)
seg_secondary <- annotate_boundary_segment_validity(seg_secondary, ward_maps)

if (file.exists(out_primary)) {
  file.remove(out_primary)
}

wrote_any_primary <- FALSE
for (era_i in eras) {
  era_segments <- seg_primary[seg_primary$era == era_i, ]
  if (nrow(era_segments) == 0) next

  era_segments <- era_segments[order(era_segments$ward_pair_id, era_segments$segment_number), ]
  st_write(era_segments, out_primary, layer = era_i, quiet = TRUE, append = FALSE)
  wrote_any_primary <- TRUE

  for (i in seq_along(bws_m)) {
    bw <- bws_ft[i]
    bw_m <- bws_m[i]
    era_segments_buffered <- era_segments
    st_geometry(era_segments_buffered) <- st_buffer(st_geometry(era_segments), bw)
    era_segments_buffered$buffer_ft <- as.numeric(bw)
    era_segments_buffered$buffer_m <- as.numeric(bw_m)
    st_write(
      era_segments_buffered,
      out_primary,
      layer = sprintf("%s_bw%dm", era_i, bw_m),
      quiet = TRUE,
      append = TRUE
    )
  }
}

if (!wrote_any_primary) {
  stop(sprintf("No segment layers were written to %s", out_primary), call. = FALSE)
}

if (file.exists(out_secondary)) {
  file.remove(out_secondary)
}

wrote_any_secondary <- FALSE
for (era_i in eras) {
  era_segments <- seg_secondary[seg_secondary$era == era_i, ]
  if (nrow(era_segments) == 0) next

  era_segments <- era_segments[order(era_segments$ward_pair_id, era_segments$segment_number), ]
  st_write(era_segments, out_secondary, layer = era_i, quiet = TRUE, append = FALSE)
  wrote_any_secondary <- TRUE

  for (i in seq_along(bws_m)) {
    bw <- bws_ft[i]
    bw_m <- bws_m[i]
    era_segments_buffered <- era_segments
    st_geometry(era_segments_buffered) <- st_buffer(st_geometry(era_segments), bw)
    era_segments_buffered$buffer_ft <- as.numeric(bw)
    era_segments_buffered$buffer_m <- as.numeric(bw_m)
    st_write(
      era_segments_buffered,
      out_secondary,
      layer = sprintf("%s_bw%dm", era_i, bw_m),
      quiet = TRUE,
      append = TRUE
    )
  }
}

if (!wrote_any_secondary) {
  stop(sprintf("No segment layers were written to %s", out_secondary), call. = FALSE)
}

if (file.exists(out_boundaries)) {
  file.remove(out_boundaries)
}

wrote_any_boundaries <- FALSE
for (era_i in eras) {
  era_boundaries <- boundary_list[[era_i]]
  if (is.null(era_boundaries) || nrow(era_boundaries) == 0) next
  st_write(era_boundaries, out_boundaries, layer = era_i, quiet = TRUE, append = FALSE)
  wrote_any_boundaries <- TRUE
}

if (!wrote_any_boundaries) {
  stop("No boundary layers written.", call. = FALSE)
}

pick_cols <- c(required_segment_cols, segment_validity_cols, "target_length_ft", "target_length_m")
class_dt <- rbind(seg_primary[, pick_cols], seg_secondary[, pick_cols])
class_dt <- st_drop_geometry(class_dt)
setDT(class_dt)
setorder(class_dt, era, ward_pair_id, target_length_ft, segment_number)
validate_segment_features(class_dt)
fwrite(class_dt, out_class)

summary_dt <- build_boundary_summary(boundary_list)
fwrite(summary_dt, out_summary)

feature_audit[, build_mode := build_mode]
feature_audit[, feature_buffer_m := feature_buffer_m]
feature_audit[, feature_buffer_ft := feature_buffer_ft]
feature_audit[, generated_at := as.character(Sys.time())]
fwrite(feature_audit, out_feature_audit)

all_eras_present <- all(vapply(
  eras,
  function(era_i) !is.null(boundary_list[[era_i]]) && nrow(boundary_list[[era_i]]) > 0,
  logical(1)
))
all_pair_lengths_positive <- all(summary_dt$min_shared_length_ft > 0, na.rm = TRUE)
all_pair_ids_unique <- all(vapply(
  eras,
  function(era_i) {
    era_boundaries <- boundary_list[[era_i]]
    if (is.null(era_boundaries) || nrow(era_boundaries) == 0) {
      return(TRUE)
    }
    !anyDuplicated(era_boundaries$ward_pair_id)
  },
  logical(1)
))
primary_invalid_segments <- sum(!seg_primary$valid_segment, na.rm = TRUE)
secondary_invalid_segments <- sum(!seg_secondary$valid_segment, na.rm = TRUE)

boundary_diagnostics <- data.table(
  check_name = c(
    "build_mode",
    "script_md5",
    "feature_buffer_m",
    "all_eras_present",
    "all_pair_lengths_positive",
    "all_pair_ids_unique",
    "primary_invalid_segments",
    "secondary_invalid_segments"
  ),
  observed_value = c(
    build_mode,
    unname(tools::md5sum("build_boundary_segments.R")),
    as.character(feature_buffer_m),
    as.character(all_eras_present),
    as.character(all_pair_lengths_positive),
    as.character(all_pair_ids_unique),
    as.character(primary_invalid_segments),
    as.character(secondary_invalid_segments)
  ),
  expected_value = c(
    "snapshot, raw_features, raw, or reuse",
    "non-empty md5",
    "positive distance",
    "TRUE",
    "TRUE",
    "TRUE",
    "reviewed count",
    "reviewed count"
  ),
  status = c(
    "info",
    "info",
    ifelse(is.finite(feature_buffer_m) && feature_buffer_m > 0, "verified", "mismatch"),
    ifelse(all_eras_present, "verified", "mismatch"),
    ifelse(all_pair_lengths_positive, "verified", "mismatch"),
    ifelse(all_pair_ids_unique, "verified", "mismatch"),
    "info",
    "info"
  ),
  details = c(
    "Canonical boundary build mode used for this artifact.",
    "Hash of the segment builder script.",
    "Meter buffer used to compute OSM polygon area shares around boundary segments.",
    "All canonical eras should have non-empty pair boundary layers.",
    "All canonical ward-pair boundaries should have strictly positive shared line length.",
    "Ward-pair IDs must be unique within era.",
    "Invalid primary segments are retained in the raw artifact with valid_segment = FALSE and excluded by analysis loaders.",
    "Invalid secondary segments are retained in the raw artifact with valid_segment = FALSE and excluded by analysis loaders."
  )
)
fwrite(boundary_diagnostics, out_diag)

# Final checks
stopifnot(
  setequal(st_layers(out_primary)$name, expected_layer_names),
  setequal(st_layers(out_secondary)$name, expected_layer_names),
  file.exists(out_class),
  file.exists(out_boundaries),
  file.exists(out_summary),
  file.exists(out_diag),
  file.exists(out_feature_audit)
)

message("Saved:")
message(sprintf(" - %s", out_primary))
message(sprintf(" - %s", out_secondary))
message(sprintf(" - %s", out_class))
message(sprintf(" - %s", out_boundaries))
message(sprintf(" - %s", out_summary))
message(sprintf(" - %s", out_diag))
message(sprintf(" - %s", out_feature_audit))
