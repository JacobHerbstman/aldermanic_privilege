# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_segment_creation/code")

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

st_agr("constant")

eras <- c("1998_2002", "2003_2014", "2015_2023", "post_2023")
segment_length_ft <- as.numeric(Sys.getenv("SEGMENT_LENGTH_FT", "1320"))
if (length(segment_length_ft) != 1 || !is.finite(segment_length_ft) || segment_length_ft <= 0) {
  stop("SEGMENT_LENGTH_FT must be a positive numeric segment length in feet.", call. = FALSE)
}
segment_length_m <- segment_length_ft * 0.3048

segment_layer_bws_m <- scan(text = Sys.getenv("SEGMENT_LAYER_BWS_M", "100 250 400"), quiet = TRUE)
if (length(segment_layer_bws_m) == 0 || any(!is.finite(segment_layer_bws_m)) || any(segment_layer_bws_m <= 0)) {
  stop("SEGMENT_LAYER_BWS_M must contain positive numeric bandwidths in meters.", call. = FALSE)
}
bws_m <- sort(unique(as.integer(round(segment_layer_bws_m))))
bws_ft <- bws_m / 0.3048

segment_output <- sprintf("../output/boundary_segments_%dft.gpkg", as.integer(round(segment_length_ft)))

feature_buffer_m <- as.numeric(Sys.getenv("SEGMENT_FEATURE_BUFFER_M", "30"))
feature_buffer_ft <- feature_buffer_m / 0.3048
if (!is.finite(feature_buffer_m) || feature_buffer_m <= 0) {
  stop("SEGMENT_FEATURE_BUFFER_M must be positive.", call. = FALSE)
}
if (!is.finite(feature_buffer_ft) || feature_buffer_ft <= 0) {
  stop("SEGMENT_FEATURE_BUFFER_M must imply a positive feature buffer in feet.", call. = FALSE)
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

validate_segment_features <- function(class_dt, target_segment_length_ft = segment_length_ft) {
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

  major_streets <- read_filtered_layer("../input/Major_Streets.shp", city_geom, "Major Streets")
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

  osm_roads <- read_filtered_layer("../input/gis_osm_roads_free_1.shp", city_geom, "OSM roads")
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

  osm_landuse <- read_filtered_layer("../input/gis_osm_landuse_a_free_1.shp", city_geom, "OSM landuse polygons")
  if (nrow(osm_landuse) > 0) {
    osm_landuse$fclass_clean <- clean_feature_text(osm_landuse$fclass)
  } else {
    osm_landuse$fclass_clean <- character()
  }

  osm_water <- read_filtered_layer("../input/gis_osm_water_a_free_1.shp", city_geom, "OSM water polygons")
  if (nrow(osm_water) > 0) {
    osm_water$fclass_clean <- clean_feature_text(osm_water$fclass)
  } else {
    osm_water$fclass_clean <- character()
  }

  osm_waterways <- read_filtered_layer("../input/gis_osm_waterways_free_1.shp", city_geom, "OSM waterways")
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
    cemetery_polygons = osm_landuse[osm_landuse$fclass_clean %in% cemetery_fclasses, ]
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

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_panel$year <- as.integer(ward_panel$year)
ward_panel$ward <- as.integer(ward_panel$ward)
ward_panel <- ward_panel[order(ward_panel$year, ward_panel$ward), ]

boundary_list <- build_canonical_boundary_list(ward_panel)
if (length(boundary_list) == 0 || all(vapply(boundary_list, is.null, logical(1)))) {
  stop("Failed to build ward-pair boundaries from ward panel.", call. = FALSE)
}

segments <- build_segments_raw(boundary_list, segment_length_ft, segment_length_m)
feature_layers <- load_feature_layers(ward_panel)
segments <- classify_segments_from_features(segments, feature_layers)

ward_maps <- load_canonical_ward_maps(ward_panel)
segments <- annotate_boundary_segment_validity(segments, ward_maps)

unlink(segment_output)

wrote_any_segments <- FALSE
for (era_i in eras) {
  era_segments <- segments[segments$era == era_i, ]
  if (nrow(era_segments) == 0) next

  era_segments <- era_segments[order(era_segments$ward_pair_id, era_segments$segment_number), ]
  st_write(era_segments, segment_output, layer = era_i, quiet = TRUE, append = FALSE)
  wrote_any_segments <- TRUE

  for (i in seq_along(bws_m)) {
    bw <- bws_ft[i]
    bw_m <- bws_m[i]
    era_segments_buffered <- era_segments
    st_geometry(era_segments_buffered) <- st_buffer(st_geometry(era_segments), bw)
    era_segments_buffered$buffer_ft <- as.numeric(bw)
    era_segments_buffered$buffer_m <- as.numeric(bw_m)
    st_write(
      era_segments_buffered,
      segment_output,
      layer = sprintf("%s_bw%dm", era_i, bw_m),
      quiet = TRUE,
      append = TRUE
    )
  }
}

if (!wrote_any_segments) {
  stop(sprintf("No segment layers were written to %s", segment_output), call. = FALSE)
}

unlink("../output/ward_pair_boundaries.gpkg")

wrote_any_boundaries <- FALSE
for (era_i in eras) {
  era_boundaries <- boundary_list[[era_i]]
  if (is.null(era_boundaries) || nrow(era_boundaries) == 0) next
  st_write(era_boundaries, "../output/ward_pair_boundaries.gpkg", layer = era_i, quiet = TRUE, append = FALSE)
  wrote_any_boundaries <- TRUE
}

if (!wrote_any_boundaries) {
  stop("No boundary layers written.", call. = FALSE)
}

pick_cols <- c(required_segment_cols, segment_validity_cols, "target_length_ft", "target_length_m")
class_dt <- st_drop_geometry(segments[, pick_cols])
setDT(class_dt)
setorder(class_dt, era, ward_pair_id, target_length_ft, segment_number)
validate_segment_features(class_dt)
fwrite(class_dt, "../output/segment_classification.csv")

summary_dt <- build_boundary_summary(boundary_list)

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
if (!all_eras_present) {
  stop("One or more eras has no ward-pair boundary layer.", call. = FALSE)
}
if (!all_pair_lengths_positive) {
  stop("One or more ward-pair boundaries has non-positive shared length.", call. = FALSE)
}
if (!all_pair_ids_unique) {
  stop("Ward-pair IDs are not unique within era.", call. = FALSE)
}

if (!setequal(st_layers(segment_output)$name, expected_layer_names)) {
  stop(sprintf("Unexpected layer set in %s.", segment_output), call. = FALSE)
}
if (!setequal(st_layers("../output/ward_pair_boundaries.gpkg")$name, eras)) {
  stop("Unexpected layer set in ../output/ward_pair_boundaries.gpkg.", call. = FALSE)
}
if (nrow(fread("../output/segment_classification.csv")) == 0) {
  stop("segment_classification.csv is empty.", call. = FALSE)
}
