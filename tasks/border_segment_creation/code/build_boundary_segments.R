source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_segment_creation/code")
# Sys.setenv(REBUILD_MODE = "raw")
# source("build_boundary_segments.R")

st_agr("constant")

eras <- c("1998_2002", "2003_2014", "2015_2023", "post_2023")
bws <- c(250, 500, 1000)

out_1320 <- "../output/boundary_segments_1320ft.gpkg"
out_2640 <- "../output/boundary_segments_2640ft.gpkg"
out_class <- "../output/segment_classification.csv"
out_boundaries <- "../output/ward_pair_boundaries.gpkg"
out_summary <- "../output/ward_pair_boundary_summary.csv"
out_diag <- "../output/ward_pair_boundary_diagnostics.csv"

ward_panel_path <- "../input/ward_panel.gpkg"

rebuild_mode <- tolower(Sys.getenv("REBUILD_MODE", "raw"))
if (!rebuild_mode %in% c("reuse", "raw")) {
  stop("REBUILD_MODE must be 'reuse' or 'raw'.", call. = FALSE)
}

expected_layer_names <- c(
  eras,
  as.vector(outer(eras, paste0("bw", bws), paste, sep = "_"))
)

required_segment_cols <- c(
  "segment_id", "ward_pair_id", "ward_a", "ward_b", "era", "segment_number",
  "n_segments_in_pair", "segment_length_ft", "centroid_lat", "centroid_lon",
  "segment_type", "nearest_street_name", "nearest_street_class",
  "nearest_street_class_mapped", "distance_to_nearest_street_ft",
  "major_overlap_arterial_ft", "major_overlap_collector_ft",
  "major_overlap_residential_ft", "water_area_share", "park_area_share",
  "waterway_overlap_ft"
)

ensure_segment_cols <- function(x, target_len) {
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
  x$segment_length_ft <- as.numeric(x$segment_length_ft)
  x$centroid_lat <- as.numeric(x$centroid_lat)
  x$centroid_lon <- as.numeric(x$centroid_lon)
  x$nearest_street_class <- suppressWarnings(as.numeric(x$nearest_street_class))
  x$distance_to_nearest_street_ft <- as.numeric(x$distance_to_nearest_street_ft)
  x$major_overlap_arterial_ft <- as.numeric(x$major_overlap_arterial_ft)
  x$major_overlap_collector_ft <- as.numeric(x$major_overlap_collector_ft)
  x$major_overlap_residential_ft <- as.numeric(x$major_overlap_residential_ft)
  x$water_area_share <- as.numeric(x$water_area_share)
  x$park_area_share <- as.numeric(x$park_area_share)
  x$waterway_overlap_ft <- as.numeric(x$waterway_overlap_ft)

  x$target_length_ft <- as.numeric(target_len)
  x
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

    nseg <- max(1L, as.integer(ceiling(len_k / target_len)))
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

build_segments_raw <- function(boundary_list, target_len) {
  rows <- list()
  geoms <- list()

  for (era_i in eras) {
    b <- boundary_list[[era_i]]
    if (is.null(b) || nrow(b) == 0) next

    for (r in seq_len(nrow(b))) {
      segs <- split_linestring_into_segments(st_geometry(b[r, ])[[1]], target_len, st_crs(b))
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
          segment_length_ft = as.numeric(st_length(seg_geom)),
          centroid_lat = as.numeric(cc[1, "Y"]),
          centroid_lon = as.numeric(cc[1, "X"]),
          segment_type = "no_feature",
          nearest_street_name = NA_character_,
          nearest_street_class = NA_real_,
          nearest_street_class_mapped = NA_character_,
          distance_to_nearest_street_ft = NA_real_,
          major_overlap_arterial_ft = 0,
          major_overlap_collector_ft = 0,
          major_overlap_residential_ft = 0,
          water_area_share = 0,
          park_area_share = 0,
          waterway_overlap_ft = 0,
          target_length_ft = as.numeric(target_len)
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

# -----------------------------------------------------------------------------
# Build path
# -----------------------------------------------------------------------------
out_1320_layers_valid <- FALSE
if (file.exists(out_1320)) {
  out_1320_layers_valid <- setequal(st_layers(out_1320)$name, expected_layer_names)
}

out_2640_layers_valid <- FALSE
if (file.exists(out_2640)) {
  out_2640_layers_valid <- setequal(st_layers(out_2640)$name, expected_layer_names)
}

can_reuse <- rebuild_mode == "reuse" &&
  out_1320_layers_valid &&
  out_2640_layers_valid &&
  file.exists(out_class) &&
  file.exists(out_boundaries)

if (can_reuse) {
  message("Rebuild mode: reuse existing canonical segment artifacts with schema checks.")

  seg_1320_by_era <- list()
  for (era_i in eras) {
    seg_1320_by_era[[era_i]] <- ensure_segment_cols(
      st_read(out_1320, layer = era_i, quiet = TRUE),
      1320
    )
  }
  seg_1320 <- do.call(rbind, seg_1320_by_era)

  seg_2640_by_era <- list()
  for (era_i in eras) {
    seg_2640_by_era[[era_i]] <- ensure_segment_cols(
      st_read(out_2640, layer = era_i, quiet = TRUE),
      2640
    )
  }
  seg_2640 <- do.call(rbind, seg_2640_by_era)

  boundaries <- seg_1320[, c("ward_a", "ward_b", "ward_pair_id", "era", "segment_length_ft")] |>
    dplyr::group_by(ward_a, ward_b, ward_pair_id, era) |>
    dplyr::summarise(length_ft = sum(segment_length_ft, na.rm = TRUE), .groups = "drop")
  boundaries <- boundaries[, c("ward_a", "ward_b", "ward_pair_id", "era", "length_ft")]
  boundaries <- boundaries[order(boundaries$era, boundaries$ward_pair_id), ]
  boundary_list <- split(boundaries, boundaries$era)
  boundary_list <- lapply(eras, function(ei) boundary_list[[ei]])
  names(boundary_list) <- eras
  build_mode <- "reuse"
} else {
  message("Rebuild mode: raw generation from ward panel (feature metrics fallback to defaults).")
  stopifnot(file.exists(ward_panel_path))

  ward_panel <- st_read(ward_panel_path, quiet = TRUE)
  ward_panel$year <- as.integer(ward_panel$year)
  ward_panel$ward <- as.integer(ward_panel$ward)
  ward_panel <- ward_panel[order(ward_panel$year, ward_panel$ward), ]

  boundary_list <- build_canonical_boundary_list(ward_panel)
  if (length(boundary_list) == 0 || all(vapply(boundary_list, is.null, logical(1)))) {
    stop("Failed to build ward-pair boundaries from ward panel.", call. = FALSE)
  }

  seg_1320 <- build_segments_raw(boundary_list, 1320)
  seg_2640 <- build_segments_raw(boundary_list, 2640)
  build_mode <- "raw"
}

if (file.exists(out_1320)) {
  file.remove(out_1320)
}

wrote_any_1320 <- FALSE
for (era_i in eras) {
  era_segments <- seg_1320[seg_1320$era == era_i, ]
  if (nrow(era_segments) == 0) next

  era_segments <- era_segments[order(era_segments$ward_pair_id, era_segments$segment_number), ]
  st_write(era_segments, out_1320, layer = era_i, quiet = TRUE, append = FALSE)
  wrote_any_1320 <- TRUE

  for (bw in bws) {
    era_segments_buffered <- era_segments
    st_geometry(era_segments_buffered) <- st_buffer(st_geometry(era_segments), bw)
    era_segments_buffered$buffer_ft <- as.numeric(bw)
    st_write(
      era_segments_buffered,
      out_1320,
      layer = sprintf("%s_bw%d", era_i, bw),
      quiet = TRUE,
      append = TRUE
    )
  }
}

if (!wrote_any_1320) {
  stop(sprintf("No segment layers were written to %s", out_1320), call. = FALSE)
}

if (file.exists(out_2640)) {
  file.remove(out_2640)
}

wrote_any_2640 <- FALSE
for (era_i in eras) {
  era_segments <- seg_2640[seg_2640$era == era_i, ]
  if (nrow(era_segments) == 0) next

  era_segments <- era_segments[order(era_segments$ward_pair_id, era_segments$segment_number), ]
  st_write(era_segments, out_2640, layer = era_i, quiet = TRUE, append = FALSE)
  wrote_any_2640 <- TRUE

  for (bw in bws) {
    era_segments_buffered <- era_segments
    st_geometry(era_segments_buffered) <- st_buffer(st_geometry(era_segments), bw)
    era_segments_buffered$buffer_ft <- as.numeric(bw)
    st_write(
      era_segments_buffered,
      out_2640,
      layer = sprintf("%s_bw%d", era_i, bw),
      quiet = TRUE,
      append = TRUE
    )
  }
}

if (!wrote_any_2640) {
  stop(sprintf("No segment layers were written to %s", out_2640), call. = FALSE)
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

pick_cols <- c(required_segment_cols, "target_length_ft")
class_dt <- rbind(seg_1320[, pick_cols], seg_2640[, pick_cols])
class_dt <- st_drop_geometry(class_dt)
setDT(class_dt)
setorder(class_dt, era, ward_pair_id, target_length_ft, segment_number)
fwrite(class_dt, out_class)

summary_dt <- build_boundary_summary(boundary_list)
fwrite(summary_dt, out_summary)

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

boundary_diagnostics <- data.table(
  check_name = c(
    "build_mode",
    "script_md5",
    "all_eras_present",
    "all_pair_lengths_positive",
    "all_pair_ids_unique"
  ),
  observed_value = c(
    build_mode,
    unname(tools::md5sum("build_boundary_segments.R")),
    as.character(all_eras_present),
    as.character(all_pair_lengths_positive),
    as.character(all_pair_ids_unique)
  ),
  expected_value = c(
    "raw or reuse",
    "non-empty md5",
    "TRUE",
    "TRUE",
    "TRUE"
  ),
  status = c(
    "info",
    "info",
    ifelse(all_eras_present, "verified", "mismatch"),
    ifelse(all_pair_lengths_positive, "verified", "mismatch"),
    ifelse(all_pair_ids_unique, "verified", "mismatch")
  ),
  details = c(
    "Canonical boundary build mode used for this artifact.",
    "Hash of the segment builder script.",
    "All canonical eras should have non-empty pair boundary layers.",
    "All canonical ward-pair boundaries should have strictly positive shared line length.",
    "Ward-pair IDs must be unique within era."
  )
)
fwrite(boundary_diagnostics, out_diag)

# Final checks
stopifnot(
  setequal(st_layers(out_1320)$name, expected_layer_names),
  setequal(st_layers(out_2640)$name, expected_layer_names),
  file.exists(out_class),
  file.exists(out_boundaries),
  file.exists(out_summary),
  file.exists(out_diag)
)

message("Saved:")
message(sprintf(" - %s", out_1320))
message(sprintf(" - %s", out_2640))
message(sprintf(" - %s", out_class))
message(sprintf(" - %s", out_boundaries))
message(sprintf(" - %s", out_summary))
message(sprintf(" - %s", out_diag))
