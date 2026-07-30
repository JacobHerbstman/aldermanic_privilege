# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_segment_creation/code")
# segment_length_ft <- 1320
# segment_layer_bws_m <- "100 250 400"

source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

st_agr("constant")

eras <- c("2003_2014", "2015_2023")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(segment_length_ft, segment_layer_bws_m)
}
if (length(cli_args) != 2) {
  stop("FATAL: Script requires 2 args: <segment_length_ft> <segment_layer_bws_m>.", call. = FALSE)
}

segment_length_ft <- as.numeric(cli_args[1])
if (length(segment_length_ft) != 1 || !is.finite(segment_length_ft) || segment_length_ft <= 0) {
  stop("segment_length_ft must be a positive numeric segment length in feet.", call. = FALSE)
}
segment_layer_bws_m <- scan(text = cli_args[2], quiet = TRUE)
if (length(segment_layer_bws_m) == 0 || any(!is.finite(segment_layer_bws_m)) || any(segment_layer_bws_m <= 0)) {
  stop("segment_layer_bws_m must contain positive numeric bandwidths in meters.", call. = FALSE)
}
bws_m <- sort(unique(as.integer(round(segment_layer_bws_m))))
bws_ft <- bws_m / 0.3048

segment_output <- sprintf("../output/boundary_segments_%dft.gpkg", as.integer(round(segment_length_ft)))

expected_layer_names <- c(
  eras,
  as.vector(outer(eras, paste0("bw", bws_m, "m"), paste, sep = "_"))
)

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

build_segments_raw <- function(boundary_list, target_len_ft) {
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
          target_length_ft = as.numeric(target_len_ft)
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

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_panel$year <- as.integer(ward_panel$year)
ward_panel$ward <- as.integer(ward_panel$ward)
ward_panel <- ward_panel[order(ward_panel$year, ward_panel$ward), ]

boundary_list <- build_canonical_boundary_list(ward_panel, eras)
if (length(boundary_list) == 0 || all(vapply(boundary_list, is.null, logical(1)))) {
  stop("Failed to build ward-pair boundaries from ward panel.", call. = FALSE)
}

segments <- build_segments_raw(boundary_list, segment_length_ft)

ward_maps <- load_canonical_ward_maps(ward_panel, eras)
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
