source("../../setup_environment/code/packages.R")
source("../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

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

write_segment_gpkg <- function(seg_sf, out_path) {
  if (file.exists(out_path)) {
    file.remove(out_path)
  }

  wrote_any <- FALSE
  for (era_i in eras) {
    d <- seg_sf[seg_sf$era == era_i, ]
    if (nrow(d) == 0) next

    d <- d[order(d$ward_pair_id, d$segment_number), ]
    st_write(d, out_path, layer = era_i, quiet = TRUE, append = FALSE)
    wrote_any <- TRUE

    for (bw in bws) {
      d_bw <- d
      st_geometry(d_bw) <- st_buffer(st_geometry(d), bw)
      d_bw$buffer_ft <- as.numeric(bw)
      st_write(d_bw, out_path, layer = sprintf("%s_bw%d", era_i, bw), quiet = TRUE, append = TRUE)
    }
  }

  if (!wrote_any) {
    stop(sprintf("No segment layers were written to %s", out_path), call. = FALSE)
  }
}

build_segment_classification <- function(seg_1320, seg_2640) {
  pick_cols <- c(required_segment_cols, "target_length_ft")
  d <- rbind(seg_1320[, pick_cols], seg_2640[, pick_cols])
  d <- st_drop_geometry(d)
  setDT(d)
  setorder(d, era, ward_pair_id, target_length_ft, segment_number)
  d
}

build_boundaries_from_segments <- function(seg_sf) {
  d <- seg_sf[, c("ward_a", "ward_b", "ward_pair_id", "era", "segment_length_ft")]
  out <- d |>
    dplyr::group_by(ward_a, ward_b, ward_pair_id, era) |>
    dplyr::summarise(length_ft = sum(segment_length_ft, na.rm = TRUE), .groups = "drop")

  out <- out[, c("ward_a", "ward_b", "ward_pair_id", "era", "length_ft")]
  out <- out[order(out$era, out$ward_pair_id), ]
  out
}

validate_layers <- function(path) {
  if (!file.exists(path)) return(FALSE)
  nm <- st_layers(path)$name
  setequal(nm, expected_layer_names)
}

read_segment_layers <- function(path, target_len) {
  out <- list()
  for (era_i in eras) {
    d <- st_read(path, layer = era_i, quiet = TRUE)
    d <- ensure_segment_cols(d, target_len)
    out[[era_i]] <- d
  }
  do.call(rbind, out)
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

write_boundaries_gpkg <- function(boundary_list, out_path) {
  if (file.exists(out_path)) {
    file.remove(out_path)
  }

  wrote <- FALSE
  for (era_i in eras) {
    b <- boundary_list[[era_i]]
    if (is.null(b) || nrow(b) == 0) next
    st_write(b, out_path, layer = era_i, quiet = TRUE, append = FALSE)
    wrote <- TRUE
  }
  if (!wrote) {
    stop("No boundary layers written.", call. = FALSE)
  }
}

build_boundary_diagnostics <- function(boundary_list, build_mode) {
  summary_dt <- build_boundary_summary(boundary_list)
  data.table(
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
      as.character(all(vapply(eras, function(ei) !is.null(boundary_list[[ei]]) && nrow(boundary_list[[ei]]) > 0, logical(1)))),
      as.character(all(summary_dt$min_shared_length_ft > 0, na.rm = TRUE)),
      as.character(all(vapply(eras, function(ei) {
        d <- boundary_list[[ei]]
        if (is.null(d) || nrow(d) == 0) return(TRUE)
        !anyDuplicated(d$ward_pair_id)
      }, logical(1))))
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
      ifelse(all(vapply(eras, function(ei) !is.null(boundary_list[[ei]]) && nrow(boundary_list[[ei]]) > 0, logical(1))), "verified", "mismatch"),
      ifelse(all(summary_dt$min_shared_length_ft > 0, na.rm = TRUE), "verified", "mismatch"),
      ifelse(all(vapply(eras, function(ei) {
        d <- boundary_list[[ei]]
        if (is.null(d) || nrow(d) == 0) return(TRUE)
        !anyDuplicated(d$ward_pair_id)
      }, logical(1))), "verified", "mismatch")
    ),
    details = c(
      "Canonical boundary build mode used for this artifact.",
      "Hash of the segment builder script.",
      "All canonical eras should have non-empty pair boundary layers.",
      "All canonical ward-pair boundaries should have strictly positive shared line length.",
      "Ward-pair IDs must be unique within era."
    )
  )
}

# -----------------------------------------------------------------------------
# Build path
# -----------------------------------------------------------------------------
can_reuse <- rebuild_mode == "reuse" &&
  validate_layers(out_1320) &&
  validate_layers(out_2640) &&
  file.exists(out_class) &&
  file.exists(out_boundaries)

if (can_reuse) {
  message("Rebuild mode: reuse existing canonical segment artifacts with schema checks.")

  seg_1320 <- read_segment_layers(out_1320, 1320)
  seg_2640 <- read_segment_layers(out_2640, 2640)

  boundaries <- build_boundaries_from_segments(seg_1320)
  boundary_list <- split(boundaries, boundaries$era)
  boundary_list <- lapply(eras, function(ei) boundary_list[[ei]])
  names(boundary_list) <- eras

  write_segment_gpkg(seg_1320, out_1320)
  write_segment_gpkg(seg_2640, out_2640)
  write_boundaries_gpkg(boundary_list, out_boundaries)

  class_dt <- build_segment_classification(seg_1320, seg_2640)
  fwrite(class_dt, out_class)
  fwrite(build_boundary_summary(boundary_list), out_summary)
  fwrite(build_boundary_diagnostics(boundary_list, "reuse"), out_diag)
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

  write_segment_gpkg(seg_1320, out_1320)
  write_segment_gpkg(seg_2640, out_2640)
  write_boundaries_gpkg(boundary_list, out_boundaries)

  class_dt <- build_segment_classification(seg_1320, seg_2640)
  fwrite(class_dt, out_class)
  fwrite(build_boundary_summary(boundary_list), out_summary)
  fwrite(build_boundary_diagnostics(boundary_list, "raw"), out_diag)
}

# Final checks
stopifnot(
  validate_layers(out_1320),
  validate_layers(out_2640),
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
