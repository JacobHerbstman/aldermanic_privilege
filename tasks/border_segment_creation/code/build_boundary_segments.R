source("../../setup_environment/code/packages.R")

library(data.table)
library(sf)

st_agr("constant")

eras <- c("1998_2002", "2003_2014", "2015_2023", "post_2023")
bws <- c(250, 500, 1000)

out_1320 <- "../output/boundary_segments_1320ft.gpkg"
out_2640 <- "../output/boundary_segments_2640ft.gpkg"
out_class <- "../output/segment_classification.csv"
out_boundaries <- "../output/ward_pair_boundaries.gpkg"

ward_panel_path <- "../input/ward_panel.gpkg"

rebuild_mode <- tolower(Sys.getenv("REBUILD_MODE", "reuse"))
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

normalize_pair_id <- function(a, b) {
  paste(pmin(a, b), pmax(a, b), sep = "_")
}

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

split_linestring_into_segments <- function(line_geom, target_len) {
  pieces <- suppressWarnings(st_cast(st_as_sfc(line_geom), "LINESTRING", warn = FALSE))
  if (length(pieces) == 0) {
    return(st_sfc(crs = st_crs(line_geom)))
  }

  seg_out <- list()
  for (k in seq_along(pieces)) {
    ls <- pieces[k]
    len_k <- as.numeric(st_length(ls))
    if (!is.finite(len_k) || len_k <= 0) next

    nseg <- max(1L, as.integer(ceiling(len_k / target_len)))
    if (nseg == 1L) {
      seg_out[[length(seg_out) + 1L]] <- ls
      next
    }

    ratios <- seq(0, 1, length.out = nseg + 1L)
    pts <- st_line_sample(ls, sample = ratios)
    pts <- st_cast(pts, "POINT", warn = FALSE)
    coords <- st_coordinates(pts)
    if (nrow(coords) < 2) {
      seg_out[[length(seg_out) + 1L]] <- ls
      next
    }

    for (i in seq_len(nrow(coords) - 1L)) {
      xy <- rbind(coords[i, c("X", "Y")], coords[i + 1L, c("X", "Y")])
      if (any(!is.finite(xy))) next
      if (all(abs(xy[1, ] - xy[2, ]) < 1e-7)) next
      seg_out[[length(seg_out) + 1L]] <- st_linestring(as.matrix(xy))
    }
  }

  if (length(seg_out) == 0L) {
    return(st_sfc(crs = st_crs(line_geom)))
  }
  st_sfc(seg_out, crs = st_crs(line_geom))
}

build_ward_pair_boundaries_raw <- function(ward_panel) {
  era_year_map <- list(
    "1998_2002" = 1998L,
    "2003_2014" = 2003L,
    "2015_2023" = 2015L,
    "post_2023" = 2024L
  )

  out <- list()
  for (era_i in eras) {
    y <- era_year_map[[era_i]]
    if (!(y %in% ward_panel$year)) {
      y <- max(ward_panel$year[ward_panel$year <= y], na.rm = TRUE)
    }

    wards <- ward_panel[ward_panel$year == y, c("ward")]
    wards <- wards[order(wards$ward), ]
    if (nrow(wards) == 0) next

    nb <- st_touches(wards)
    rows <- list()
    geoms <- list()

    for (i in seq_len(nrow(wards))) {
      nbrs <- nb[[i]]
      nbrs <- nbrs[nbrs > i]
      if (length(nbrs) == 0) next

      for (j in nbrs) {
        g_i <- st_geometry(wards[i, ])
        g_j <- st_geometry(wards[j, ])
        b_i <- st_boundary(g_i)
        b_j <- st_boundary(g_j)
        inter <- suppressWarnings(st_intersection(b_i, b_j))
        inter <- st_collection_extract(inter, "LINESTRING")
        if (length(inter) == 0) next

        g <- st_line_merge(st_union(inter))
        g <- st_collection_extract(g, "LINESTRING")
        if (length(g) == 0) next

        rows[[length(rows) + 1L]] <- data.table(
          ward_a = as.integer(min(wards$ward[i], wards$ward[j])),
          ward_b = as.integer(max(wards$ward[i], wards$ward[j])),
          ward_pair_id = normalize_pair_id(wards$ward[i], wards$ward[j]),
          era = era_i,
          length_ft = as.numeric(sum(st_length(g), na.rm = TRUE))
        )
        geoms[[length(geoms) + 1L]] <- st_union(g)
      }
    }

    if (length(rows) == 0) next
    dt <- rbindlist(rows)
    sf_i <- st_sf(dt, geom = st_sfc(geoms, crs = st_crs(wards)))
    sf_i <- sf_i[order(sf_i$ward_pair_id), ]
    out[[era_i]] <- sf_i
  }

  out
}

build_segments_raw <- function(boundary_list, target_len) {
  rows <- list()
  geoms <- list()

  for (era_i in eras) {
    b <- boundary_list[[era_i]]
    if (is.null(b) || nrow(b) == 0) next

    for (r in seq_len(nrow(b))) {
      segs <- split_linestring_into_segments(st_geometry(b[r, ])[[1]], target_len)
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

# -----------------------------------------------------------------------------
# Build path
# -----------------------------------------------------------------------------
can_reuse <- rebuild_mode == "reuse" &&
  validate_layers(out_1320) &&
  validate_layers(out_2640) &&
  file.exists(out_class)

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
} else {
  message("Rebuild mode: raw generation from ward panel (feature metrics fallback to defaults).")
  stopifnot(file.exists(ward_panel_path))

  ward_panel <- st_read(ward_panel_path, layer = "ward_panel", quiet = TRUE)
  ward_panel$year <- as.integer(ward_panel$year)
  ward_panel$ward <- as.integer(ward_panel$ward)
  ward_panel <- ward_panel[order(ward_panel$year, ward_panel$ward), ]

  boundary_list <- build_ward_pair_boundaries_raw(ward_panel)
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
}

# Final checks
stopifnot(validate_layers(out_1320), validate_layers(out_2640), file.exists(out_class), file.exists(out_boundaries))

message("Saved:")
message(sprintf(" - %s", out_1320))
message(sprintf(" - %s", out_2640))
message(sprintf(" - %s", out_class))
message(sprintf(" - %s", out_boundaries))
