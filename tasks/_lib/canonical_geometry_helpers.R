canonical_geometry_spec <- function() {
  tibble::tibble(
    era = c("1998_2002", "2003_2014", "2015_2023", "post_2023"),
    map_year = c(1998L, 2003L, 2015L, 2024L),
    start_date = as.Date(c("1900-01-01", "2003-05-01", "2015-05-18", "2023-05-15")),
    end_date = as.Date(c("2003-04-30", "2015-05-17", "2023-05-14", NA))
  )
}

normalize_pair_id <- function(a, b, sep = "_") {
  a <- suppressWarnings(as.integer(a))
  b <- suppressWarnings(as.integer(b))
  out <- ifelse(
    is.na(a) | is.na(b),
    NA_character_,
    paste(pmin(a, b), pmax(a, b), sep = sep)
  )
  as.character(out)
}

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  if (!any(ok)) {
    return(out)
  }

  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    normalize_pair_id(v[1], v[2], sep = "-")
  }, character(1))
  out
}

canonical_era_levels <- function() {
  canonical_geometry_spec()$era
}

canonical_map_year_for_era <- function(era) {
  spec <- canonical_geometry_spec()
  out <- spec$map_year[match(as.character(era), spec$era)]
  as.integer(out)
}

canonical_era_from_boundary_year <- function(boundary_year) {
  boundary_year <- suppressWarnings(as.integer(boundary_year))
  out <- rep(NA_character_, length(boundary_year))
  out[boundary_year == 1998L] <- "1998_2002"
  out[boundary_year == 2003L] <- "2003_2014"
  out[boundary_year == 2015L] <- "2015_2023"
  out[boundary_year == 2024L] <- "post_2023"
  out
}

canonical_boundary_year_from_date <- function(x) {
  x <- as.Date(x)
  out <- rep(NA_integer_, length(x))
  out[!is.na(x) & x < as.Date("2003-05-01")] <- 1998L
  out[!is.na(x) & x >= as.Date("2003-05-01") & x < as.Date("2015-05-18")] <- 2003L
  out[!is.na(x) & x >= as.Date("2015-05-18") & x < as.Date("2023-05-15")] <- 2015L
  out[!is.na(x) & x >= as.Date("2023-05-15")] <- 2024L
  out
}

canonical_era_from_date <- function(x, allow_pre_2003 = TRUE) {
  boundary_year <- canonical_boundary_year_from_date(x)
  if (!allow_pre_2003) {
    boundary_year[boundary_year == 1998L] <- 2003L
  }
  canonical_era_from_boundary_year(boundary_year)
}

aggregate_ward_map <- function(ward_panel, map_year) {
  ward_panel$year <- suppressWarnings(as.integer(ward_panel$year))
  ward_panel$ward <- suppressWarnings(as.integer(ward_panel$ward))

  ward_sf <- ward_panel[ward_panel$year == as.integer(map_year), ]
  if (nrow(ward_sf) == 0) {
    stop(sprintf("No ward polygons found for map year %d.", map_year), call. = FALSE)
  }

  if (!("ward" %in% names(ward_sf))) {
    stop("Ward panel is missing ward column.", call. = FALSE)
  }

  ward_sf |>
    dplyr::select(ward) |>
    sf::st_make_valid() |>
    dplyr::group_by(ward) |>
    dplyr::summarise(do_union = TRUE, .groups = "drop") |>
    sf::st_as_sf(crs = sf::st_crs(ward_sf))
}

load_canonical_ward_maps <- function(ward_panel) {
  eras <- canonical_era_levels()
  out <- lapply(eras, function(era_i) {
    aggregate_ward_map(ward_panel, canonical_map_year_for_era(era_i))
  })
  names(out) <- eras
  out
}

extract_shared_line_geometry <- function(geom_a, geom_b) {
  inter <- suppressWarnings(
    sf::st_intersection(
      sf::st_boundary(geom_a),
      sf::st_boundary(geom_b)
    )
  )

  if (length(inter) == 0 || all(sf::st_is_empty(inter))) {
    return(NULL)
  }

  gtypes <- unique(as.character(sf::st_geometry_type(inter)))
  line_part <- if (all(gtypes %in% c("LINESTRING", "MULTILINESTRING"))) {
    suppressWarnings(sf::st_cast(inter, "LINESTRING"))
  } else if ("GEOMETRYCOLLECTION" %in% gtypes) {
    suppressWarnings(sf::st_collection_extract(inter, "LINESTRING"))
  } else {
    NULL
  }

  if (is.null(line_part) || length(line_part) == 0 || all(sf::st_is_empty(line_part))) {
    return(NULL)
  }

  line_part <- line_part[as.numeric(sf::st_length(line_part)) > 0, ]
  if (length(line_part) == 0) {
    return(NULL)
  }

  merged_input <- suppressWarnings(sf::st_cast(sf::st_union(line_part), "MULTILINESTRING"))
  merged <- suppressWarnings(sf::st_line_merge(merged_input))
  merged <- suppressWarnings(sf::st_collection_extract(merged, "LINESTRING"))
  if (length(merged) == 0 || all(sf::st_is_empty(merged))) {
    return(NULL)
  }

  merged
}

build_canonical_boundary_list <- function(ward_panel) {
  ward_maps <- load_canonical_ward_maps(ward_panel)
  eras <- canonical_era_levels()

  out <- lapply(eras, function(era_i) {
    ward_sf <- ward_maps[[era_i]]
    wards <- sort(unique(ward_sf$ward))
    rows <- list()
    geoms <- list()

    if (length(wards) < 2) {
      return(NULL)
    }

    idx <- 0L
    for (i in seq_len(length(wards) - 1L)) {
      for (j in (i + 1L):length(wards)) {
        ward_a <- wards[i]
        ward_b <- wards[j]
        geom_a <- sf::st_geometry(ward_sf[ward_sf$ward == ward_a, ])
        geom_b <- sf::st_geometry(ward_sf[ward_sf$ward == ward_b, ])
        shared <- extract_shared_line_geometry(geom_a, geom_b)
        if (is.null(shared) || length(shared) == 0) {
          next
        }

        idx <- idx + 1L
        rows[[idx]] <- data.table::data.table(
          map_year = canonical_map_year_for_era(era_i),
          era = era_i,
          ward_a = as.integer(min(ward_a, ward_b)),
          ward_b = as.integer(max(ward_a, ward_b)),
          ward_pair_id = normalize_pair_id(ward_a, ward_b, sep = "_"),
          shared_length_ft = as.numeric(sum(sf::st_length(shared), na.rm = TRUE))
        )
        geoms[[idx]] <- sf::st_union(shared)[[1]]
      }
    }

    if (length(rows) == 0) {
      return(NULL)
    }

    sf_i <- sf::st_sf(
      data.table::rbindlist(rows),
      geometry = sf::st_sfc(geoms, crs = sf::st_crs(ward_sf))
    )
    sf_i[order(sf_i$ward_pair_id), ]
  })

  names(out) <- eras
  out
}

build_boundary_summary <- function(boundary_list) {
  eras <- canonical_era_levels()
  data.table::rbindlist(lapply(eras, function(era_i) {
    d <- boundary_list[[era_i]]
    data.table::data.table(
      era = era_i,
      map_year = canonical_map_year_for_era(era_i),
      n_pairs = if (is.null(d)) 0L else nrow(d),
      total_shared_length_ft = if (is.null(d) || nrow(d) == 0) NA_real_ else sum(d$shared_length_ft, na.rm = TRUE),
      mean_shared_length_ft = if (is.null(d) || nrow(d) == 0) NA_real_ else mean(d$shared_length_ft, na.rm = TRUE),
      min_shared_length_ft = if (is.null(d) || nrow(d) == 0) NA_real_ else min(d$shared_length_ft, na.rm = TRUE),
      max_shared_length_ft = if (is.null(d) || nrow(d) == 0) NA_real_ else max(d$shared_length_ft, na.rm = TRUE)
    )
  }), fill = TRUE)
}

load_boundary_layers <- function(boundary_gpkg, eras = canonical_era_levels()) {
  layer_names <- sf::st_layers(boundary_gpkg)$name
  missing_layers <- setdiff(eras, layer_names)
  if (length(missing_layers) > 0) {
    stop(sprintf(
      "Boundary GPKG is missing canonical layers: %s",
      paste(missing_layers, collapse = ", ")
    ), call. = FALSE)
  }

  out <- lapply(eras, function(era_i) {
    d <- sf::st_read(boundary_gpkg, layer = era_i, quiet = TRUE)
    d$ward_a <- suppressWarnings(as.integer(d$ward_a))
    d$ward_b <- suppressWarnings(as.integer(d$ward_b))
    d$ward_pair_id <- as.character(d$ward_pair_id)
    d
  })
  names(out) <- eras
  out
}

load_segment_layers <- function(segment_gpkg, buffer_m = NULL, buffer_ft = NULL, eras = canonical_era_levels()) {
  layer_names <- sf::st_layers(segment_gpkg)$name
  if (!is.null(buffer_m) && !is.null(buffer_ft)) {
    stop("Supply only one of buffer_m or buffer_ft.", call. = FALSE)
  }
  if (is.null(buffer_m) && is.null(buffer_ft)) {
    stop("Supply buffer_m or buffer_ft explicitly.", call. = FALSE)
  }

  if (!is.null(buffer_m)) {
    if (!is.finite(buffer_m) || buffer_m <= 0) {
      stop("buffer_m must be positive.", call. = FALSE)
    }
    needed_layers <- paste0(eras, "_bw", as.integer(round(buffer_m)), "m")
  } else {
    if (!is.finite(buffer_ft) || buffer_ft <= 0) {
      stop("buffer_ft must be positive.", call. = FALSE)
    }
    needed_layers <- paste0(eras, "_bw", as.integer(round(buffer_ft)))
  }

  missing_layers <- setdiff(needed_layers, layer_names)
  if (length(missing_layers) > 0) {
    stop(sprintf(
      "Segment GPKG is missing layers: %s",
      paste(missing_layers, collapse = ", ")
    ), call. = FALSE)
  }

  out <- lapply(eras, function(era_i) {
    layer_name <- if (!is.null(buffer_m)) {
      paste0(era_i, "_bw", as.integer(round(buffer_m)), "m")
    } else {
      paste0(era_i, "_bw", as.integer(round(buffer_ft)))
    }
    d <- sf::st_read(segment_gpkg, layer = layer_name, quiet = TRUE)
    d$segment_id <- as.character(d$segment_id)
    d$pair_dash <- normalize_pair_dash(d$ward_pair_id)
    d[!is.na(d$pair_dash), ]
  })
  names(out) <- eras
  out
}

load_segment_line_layers <- function(segment_gpkg, eras = canonical_era_levels()) {
  layer_names <- sf::st_layers(segment_gpkg)$name
  missing_layers <- setdiff(eras, layer_names)
  if (length(missing_layers) > 0) {
    stop(sprintf(
      "Segment GPKG is missing unbuffered layers: %s",
      paste(missing_layers, collapse = ", ")
    ), call. = FALSE)
  }

  out <- lapply(eras, function(era_i) {
    d <- sf::st_read(segment_gpkg, layer = era_i, quiet = TRUE)
    missing_cols <- setdiff(c("segment_id", "ward_pair_id"), names(d))
    if (length(missing_cols) > 0) {
      stop(sprintf(
        "Segment layer %s is missing required columns: %s",
        era_i,
        paste(missing_cols, collapse = ", ")
      ), call. = FALSE)
    }
    if (is.na(sf::st_crs(d))) {
      stop(sprintf("Segment layer %s has missing CRS.", era_i), call. = FALSE)
    }
    geom_type <- unique(as.character(sf::st_geometry_type(d)))
    bad_geom <- setdiff(geom_type, c("LINESTRING", "MULTILINESTRING"))
    if (length(bad_geom) > 0) {
      stop(sprintf(
        "Segment layer %s must contain only line geometries; found: %s",
        era_i,
        paste(bad_geom, collapse = ", ")
      ), call. = FALSE)
    }
    if (any(sf::st_is_empty(d))) {
      stop(sprintf("Segment layer %s contains empty geometries.", era_i), call. = FALSE)
    }

    d$segment_id <- as.character(d$segment_id)
    d$pair_dash <- normalize_pair_dash(d$ward_pair_id)
    if (any(is.na(d$segment_id) | d$segment_id == "")) {
      stop(sprintf("Segment layer %s contains missing segment_id values.", era_i), call. = FALSE)
    }
    if (anyDuplicated(d$segment_id) > 0) {
      stop(sprintf("Segment layer %s contains duplicate segment_id values.", era_i), call. = FALSE)
    }
    if (any(is.na(d$pair_dash))) {
      stop(sprintf("Segment layer %s contains ward_pair_id values that cannot be normalized.", era_i), call. = FALSE)
    }

    d <- d[order(d$pair_dash, d$segment_id), ]
    d
  })
  names(out) <- eras
  out
}

assert_point_geometries <- function(points_sf, label = "points_sf") {
  if (!inherits(points_sf, "sf")) {
    stop(sprintf("%s must be an sf object.", label), call. = FALSE)
  }
  if (is.na(sf::st_crs(points_sf))) {
    stop(sprintf("%s has missing CRS.", label), call. = FALSE)
  }

  geom <- sf::st_geometry(points_sf)
  missing_geom <- is.na(geom)
  non_missing_geom <- geom[!missing_geom]
  if (length(non_missing_geom) > 0) {
    geom_type <- unique(as.character(sf::st_geometry_type(non_missing_geom)))
    bad_geom <- setdiff(geom_type, c("POINT", "MULTIPOINT"))
    if (length(bad_geom) > 0) {
      stop(sprintf(
        "%s must contain only point geometries; found: %s",
        label,
        paste(bad_geom, collapse = ", ")
      ), call. = FALSE)
    }
  }

  empty_geom <- rep(FALSE, length(geom))
  if (length(non_missing_geom) > 0) {
    empty_geom[!missing_geom] <- sf::st_is_empty(non_missing_geom)
  }
  missing_geom | empty_geom
}

assign_points_to_wards <- function(points_sf, ward_sf) {
  if (nrow(points_sf) == 0) {
    return(integer(0))
  }

  if (sf::st_crs(points_sf) != sf::st_crs(ward_sf)) {
    points_sf <- sf::st_transform(points_sf, sf::st_crs(ward_sf))
  }

  ward_hits <- sf::st_within(points_sf, ward_sf)
  vapply(ward_hits, function(v) {
    if (length(v) == 0) {
      return(NA_integer_)
    }
    suppressWarnings(as.integer(ward_sf$ward[v[1]]))
  }, integer(1))
}

assign_points_to_boundaries <- function(points_sf, era_values, ward_maps, boundary_lines, chunk_n = 5000L) {
  stopifnot(length(era_values) == nrow(points_sf))

  out_ward <- rep(NA_integer_, nrow(points_sf))
  out_neighbor <- rep(NA_integer_, nrow(points_sf))
  out_pair <- rep(NA_character_, nrow(points_sf))
  out_dist <- rep(NA_real_, nrow(points_sf))

  eras <- unique(stats::na.omit(as.character(era_values)))
  for (era_i in eras) {
    idx_era <- which(as.character(era_values) == era_i)
    if (length(idx_era) == 0) next

    pts_era <- points_sf[idx_era, ]
    ward_sf <- ward_maps[[era_i]]
    lines_sf <- boundary_lines[[era_i]]

    if (is.null(ward_sf) || nrow(ward_sf) == 0 || is.null(lines_sf) || nrow(lines_sf) == 0) {
      next
    }

    if (sf::st_crs(pts_era) != sf::st_crs(ward_sf)) {
      pts_era <- sf::st_transform(pts_era, sf::st_crs(ward_sf))
    }

    ward_match <- assign_points_to_wards(pts_era, ward_sf)
    out_ward[idx_era] <- ward_match

    valid_idx <- which(!is.na(ward_match))
    if (length(valid_idx) == 0) {
      next
    }

    ward_vals <- sort(unique(ward_match[valid_idx]))
    for (w in ward_vals) {
      idx_w_local <- valid_idx[ward_match[valid_idx] == w]
      edges_w <- lines_sf[lines_sf$ward_a == w | lines_sf$ward_b == w, ]
      if (length(idx_w_local) == 0 || nrow(edges_w) == 0) next

      starts <- seq(1L, length(idx_w_local), by = chunk_n)
      for (s in starts) {
        e <- min(s + chunk_n - 1L, length(idx_w_local))
        idx_chunk_local <- idx_w_local[s:e]
        nearest_idx <- sf::st_nearest_feature(pts_era[idx_chunk_local, ], edges_w)
        nearest_edges <- edges_w[nearest_idx, ]
        dists <- sf::st_distance(sf::st_geometry(pts_era[idx_chunk_local, ]), sf::st_geometry(nearest_edges), by_element = TRUE)

        neighbor_vals <- ifelse(
          w == nearest_edges$ward_a,
          nearest_edges$ward_b,
          nearest_edges$ward_a
        )

        idx_global <- idx_era[idx_chunk_local]
        out_neighbor[idx_global] <- suppressWarnings(as.integer(neighbor_vals))
        out_pair[idx_global] <- normalize_pair_id(w, neighbor_vals, sep = "_")
        out_dist[idx_global] <- as.numeric(dists)
      }
    }
  }

  tibble::tibble(
    ward = out_ward,
    neighbor_ward = out_neighbor,
    ward_pair_id = out_pair,
    dist_m = out_dist * 0.3048,
    dist_ft = out_dist
  )
}

assign_points_to_segments <- function(points_sf, era_values, pair_values, segment_layers, chunk_n = 50000L) {
  stop(
    "assign_points_to_segments() used buffered-polygon first-hit assignment and is deprecated. Use load_segment_line_layers() with assign_points_to_nearest_segments().",
    call. = FALSE
  )
}

assign_points_to_nearest_segments <- function(points_sf, era_values, pair_values, segment_layers, max_distance, chunk_n = 50000L, tie_tolerance = units::set_units(1e-7, "m")) {
  stopifnot(length(era_values) == nrow(points_sf), length(pair_values) == nrow(points_sf))

  if (!inherits(max_distance, "units")) {
    stop("max_distance must be a units object, e.g. units::set_units(250, 'm').", call. = FALSE)
  }
  if (!inherits(tie_tolerance, "units")) {
    stop("tie_tolerance must be a units object.", call. = FALSE)
  }
  empty_point <- assert_point_geometries(points_sf, "points_sf")

  max_distance_m <- as.numeric(units::set_units(max_distance, "m"))
  tie_tolerance_m <- as.numeric(units::set_units(tie_tolerance, "m"))
  if (!is.finite(max_distance_m) || max_distance_m <= 0) {
    stop("max_distance must be positive.", call. = FALSE)
  }
  if (!is.finite(tie_tolerance_m) || tie_tolerance_m < 0) {
    stop("tie_tolerance must be nonnegative.", call. = FALSE)
  }

  # pair_values is retained for the caller contract and audits; assignment is global.
  seg_id <- rep(NA_character_, nrow(points_sf))
  eras <- unique(stats::na.omit(as.character(era_values)))

  for (era_i in eras) {
    idx_era <- which(as.character(era_values) == era_i & !empty_point)
    seg_era <- segment_layers[[era_i]]
    if (length(idx_era) == 0 || is.null(seg_era) || nrow(seg_era) == 0) {
      next
    }

    seg_era <- seg_era[order(seg_era$segment_id), ]
    if (sf::st_crs(points_sf) != sf::st_crs(seg_era)) {
      pts_era <- sf::st_transform(points_sf[idx_era, ], sf::st_crs(seg_era))
    } else {
      pts_era <- points_sf[idx_era, ]
    }

    starts <- seq(1L, length(idx_era), by = chunk_n)
    for (s in starts) {
      e <- min(s + chunk_n - 1L, length(idx_era))
      local_chunk <- s:e
      global_chunk <- idx_era[local_chunk]

      nearest_idx <- sf::st_nearest_feature(pts_era[local_chunk, ], seg_era)
      valid_nearest <- !is.na(nearest_idx)
      if (!any(valid_nearest)) next

      valid_local <- local_chunk[valid_nearest]
      valid_global <- global_chunk[valid_nearest]
      nearest_segments <- seg_era[nearest_idx[valid_nearest], ]
      dists <- sf::st_distance(
        sf::st_geometry(pts_era[valid_local, ]),
        sf::st_geometry(nearest_segments),
        by_element = TRUE
      )
      dist_m <- as.numeric(units::set_units(dists, "m"))

      ok <- is.finite(dist_m) & dist_m <= max_distance_m
      if (any(ok)) {
        valid_local_ok <- valid_local[ok]
        valid_global_ok <- valid_global[ok]
        chosen_ids <- as.character(nearest_segments$segment_id[ok])

        candidate_hits <- sf::st_is_within_distance(
          pts_era[valid_local_ok, ],
          seg_era,
          dist = units::set_units(max_distance_m + tie_tolerance_m, "m")
        )
        multi_hit <- which(lengths(candidate_hits) > 1L)
        for (h in multi_hit) {
          candidate_idx <- candidate_hits[[h]]
          candidate_segments <- seg_era[candidate_idx, ]
          candidate_dists <- sf::st_distance(
            sf::st_geometry(pts_era[valid_local_ok[h], ]),
            sf::st_geometry(candidate_segments),
            by_element = FALSE
          )
          candidate_dist_m <- as.numeric(units::set_units(candidate_dists[1, ], "m"))
          min_dist_m <- min(candidate_dist_m, na.rm = TRUE)
          tied <- which(is.finite(candidate_dist_m) & candidate_dist_m <= min_dist_m + tie_tolerance_m)
          if (length(tied) > 0) {
            chosen_ids[h] <- sort(as.character(candidate_segments$segment_id[tied]))[1]
          }
        }

        seg_id[valid_global_ok] <- chosen_ids
      }
    }
  }

  seg_id
}

audit_nearest_segment_pair_constraints <- function(points_sf, era_values, pair_values, segment_layers, constrained_segment_id, max_distance, chunk_n = 50000L) {
  stopifnot(
    length(era_values) == nrow(points_sf),
    length(pair_values) == nrow(points_sf),
    length(constrained_segment_id) == nrow(points_sf)
  )

  if (!inherits(max_distance, "units")) {
    stop("max_distance must be a units object, e.g. units::set_units(250, 'm').", call. = FALSE)
  }
  empty_point <- assert_point_geometries(points_sf, "points_sf")

  max_distance_m <- as.numeric(units::set_units(max_distance, "m"))
  if (!is.finite(max_distance_m) || max_distance_m <= 0) {
    stop("max_distance must be positive.", call. = FALSE)
  }

  n <- nrow(points_sf)
  pair_dash <- normalize_pair_dash(pair_values)
  constrained_segment_id <- as.character(constrained_segment_id)
  constrained_segment_id[constrained_segment_id == ""] <- NA_character_

  constrained_pair_dash <- rep(NA_character_, n)
  constrained_dist_m <- rep(NA_real_, n)
  unconstrained_segment_id <- rep(NA_character_, n)
  unconstrained_pair_dash <- rep(NA_character_, n)
  unconstrained_dist_m <- rep(NA_real_, n)

  eras <- unique(stats::na.omit(as.character(era_values)))
  for (era_i in eras) {
    idx_era <- which(as.character(era_values) == era_i & !empty_point)
    seg_era <- segment_layers[[era_i]]
    if (length(idx_era) == 0 || is.null(seg_era) || nrow(seg_era) == 0) {
      next
    }

    seg_era <- seg_era[order(seg_era$segment_id), ]
    if (sf::st_crs(points_sf) != sf::st_crs(seg_era)) {
      pts_era <- sf::st_transform(points_sf[idx_era, ], sf::st_crs(seg_era))
    } else {
      pts_era <- points_sf[idx_era, ]
    }

    idx_constrained <- idx_era[!is.na(constrained_segment_id[idx_era])]
    if (length(idx_constrained) > 0) {
      constrained_match <- match(constrained_segment_id[idx_constrained], seg_era$segment_id)
      valid_constrained <- !is.na(constrained_match)
      if (any(valid_constrained)) {
        constrained_global <- idx_constrained[valid_constrained]
        constrained_local <- match(constrained_global, idx_era)
        constrained_segments <- seg_era[constrained_match[valid_constrained], ]
        constrained_dists <- sf::st_distance(
          sf::st_geometry(pts_era[constrained_local, ]),
          sf::st_geometry(constrained_segments),
          by_element = TRUE
        )
        constrained_dist_m[constrained_global] <- as.numeric(units::set_units(constrained_dists, "m"))
        constrained_pair_dash[constrained_global] <- constrained_segments$pair_dash
      }
    }

    starts <- seq(1L, length(idx_era), by = chunk_n)
    for (s in starts) {
      e <- min(s + chunk_n - 1L, length(idx_era))
      local_chunk <- s:e
      global_chunk <- idx_era[local_chunk]

      nearest_idx <- sf::st_nearest_feature(pts_era[local_chunk, ], seg_era)
      valid_nearest <- !is.na(nearest_idx)
      if (!any(valid_nearest)) next

      nearest_global <- global_chunk[valid_nearest]
      nearest_segments <- seg_era[nearest_idx[valid_nearest], ]
      nearest_dists <- sf::st_distance(
        sf::st_geometry(pts_era[local_chunk[valid_nearest], ]),
        sf::st_geometry(nearest_segments),
        by_element = TRUE
      )
      nearest_dist_m <- as.numeric(units::set_units(nearest_dists, "m"))

      unconstrained_segment_id[nearest_global] <- as.character(nearest_segments$segment_id)
      unconstrained_pair_dash[nearest_global] <- nearest_segments$pair_dash
      unconstrained_dist_m[nearest_global] <- nearest_dist_m
    }
  }

  tibble::tibble(
    constrained_pair_dash = constrained_pair_dash,
    constrained_segment_id = constrained_segment_id,
    constrained_segment_dist_m = constrained_dist_m,
    unconstrained_segment_id = unconstrained_segment_id,
    unconstrained_pair_dash = unconstrained_pair_dash,
    unconstrained_segment_dist_m = unconstrained_dist_m,
    unconstrained_within_radius = is.finite(unconstrained_dist_m) & unconstrained_dist_m <= max_distance_m,
    constrained_pair_matches_input = !is.na(pair_dash) & !is.na(constrained_pair_dash) & pair_dash == constrained_pair_dash,
    unconstrained_pair_matches_input = !is.na(pair_dash) & !is.na(unconstrained_pair_dash) & pair_dash == unconstrained_pair_dash,
    unconstrained_matches_constrained_segment = !is.na(constrained_segment_id) & !is.na(unconstrained_segment_id) & constrained_segment_id == unconstrained_segment_id,
    constrained_extra_dist_m = constrained_dist_m - unconstrained_dist_m
  )
}
