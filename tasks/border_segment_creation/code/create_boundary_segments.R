#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(units)
  library(readr)
  library(stringr)
  library(purrr)
  library(digest)
})

options(scipen = 999)

parse_args <- function(args) {
  out <- list()
  if (length(args) == 0) return(out)
  for (a in args) {
    if (!startsWith(a, "--")) next
    kv <- strsplit(sub("^--", "", a), "=", fixed = TRUE)[[1]]
    if (length(kv) == 2) out[[kv[1]]] <- kv[2]
  }
  out
}

arg_or_default <- function(args, key, default) {
  if (!is.null(args[[key]]) && nzchar(args[[key]])) return(args[[key]])
  default
}

numeric_csv <- function(x) {
  as.numeric(strsplit(x, ",", fixed = TRUE)[[1]])
}

expand_bbox <- function(geom, pad_ft = 1500) {
  bb <- st_bbox(geom)
  c(
    xmin = as.numeric(bb["xmin"] - pad_ft),
    xmax = as.numeric(bb["xmax"] + pad_ft),
    ymin = as.numeric(bb["ymin"] - pad_ft),
    ymax = as.numeric(bb["ymax"] + pad_ft)
  )
}

bbox_to_wkt <- function(sf_obj, pad_deg = 0.05) {
  bb <- st_bbox(st_transform(sf_obj, 4326))
  xmin <- as.numeric(bb["xmin"] - pad_deg)
  ymin <- as.numeric(bb["ymin"] - pad_deg)
  xmax <- as.numeric(bb["xmax"] + pad_deg)
  ymax <- as.numeric(bb["ymax"] + pad_deg)
  sprintf(
    "POLYGON((%f %f,%f %f,%f %f,%f %f,%f %f))",
    xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin
  )
}

as_ft <- function(x) {
  as.numeric(set_units(x, "US_survey_foot"))
}

as_sqft <- function(x) {
  as.numeric(set_units(x, "US_survey_foot^2"))
}

line_union_merge <- function(geom) {
  u <- st_union(geom)
  gt <- as.character(st_geometry_type(u))
  if (length(gt) > 0 && gt[1] == "MULTILINESTRING") {
    m <- tryCatch(st_line_merge(u), error = function(e) u)
    return(m)
  }
  u
}

safe_path_label <- function(x) {
  gsub("[^A-Za-z0-9_]+", "_", x)
}

make_plot <- function(p, path, width = 10, height = 8, dpi = 220) {
  ggsave(path, p, width = width, height = height, dpi = dpi)
}

assert_gate <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

map_major_class <- function(class_raw, street_typ) {
  cls <- suppressWarnings(as.integer(class_raw))
  stt <- toupper(ifelse(is.na(street_typ), "", as.character(street_typ)))
  if (stt %in% c("EXPY", "HWY", "TOLL", "FWY")) return("arterial")
  if (!is.na(cls) && cls %in% c(1, 2)) return("arterial")
  if (!is.na(cls) && cls == 3) return("collector")
  if (!is.na(cls) && cls == 9) return("residential")
  "residential"
}

build_street_name <- function(df) {
  nm <- str_squish(paste(df$PRE_DIR, df$STREET, df$STREET_TYP))
  nm[nm == ""] <- as.character(df$STREET_NAM[nm == ""])
  nm[nm == ""] <- as.character(df$STREET[nm == ""])
  nm
}

extract_eras_from_year_panel <- function(ward_panel) {
  stopifnot("year" %in% names(ward_panel), "ward" %in% names(ward_panel))
  ward_panel <- ward_panel %>%
    mutate(year = as.integer(year), ward = as.integer(ward)) %>%
    arrange(year, ward)

  wkb <- st_as_binary(st_geometry(ward_panel), EWKB = TRUE)
  ward_panel$geom_hash <- vapply(wkb, digest, character(1), algo = "xxhash64")

  year_hash <- ward_panel %>%
    st_drop_geometry() %>%
    group_by(year) %>%
    summarise(
      year_hash = digest(paste(geom_hash, collapse = "|"), algo = "xxhash64"),
      n = n(),
      wards = n_distinct(ward),
      .groups = "drop"
    ) %>%
    arrange(year)

  era_tbl <- year_hash %>%
    group_by(year_hash) %>%
    summarise(
      start_year = min(year),
      end_year = max(year),
      n_years = n(),
      years = paste(year, collapse = ","),
      .groups = "drop"
    ) %>%
    arrange(start_year) %>%
    mutate(
      era = case_when(
        start_year >= 2024 ~ "post_2023",
        TRUE ~ paste0(start_year, "_", end_year)
      ),
      era_three = case_when(
        end_year <= 2014 ~ "pre_2015",
        end_year <= 2023 ~ "2015_2023",
        TRUE ~ "post_2023"
      )
    )

  list(era_table = era_tbl, year_table = year_hash)
}

extract_shared_boundaries <- function(wards_era, era_name) {
  wards_era <- wards_era %>% arrange(ward)
  touch_list <- st_touches(wards_era)
  rec <- list()

  for (i in seq_len(nrow(wards_era))) {
    js <- touch_list[[i]]
    js <- js[js > i]
    if (length(js) == 0) next

    for (j in js) {
      i_line <- st_boundary(wards_era[i, ])
      j_line <- st_boundary(wards_era[j, ])
      inter <- suppressWarnings(st_intersection(i_line, j_line))
      if (nrow(inter) == 0) next

      line_parts <- suppressWarnings(st_collection_extract(st_geometry(inter), "LINESTRING"))
      if (length(line_parts) == 0) next
      line_parts <- line_parts[as_ft(st_length(line_parts)) > 0]
      if (length(line_parts) == 0) next

      merged <- line_union_merge(line_parts)
      len_ft <- as_ft(st_length(merged))
      if (is.na(len_ft) || len_ft <= 0) next

      wa <- as.integer(wards_era$ward[i])
      wb <- as.integer(wards_era$ward[j])

      rec[[length(rec) + 1]] <- list(
        ward_a = min(wa, wb),
        ward_b = max(wa, wb),
        ward_pair_id = sprintf("%d_%d", min(wa, wb), max(wa, wb)),
        era = era_name,
        geometry = merged[[1]]
      )
    }
  }

  if (length(rec) == 0) {
    return(st_sf(
      ward_a = integer(0),
      ward_b = integer(0),
      ward_pair_id = character(0),
      era = character(0),
      length_ft = numeric(0),
      geometry = st_sfc(crs = st_crs(wards_era))
    ))
  }

  df <- bind_rows(lapply(rec, function(x) tibble(
    ward_a = x$ward_a,
    ward_b = x$ward_b,
    ward_pair_id = x$ward_pair_id,
    era = x$era
  )))
  geom <- st_sfc(lapply(rec, function(x) x$geometry), crs = st_crs(wards_era))
  out <- st_sf(df, geometry = geom)

  out <- out %>%
    group_by(ward_a, ward_b, ward_pair_id, era) %>%
    summarise(geometry = line_union_merge(geometry), .groups = "drop")

  out$length_ft <- as_ft(st_length(out))
  out <- out %>% select(ward_a, ward_b, ward_pair_id, era, length_ft, geometry)
  out
}

split_linestring_matrix <- function(coords, n_segments) {
  if (nrow(coords) < 2) return(list(coords))

  dxy <- coords[-1, , drop = FALSE] - coords[-nrow(coords), , drop = FALSE]
  step <- sqrt(rowSums(dxy^2))
  keep <- c(TRUE, step > 0)
  coords <- coords[keep, , drop = FALSE]

  if (nrow(coords) < 2) return(list(coords))
  dxy <- coords[-1, , drop = FALSE] - coords[-nrow(coords), , drop = FALSE]
  step <- sqrt(rowSums(dxy^2))
  cumd <- c(0, cumsum(step))
  total <- tail(cumd, 1)

  if (total == 0 || n_segments <= 1) return(list(coords))

  breaks <- seq(0, total, length.out = n_segments + 1)
  idx <- findInterval(breaks, cumd, rightmost.closed = TRUE, all.inside = TRUE)
  idx[idx >= nrow(coords)] <- nrow(coords) - 1

  break_pts <- matrix(NA_real_, nrow = length(breaks), ncol = 2)
  for (b in seq_along(breaks)) {
    i <- idx[b]
    d <- breaks[b]
    d0 <- cumd[i]
    d1 <- cumd[i + 1]
    if (d1 <= d0) {
      break_pts[b, ] <- coords[i, ]
    } else {
      t <- (d - d0) / (d1 - d0)
      break_pts[b, ] <- coords[i, ] + t * (coords[i + 1, ] - coords[i, ])
    }
  }

  out <- vector("list", n_segments)
  for (k in seq_len(n_segments)) {
    i0 <- idx[k]
    i1 <- idx[k + 1]
    interior <- matrix(numeric(0), ncol = 2)
    start_int <- i0 + 1
    end_int <- i1
    if (start_int <= end_int) interior <- coords[start_int:end_int, , drop = FALSE]
    seg_coords <- rbind(break_pts[k, , drop = FALSE], interior, break_pts[k + 1, , drop = FALSE])

    if (nrow(seg_coords) > 1) {
      dif <- seg_coords[-1, , drop = FALSE] - seg_coords[-nrow(seg_coords), , drop = FALSE]
      keep2 <- c(TRUE, sqrt(rowSums(dif^2)) > 1e-7)
      seg_coords <- seg_coords[keep2, , drop = FALSE]
    }
    if (nrow(seg_coords) < 2) {
      seg_coords <- rbind(break_pts[k, , drop = FALSE], break_pts[k + 1, , drop = FALSE])
    }
    out[[k]] <- seg_coords
  }
  out
}

segment_boundary_geometry <- function(geom, target_len_ft, crs_obj) {
  g <- st_sfc(geom, crs = crs_obj)
  parts <- suppressWarnings(st_cast(g, "LINESTRING"))
  seg_out <- list()

  for (i in seq_along(parts)) {
    part <- parts[i]
    part_len <- as_ft(st_length(part))
    if (is.na(part_len) || part_len <= 0) next
    n_seg <- max(1L, as.integer(round(part_len / target_len_ft)))
    coords <- st_coordinates(part)[, c("X", "Y"), drop = FALSE]
    mats <- split_linestring_matrix(coords, n_seg)
    for (m in mats) seg_out[[length(seg_out) + 1]] <- st_linestring(m)
  }
  seg_out
}

build_segments <- function(boundaries, target_len_ft, verbose = FALSE) {
  rec <- list()
  geoms <- list()
  crs_obj <- st_crs(boundaries)
  ridx <- 1L

  for (i in seq_len(nrow(boundaries))) {
    if (verbose && (i %% 20 == 0 || i == nrow(boundaries))) {
      cat(sprintf("    build_segments progress: %d/%d boundaries at %d ft\n", i, nrow(boundaries), target_len_ft))
      flush.console()
    }
    seg_geoms <- segment_boundary_geometry(st_geometry(boundaries)[[i]], target_len_ft, crs_obj)
    if (length(seg_geoms) == 0) next
    n_pair <- length(seg_geoms)
    for (j in seq_len(n_pair)) {
      rec[[ridx]] <- tibble(
        ward_pair_id = boundaries$ward_pair_id[i],
        ward_a = as.integer(boundaries$ward_a[i]),
        ward_b = as.integer(boundaries$ward_b[i]),
        era = as.character(boundaries$era[i]),
        segment_number = j,
        n_segments_in_pair = n_pair
      )
      geoms[[ridx]] <- seg_geoms[[j]]
      ridx <- ridx + 1L
    }
  }

  if (length(rec) == 0) {
    return(st_sf(
      segment_id = character(0),
      ward_pair_id = character(0),
      ward_a = integer(0),
      ward_b = integer(0),
      era = character(0),
      segment_number = integer(0),
      n_segments_in_pair = integer(0),
      segment_length_ft = numeric(0),
      centroid_lat = numeric(0),
      centroid_lon = numeric(0),
      geometry = st_sfc(crs = crs_obj)
    ))
  }

  seg <- st_sf(bind_rows(rec), geometry = st_sfc(geoms, crs = crs_obj))
  seg$segment_length_ft <- as_ft(st_length(seg))
  seg$segment_id <- sprintf(
    "%s_%s_%d",
    seg$ward_pair_id,
    seg$era,
    as.integer(seg$segment_number)
  )

  cent <- suppressWarnings(st_centroid(seg))
  cent_ll <- st_transform(cent, 4326)
  cent_xy <- st_coordinates(cent_ll)
  seg$centroid_lon <- cent_xy[, "X"]
  seg$centroid_lat <- cent_xy[, "Y"]

  seg <- seg %>%
    select(
      segment_id, ward_pair_id, ward_a, ward_b, era, segment_number,
      n_segments_in_pair, segment_length_ft, centroid_lat, centroid_lon, geometry
    )
  seg
}

validate_segments_against_boundaries <- function(segments, boundaries, target_len_ft) {
  seg_stat <- segments %>%
    group_by(ward_pair_id, era) %>%
    summarise(
      seg_sum_len_ft = sum(segment_length_ft, na.rm = TRUE),
      seg_union_len_ft = as_ft(st_length(st_union(geometry))),
      n_segments = n(),
      .groups = "drop"
    )

  bnd_stat <- boundaries %>%
    st_drop_geometry() %>%
    select(ward_pair_id, era, boundary_len_ft = length_ft)

  seg_stat %>%
    left_join(bnd_stat, by = c("ward_pair_id", "era")) %>%
    mutate(
      abs_len_diff_ft = abs(seg_union_len_ft - boundary_len_ft),
      overlap_or_gap_ft = abs(seg_sum_len_ft - seg_union_len_ft),
      target_len_ft = target_len_ft
    )
}

classify_segments <- function(segments,
                              major_streets,
                              water_poly,
                              waterways,
                              park_poly,
                              corridor_ft = 50,
                              nearest_cutoff_ft = 100) {
  n <- nrow(segments)
  if (n == 0) return(segments)

  corridors <- st_buffer(segments, corridor_ft)
  midpoints <- suppressWarnings(st_line_sample(segments, sample = 0.5))
  if (length(midpoints) != n || any(st_is_empty(midpoints))) midpoints <- st_centroid(segments)

  nearest_idx <- st_nearest_feature(midpoints, major_streets)
  nearest_dist <- as_ft(st_distance(midpoints, major_streets[nearest_idx, ], by_element = TRUE))
  nearest_name_all <- as.character(major_streets$street_name[nearest_idx])
  nearest_raw_all <- as.character(major_streets$street_class_raw[nearest_idx])
  nearest_map_all <- as.character(major_streets$street_class_mapped[nearest_idx])

  major_hits <- st_intersects(corridors, major_streets)
  water_hits <- st_intersects(corridors, water_poly)
  park_hits <- st_intersects(corridors, park_poly)
  waterway_hits <- st_intersects(corridors, waterways)

  seg_type <- character(n)
  nearest_name <- character(n)
  nearest_class_raw <- character(n)
  nearest_class_mapped <- character(n)
  dist_to_nearest <- numeric(n)
  major_art_ft <- numeric(n)
  major_col_ft <- numeric(n)
  major_res_ft <- numeric(n)
  water_share <- numeric(n)
  park_share <- numeric(n)
  waterway_ft <- numeric(n)

  for (i in seq_len(n)) {
    m_idx <- major_hits[[i]]
    class_weight <- c(arterial = 0, collector = 0, residential = 0)
    top_name <- NA_character_
    top_class_raw <- NA_character_

    if (length(m_idx) > 0) {
      m_hit <- major_streets[m_idx, ]
      m_tbl <- m_hit %>%
        st_drop_geometry() %>%
        mutate(seg_len_ft = as_ft(st_length(m_hit)))
      cls_sum <- m_tbl %>%
        group_by(street_class_mapped) %>%
        summarise(weight = sum(seg_len_ft), .groups = "drop")
      for (k in seq_len(nrow(cls_sum))) {
        nm <- cls_sum$street_class_mapped[k]
        if (nm %in% names(class_weight)) class_weight[[nm]] <- class_weight[[nm]] + cls_sum$weight[k]
      }

      nearest_within_idx <- st_nearest_feature(midpoints[i], m_hit)
      top_name <- as.character(m_hit$street_name[nearest_within_idx])
      top_class_raw <- as.character(m_hit$street_class_raw[nearest_within_idx])
    } else {
      top_name <- nearest_name_all[i]
      top_class_raw <- nearest_raw_all[i]
    }

    near_dist <- nearest_dist[i]
    near_name <- nearest_name_all[i]
    near_raw <- nearest_raw_all[i]
    near_map <- nearest_map_all[i]

    has_water <- length(water_hits[[i]]) > 0
    has_park <- length(park_hits[[i]]) > 0
    has_waterway <- length(waterway_hits[[i]]) > 0
    park_water_flag <- has_water || has_park || has_waterway

    m_total <- sum(class_weight)
    if (m_total > 0) {
      ord <- sort(class_weight, decreasing = TRUE)
      primary <- names(ord)[1]
      share2 <- ifelse(length(ord) > 1, ord[2] / m_total, 0)
      share1 <- ord[1] / m_total
      if ((park_water_flag && share1 < 0.80) || share2 >= 0.35) {
        seg_type_i <- "mixed"
      } else {
        seg_type_i <- primary
      }
    } else {
      if (park_water_flag) {
        seg_type_i <- "park_water"
      } else if (!is.na(near_dist) && near_dist <= nearest_cutoff_ft && !is.na(near_map)) {
        seg_type_i <- near_map
      } else {
        seg_type_i <- "no_feature"
      }
    }

    seg_type[i] <- seg_type_i
    nearest_name[i] <- ifelse(!is.na(top_name), top_name, near_name)
    nearest_class_raw[i] <- ifelse(!is.na(top_class_raw), top_class_raw, near_raw)
    nearest_class_mapped[i] <- near_map
    dist_to_nearest[i] <- near_dist
    major_art_ft[i] <- class_weight["arterial"]
    major_col_ft[i] <- class_weight["collector"]
    major_res_ft[i] <- class_weight["residential"]
    water_share[i] <- as.numeric(has_water)
    park_share[i] <- as.numeric(has_park)
    waterway_ft[i] <- as.numeric(has_waterway)

    if (i %% 500 == 0 || i == n) {
      cat(sprintf("    classify_segments progress: %d/%d\n", i, n))
      flush.console()
    }
  }

  segments$segment_type <- seg_type
  segments$nearest_street_name <- nearest_name
  segments$nearest_street_class <- nearest_class_raw
  segments$nearest_street_class_mapped <- nearest_class_mapped
  segments$distance_to_nearest_street_ft <- dist_to_nearest
  segments$major_overlap_arterial_ft <- major_art_ft
  segments$major_overlap_collector_ft <- major_col_ft
  segments$major_overlap_residential_ft <- major_res_ft
  segments$water_area_share <- water_share
  segments$park_area_share <- park_share
  segments$waterway_overlap_ft <- waterway_ft
  segments
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
target_crs <- 3435
target_lengths <- numeric_csv(arg_or_default(args, "target_lengths", "1320,660,2640"))
target_lengths <- sort(unique(as.integer(target_lengths)))
corridor_ft <- as.numeric(arg_or_default(args, "corridor_ft", "50"))
nearest_cutoff_ft <- as.numeric(arg_or_default(args, "nearest_ft", "100"))

ward_panel_path <- arg_or_default(args, "ward_panel", "../input/ward_panel.gpkg")
major_streets_path <- arg_or_default(args, "major_streets", "../input/Major_Streets.shp")
osm_water_path <- arg_or_default(args, "osm_water", "../input/gis_osm_water_a_free_1.shp")
osm_waterways_path <- arg_or_default(args, "osm_waterways", "../input/gis_osm_waterways_free_1.shp")
osm_landuse_path <- arg_or_default(args, "osm_landuse", "../input/gis_osm_landuse_a_free_1.shp")
output_dir <- arg_or_default(args, "output_dir", "../output")
diagnostics_dir <- arg_or_default(args, "diagnostics_dir", "../diagnostics")
export_dir <- arg_or_default(args, "export_dir", "../../../data_raw/boundaries/segments")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

diag_lines <- character()
diag_path <- file.path(diagnostics_dir, "segment_diagnostics.md")

log_line <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n")
  diag_lines <<- c(diag_lines, msg)
}

finalize_diag <- function() {
  writeLines(diag_lines, diag_path)
}

log_line("# Border Segment Creation Diagnostics")
log_line("")
log_line("## Runtime")
log_line("- Working directory: ", normalizePath(getwd()))
log_line("- Ward panel input: ", normalizePath(ward_panel_path))
log_line("- Major streets input: ", normalizePath(major_streets_path))
log_line("- CRS target: EPSG:", target_crs)
log_line("- Segment lengths (ft): ", paste(target_lengths, collapse = ", "))
log_line("- Corridor buffer (ft): ", corridor_ft)
log_line("- Nearest street cutoff (ft): ", nearest_cutoff_ft)

log_line("")
log_line("## Phase 1: Load and validate ward boundaries")

layer_info <- st_layers(ward_panel_path)
log_line("- Layers in ward gpkg: ", paste(layer_info$name, collapse = ", "))
for (ly in layer_info$name) {
  tmp <- st_read(ward_panel_path, layer = ly, quiet = TRUE)
  cr <- st_crs(tmp)
  log_line("- Layer ", ly, ": rows=", nrow(tmp), " cols=[", paste(names(tmp), collapse = ", "), "]")
  log_line("  - CRS EPSG=", cr$epsg, " name=", cr$Name, " units=", cr$units_gdal)
  if ("year" %in% names(tmp)) {
    yr_tab <- tmp %>% st_drop_geometry() %>% count(year, name = "n")
    log_line("  - Year range: ", min(yr_tab$year), " to ", max(yr_tab$year), " (", nrow(yr_tab), " years)")
  }
}

ward_raw <- st_read(ward_panel_path, layer = layer_info$name[1], quiet = TRUE)
sf_col <- attr(ward_raw, "sf_column")

era_meta <- NULL
wards_by_era <- list()

if ("year" %in% names(ward_raw)) {
  era_obj <- extract_eras_from_year_panel(ward_raw)
  era_meta <- era_obj$era_table
  log_line("- Detected geometry regimes: ", nrow(era_meta))
  for (i in seq_len(nrow(era_meta))) {
    log_line(
      "  - ", era_meta$era[i], " (", era_meta$start_year[i], "-", era_meta$end_year[i],
      "; collapsed_three_era=", era_meta$era_three[i], ")"
    )
  }
  for (i in seq_len(nrow(era_meta))) {
    e <- era_meta$era[i]
    y0 <- era_meta$start_year[i]
    wards_i <- ward_raw %>%
      mutate(year = as.integer(year), ward = as.integer(ward)) %>%
      filter(year == y0) %>%
      select(ward, geometry = all_of(sf_col))
    wards_i$era <- e
    wards_by_era[[e]] <- wards_i
  }
} else if ("era" %in% names(ward_raw)) {
  era_vals <- sort(unique(as.character(ward_raw$era)))
  era_meta <- tibble(
    era = era_vals,
    start_year = NA_integer_,
    end_year = NA_integer_,
    era_three = era_vals
  )
  for (e in era_vals) {
    wards_i <- ward_raw %>%
      mutate(ward = as.integer(ward)) %>%
      filter(as.character(era) == e) %>%
      select(ward, geometry = all_of(sf_col))
    wards_i$era <- e
    wards_by_era[[e]] <- wards_i
  }
} else {
  stop("Ward panel must contain either a 'year' or 'era' column.", call. = FALSE)
}

ward_validation_rows <- list()

for (e in names(wards_by_era)) {
  w <- wards_by_era[[e]]

  if (is.na(st_crs(w)$epsg) || st_crs(w)$epsg != target_crs) {
    w <- st_transform(w, target_crs)
  }

  valid_before <- sum(!st_is_valid(w))
  if (valid_before > 0) w <- st_make_valid(w)
  valid_after <- sum(!st_is_valid(w))

  if (anyDuplicated(w$ward) > 0) {
    w <- w %>%
      group_by(ward, era) %>%
      summarise(geometry = st_union(geometry), .groups = "drop")
  }

  n_wards <- nrow(w)
  n_uniq <- n_distinct(w$ward)
  cr <- st_crs(w)
  units_txt <- ifelse(is.null(cr$units_gdal), "unknown", cr$units_gdal)

  ward_validation_rows[[length(ward_validation_rows) + 1]] <- tibble(
    era = e,
    n_rows = n_wards,
    n_unique_wards = n_uniq,
    invalid_before = valid_before,
    invalid_after = valid_after,
    epsg = cr$epsg,
    units = units_txt
  )

  lab <- st_point_on_surface(w)
  p <- ggplot() +
    geom_sf(data = w, fill = "grey96", color = "grey55", linewidth = 0.20) +
    geom_sf_text(data = lab, aes(label = ward), size = 2.2) +
    coord_sf() +
    theme_minimal() +
    labs(
      title = paste0("Phase 1 Ward Map: ", e),
      subtitle = paste0("n_wards=", n_wards, " | CRS EPSG:", cr$epsg, " (", units_txt, ")"),
      x = "Easting (ft)",
      y = "Northing (ft)"
    )
  make_plot(p, file.path(diagnostics_dir, paste0("phase1_ward_map_", safe_path_label(e), ".png")))

  wards_by_era[[e]] <- w
}

ward_validation <- bind_rows(ward_validation_rows)
write_csv(ward_validation, file.path(diagnostics_dir, "phase1_ward_validation.csv"))

for (i in seq_len(nrow(ward_validation))) {
  row <- ward_validation[i, ]
  log_line(
    "- ", row$era, ": rows=", row$n_rows,
    ", unique_wards=", row$n_unique_wards,
    ", invalid_before=", row$invalid_before,
    ", invalid_after=", row$invalid_after,
    ", epsg=", row$epsg,
    ", units=", row$units
  )
}

assert_gate(all(ward_validation$n_unique_wards == 50), "PHASE 1 GATE FAILED: not exactly 50 unique wards in every era.")
assert_gate(all(ward_validation$invalid_after == 0), "PHASE 1 GATE FAILED: invalid ward geometries remain after repair.")
assert_gate(all(ward_validation$epsg == target_crs), "PHASE 1 GATE FAILED: not all ward geometries are EPSG:3435.")
log_line("- PHASE 1 GATE PASSED")

log_line("")
log_line("## Phase 2: Extract shared ward-pair boundaries")

boundaries_by_era <- list()
phase2_stats <- list()
phase2_outliers <- list()

for (e in names(wards_by_era)) {
  w <- wards_by_era[[e]]
  b <- extract_shared_boundaries(w, e)
  boundaries_by_era[[e]] <- b

  geom_types <- unique(as.character(st_geometry_type(b)))
  b_count <- nrow(b)
  stats_tbl <- tibble(
    era = e,
    boundary_pairs = b_count,
    min_len_ft = min(b$length_ft),
    median_len_ft = median(b$length_ft),
    mean_len_ft = mean(b$length_ft),
    max_len_ft = max(b$length_ft),
    geometry_types = paste(geom_types, collapse = ",")
  )
  phase2_stats[[length(phase2_stats) + 1]] <- stats_tbl

  short <- b %>% st_drop_geometry() %>% filter(length_ft < 100) %>% mutate(flag = "short_lt_100ft")
  long <- b %>% st_drop_geometry() %>% filter(length_ft > 30000) %>% mutate(flag = "long_gt_30000ft")
  outl <- bind_rows(short, long) %>% mutate(era = e)
  phase2_outliers[[length(phase2_outliers) + 1]] <- outl

  p_all <- ggplot() +
    geom_sf(data = w, fill = NA, color = "grey80", linewidth = 0.20) +
    geom_sf(data = b, aes(color = ward_pair_id), linewidth = 0.55, show.legend = FALSE) +
    coord_sf() +
    theme_minimal() +
    labs(
      title = paste0("Phase 2 Shared Ward-Pair Boundaries: ", e),
      subtitle = paste0("pairs=", b_count),
      x = "Easting (ft)",
      y = "Northing (ft)"
    )
  make_plot(p_all, file.path(diagnostics_dir, paste0("phase2_all_boundaries_", safe_path_label(e), ".png")))

  p_hist <- ggplot(st_drop_geometry(b), aes(x = length_ft)) +
    geom_histogram(bins = 35, fill = "#2b8cbe", color = "white", linewidth = 0.2) +
    theme_minimal() +
    labs(
      title = paste0("Phase 2 Boundary Length Histogram: ", e),
      x = "Boundary length (ft)",
      y = "Count"
    )
  make_plot(p_hist, file.path(diagnostics_dir, paste0("phase2_boundary_length_hist_", safe_path_label(e), ".png")), width = 9, height = 6)

  zoom_pairs <- unique(c("2_43", b %>% arrange(desc(length_ft)) %>% pull(ward_pair_id) %>% head(3)))
  for (pid in zoom_pairs) {
    bi <- b %>% filter(ward_pair_id == pid)
    if (nrow(bi) == 0) next
    bb <- expand_bbox(bi, pad_ft = 1600)
    p_zoom <- ggplot() +
      geom_sf(data = w, fill = NA, color = "grey80", linewidth = 0.20) +
      geom_sf(data = bi, color = "black", linewidth = 1.15) +
      coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
      theme_minimal() +
      labs(
        title = paste0("Phase 2 Zoom Boundary ", gsub("_", "-", pid), " (", e, ")"),
        x = "Easting (ft)",
        y = "Northing (ft)"
      )
    make_plot(
      p_zoom,
      file.path(diagnostics_dir, paste0("phase2_zoom_", safe_path_label(e), "_pair_", safe_path_label(pid), ".png")),
      width = 8,
      height = 8
    )
  }
}

phase2_stats_df <- bind_rows(phase2_stats)
phase2_outliers_df <- bind_rows(phase2_outliers)
write_csv(phase2_stats_df, file.path(diagnostics_dir, "phase2_boundary_stats.csv"))
write_csv(phase2_outliers_df, file.path(diagnostics_dir, "phase2_boundary_outliers.csv"))

for (i in seq_len(nrow(phase2_stats_df))) {
  row <- phase2_stats_df[i, ]
  log_line(
    "- ", row$era, ": pairs=", row$boundary_pairs,
    ", length_ft[min/med/mean/max]=",
    round(row$min_len_ft, 1), "/", round(row$median_len_ft, 1), "/",
    round(row$mean_len_ft, 1), "/", round(row$max_len_ft, 1),
    ", geom_types=", row$geometry_types
  )
}
if (nrow(phase2_outliers_df) > 0) {
  log_line("- Outlier boundaries flagged (short/long): ", nrow(phase2_outliers_df))
}

assert_gate(
  all(grepl("LINESTRING", phase2_stats_df$geometry_types)),
  "PHASE 2 GATE FAILED: non-line boundary geometry detected."
)
if (any(phase2_stats_df$boundary_pairs < 120)) {
  log_line("- Note: Some eras have <120 boundary pairs; inspect phase2 maps before downstream use.")
}
assert_gate(
  all(phase2_stats_df$boundary_pairs >= 90 & phase2_stats_df$boundary_pairs <= 220),
  "PHASE 2 GATE FAILED: boundary-pair count outside broad plausibility range [90, 220]."
)
if ("2015_2023" %in% names(boundaries_by_era)) {
  assert_gate(
    any(boundaries_by_era[["2015_2023"]]$ward_pair_id == "2_43"),
    "PHASE 2 GATE FAILED: ward pair 2_43 missing in 2015_2023 era."
  )
}
log_line("- PHASE 2 GATE PASSED")

ward_pair_boundaries_path <- file.path(output_dir, "ward_pair_boundaries.gpkg")
if (file.exists(ward_pair_boundaries_path)) file.remove(ward_pair_boundaries_path)

append_flag <- FALSE
for (e in names(boundaries_by_era)) {
  st_write(
    boundaries_by_era[[e]],
    ward_pair_boundaries_path,
    layer = safe_path_label(e),
    append = append_flag,
    quiet = TRUE
  )
  append_flag <- TRUE
}

log_line("")
log_line("## Phase 3: Segment each shared boundary")

boundaries_all <- do.call(rbind, boundaries_by_era)
segments_by_target <- list()
segment_validation_all <- list()

  for (target_len in target_lengths) {
  log_line("- Building segments at target length ", target_len, " ft")

  seg_list <- list()
  for (e in names(boundaries_by_era)) {
    log_line("  - era ", e, ": start segmentation")
    seg_e <- build_segments(boundaries_by_era[[e]], target_len, verbose = TRUE)
    log_line("  - era ", e, ": done segmentation (segments=", nrow(seg_e), ")")
    seg_list[[e]] <- seg_e
  }
  seg_all <- do.call(rbind, seg_list)

  seg_valid <- validate_segments_against_boundaries(seg_all, boundaries_all, target_len)
  segment_validation_all[[as.character(target_len)]] <- seg_valid

  segments_by_target[[as.character(target_len)]] <- seg_all

  log_line(
    "  - Total segments=", nrow(seg_all),
    ", min/med/mean/max segment length ft=",
    round(min(seg_all$segment_length_ft), 1), "/",
    round(median(seg_all$segment_length_ft), 1), "/",
    round(mean(seg_all$segment_length_ft), 1), "/",
    round(max(seg_all$segment_length_ft), 1)
  )

  p_hist_seg <- ggplot(st_drop_geometry(seg_all), aes(x = segment_length_ft)) +
    geom_histogram(bins = 40, fill = "#8856a7", color = "white", linewidth = 0.2) +
    theme_minimal() +
    labs(
      title = paste0("Phase 3 Segment Length Histogram (", target_len, " ft target)"),
      x = "Segment length (ft)",
      y = "Count"
    )
  make_plot(
    p_hist_seg,
    file.path(diagnostics_dir, paste0("phase3_segment_length_hist_", target_len, "ft.png")),
    width = 9,
    height = 6
  )

  era_for_zoom <- if ("2015_2023" %in% seg_all$era) "2015_2023" else unique(seg_all$era)[1]
  pair_for_zoom <- "2_43"
  seg_zoom <- seg_all %>% filter(era == era_for_zoom, ward_pair_id == pair_for_zoom)
  if (nrow(seg_zoom) > 0) {
    bb <- expand_bbox(seg_zoom, pad_ft = 1500)
    lab <- st_point_on_surface(seg_zoom)
    wards_zoom <- wards_by_era[[era_for_zoom]]

    p_zoom <- ggplot() +
      geom_sf(data = wards_zoom, fill = NA, color = "grey80", linewidth = 0.20) +
      geom_sf(data = seg_zoom, aes(color = factor(segment_number)), linewidth = 1.0, show.legend = FALSE) +
      geom_sf_text(data = lab, aes(label = segment_number), size = 2.3) +
      coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
      theme_minimal() +
      labs(
        title = paste0("Phase 3 Segments: pair 2-43 (", era_for_zoom, ", target ", target_len, " ft)"),
        x = "Easting (ft)",
        y = "Northing (ft)"
      )
    make_plot(
      p_zoom,
      file.path(diagnostics_dir, paste0("phase3_segments_pair_2_43_", safe_path_label(era_for_zoom), "_", target_len, "ft.png")),
      width = 8.4,
      height = 8
    )
  }

  comp_pairs <- seg_all %>%
    st_drop_geometry() %>%
    count(era, ward_pair_id, n_segments_in_pair, sort = TRUE) %>%
    arrange(desc(n_segments_in_pair)) %>%
    distinct(ward_pair_id) %>%
    pull(ward_pair_id) %>%
    head(4)

  for (pid in comp_pairs) {
    seg_pid <- seg_all %>% filter(ward_pair_id == pid, era == era_for_zoom)
    if (nrow(seg_pid) == 0) next
    bb <- expand_bbox(seg_pid, pad_ft = 1200)
    p_pid <- ggplot() +
      geom_sf(data = wards_by_era[[era_for_zoom]], fill = NA, color = "grey80", linewidth = 0.20) +
      geom_sf(data = seg_pid, aes(color = factor(segment_number)), linewidth = 1.0, show.legend = FALSE) +
      coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
      theme_minimal() +
      labs(
        title = paste0("Phase 3 Segment Check: pair ", gsub("_", "-", pid), " (", target_len, " ft)"),
        x = "Easting (ft)",
        y = "Northing (ft)"
      )
    make_plot(
      p_pid,
      file.path(diagnostics_dir, paste0("phase3_segments_pair_", safe_path_label(pid), "_", target_len, "ft.png")),
      width = 8,
      height = 8
    )
  }
}

segment_validation_df <- bind_rows(segment_validation_all)
write_csv(segment_validation_df, file.path(diagnostics_dir, "phase3_segment_validation.csv"))

segment_id_check <- bind_rows(lapply(names(segments_by_target), function(nm) {
  tibble(target_len_ft = nm, segment_id = segments_by_target[[nm]]$segment_id)
}))
assert_gate(
  all(!duplicated(paste0(segment_id_check$target_len_ft, "::", segment_id_check$segment_id))),
  "PHASE 3 GATE FAILED: duplicate segment IDs detected within a target length."
)
assert_gate(
  max(segment_validation_df$abs_len_diff_ft, na.rm = TRUE) <= 10,
  "PHASE 3 GATE FAILED: segment union length does not match original boundary."
)
assert_gate(
  max(segment_validation_df$overlap_or_gap_ft, na.rm = TRUE) <= 10,
  "PHASE 3 GATE FAILED: segment tiling has overlap/gap length above tolerance."
)
log_line("- PHASE 3 GATE PASSED")

log_line("")
log_line("## Phase 4: Classify segments by geographic feature")

city_union <- st_union(do.call(rbind, wards_by_era))
wkt_filter <- bbox_to_wkt(st_as_sf(data.frame(id = 1), geometry = st_sfc(city_union)))

major <- st_read(major_streets_path, quiet = TRUE)
if (is.na(st_crs(major)$epsg) || st_crs(major)$epsg != target_crs) major <- st_transform(major, target_crs)
major <- major %>%
  mutate(
    street_name = build_street_name(.),
    street_class_raw = as.character(CLASS),
    street_class_mapped = vapply(seq_len(n()), function(i) map_major_class(CLASS[i], STREET_TYP[i]), character(1))
  ) %>%
  select(street_name, street_class_raw, street_class_mapped, everything())

log_line("- Major streets rows=", nrow(major), "; class values=", paste(sort(unique(major$street_class_raw)), collapse = ","))

water_poly <- st_read(osm_water_path, quiet = TRUE, wkt_filter = wkt_filter)
waterways <- st_read(osm_waterways_path, quiet = TRUE, wkt_filter = wkt_filter)
landuse <- st_read(osm_landuse_path, quiet = TRUE, wkt_filter = wkt_filter)

water_poly <- st_transform(water_poly, target_crs)
waterways <- st_transform(waterways, target_crs)
landuse <- st_transform(landuse, target_crs)
park_poly <- landuse %>%
  filter(fclass %in% c("park", "recreation_ground", "nature_reserve"))

log_line("- OSM water polygons=", nrow(water_poly), ", waterways=", nrow(waterways), ", park polygons=", nrow(park_poly))

for (target_len in names(segments_by_target)) {
  log_line("- Classifying segments for ", target_len, " ft resolution")
  segments_by_target[[target_len]] <- classify_segments(
    segments = segments_by_target[[target_len]],
    major_streets = major,
    water_poly = water_poly,
    waterways = waterways,
    park_poly = park_poly,
    corridor_ft = corridor_ft,
    nearest_cutoff_ft = nearest_cutoff_ft
  )
}

seg1320 <- segments_by_target[["1320"]]
if (is.null(seg1320)) seg1320 <- segments_by_target[[1]]

type_tab <- seg1320 %>% st_drop_geometry() %>% count(era, segment_type, sort = TRUE)
write_csv(type_tab, file.path(diagnostics_dir, "phase4_segment_type_counts.csv"))
log_line("- Segment type x era table written: phase4_segment_type_counts.csv")

pair_era <- if ("2015_2023" %in% seg1320$era) "2015_2023" else unique(seg1320$era)[1]
pair_243 <- seg1320 %>% filter(ward_pair_id == "2_43", era == pair_era) %>%
  st_drop_geometry() %>%
  select(segment_id, segment_number, segment_type, nearest_street_name, nearest_street_class, distance_to_nearest_street_ft)
write_csv(pair_243, file.path(diagnostics_dir, "phase4_pair_2_43_classification.csv"))

segment_type_cols <- c(
  arterial = "#d7301f",
  collector = "#fc8d59",
  residential = "#74add1",
  park_water = "#2c7fb8",
  no_feature = "#636363",
  mixed = "#41ab5d"
)

pairs_for_map <- unique(c(
  "2_43",
  seg1320 %>% st_drop_geometry() %>% count(ward_pair_id, sort = TRUE) %>% pull(ward_pair_id) %>% head(4)
))

for (pid in pairs_for_map) {
  si <- seg1320 %>% filter(ward_pair_id == pid, era == pair_era)
  if (nrow(si) == 0) next
  bb <- expand_bbox(si, pad_ft = 1400)
  p <- ggplot() +
    geom_sf(data = wards_by_era[[pair_era]], fill = NA, color = "grey85", linewidth = 0.2) +
    geom_sf(data = si, aes(color = segment_type), linewidth = 1.1) +
    scale_color_manual(values = segment_type_cols, drop = FALSE) +
    coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"])) +
    theme_minimal() +
    labs(
      title = paste0("Phase 4 Segment Classification: pair ", gsub("_", "-", pid), " (", pair_era, ")"),
      color = "segment_type",
      x = "Easting (ft)",
      y = "Northing (ft)"
    )
  make_plot(
    p,
    file.path(diagnostics_dir, paste0("phase4_segment_type_map_pair_", safe_path_label(pid), ".png")),
    width = 8.2,
    height = 8
  )
}

assert_gate(
  all(!is.na(seg1320$segment_type) & seg1320$segment_type != ""),
  "PHASE 4 GATE FAILED: missing segment classifications."
)
assert_gate(
  n_distinct(seg1320$segment_type) >= 2,
  "PHASE 4 GATE FAILED: implausible classification distribution (single class only)."
)
log_line("- PHASE 4 GATE PASSED")

log_line("")
log_line("## Phase 5: Build assignment lookup infrastructure")

write_segments_gpkg <- function(seg_sf, out_path) {
  eras <- unique(seg_sf$era)
  if (file.exists(out_path)) file.remove(out_path)
  append_mode <- FALSE
  for (e in eras) {
    se <- seg_sf %>% filter(era == e)
    layer_name <- safe_path_label(e)
    st_write(se, out_path, layer = layer_name, append = append_mode, quiet = TRUE)
    append_mode <- TRUE

    for (bw in c(250, 500, 1000)) {
      sb <- st_buffer(se, bw)
      sb$buffer_ft <- bw
      st_write(sb, out_path, layer = paste0(layer_name, "_bw", bw), append = TRUE, quiet = TRUE)
    }
  }
}

segment_target_names <- names(segments_by_target)
segment_gpkg_files <- character(length(segment_target_names))
for (i in seq_along(segment_target_names)) {
  nm <- segment_target_names[i]
  out_file <- paste0("boundary_segments_", nm, "ft.gpkg")
  write_segments_gpkg(segments_by_target[[nm]], file.path(output_dir, out_file))
  segment_gpkg_files[i] <- out_file
}

segment_meta <- bind_rows(lapply(names(segments_by_target), function(nm) {
  segments_by_target[[nm]] %>%
    st_drop_geometry() %>%
    mutate(target_length_ft = as.integer(nm))
}))
write_csv(segment_meta, file.path(output_dir, "segment_classification.csv"))

source("assign_to_segment.R")

test_points <- tibble::tibble(
  point_id = c("lincoln_park_near_2_43", "hyde_park", "far_point"),
  lon = c(-87.6515, -87.5955, -87.8200),
  lat = c(41.9250, 41.7930, 41.6200)
)

assignment_target <- if ("1320" %in% names(segments_by_target)) "1320" else names(segments_by_target)[1]
assignment_gpkg <- paste0("boundary_segments_", assignment_target, "ft.gpkg")
test_era <- if ("2015_2023" %in% unique(segments_by_target[[assignment_target]]$era)) "2015_2023" else unique(segments_by_target[[assignment_target]]$era)[1]
test_assign <- assign_to_segment(
  points = test_points,
  segments_gpkg = file.path(output_dir, assignment_gpkg),
  ward_panel_gpkg = ward_panel_path,
  era = test_era,
  lon_col = "lon",
  lat_col = "lat",
  point_crs = 4326
)
write_csv(st_drop_geometry(test_assign), file.path(diagnostics_dir, "phase5_assignment_test_points.csv"))

assert_gate(nrow(test_assign) == 3, "PHASE 5 GATE FAILED: assignment test did not return all test points.")
assert_gate(all(!is.na(test_assign$nearest_segment_id)), "PHASE 5 GATE FAILED: nearest segment assignment failed for test points.")
log_line("- PHASE 5 GATE PASSED")

log_line("")
log_line("## Copy outputs to data_raw/boundaries/segments")

primary_outputs <- c(
  "ward_pair_boundaries.gpkg",
  segment_gpkg_files,
  "segment_classification.csv"
)

for (f in primary_outputs) {
  src <- file.path(output_dir, f)
  dst <- file.path(export_dir, f)
  ok <- file.copy(src, dst, overwrite = TRUE)
  if (!ok) stop("Failed to copy output to export dir: ", f, call. = FALSE)
}

assign_src <- file.path(getwd(), "assign_to_segment.R")
assign_out <- file.path(output_dir, "assign_to_segment.R")
assign_exp <- file.path(export_dir, "assign_to_segment.R")
file.copy(assign_src, assign_out, overwrite = TRUE)
file.copy(assign_src, assign_exp, overwrite = TRUE)

diag_export <- file.path(export_dir, "diagnostics")
dir.create(diag_export, recursive = TRUE, showWarnings = FALSE)
diag_files <- list.files(diagnostics_dir, full.names = TRUE)
if (length(diag_files) > 0) {
  file.copy(diag_files, diag_export, overwrite = TRUE)
}

log_line("- Primary outputs copied to: ", normalizePath(export_dir))
log_line("- Diagnostics copied to: ", normalizePath(diag_export))
log_line("")
log_line("## Final checkpoint summary")
log_line("- [x] Wards per era = 50")
log_line("- [x] Ward geometries valid and EPSG:3435")
log_line("- [x] Shared boundaries extracted as lines")
log_line("- [x] Segment tiling checks passed")
log_line("- [x] Segment classification completed")
log_line("- [x] Buffer layers (250/500/1000 ft) written")
log_line("- [x] Assignment function smoke-tested")
finalize_diag()
