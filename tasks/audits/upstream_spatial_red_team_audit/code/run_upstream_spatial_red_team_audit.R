source("../../../setup_environment/code/packages.R")

library(arrow)
library(data.table)
library(dplyr)
library(fs)
library(purrr)
library(readr)
library(sf)
library(stringr)
library(tibble)
library(tidyr)
library(withr)

# Interactive run:
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/upstream_spatial_red_team_audit/code")
# source("run_upstream_spatial_red_team_audit.R")

repo_root <- normalizePath("../../../..", mustWork = TRUE)
task_root <- normalizePath("..", mustWork = TRUE)
output_dir <- file.path(task_root, "output")
assignment_issue_dir <- file.path(output_dir, "assignment_issue_tables")
distance_issue_dir <- file.path(output_dir, "distance_issue_tables")
dir_create(output_dir)
dir_create(assignment_issue_dir)
dir_create(distance_issue_dir)

seed_value <- 20260330L
set.seed(seed_value)

raw_cleaning_path <- file.path(output_dir, "raw_cleaning_integrity.csv")
topology_checks_path <- file.path(output_dir, "spatial_topology_checks.csv")
assignment_summary_path <- file.path(output_dir, "assignment_consistency_summary.csv")
distance_summary_path <- file.path(output_dir, "distance_consistency_summary.csv")
sample_ladder_path <- file.path(output_dir, "branch_sample_ladders.csv")
invariance_checks_path <- file.path(output_dir, "invariance_checks.csv")
manual_queue_path <- file.path(output_dir, "manual_spotcheck_queue.csv")
manual_layers_path <- file.path(output_dir, "manual_spotcheck_layers.gpkg")
memo_path <- file.path(output_dir, "upstream_spatial_red_team_memo.md")
propagation_queue_path <- file.path(output_dir, "fix_candidate_propagation_queue.csv")
branch_verdicts_path <- file.path(output_dir, "upstream_branch_verdicts.csv")

crs_projected <- 3435
chicago_lat_min <- 41
chicago_lat_max <- 43
chicago_lon_min <- -89
chicago_lon_max <- -87

era_from_date <- function(date_value) {
  date_value <- as.Date(date_value)
  case_when(
    date_value < as.Date("2003-05-01") ~ "1998_2002",
    date_value < as.Date("2015-05-18") ~ "2003_2014",
    date_value < as.Date("2023-05-15") ~ "2015_2023",
    TRUE ~ "post_2023"
  )
}

normalize_pair_key <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  pieces <- str_split_fixed(str_replace_all(x, "-", "_"), "_", 2)
  ward_a <- suppressWarnings(as.integer(pieces[, 1]))
  ward_b <- suppressWarnings(as.integer(pieces[, 2]))
  out <- ifelse(
    is.na(ward_a) | is.na(ward_b),
    NA_character_,
    paste(pmin(ward_a, ward_b), pmax(ward_a, ward_b), sep = "_")
  )
  out
}

representative_year <- function(era_value) {
  c(
    "1998_2002" = 1998L,
    "2003_2014" = 2003L,
    "2015_2023" = 2015L,
    "post_2023" = 2024L
  )[[era_value]]
}

null_coalesce <- function(x, y) {
  if (is.null(x)) y else x
}

bbox_failures <- function(lat, lon) {
  sum(
    is.finite(lat) & is.finite(lon) &
      (lat < chicago_lat_min | lat > chicago_lat_max | lon < chicago_lon_min | lon > chicago_lon_max),
    na.rm = TRUE
  )
}

make_issue_row <- function(branch, source, check, value, detail) {
  tibble(branch = branch, source = source, check = check, value = as.character(value), detail = detail)
}

load_ward_maps_independent <- function(ward_panel_path) {
  ward_panel <- st_read(ward_panel_path, quiet = TRUE) %>%
    mutate(year = as.integer(year), ward = as.integer(ward)) %>%
    st_transform(crs_projected)

  list(
    "1998_2002" = ward_panel %>% filter(year == 1998) %>% distinct(ward, .keep_all = TRUE),
    "2003_2014" = ward_panel %>% filter(year == 2003) %>% distinct(ward, .keep_all = TRUE),
    "2015_2023" = ward_panel %>% filter(year == 2015) %>% distinct(ward, .keep_all = TRUE),
    "post_2023" = ward_panel %>% filter(year == 2024) %>% distinct(ward, .keep_all = TRUE)
  )
}

load_layers_named <- function(gpkg_path, filter_fun = NULL) {
  layer_info <- st_layers(gpkg_path)$name
  if (!is.null(filter_fun)) {
    layer_info <- layer_info[filter_fun(layer_info)]
  }
  set_names(
    map(layer_info, ~ st_read(gpkg_path, layer = .x, quiet = TRUE) %>% st_transform(crs_projected)),
    layer_info
  )
}

load_blocks_2010 <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    rename(geometry = the_geom, block_id = GEOID10) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(crs_projected) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)
}

load_blocks_2020 <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    rename(geometry = the_geom, block_id = GEOID20) %>%
    st_as_sf(wkt = "geometry", crs = 4269) %>%
    st_transform(crs_projected) %>%
    mutate(block_id = as.character(block_id)) %>%
    distinct(block_id, .keep_all = TRUE)
}

pairwise_overlap_summary <- function(polygons_sf) {
  n <- nrow(polygons_sf)
  if (n <= 1) {
    return(tibble(n_overlap_pairs = 0L, overlap_area_sqft = 0))
  }

  overlap_areas <- c()
  for (i in seq_len(n - 1)) {
    hits <- st_intersects(polygons_sf[i, ], polygons_sf[(i + 1):n, ], sparse = TRUE)[[1]]
    if (length(hits) == 0) {
      next
    }
    for (j_hit in hits) {
      j <- i + j_hit
      overlap_geom <- suppressWarnings(st_intersection(polygons_sf[i, ], polygons_sf[j, ]))
      if (nrow(overlap_geom) == 0) {
        next
      }
      overlap_area <- as.numeric(sum(st_area(overlap_geom), na.rm = TRUE))
      if (is.finite(overlap_area) && overlap_area > 0) {
        overlap_areas <- c(overlap_areas, overlap_area)
      }
    }
  }

  tibble(
    n_overlap_pairs = length(overlap_areas),
    overlap_area_sqft = sum(overlap_areas)
  )
}

assign_points_independent <- function(points_sf, date_values, ward_maps, boundary_layers, chunk_n = 5000L) {
  if (nrow(points_sf) == 0) {
    return(tibble())
  }

  points_sf <- points_sf %>%
    mutate(
      .audit_row = seq_len(n()),
      .audit_era = era_from_date(date_values)
    )

  out_parts <- vector("list", length(unique(points_sf$.audit_era)))
  eras <- unique(points_sf$.audit_era)

  for (e in seq_along(eras)) {
    era_value <- eras[[e]]
    pts_era <- points_sf %>% filter(.audit_era == era_value)
    ward_map <- ward_maps[[era_value]]
    boundary_lines <- boundary_layers[[era_value]]
    if (nrow(pts_era) == 0 || is.null(ward_map) || is.null(boundary_lines)) {
      next
    }

    chunks <- split(seq_len(nrow(pts_era)), ceiling(seq_len(nrow(pts_era)) / chunk_n))
    chunk_out <- vector("list", length(chunks))

    for (k in seq_along(chunks)) {
      idx <- chunks[[k]]
      pts_chunk <- pts_era[idx, ] %>% st_transform(st_crs(ward_map))

      within_list <- st_within(pts_chunk, ward_map)
      within_n <- lengths(within_list)
      largest_join <- st_join(
        pts_chunk %>% select(.audit_row),
        ward_map %>% select(ward),
        join = st_intersects,
        left = TRUE,
        largest = TRUE
      ) %>%
        st_drop_geometry() %>%
        rename(ward_largest = ward)

      ward_exact <- map_int(within_list, function(idx_ward) {
        if (length(idx_ward) == 1) ward_map$ward[idx_ward] else NA_integer_
      })

      pts_chunk_tbl <- pts_chunk %>%
        st_drop_geometry() %>%
        select(.audit_row) %>%
        mutate(
          independent_ward = case_when(
            within_n == 1 ~ ward_exact,
            TRUE ~ largest_join$ward_largest
          ),
          ward_assignment_reason = case_when(
            within_n == 1 ~ "exact_within_unique",
            within_n > 1 & !is.na(largest_join$ward_largest) ~ "multiple_within_largest_fallback",
            within_n == 0 & !is.na(largest_join$ward_largest) ~ "no_within_largest_fallback",
            TRUE ~ "unassigned"
          ),
          ward_match_count = within_n
        )

      independent_pair <- rep(NA_character_, nrow(pts_chunk))
      independent_partner <- rep(NA_integer_, nrow(pts_chunk))
      independent_dist <- rep(NA_real_, nrow(pts_chunk))
      boundary_reason <- rep("no_assigned_ward", nrow(pts_chunk))

      assigned_wards <- unique(pts_chunk_tbl$independent_ward[!is.na(pts_chunk_tbl$independent_ward)])
      for (ward_value in assigned_wards) {
        ward_idx <- which(pts_chunk_tbl$independent_ward == ward_value)
        pts_ward <- pts_chunk[ward_idx, ]
        candidate_lines <- boundary_lines %>%
          filter(ward_a == ward_value | ward_b == ward_value)

        if (nrow(candidate_lines) == 0) {
          boundary_reason[ward_idx] <- "no_boundary_for_assigned_ward"
          next
        }

        nearest_idx <- st_nearest_feature(pts_ward, candidate_lines)
        nearest_lines <- candidate_lines[nearest_idx, ]
        independent_pair[ward_idx] <- nearest_lines$ward_pair_id
        independent_partner[ward_idx] <- if_else(
          nearest_lines$ward_a == ward_value,
          nearest_lines$ward_b,
          nearest_lines$ward_a
        )
        independent_dist[ward_idx] <- as.numeric(st_distance(pts_ward, nearest_lines, by_element = TRUE))
        boundary_reason[ward_idx] <- "nearest_boundary_for_assigned_ward"
      }

      chunk_out[[k]] <- pts_chunk_tbl %>%
        mutate(
          independent_pair = independent_pair,
          independent_partner_ward = independent_partner,
          independent_dist_ft = independent_dist,
          boundary_assignment_reason = boundary_reason
        )
    }

    out_parts[[e]] <- bind_rows(chunk_out)
  }

  bind_rows(out_parts)
}

assign_segment_independent <- function(points_sf, pair_values, era_values, segment_layers, chunk_n = 5000L) {
  if (nrow(points_sf) == 0) {
    return(tibble())
  }

  points_sf <- points_sf %>%
    mutate(
      .audit_row = seq_len(n()),
      .audit_pair = normalize_pair_key(pair_values),
      .audit_era = as.character(era_values)
    )

  out_parts <- list()
  eras <- unique(points_sf$.audit_era)

  for (era_value in eras) {
    layer_name <- paste0(era_value, "_bw250m")
    segment_layer <- segment_layers[[layer_name]]
    if (is.null(segment_layer)) {
      next
    }

    pts_era <- points_sf %>% filter(.audit_era == era_value, !is.na(.audit_pair), .audit_pair != "")
    if (nrow(pts_era) == 0) {
      next
    }

    chunks <- split(seq_len(nrow(pts_era)), ceiling(seq_len(nrow(pts_era)) / chunk_n))
    chunk_out <- vector("list", length(chunks))

    for (k in seq_along(chunks)) {
      idx <- chunks[[k]]
      pts_chunk <- pts_era[idx, ] %>% st_transform(st_crs(segment_layer))
      seg_join <- st_join(
        pts_chunk %>% select(.audit_row, .audit_pair),
        segment_layer %>% select(segment_id, ward_pair_id),
        join = st_within,
        left = TRUE
      ) %>%
        st_drop_geometry() %>%
        mutate(ward_pair_key = normalize_pair_key(ward_pair_id))

      seg_summary <- seg_join %>%
        mutate(pair_match = ward_pair_key == .audit_pair) %>%
        group_by(.audit_row) %>%
        summarise(
          independent_segment_id = if (any(pair_match, na.rm = TRUE)) first(segment_id[which(pair_match)]) else NA_character_,
          segment_match_count = sum(!is.na(segment_id)),
          pair_consistent_match_count = sum(pair_match, na.rm = TRUE),
          segment_assignment_reason = case_when(
            pair_consistent_match_count == 1 ~ "exact_pair_buffer_unique",
            pair_consistent_match_count > 1 ~ "multiple_pair_buffers",
            segment_match_count >= 1 ~ "segment_buffer_pair_mismatch",
            TRUE ~ "unassigned"
          ),
          .groups = "drop"
        )

      chunk_out[[k]] <- seg_summary
    }

    out_parts[[era_value]] <- bind_rows(chunk_out)
  }

  bind_rows(out_parts)
}

assign_blocks_independent <- function(points_sf, blocks_sf, block_id_col = "block_id", chunk_n = 50000L) {
  if (nrow(points_sf) == 0) {
    return(tibble())
  }

  points_sf <- points_sf %>% mutate(.audit_row = seq_len(n()))
  chunks <- split(seq_len(nrow(points_sf)), ceiling(seq_len(nrow(points_sf)) / chunk_n))
  out <- vector("list", length(chunks))

  for (k in seq_along(chunks)) {
    pts_chunk <- points_sf[chunks[[k]], ] %>% st_transform(st_crs(blocks_sf))
    join_tbl <- st_join(
      pts_chunk %>% select(.audit_row),
      blocks_sf %>% select(all_of(block_id_col)),
      join = st_within,
      left = TRUE
    ) %>%
      st_drop_geometry() %>%
      rename(independent_block_id = all_of(block_id_col)) %>%
      group_by(.audit_row) %>%
      summarise(
        independent_block_id = first(independent_block_id),
        block_match_count = sum(!is.na(independent_block_id)),
        block_assignment_reason = case_when(
          block_match_count == 1 ~ "exact_within_unique",
          block_match_count > 1 ~ "multiple_blocks",
          TRUE ~ "unassigned"
        ),
        .groups = "drop"
      )
    out[[k]] <- join_tbl
  }

  bind_rows(out)
}

majority_area_assign_blocks <- function(blocks_sf, ward_map_sf) {
  intersections <- suppressWarnings(st_intersection(
    blocks_sf %>% select(block_id),
    ward_map_sf %>% select(ward)
  ))

  if (nrow(intersections) == 0) {
    return(tibble())
  }

  intersections %>%
    mutate(intersection_area_sqft = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(block_id) %>%
    summarise(
      independent_ward = ward[which.max(intersection_area_sqft)],
      independent_ward_share = max(intersection_area_sqft) / sum(intersection_area_sqft),
      independent_n_wards = n_distinct(ward),
      .groups = "drop"
    )
}

signed_distance_check <- function(df, dist_col, signed_col, ward_col, pair_col) {
  work <- df %>%
    filter(
      !is.na(.data[[dist_col]]),
      !is.na(.data[[signed_col]]),
      !is.na(.data[[ward_col]]),
      !is.na(.data[[pair_col]])
    ) %>%
    mutate(
      abs_consistent = abs(abs(.data[[signed_col]]) - .data[[dist_col]]) <= 1e-6,
      ward_char = as.character(.data[[ward_col]]),
      pair_key = normalize_pair_key(.data[[pair_col]]),
      pair_first = str_split_fixed(pair_key, "_", 2)[, 1],
      pair_second = str_split_fixed(pair_key, "_", 2)[, 2],
      sign_value = sign(.data[[signed_col]]),
      orientation_key = case_when(
        ward_char == pair_first ~ paste0(pair_key, "_first"),
        ward_char == pair_second ~ paste0(pair_key, "_second"),
        TRUE ~ paste0(pair_key, "_other")
      )
    )

  orientation_summary <- work %>%
    count(orientation_key, sign_value) %>%
    group_by(orientation_key) %>%
    summarise(
      dominant_sign = sign_value[which.max(n)],
      n_total = sum(n),
      n_off_sign = n_total - max(n),
      .groups = "drop"
    )

  tibble(
    n_rows = nrow(work),
    n_abs_inconsistent = sum(!work$abs_consistent),
    n_orientation_inconsistent = sum(orientation_summary$n_off_sign)
  )
}

build_manual_spotcheck <- function(branch_name, ordinary_tbl, boundary_tbl, ambiguous_tbl, excluded_tbl, id_col, lon_col, lat_col) {
  take_rows <- function(tbl, stratum_name, n_take = 5L) {
    if (nrow(tbl) == 0) {
      return(tibble())
    }
    tbl %>%
      slice_sample(n = min(n_take, nrow(.))) %>%
      mutate(stratum = stratum_name)
  }

  bind_rows(
    take_rows(ordinary_tbl, "ordinary_matched"),
    take_rows(boundary_tbl, "boundary_adjacent"),
    take_rows(ambiguous_tbl, "fallback_or_ambiguous"),
    take_rows(excluded_tbl, "excluded_or_unmatched")
  ) %>%
    mutate(
      branch = branch_name,
      source_id = as.character(.data[[id_col]]),
      longitude = as.numeric(.data[[lon_col]]),
      latitude = as.numeric(.data[[lat_col]])
    )
}

write_issue_table <- function(df, path) {
  if (nrow(df) == 0) {
    write_csv(tibble(note = "no issues"), path)
  } else {
    write_csv(df, path)
  }
}

city_boundary <- st_read(file.path(repo_root, "data_raw/Boundaries_-_City_20250920.geojson"), quiet = TRUE) %>%
  st_transform(crs_projected)
ward_maps <- load_ward_maps_independent(file.path(repo_root, "tasks/ward_panel_create/output/ward_panel.gpkg"))
boundary_layers <- load_layers_named(
  file.path(repo_root, "tasks/border_segment_creation/output/ward_pair_boundaries.gpkg")
)
segment_layers <- load_layers_named(
  file.path(repo_root, "tasks/border_segment_creation/output/boundary_segments_1320ft.gpkg"),
  filter_fun = function(layer_names) str_detect(layer_names, "_bw250m$")
)
blocks_2010 <- load_blocks_2010(file.path(repo_root, "data_raw/CensusBlockTIGER2010_20250721.csv"))
blocks_2020 <- load_blocks_2020(file.path(repo_root, "tasks/census_block_2020_cleaning/output/census_blocks_2020.csv"))

raw_integrity_rows <- list()
topology_rows <- list()
assignment_summary_rows <- list()
distance_summary_rows <- list()
sample_ladder_rows <- list()
invariance_rows <- list()
manual_queue_parts <- list()
propagation_rows <- list()

# ------------------------------------------------------------------------------
# Raw cleaning integrity
# ------------------------------------------------------------------------------
permits_raw <- read_csv(file.path(repo_root, "tasks/download_building_permits/output/building_permits.csv"), show_col_types = FALSE) %>%
  rename_with(tolower)
permits_clean <- st_read(
  file.path(repo_root, "tasks/clean_building_permits/output/building_permits_clean.gpkg"),
  query = paste(
    "SELECT id, pin, ward, permit_status, permit_issued, high_discretion,",
    "application_start_date_ym, issue_date_ym, latitude, longitude",
    "FROM building_permits_clean"
  ),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  mutate(id = as.character(id))

raw_integrity_rows[[length(raw_integrity_rows) + 1]] <- bind_rows(
  make_issue_row("permits", "raw", "raw_rows", nrow(permits_raw), "Total raw Chicago permit records."),
  make_issue_row("permits", "clean", "clean_rows", nrow(permits_clean), "Rows in current cleaned permit output."),
  make_issue_row("permits", "raw", "raw_missing_lat_lon", sum(is.na(permits_raw$latitude) | is.na(permits_raw$longitude)), "Rows missing latitude or longitude before cleaning."),
  make_issue_row("permits", "raw", "raw_xy_only_candidates", sum((is.na(permits_raw$latitude) | is.na(permits_raw$longitude)) & is.finite(permits_raw$xcoordinate) & is.finite(permits_raw$ycoordinate)), "Rows that rely on projected x/y fallback conversion."),
  make_issue_row("permits", "clean", "clean_duplicate_id", sum(duplicated(permits_clean$id)), "Duplicate IDs in cleaned permits."),
  make_issue_row("permits", "clean", "clean_missing_pin", sum(is.na(permits_clean$pin) | permits_clean$pin == ""), "Missing PINs in cleaned permits."),
  make_issue_row("permits", "clean", "clean_outside_bbox", bbox_failures(permits_clean$latitude, permits_clean$longitude), "Cleaned permits outside broad Chicago bbox.")
)

parcels_geo <- st_read(file.path(repo_root, "tasks/geocode_ccao_data/output/geocoded_residential_data.gpkg"), quiet = TRUE) %>%
  st_transform(crs_projected) %>%
  mutate(
    longitude = st_coordinates(st_transform(., 4326))[, 1],
    latitude = st_coordinates(st_transform(., 4326))[, 2]
  )

raw_integrity_rows[[length(raw_integrity_rows) + 1]] <- bind_rows(
  make_issue_row("density", "geocoded_parcels", "geocoded_rows", nrow(parcels_geo), "Rows in geocoded parcel output."),
  make_issue_row("density", "geocoded_parcels", "duplicate_pin", sum(duplicated(parcels_geo$pin)), "Duplicate PINs in geocoded parcels."),
  make_issue_row("density", "geocoded_parcels", "missing_yearbuilt", sum(is.na(parcels_geo$yearbuilt)), "Geocoded parcels missing yearbuilt."),
  make_issue_row("density", "geocoded_parcels", "outside_bbox", bbox_failures(parcels_geo$latitude, parcels_geo$longitude), "Geocoded parcels outside broad Chicago bbox."),
  make_issue_row("density", "geocoded_parcels", "outside_city_boundary", sum(!(lengths(st_within(parcels_geo, city_boundary)) > 0)), "Geocoded parcels outside Chicago city boundary.")
)

sales_raw_n <- fread(file.path(repo_root, "tasks/download_parcel_sales_data/output/parcel_sales_city.csv"), select = "pin")[, .N]
sales_scores <- read_csv(file.path(repo_root, "tasks/calculate_sale_distances/output/sales_pre_scores.csv"), show_col_types = FALSE) %>%
  mutate(pin = as.character(pin))
sales_coord_lookup <- sales_scores %>%
  distinct(pin, sale_date, .keep_all = TRUE) %>%
  select(pin, sale_date, longitude, latitude)
raw_integrity_rows[[length(raw_integrity_rows) + 1]] <- bind_rows(
  make_issue_row("home_sales", "raw_sales", "raw_rows", sales_raw_n, "Total raw assessor sales records."),
  make_issue_row("home_sales", "sales_pre_scores", "scored_rows", nrow(sales_scores), "Rows in scored sales output."),
  make_issue_row("home_sales", "sales_pre_scores", "duplicate_pin_sale_date", sales_scores %>% summarise(n = n() - n_distinct(pin, sale_date)) %>% pull(n), "Duplicate PIN x sale_date rows in scored sales output."),
  make_issue_row("home_sales", "sales_pre_scores", "missing_coordinates", sum(is.na(sales_scores$latitude) | is.na(sales_scores$longitude)), "Missing coordinates in scored sales output."),
  make_issue_row("home_sales", "sales_pre_scores", "outside_bbox", bbox_failures(sales_scores$latitude, sales_scores$longitude), "Scored sales outside broad Chicago bbox.")
)

rental_raw <- open_dataset(file.path(repo_root, "tasks/process_rent_data/output/chicago_rent_panel.parquet"))
rental_raw_n <- rental_raw %>% summarise(n = n()) %>% collect() %>% pull(n)
rent_pre_scores <- read_parquet(
  file.path(repo_root, "tasks/calculate_rent_distances/output/rent_pre_scores_full.parquet"),
  col_select = c("id", "file_date", "ward", "ward_pair_id", "dist_ft", "latitude", "longitude")
)
rent_coord_lookup <- rent_pre_scores %>%
  distinct(id, file_date, .keep_all = TRUE) %>%
  select(id, file_date, longitude, latitude)
raw_integrity_rows[[length(raw_integrity_rows) + 1]] <- bind_rows(
  make_issue_row("rentals", "raw_rent_panel", "raw_rows", rental_raw_n, "Rows in processed rental panel before ward-distance assignment."),
  make_issue_row("rentals", "rent_pre_scores_full", "scored_rows", nrow(rent_pre_scores), "Rows in rental ward-distance pre-score output."),
  make_issue_row("rentals", "rent_pre_scores_full", "duplicate_id_file_date", rent_pre_scores %>% summarise(n = n() - n_distinct(id, file_date)) %>% pull(n), "Duplicate listing id x file_date rows in rental pre-scores."),
  make_issue_row("rentals", "rent_pre_scores_full", "missing_coordinates", sum(is.na(rent_pre_scores$latitude) | is.na(rent_pre_scores$longitude)), "Missing coordinates in rental pre-scores."),
  make_issue_row("rentals", "rent_pre_scores_full", "outside_bbox", bbox_failures(rent_pre_scores$latitude, rent_pre_scores$longitude), "Rental pre-scores outside broad Chicago bbox.")
)

raw_cleaning_integrity <- bind_rows(raw_integrity_rows)
write_csv(raw_cleaning_integrity, raw_cleaning_path)

# ------------------------------------------------------------------------------
# Spatial topology checks
# ------------------------------------------------------------------------------
for (era_value in names(ward_maps)) {
  ward_map <- ward_maps[[era_value]]
  overlap_summary <- pairwise_overlap_summary(ward_map)
  ward_union <- st_union(ward_map)
  gap_area <- as.numeric(sum(st_area(st_difference(st_union(city_boundary), st_intersection(st_union(city_boundary), ward_union))), na.rm = TRUE))
  topology_rows[[length(topology_rows) + 1]] <- tibble(
    object = "ward_map",
    era = era_value,
    n_features = nrow(ward_map),
    n_duplicate_ids = sum(duplicated(ward_map$ward)),
    n_invalid_geom = sum(!st_is_valid(ward_map)),
    n_empty_geom = sum(st_is_empty(ward_map)),
    n_overlap_pairs = overlap_summary$n_overlap_pairs,
    overlap_area_sqft = overlap_summary$overlap_area_sqft,
    uncovered_city_area_sqft = gap_area,
    n_zero_length = NA_integer_,
    detail = "Representative ward-map topology by era."
  )

  boundary_layer <- boundary_layers[[era_value]]
  topology_rows[[length(topology_rows) + 1]] <- tibble(
    object = "ward_pair_boundaries",
    era = era_value,
    n_features = nrow(boundary_layer),
    n_duplicate_ids = sum(duplicated(boundary_layer$ward_pair_id)),
    n_invalid_geom = sum(!st_is_valid(boundary_layer)),
    n_empty_geom = sum(st_is_empty(boundary_layer)),
    n_overlap_pairs = NA_integer_,
    overlap_area_sqft = NA_real_,
    uncovered_city_area_sqft = NA_real_,
    n_zero_length = sum(as.numeric(st_length(boundary_layer)) <= 0, na.rm = TRUE),
    detail = "Canonical ward-pair boundary lines by era."
  )

  segment_layer <- segment_layers[[paste0(era_value, "_bw250m")]]
  topology_rows[[length(topology_rows) + 1]] <- tibble(
    object = "boundary_segments_bw250m",
    era = era_value,
    n_features = nrow(segment_layer),
    n_duplicate_ids = sum(duplicated(segment_layer$segment_id)),
    n_invalid_geom = sum(!st_is_valid(segment_layer)),
    n_empty_geom = sum(st_is_empty(segment_layer)),
    n_overlap_pairs = NA_integer_,
    overlap_area_sqft = NA_real_,
    uncovered_city_area_sqft = NA_real_,
    n_zero_length = sum(as.numeric(segment_layer$segment_length_ft) <= 0, na.rm = TRUE),
    detail = "Segment-buffer polygons used by segment-assignment consumers."
  )
}

block_treatment_pre <- read_csv(file.path(repo_root, "tasks/create_block_treatment_panel/output/block_treatment_pre_scores.csv"), show_col_types = FALSE)
topology_rows[[length(topology_rows) + 1]] <- tibble(
  object = "block_treatment_pre_scores",
  era = "cohort_mixed",
  n_features = nrow(block_treatment_pre),
  n_duplicate_ids = sum(duplicated(paste(block_treatment_pre$block_id, block_treatment_pre$cohort, sep = "_"))),
  n_invalid_geom = NA_integer_,
  n_empty_geom = NA_integer_,
  n_overlap_pairs = sum(block_treatment_pre$ward_origin_n_wards > 1 | block_treatment_pre$ward_dest_n_wards > 1, na.rm = TRUE),
  overlap_area_sqft = NA_real_,
  uncovered_city_area_sqft = NA_real_,
  n_zero_length = NA_integer_,
  detail = "Blocks spanning multiple ward assignments in treatment construction."
)

spatial_topology_checks <- bind_rows(topology_rows)
write_csv(spatial_topology_checks, topology_checks_path)

# ------------------------------------------------------------------------------
# Density branch: parcels / ward / pair / distance / segment
# ------------------------------------------------------------------------------
parcels_with_geom <- st_read(file.path(repo_root, "tasks/calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg"), quiet = TRUE) %>%
  st_transform(crs_projected) %>%
  mutate(pin = as.character(pin))
parcels_scores <- read_csv(file.path(repo_root, "tasks/merge_in_scores/output/parcels_with_ward_distances.csv"), show_col_types = FALSE) %>%
  mutate(pin = as.character(pin))
parcel_compare_input <- parcels_with_geom %>%
  left_join(
    parcels_scores %>% select(pin, segment_id, segment_reason, sign, signed_distance),
    by = "pin"
  )

parcel_independent <- assign_points_independent(
  points_sf = parcel_compare_input %>% select(pin, ward, ward_pair, dist_to_boundary, sign, signed_distance),
  date_values = parcel_compare_input$construction_date,
  ward_maps = ward_maps,
  boundary_layers = boundary_layers,
  chunk_n = 5000L
)

parcel_segment_independent <- assign_segment_independent(
  points_sf = parcel_compare_input,
  pair_values = parcel_compare_input$ward_pair,
  era_values = era_from_date(parcel_compare_input$construction_date),
  segment_layers = segment_layers,
  chunk_n = 5000L
)

parcel_compare <- parcel_compare_input %>%
  st_drop_geometry() %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(parcel_independent, by = ".audit_row") %>%
  left_join(parcel_segment_independent, by = ".audit_row") %>%
  mutate(
    ward_match = ward == independent_ward,
    pair_match = normalize_pair_key(ward_pair) == normalize_pair_key(independent_pair),
    segment_match = case_when(
      is.na(segment_id) & is.na(independent_segment_id) ~ TRUE,
      TRUE ~ segment_id == independent_segment_id
    ),
    abs_dist_diff_ft = abs(dist_to_boundary - independent_dist_ft)
  )

assignment_summary_rows[[length(assignment_summary_rows) + 1]] <- tibble(
  branch = "density",
  object = c("ward", "ward_pair", "segment"),
  comparison_basis = c("full_population", "full_population", "full_population"),
  n_compare = c(sum(!is.na(parcel_compare$ward)), sum(!is.na(parcel_compare$ward_pair)), sum(!is.na(parcel_compare$segment_id) | !is.na(parcel_compare$independent_segment_id))),
  n_issue = c(sum(!parcel_compare$ward_match, na.rm = TRUE), sum(!parcel_compare$pair_match, na.rm = TRUE), sum(!parcel_compare$segment_match, na.rm = TRUE)),
  pct_match = c(mean(parcel_compare$ward_match, na.rm = TRUE), mean(parcel_compare$pair_match, na.rm = TRUE), mean(parcel_compare$segment_match, na.rm = TRUE)),
  note = c("Producer ward vs independent ward.", "Producer ward-pair vs nearest boundary pair for assigned ward.", "Producer segment assignment vs independent segment-buffer join.")
)

distance_summary_rows[[length(distance_summary_rows) + 1]] <- tibble(
  branch = "density",
  comparison_basis = "full_population",
  n_compare = sum(is.finite(parcel_compare$dist_to_boundary) & is.finite(parcel_compare$independent_dist_ft)),
  mean_abs_diff_ft = mean(parcel_compare$abs_dist_diff_ft, na.rm = TRUE),
  max_abs_diff_ft = max(parcel_compare$abs_dist_diff_ft, na.rm = TRUE),
  n_large_diff_5ft = sum(parcel_compare$abs_dist_diff_ft > 5, na.rm = TRUE),
  n_large_diff_25ft = sum(parcel_compare$abs_dist_diff_ft > 25, na.rm = TRUE),
  n_sign_inconsistent = signed_distance_check(parcel_compare, "dist_to_boundary", "signed_distance", "ward", "ward_pair")$n_orientation_inconsistent,
  note = "Parcel boundary distances and sign orientation."
)

parcel_assignment_issues <- parcel_compare %>%
  filter(!ward_match | !pair_match | !segment_match | ward_assignment_reason != "exact_within_unique" | segment_assignment_reason != "exact_pair_buffer_unique") %>%
  select(pin, construction_year, ward, independent_ward, ward_pair, independent_pair, segment_id, independent_segment_id, ward_assignment_reason, segment_assignment_reason, ward_match_count, pair_match, segment_match)
write_issue_table(parcel_assignment_issues, file.path(assignment_issue_dir, "parcel_assignment_issues.csv"))

parcel_distance_issues <- parcel_compare %>%
  filter(abs_dist_diff_ft > 5 | abs(abs(signed_distance) - dist_to_boundary) > 1e-6) %>%
  transmute(pin, construction_year, ward_pair, dist_to_boundary, independent_dist_ft, abs_dist_diff_ft, signed_distance)
write_issue_table(parcel_distance_issues, file.path(distance_issue_dir, "parcel_distance_issues.csv"))

sample_ladder_rows[[length(sample_ladder_rows) + 1]] <- bind_rows(
  tibble(branch = "density", stage = "geocoded_parcels", n_rows = nrow(parcels_geo), unique_ids = n_distinct(parcels_geo$pin), detail = "Geocoded parcel output."),
  tibble(branch = "density", stage = "parcel_geometry_year_window", n_rows = nrow(parcels_with_geom), unique_ids = n_distinct(parcels_with_geom$pin), detail = "Parcels kept for boundary-distance construction."),
  tibble(branch = "density", stage = "merge_in_scores_output", n_rows = nrow(parcels_scores), unique_ids = n_distinct(parcels_scores$pin), detail = "Final parcel score output used by density branch."),
  tibble(branch = "density", stage = "main_fe_sample", n_rows = parcels_scores %>% filter(construction_year >= 2006, unitscount > 1, dist_to_boundary <= 500, density_far > 0) %>% nrow(), unique_ids = parcels_scores %>% filter(construction_year >= 2006, unitscount > 1, dist_to_boundary <= 500, density_far > 0) %>% summarise(n = n_distinct(pin)) %>% pull(n), detail = "Main FAR FE sample.")
)

# ------------------------------------------------------------------------------
# Sales branch
# ------------------------------------------------------------------------------
sales_sf <- st_as_sf(
  sales_scores %>% mutate(sale_date = as.Date(sale_date)),
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>% st_transform(crs_projected)

sales_independent <- assign_points_independent(
  points_sf = sales_sf %>% select(pin, sale_date, ward, ward_pair_id, dist_ft, sign, signed_dist),
  date_values = sales_scores$sale_date,
  ward_maps = ward_maps,
  boundary_layers = boundary_layers,
  chunk_n = 10000L
)

sales_compare <- sales_scores %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(sales_independent, by = ".audit_row") %>%
  mutate(
    ward_match = ward == independent_ward,
    pair_match = normalize_pair_key(ward_pair_id) == normalize_pair_key(independent_pair),
    abs_dist_diff_ft = abs(dist_ft - independent_dist_ft)
  )

sales_panel_2015 <- read_parquet(
  file.path(repo_root, "tasks/audits/create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet"),
  col_select = c("pin", "sale_date", "block_id", "segment_id_cohort", "ward_pair_id", "dist_ft", "relative_year")
) %>%
  mutate(sale_date = as.Date(sale_date))
sales_panel_2015_with_coords <- sales_panel_2015 %>%
  left_join(sales_coord_lookup, by = c("pin", "sale_date"))
sales_panel_2015_coord_issues <- sales_panel_2015_with_coords %>%
  filter(is.na(longitude) | is.na(latitude))

sales_panel_2015_sf <- st_as_sf(
  sales_panel_2015_with_coords %>%
    filter(!is.na(longitude), !is.na(latitude)),
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>% st_transform(crs_projected)

sales_block_independent <- assign_blocks_independent(sales_panel_2015_sf, blocks_2010, "block_id", chunk_n = 10000L)
sales_segment_independent <- assign_segment_independent(
  points_sf = sales_panel_2015_sf,
  pair_values = sales_panel_2015_with_coords %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    pull(ward_pair_id),
  era_values = rep("2015_2023", nrow(sales_panel_2015_sf)),
  segment_layers = segment_layers,
  chunk_n = 10000L
)

sales_panel_compare <- sales_panel_2015_with_coords %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(sales_block_independent, by = ".audit_row") %>%
  left_join(sales_segment_independent, by = ".audit_row") %>%
  mutate(
    block_match = block_id == independent_block_id,
    segment_match = case_when(
      is.na(segment_id_cohort) & is.na(independent_segment_id) ~ TRUE,
      TRUE ~ segment_id_cohort == independent_segment_id
    )
  )

assignment_summary_rows[[length(assignment_summary_rows) + 1]] <- tibble(
  branch = "home_sales",
  object = c("ward", "ward_pair", "block", "segment"),
  comparison_basis = c("full_scored_sales", "full_scored_sales", "sales_panel_2015_full", "sales_panel_2015_full"),
  n_compare = c(sum(!is.na(sales_compare$ward)), sum(!is.na(sales_compare$ward_pair_id)), nrow(sales_panel_compare), nrow(sales_panel_compare)),
  n_issue = c(sum(!sales_compare$ward_match, na.rm = TRUE), sum(!sales_compare$pair_match, na.rm = TRUE), sum(!sales_panel_compare$block_match, na.rm = TRUE), sum(!sales_panel_compare$segment_match, na.rm = TRUE)),
  pct_match = c(mean(sales_compare$ward_match, na.rm = TRUE), mean(sales_compare$pair_match, na.rm = TRUE), mean(sales_panel_compare$block_match, na.rm = TRUE), mean(sales_panel_compare$segment_match, na.rm = TRUE)),
  note = c("Scored sales ward assignment.", "Scored sales nearest ward-pair assignment.", "2015 sales panel block assignment.", "2015 sales panel segment assignment.")
)

distance_summary_rows[[length(distance_summary_rows) + 1]] <- tibble(
  branch = "home_sales",
  comparison_basis = "full_scored_sales",
  n_compare = sum(is.finite(sales_compare$dist_ft) & is.finite(sales_compare$independent_dist_ft)),
  mean_abs_diff_ft = mean(sales_compare$abs_dist_diff_ft, na.rm = TRUE),
  max_abs_diff_ft = max(sales_compare$abs_dist_diff_ft, na.rm = TRUE),
  n_large_diff_5ft = sum(sales_compare$abs_dist_diff_ft > 5, na.rm = TRUE),
  n_large_diff_25ft = sum(sales_compare$abs_dist_diff_ft > 25, na.rm = TRUE),
  n_sign_inconsistent = signed_distance_check(sales_compare, "dist_ft", "signed_dist", "ward", "ward_pair_id")$n_orientation_inconsistent,
  note = "Scored sales boundary distances and sign orientation."
)

write_issue_table(
  sales_compare %>%
    filter(!ward_match | !pair_match | ward_assignment_reason != "exact_within_unique") %>%
    select(pin, sale_date, ward, independent_ward, ward_pair_id, independent_pair, ward_assignment_reason, ward_match_count, pair_match),
  file.path(assignment_issue_dir, "sales_assignment_issues.csv")
)

write_issue_table(
  sales_panel_compare %>%
    filter(!block_match | !segment_match | block_assignment_reason != "exact_within_unique" | segment_assignment_reason != "exact_pair_buffer_unique") %>%
    select(pin, sale_date, block_id, independent_block_id, segment_id_cohort, independent_segment_id, block_assignment_reason, segment_assignment_reason),
  file.path(assignment_issue_dir, "sales_panel_assignment_issues.csv")
)

write_issue_table(
  sales_panel_2015_coord_issues %>%
    distinct(pin, sale_date, .keep_all = TRUE) %>%
    select(pin, sale_date, block_id, segment_id_cohort, ward_pair_id, dist_ft),
  file.path(assignment_issue_dir, "sales_panel_coordinate_join_issues.csv")
)

write_issue_table(
  sales_compare %>%
    filter(abs_dist_diff_ft > 5) %>%
    select(pin, sale_date, ward_pair_id, dist_ft, independent_dist_ft, abs_dist_diff_ft, signed_dist),
  file.path(distance_issue_dir, "sales_distance_issues.csv")
)

sample_ladder_rows[[length(sample_ladder_rows) + 1]] <- bind_rows(
  tibble(branch = "home_sales", stage = "raw_sales", n_rows = sales_raw_n, unique_ids = NA_integer_, detail = "Raw assessor sales rows."),
  tibble(branch = "home_sales", stage = "sales_pre_scores", n_rows = nrow(sales_scores), unique_ids = sales_scores %>% summarise(n = n_distinct(pin, sale_date)) %>% pull(n), detail = "Scored sales output."),
  tibble(branch = "home_sales", stage = "sales_panel_2015", n_rows = nrow(sales_panel_2015), unique_ids = sales_panel_2015 %>% summarise(n = n_distinct(pin, sale_date)) %>% pull(n), detail = "2015 home-sales panel."),
  tibble(branch = "home_sales", stage = "sales_panel_2015_missing_coordinate_backmerge", n_rows = nrow(sales_panel_2015_coord_issues), unique_ids = sales_panel_2015_coord_issues %>% summarise(n = n_distinct(pin, sale_date)) %>% pull(n), detail = "2015 panel rows that do not merge back to scored coordinates by PIN x sale_date."),
  tibble(branch = "home_sales", stage = "main_did_sample", n_rows = sales_panel_2015 %>% filter(dist_ft <= 1000, relative_year >= -5, relative_year <= 5) %>% nrow(), unique_ids = sales_panel_2015 %>% filter(dist_ft <= 1000, relative_year >= -5, relative_year <= 5) %>% summarise(n = n_distinct(pin, sale_date)) %>% pull(n), detail = "2015 DID analysis sample.")
)

# ------------------------------------------------------------------------------
# Rental branch (sample-based for heavy geospatial checks)
# ------------------------------------------------------------------------------
rent_pre_scores_dt <- as.data.table(rent_pre_scores)
rent_sample_ids <- rent_pre_scores_dt[, .(sample_key = .I)][sample(.N, min(100000L, .N))]
rental_sample <- rent_pre_scores_dt[rent_sample_ids$sample_key] %>% as_tibble()
rental_sample_sf <- st_as_sf(rental_sample, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_projected)

rental_independent <- assign_points_independent(
  points_sf = rental_sample_sf %>% select(id, file_date, ward, ward_pair_id, dist_ft),
  date_values = rental_sample$file_date,
  ward_maps = ward_maps,
  boundary_layers = boundary_layers,
  chunk_n = 10000L
)

rental_compare <- rental_sample %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(rental_independent, by = ".audit_row") %>%
  mutate(
    ward_match = ward == independent_ward,
    pair_match = normalize_pair_key(ward_pair_id) == normalize_pair_key(independent_pair),
    abs_dist_diff_ft = abs(dist_ft - independent_dist_ft)
  )

rental_panel_2023 <- read_parquet(
  file.path(repo_root, "tasks/audits/create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet"),
  col_select = c("id", "file_date", "block_id", "segment_id_cohort", "ward_pair_id", "dist_ft", "relative_year")
)
rental_panel_2023_sample <- rental_panel_2023 %>% slice_sample(n = min(100000L, nrow(rental_panel_2023)))
rental_panel_2023_sample_with_coords <- rental_panel_2023_sample %>%
  left_join(rent_coord_lookup, by = c("id", "file_date"))
rental_panel_2023_coord_issues <- rental_panel_2023_sample_with_coords %>%
  filter(is.na(longitude) | is.na(latitude))
rental_panel_2023_sample_sf <- st_as_sf(
  rental_panel_2023_sample_with_coords %>%
    filter(!is.na(longitude), !is.na(latitude)),
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>% st_transform(crs_projected)

rental_block_independent <- assign_blocks_independent(rental_panel_2023_sample_sf, blocks_2020, "block_id", chunk_n = 10000L)
rental_segment_independent <- assign_segment_independent(
  points_sf = rental_panel_2023_sample_sf,
  pair_values = rental_panel_2023_sample_with_coords %>%
    filter(!is.na(longitude), !is.na(latitude)) %>%
    pull(ward_pair_id),
  era_values = rep("post_2023", nrow(rental_panel_2023_sample_sf)),
  segment_layers = segment_layers,
  chunk_n = 10000L
)

rental_panel_compare <- rental_panel_2023_sample_with_coords %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(rental_block_independent, by = ".audit_row") %>%
  left_join(rental_segment_independent, by = ".audit_row") %>%
  mutate(
    block_match = block_id == independent_block_id,
    segment_match = case_when(
      is.na(segment_id_cohort) & is.na(independent_segment_id) ~ TRUE,
      TRUE ~ segment_id_cohort == independent_segment_id
    )
  )

assignment_summary_rows[[length(assignment_summary_rows) + 1]] <- tibble(
  branch = "rentals",
  object = c("ward", "ward_pair", "block", "segment"),
  comparison_basis = c("rent_pre_scores_sample_100k", "rent_pre_scores_sample_100k", "rental_panel_2023_sample_100k", "rental_panel_2023_sample_100k"),
  n_compare = c(nrow(rental_compare), nrow(rental_compare), nrow(rental_panel_compare), nrow(rental_panel_compare)),
  n_issue = c(sum(!rental_compare$ward_match, na.rm = TRUE), sum(!rental_compare$pair_match, na.rm = TRUE), sum(!rental_panel_compare$block_match, na.rm = TRUE), sum(!rental_panel_compare$segment_match, na.rm = TRUE)),
  pct_match = c(mean(rental_compare$ward_match, na.rm = TRUE), mean(rental_compare$pair_match, na.rm = TRUE), mean(rental_panel_compare$block_match, na.rm = TRUE), mean(rental_panel_compare$segment_match, na.rm = TRUE)),
  note = c("Rental ward assignment on deterministic 100k sample.", "Rental ward-pair assignment on deterministic 100k sample.", "Rental 2023 panel block assignment on deterministic 100k sample.", "Rental 2023 panel segment assignment on deterministic 100k sample.")
)

distance_summary_rows[[length(distance_summary_rows) + 1]] <- tibble(
  branch = "rentals",
  comparison_basis = "rent_pre_scores_sample_100k",
  n_compare = sum(is.finite(rental_compare$dist_ft) & is.finite(rental_compare$independent_dist_ft)),
  mean_abs_diff_ft = mean(rental_compare$abs_dist_diff_ft, na.rm = TRUE),
  max_abs_diff_ft = max(rental_compare$abs_dist_diff_ft, na.rm = TRUE),
  n_large_diff_5ft = sum(rental_compare$abs_dist_diff_ft > 5, na.rm = TRUE),
  n_large_diff_25ft = sum(rental_compare$abs_dist_diff_ft > 25, na.rm = TRUE),
  n_sign_inconsistent = NA_integer_,
  note = "Rental boundary distances on deterministic 100k sample."
)

write_issue_table(
  rental_compare %>%
    filter(!ward_match | !pair_match | ward_assignment_reason != "exact_within_unique") %>%
    select(id, file_date, ward, independent_ward, ward_pair_id, independent_pair, ward_assignment_reason, ward_match_count, pair_match),
  file.path(assignment_issue_dir, "rental_assignment_issues.csv")
)

write_issue_table(
  rental_panel_compare %>%
    filter(!block_match | !segment_match | block_assignment_reason != "exact_within_unique" | segment_assignment_reason != "exact_pair_buffer_unique") %>%
    select(id, file_date, block_id, independent_block_id, segment_id_cohort, independent_segment_id, block_assignment_reason, segment_assignment_reason),
  file.path(assignment_issue_dir, "rental_panel_assignment_issues.csv")
)

write_issue_table(
  rental_panel_2023_coord_issues %>%
    distinct(id, file_date, .keep_all = TRUE) %>%
    select(id, file_date, block_id, segment_id_cohort, ward_pair_id, dist_ft),
  file.path(assignment_issue_dir, "rental_panel_coordinate_join_issues.csv")
)

write_issue_table(
  rental_compare %>%
    filter(abs_dist_diff_ft > 5) %>%
    select(id, file_date, ward_pair_id, dist_ft, independent_dist_ft, abs_dist_diff_ft),
  file.path(distance_issue_dir, "rental_distance_issues.csv")
)

sample_ladder_rows[[length(sample_ladder_rows) + 1]] <- bind_rows(
  tibble(branch = "rentals", stage = "raw_rent_panel", n_rows = rental_raw_n, unique_ids = NA_integer_, detail = "Processed rent panel rows."),
  tibble(branch = "rentals", stage = "rent_pre_scores_full", n_rows = nrow(rent_pre_scores), unique_ids = rent_pre_scores %>% summarise(n = n_distinct(id, file_date)) %>% pull(n), detail = "Rental pre-score output."),
  tibble(branch = "rentals", stage = "rental_panel_2023", n_rows = nrow(rental_panel_2023), unique_ids = rental_panel_2023 %>% summarise(n = n_distinct(id, file_date)) %>% pull(n), detail = "2023 rental listing panel."),
  tibble(branch = "rentals", stage = "rental_panel_2023_sample_missing_coordinate_backmerge", n_rows = nrow(rental_panel_2023_coord_issues), unique_ids = rental_panel_2023_coord_issues %>% summarise(n = n_distinct(id, file_date)) %>% pull(n), detail = "Sampled 2023 rental panel rows that do not merge back to rental pre-score coordinates by listing id x file_date."),
  tibble(branch = "rentals", stage = "rental_main_sample", n_rows = rental_panel_2023 %>% filter(dist_ft <= 1000, relative_year >= -5, relative_year <= 5) %>% nrow(), unique_ids = rental_panel_2023 %>% filter(dist_ft <= 1000, relative_year >= -5, relative_year <= 5) %>% summarise(n = n_distinct(id, file_date)) %>% pull(n), detail = "2023 rental analysis sample.")
)

# ------------------------------------------------------------------------------
# Permit branch and block-treatment branch
# ------------------------------------------------------------------------------
permit_points <- st_as_sf(
  permits_clean %>%
    filter(permit_issued == 1) %>%
    mutate(issue_date = as.Date(issue_date_ym)),
  coords = c("longitude", "latitude"),
  crs = 4326,
  remove = FALSE
) %>% st_transform(crs_projected)

permit_points_sample <- permit_points %>% slice_sample(n = min(100000L, nrow(.)))
permit_independent_ward <- assign_points_independent(
  points_sf = permit_points_sample %>% select(id, ward),
  date_values = permit_points_sample$issue_date,
  ward_maps = ward_maps,
  boundary_layers = boundary_layers,
  chunk_n = 10000L
)

permit_ward_compare <- permit_points_sample %>%
  st_drop_geometry() %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(permit_independent_ward, by = ".audit_row") %>%
  mutate(ward_match = as.integer(ward) == independent_ward)

permit_block_2010_independent <- assign_blocks_independent(permit_points, blocks_2010, "block_id", chunk_n = 50000L)
permit_block_2020_independent <- assign_blocks_independent(permit_points, blocks_2020, "block_id", chunk_n = 50000L)

permit_missing_2010_current <- read_csv(file.path(repo_root, "tasks/audits/permit_event_study_audit/output/permit_block_assignment_missing_2010.csv"), show_col_types = FALSE) %>%
  mutate(id = as.character(id))
permit_missing_2020_current <- read_csv(file.path(repo_root, "tasks/audits/permit_event_study_audit/output/permit_block_assignment_missing_2020.csv"), show_col_types = FALSE) %>%
  mutate(id = as.character(id))

permit_block_2010_compare <- permit_points %>%
  st_drop_geometry() %>%
  select(id) %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(permit_block_2010_independent, by = ".audit_row") %>%
  mutate(
    independent_unmatched = is.na(independent_block_id),
    current_unmatched = id %in% permit_missing_2010_current$id
  )
permit_block_2020_compare <- permit_points %>%
  st_drop_geometry() %>%
  select(id) %>%
  mutate(.audit_row = seq_len(n())) %>%
  left_join(permit_block_2020_independent, by = ".audit_row") %>%
  mutate(
    independent_unmatched = is.na(independent_block_id),
    current_unmatched = id %in% permit_missing_2020_current$id
  )

assignment_summary_rows[[length(assignment_summary_rows) + 1]] <- tibble(
  branch = "permits",
  object = c("ward_raw_field", "block_assignability_2010", "block_assignability_2020"),
  comparison_basis = c("issued_permits_sample_100k", "issued_permits_full", "issued_permits_full"),
  n_compare = c(nrow(permit_ward_compare), nrow(permit_block_2010_compare), nrow(permit_block_2020_compare)),
  n_issue = c(sum(!permit_ward_compare$ward_match, na.rm = TRUE), sum(permit_block_2010_compare$independent_unmatched != permit_block_2010_compare$current_unmatched, na.rm = TRUE), sum(permit_block_2020_compare$independent_unmatched != permit_block_2020_compare$current_unmatched, na.rm = TRUE)),
  pct_match = c(mean(permit_ward_compare$ward_match, na.rm = TRUE), mean(permit_block_2010_compare$independent_unmatched == permit_block_2010_compare$current_unmatched, na.rm = TRUE), mean(permit_block_2020_compare$independent_unmatched == permit_block_2020_compare$current_unmatched, na.rm = TRUE)),
  note = c("Raw permit ward field vs independent ward assignment.", "Independent 2010 block unmatched set vs current unmatched table.", "Independent 2020 block unmatched set vs current unmatched table.")
)

write_issue_table(
  permit_ward_compare %>%
    filter(!ward_match | ward_assignment_reason != "exact_within_unique") %>%
    select(id, ward, independent_ward, ward_assignment_reason, ward_match_count),
  file.path(assignment_issue_dir, "permit_ward_assignment_issues.csv")
)

write_issue_table(
  permit_block_2010_compare %>%
    filter(independent_unmatched != current_unmatched) %>%
    select(id, independent_block_id, independent_unmatched, current_unmatched, block_assignment_reason),
  file.path(assignment_issue_dir, "permit_block_assignability_2010_issues.csv")
)

write_issue_table(
  permit_block_2020_compare %>%
    filter(independent_unmatched != current_unmatched) %>%
    select(id, independent_block_id, independent_unmatched, current_unmatched, block_assignment_reason),
  file.path(assignment_issue_dir, "permit_block_assignability_2020_issues.csv")
)

permit_panel_2015 <- read_parquet(
  file.path(repo_root, "tasks/create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"),
  col_select = c("cohort_block_id", "block_id", "block_vintage", "ward_origin", "ward_dest", "ward_pair_id", "dist_ft", "relative_year")
)

block_treatment_current <- read_csv(file.path(repo_root, "tasks/create_block_treatment_panel/output/block_treatment_pre_scores.csv"), show_col_types = FALSE) %>%
  mutate(block_id = as.character(block_id))

blocks_2010_for_treatment <- blocks_2010 %>% select(block_id)
blocks_2020_for_treatment <- blocks_2020 %>% select(block_id)

assign_treatment_by_cohort <- function(blocks_sf, cohort_value, origin_era, dest_era) {
  current_blocks <- block_treatment_current %>% filter(cohort == cohort_value, block_vintage == unique(blocks_sf$block_vintage))
  if (nrow(current_blocks) == 0) {
    return(tibble())
  }
  blocks_subset <- blocks_sf %>% filter(block_id %in% current_blocks$block_id)
  origin_assign <- majority_area_assign_blocks(blocks_subset, ward_maps[[origin_era]])
  dest_assign <- majority_area_assign_blocks(blocks_subset, ward_maps[[dest_era]])
  current_blocks %>%
    left_join(origin_assign, by = "block_id") %>%
    rename(independent_ward_origin = independent_ward, independent_origin_share = independent_ward_share, independent_origin_n_wards = independent_n_wards) %>%
    left_join(dest_assign, by = "block_id") %>%
    rename(independent_ward_dest = independent_ward, independent_dest_share = independent_ward_share, independent_dest_n_wards = independent_n_wards)
}

blocks_2010_for_treatment$block_vintage <- 2010
blocks_2020_for_treatment$block_vintage <- 2020
treatment_compare <- bind_rows(
  assign_treatment_by_cohort(blocks_2010_for_treatment, 2015, "2003_2014", "2015_2023"),
  assign_treatment_by_cohort(blocks_2020_for_treatment, 2023, "2015_2023", "post_2023")
) %>%
  mutate(
    origin_match = ward_origin == independent_ward_origin,
    dest_match = ward_dest == independent_ward_dest
  )

assignment_summary_rows[[length(assignment_summary_rows) + 1]] <- tibble(
  branch = "permits",
  object = c("treatment_origin", "treatment_dest"),
  comparison_basis = c("block_treatment_pre_scores_full", "block_treatment_pre_scores_full"),
  n_compare = c(nrow(treatment_compare), nrow(treatment_compare)),
  n_issue = c(sum(!treatment_compare$origin_match, na.rm = TRUE), sum(!treatment_compare$dest_match, na.rm = TRUE)),
  pct_match = c(mean(treatment_compare$origin_match, na.rm = TRUE), mean(treatment_compare$dest_match, na.rm = TRUE)),
  note = c("Independent majority-area origin ward assignment.", "Independent majority-area destination ward assignment.")
)

write_issue_table(
  treatment_compare %>%
    filter(!origin_match | !dest_match) %>%
    select(block_id, cohort, block_vintage, ward_origin, independent_ward_origin, ward_dest, independent_ward_dest, ward_origin_share, independent_origin_share, ward_dest_share, independent_dest_share, min_assignment_share),
  file.path(assignment_issue_dir, "block_treatment_assignment_issues.csv")
)

sample_ladder_rows[[length(sample_ladder_rows) + 1]] <- bind_rows(
  tibble(branch = "permits", stage = "raw_permits", n_rows = nrow(permits_raw), unique_ids = n_distinct(permits_raw$id), detail = "Raw permits."),
  tibble(branch = "permits", stage = "clean_permits", n_rows = nrow(permits_clean), unique_ids = n_distinct(permits_clean$id), detail = "Cleaned permits."),
  tibble(branch = "permits", stage = "issued_clean_permits", n_rows = permits_clean %>% filter(permit_issued == 1) %>% nrow(), unique_ids = permits_clean %>% filter(permit_issued == 1) %>% summarise(n = n_distinct(id)) %>% pull(n), detail = "Issued cleaned permits."),
  tibble(branch = "permits", stage = "permit_panel_2015", n_rows = nrow(permit_panel_2015), unique_ids = n_distinct(permit_panel_2015$cohort_block_id), detail = "2015 permit panel rows."),
  tibble(branch = "permits", stage = "permit_panel_2015_did_sample", n_rows = permit_panel_2015 %>% filter(relative_year >= -5, relative_year <= 5, dist_ft <= 1000) %>% nrow(), unique_ids = permit_panel_2015 %>% filter(relative_year >= -5, relative_year <= 5, dist_ft <= 1000) %>% summarise(n = n_distinct(cohort_block_id)) %>% pull(n), detail = "2015 permit DID sample.")
)

# ------------------------------------------------------------------------------
# Invariance checks
# ------------------------------------------------------------------------------
parcel_invariance_sample <- parcel_compare_input %>% slice_sample(n = min(2000L, nrow(.)))
parcel_invariance_sf <- parcel_compare_input %>% slice(match(parcel_invariance_sample$pin, pin))

run_assignment_signature <- function(points_sf, date_values, chunk_n, shuffle = FALSE) {
  work <- points_sf
  if (shuffle) {
    work <- work %>% slice_sample(prop = 1)
  }
  assign_points_independent(
    work,
    date_values[match(work$.audit_source_id, points_sf$.audit_source_id)],
    ward_maps,
    boundary_layers,
    chunk_n = chunk_n
  ) %>%
    mutate(.audit_source_id = work$.audit_source_id[.audit_row]) %>%
    arrange(.audit_source_id) %>%
    transmute(
      .audit_source_id,
      signature = paste(independent_ward, independent_pair, round(independent_dist_ft, 3), sep = "|")
    )
}

parcel_invariance_points <- parcel_invariance_sample %>%
  mutate(.audit_source_id = pin) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs_projected)
parcel_signature_500 <- run_assignment_signature(parcel_invariance_points, parcel_invariance_sample$construction_date, 500L, shuffle = FALSE)
parcel_signature_5000 <- run_assignment_signature(parcel_invariance_points, parcel_invariance_sample$construction_date, 5000L, shuffle = FALSE)
parcel_signature_shuffle <- run_assignment_signature(parcel_invariance_points, parcel_invariance_sample$construction_date, 500L, shuffle = TRUE)

invariance_rows[[length(invariance_rows) + 1]] <- tibble(
  branch = "density",
  check = c("chunk_size_500_vs_5000", "row_order_shuffle"),
  comparison_basis = "parcel_sample_2000",
  value = c(
    sum(parcel_signature_500$signature != parcel_signature_5000$signature),
    sum(parcel_signature_500$signature != parcel_signature_shuffle$signature)
  ),
  detail = c("Independent parcel assignment differences across chunk sizes.", "Independent parcel assignment differences after row-order shuffle.")
)

sales_invariance_sample <- sales_scores %>% slice_sample(n = min(2000L, nrow(.)))
sales_invariance_points <- st_as_sf(sales_invariance_sample %>% mutate(.audit_source_id = paste(pin, sale_date)), coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% st_transform(crs_projected)
sales_signature_500 <- run_assignment_signature(sales_invariance_points, sales_invariance_sample$sale_date, 500L, shuffle = FALSE)
sales_signature_5000 <- run_assignment_signature(sales_invariance_points, sales_invariance_sample$sale_date, 5000L, shuffle = FALSE)
sales_signature_shuffle <- run_assignment_signature(sales_invariance_points, sales_invariance_sample$sale_date, 500L, shuffle = TRUE)

invariance_rows[[length(invariance_rows) + 1]] <- tibble(
  branch = "home_sales",
  check = c("chunk_size_500_vs_5000", "row_order_shuffle"),
  comparison_basis = "sales_sample_2000",
  value = c(
    sum(sales_signature_500$signature != sales_signature_5000$signature),
    sum(sales_signature_500$signature != sales_signature_shuffle$signature)
  ),
  detail = c("Independent sales assignment differences across chunk sizes.", "Independent sales assignment differences after row-order shuffle.")
)

rental_invariance_sample <- rental_sample %>% slice_sample(n = min(2000L, nrow(.)))
rental_invariance_points <- st_as_sf(rental_invariance_sample %>% mutate(.audit_source_id = paste(id, file_date)), coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% st_transform(crs_projected)
rental_signature_500 <- run_assignment_signature(rental_invariance_points, rental_invariance_sample$file_date, 500L, shuffle = FALSE)
rental_signature_5000 <- run_assignment_signature(rental_invariance_points, rental_invariance_sample$file_date, 5000L, shuffle = FALSE)
rental_signature_shuffle <- run_assignment_signature(rental_invariance_points, rental_invariance_sample$file_date, 500L, shuffle = TRUE)

invariance_rows[[length(invariance_rows) + 1]] <- tibble(
  branch = "rentals",
  check = c("chunk_size_500_vs_5000", "row_order_shuffle"),
  comparison_basis = "rental_sample_2000",
  value = c(
    sum(rental_signature_500$signature != rental_signature_5000$signature),
    sum(rental_signature_500$signature != rental_signature_shuffle$signature)
  ),
  detail = c("Independent rental assignment differences across chunk sizes.", "Independent rental assignment differences after row-order shuffle.")
)

near_boundary_sample <- parcel_compare_input %>%
  st_transform(crs_projected) %>%
  mutate(current_dist = parcel_compare$dist_to_boundary) %>%
  filter(current_dist <= 10) %>%
  slice_sample(n = min(500L, nrow(.)))

if (nrow(near_boundary_sample) > 0) {
  jittered_east <- st_geometry(near_boundary_sample) + c(1, 0)
  jittered_west <- st_geometry(near_boundary_sample) + c(-1, 0)
  jitter_points <- near_boundary_sample
  st_geometry(jitter_points) <- jittered_east
  east_assign <- assign_points_independent(jitter_points, near_boundary_sample$construction_date, ward_maps, boundary_layers, chunk_n = 500L)
  st_geometry(jitter_points) <- jittered_west
  west_assign <- assign_points_independent(jitter_points, near_boundary_sample$construction_date, ward_maps, boundary_layers, chunk_n = 500L)
  base_assign <- assign_points_independent(near_boundary_sample, near_boundary_sample$construction_date, ward_maps, boundary_layers, chunk_n = 500L)
  boundary_jitter_changes <- sum(base_assign$independent_ward != east_assign$independent_ward | base_assign$independent_ward != west_assign$independent_ward, na.rm = TRUE)
} else {
  boundary_jitter_changes <- 0L
}

invariance_rows[[length(invariance_rows) + 1]] <- tibble(
  branch = "density",
  check = "boundary_jitter_plus_minus_1ft",
  comparison_basis = "parcel_near_boundary_sample_500",
  value = boundary_jitter_changes,
  detail = "Ward assignment changes after deterministic one-foot east/west jitter for near-boundary parcels."
)

invariance_checks <- bind_rows(invariance_rows)
write_csv(invariance_checks, invariance_checks_path)

# ------------------------------------------------------------------------------
# Manual spotcheck packet
# ------------------------------------------------------------------------------
parcel_manual_base <- parcel_compare %>%
  left_join(
    parcels_with_geom %>%
      st_transform(4326) %>%
      mutate(
        longitude = st_coordinates(.)[, 1],
        latitude = st_coordinates(.)[, 2]
      ) %>%
      st_drop_geometry() %>%
      select(pin, longitude, latitude),
    by = "pin"
  )

manual_queue_parts[[length(manual_queue_parts) + 1]] <- build_manual_spotcheck(
  branch_name = "density",
  ordinary_tbl = parcel_manual_base %>% filter(ward_match, pair_match, segment_match),
  boundary_tbl = parcel_manual_base %>% filter(independent_dist_ft <= 25),
  ambiguous_tbl = parcel_manual_base %>% filter(ward_assignment_reason != "exact_within_unique" | segment_assignment_reason != "exact_pair_buffer_unique"),
  excluded_tbl = parcel_manual_base %>% filter(!pair_match | !segment_match),
  id_col = "pin",
  lon_col = "longitude",
  lat_col = "latitude"
)

manual_queue_parts[[length(manual_queue_parts) + 1]] <- build_manual_spotcheck(
  branch_name = "permits",
  ordinary_tbl = permit_ward_compare %>% left_join(permit_points_sample %>% st_drop_geometry() %>% select(id, longitude, latitude), by = "id") %>% filter(ward_match),
  boundary_tbl = permit_ward_compare %>% left_join(permit_points_sample %>% st_drop_geometry() %>% select(id, longitude, latitude), by = "id") %>% filter(ward_assignment_reason != "exact_within_unique"),
  ambiguous_tbl = permit_ward_compare %>% left_join(permit_points_sample %>% st_drop_geometry() %>% select(id, longitude, latitude), by = "id") %>% filter(ward_match_count != 1),
  excluded_tbl = permit_block_2020_compare %>% left_join(permit_points %>% st_drop_geometry() %>% select(id, longitude, latitude), by = "id") %>% filter(independent_unmatched | current_unmatched),
  id_col = "id",
  lon_col = "longitude",
  lat_col = "latitude"
)

manual_queue_parts[[length(manual_queue_parts) + 1]] <- build_manual_spotcheck(
  branch_name = "home_sales",
  ordinary_tbl = sales_compare %>% filter(ward_match, pair_match),
  boundary_tbl = sales_compare %>% filter(independent_dist_ft <= 25),
  ambiguous_tbl = sales_compare %>% filter(ward_assignment_reason != "exact_within_unique"),
  excluded_tbl = sales_panel_compare %>% left_join(sales_coord_lookup, by = c("pin", "sale_date")) %>% filter(!block_match | !segment_match),
  id_col = "pin",
  lon_col = "longitude",
  lat_col = "latitude"
)

manual_queue_parts[[length(manual_queue_parts) + 1]] <- build_manual_spotcheck(
  branch_name = "rentals",
  ordinary_tbl = rental_compare %>% filter(ward_match, pair_match),
  boundary_tbl = rental_compare %>% filter(independent_dist_ft <= 25),
  ambiguous_tbl = rental_compare %>% filter(ward_assignment_reason != "exact_within_unique"),
  excluded_tbl = rental_panel_compare %>% left_join(rent_coord_lookup, by = c("id", "file_date")) %>% filter(!block_match | !segment_match),
  id_col = "id",
  lon_col = "longitude",
  lat_col = "latitude"
)

manual_spotcheck_queue <- bind_rows(manual_queue_parts) %>%
  mutate(
    review_note = case_when(
      stratum == "ordinary_matched" ~ "Ordinary matched record.",
      stratum == "boundary_adjacent" ~ "Near-boundary record; check assignment sensitivity.",
      stratum == "fallback_or_ambiguous" ~ "Assignment required fallback or ambiguity handling.",
      TRUE ~ "Excluded or unmatched record; confirm exclusion is defensible."
    )
  ) %>%
  select(branch, stratum, source_id, longitude, latitude, review_note, everything())
write_csv(manual_spotcheck_queue, manual_queue_path)

if (nrow(manual_spotcheck_queue) > 0) {
  manual_spotcheck_sf <- st_as_sf(manual_spotcheck_queue, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(crs_projected)
  st_write(manual_spotcheck_sf, manual_layers_path, delete_dsn = TRUE, quiet = TRUE)
}

# ------------------------------------------------------------------------------
# Summaries, verdicts, and propagation queue
# ------------------------------------------------------------------------------
assignment_consistency_summary <- bind_rows(assignment_summary_rows)
distance_consistency_summary <- bind_rows(distance_summary_rows)
branch_sample_ladders <- bind_rows(sample_ladder_rows)

write_csv(assignment_consistency_summary, assignment_summary_path)
write_csv(distance_consistency_summary, distance_summary_path)
write_csv(branch_sample_ladders, sample_ladder_path)

branch_metrics <- list(
  density = list(
    assignment = assignment_consistency_summary %>% filter(branch == "density"),
    distance = distance_consistency_summary %>% filter(branch == "density"),
    raw = raw_cleaning_integrity %>% filter(branch == "density"),
    invariance = invariance_checks %>% filter(branch == "density")
  ),
  permits = list(
    assignment = assignment_consistency_summary %>% filter(branch == "permits"),
    distance = distance_consistency_summary %>% filter(branch == "permits"),
    raw = raw_cleaning_integrity %>% filter(branch == "permits"),
    invariance = invariance_checks %>% filter(branch == "permits")
  ),
  home_sales = list(
    assignment = assignment_consistency_summary %>% filter(branch == "home_sales"),
    distance = distance_consistency_summary %>% filter(branch == "home_sales"),
    raw = raw_cleaning_integrity %>% filter(branch == "home_sales"),
    invariance = invariance_checks %>% filter(branch == "home_sales")
  ),
  rentals = list(
    assignment = assignment_consistency_summary %>% filter(branch == "rentals"),
    distance = distance_consistency_summary %>% filter(branch == "rentals"),
    raw = raw_cleaning_integrity %>% filter(branch == "rentals"),
    invariance = invariance_checks %>% filter(branch == "rentals")
  )
)

make_branch_verdict <- function(branch_name, metrics) {
  raw_issue_checks <- c(
    "raw_missing_lat_lon",
    "raw_xy_only_candidates",
    "clean_duplicate_id",
    "clean_missing_pin",
    "clean_outside_bbox",
    "duplicate_pin",
    "missing_yearbuilt",
    "outside_bbox",
    "outside_city_boundary",
    "duplicate_pin_sale_date",
    "missing_coordinates",
    "duplicate_id_file_date"
  )

  assignment_bad_rate <- if (nrow(metrics$assignment) == 0) {
    0
  } else {
    max(replace_na(1 - metrics$assignment$pct_match, 0))
  }

  distance_large <- if (nrow(metrics$distance) == 0) {
    0
  } else {
    max(replace_na(metrics$distance$n_large_diff_25ft, 0))
  }

  any_raw_issue <- if (nrow(metrics$raw) == 0) {
    FALSE
  } else {
    metrics$raw %>%
      filter(check %in% raw_issue_checks) %>%
      summarise(any_issue = any(as.numeric(value) > 0, na.rm = TRUE)) %>%
      pull(any_issue) %>%
      null_coalesce(FALSE)
  }
  any_invariance_issue <- any(as.numeric(metrics$invariance$value) > 0, na.rm = TRUE)

  stoplight <- case_when(
    is.finite(assignment_bad_rate) && assignment_bad_rate > 0.005 ~ "red",
    distance_large > 0 ~ "red",
    any_invariance_issue ~ "yellow",
    any_raw_issue ~ "yellow",
    TRUE ~ "green"
  )

  verdict <- case_when(
    stoplight == "green" ~ "survives",
    stoplight == "yellow" ~ "survives_but_fragile",
    TRUE ~ "fails_as_written"
  )

  summary <- case_when(
    stoplight == "green" ~ "Upstream spatial construction looks internally consistent under the current audit grid.",
    stoplight == "yellow" ~ "No catastrophic upstream failure, but at least one data-cleaning, assignment, or invariance issue remains.",
    TRUE ~ "At least one upstream spatial construction check shows a material inconsistency that could contaminate downstream results."
  )

  tibble(branch = branch_name, stoplight = stoplight, verdict = verdict, summary = summary)
}

upstream_branch_verdicts <- bind_rows(
  make_branch_verdict("density", branch_metrics$density),
  make_branch_verdict("permits", branch_metrics$permits),
  make_branch_verdict("home_sales", branch_metrics$home_sales),
  make_branch_verdict("rentals", branch_metrics$rentals)
)
write_csv(upstream_branch_verdicts, branch_verdicts_path)

if (upstream_branch_verdicts$stoplight[upstream_branch_verdicts$branch == "density"] != "green") {
  propagation_rows[[length(propagation_rows) + 1]] <- tibble(
    issue_id = "U001",
    branch = "density",
    finding_class = "robustness_fragility",
    upstream_stage = "parcel_geocode_or_boundary_assignment",
    rebuild_tasks = "geocode_ccao_data -> calculate_ward_boundary_distances -> assign_segment_ids -> merge_in_scores -> border_pair_FE_regressions -> nonparametric_rd_density_linear_display -> summary_stats_new_construction",
    rationale = "Any upstream density spatial issue propagates directly into FE and RD parcel samples."
  )
}
if (upstream_branch_verdicts$stoplight[upstream_branch_verdicts$branch == "permits"] != "green") {
  propagation_rows[[length(propagation_rows) + 1]] <- tibble(
    issue_id = "U002",
    branch = "permits",
    finding_class = "robustness_fragility",
    upstream_stage = "permit_cleaning_block_assignment_or_treatment",
    rebuild_tasks = "clean_building_permits -> create_block_treatment_panel -> merge_event_study_scores -> create_event_study_permit_data -> run_event_study_permit -> permit_summary_stats -> create_alderman_uncertainty_index",
    rationale = "Permit branch depends on cleaned permits, block treatment, and permit panel construction."
  )
}
if (upstream_branch_verdicts$stoplight[upstream_branch_verdicts$branch == "home_sales"] != "green") {
  propagation_rows[[length(propagation_rows) + 1]] <- tibble(
    issue_id = "U003",
    branch = "home_sales",
    finding_class = "robustness_fragility",
    upstream_stage = "sales_geocode_or_block_segment_assignment",
    rebuild_tasks = "calculate_sale_distances -> merge_event_study_scores -> create_event_study_sales_data_disaggregate -> run_event_study_sales_disaggregate; create_event_study_permit_data -> event_study_treatment_maps",
    rationale = "Home-sales branch depends on scored sales distances and event-study panel assignment."
  )
}
if (upstream_branch_verdicts$stoplight[upstream_branch_verdicts$branch == "rentals"] != "green") {
  propagation_rows[[length(propagation_rows) + 1]] <- tibble(
    issue_id = "U004",
    branch = "rentals",
    finding_class = "robustness_fragility",
    upstream_stage = "rental_geocode_or_block_segment_assignment",
    rebuild_tasks = "calculate_rent_distances -> merge_event_study_scores -> create_event_study_rental_data_disaggregate -> run_event_study_rental_disaggregate",
    rationale = "Rental branch depends on rental ward-distance scoring and listing-panel assignment."
  )
}
if (nrow(treatment_compare %>% filter(!origin_match | !dest_match)) > 0) {
  propagation_rows[[length(propagation_rows) + 1]] <- tibble(
    issue_id = "U005",
    branch = "permits/home_sales/rentals",
    finding_class = "code_bug",
    upstream_stage = "block_treatment_assignment",
    rebuild_tasks = "create_block_treatment_panel -> merge_event_study_scores -> create_event_study_permit_data + create_event_study_sales_data_disaggregate + create_event_study_rental_data_disaggregate -> rerun downstream estimators",
    rationale = "Treatment-panel assignment propagates to permit, sales, and rental event-study designs."
  )
}

fix_candidate_propagation_queue <- if (length(propagation_rows) == 0) {
  tibble(
    issue_id = character(),
    branch = character(),
    finding_class = character(),
    upstream_stage = character(),
    rebuild_tasks = character(),
    rationale = character()
  )
} else {
  bind_rows(propagation_rows)
}
write_csv(fix_candidate_propagation_queue, propagation_queue_path)

memo_lines <- c(
  "# Upstream Spatial Red-Team Audit",
  "",
  sprintf("Generated on %s.", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  "",
  "## Branch Verdicts",
  sprintf("- Density: %s.", upstream_branch_verdicts$verdict[upstream_branch_verdicts$branch == "density"]),
  sprintf("- Permits: %s.", upstream_branch_verdicts$verdict[upstream_branch_verdicts$branch == "permits"]),
  sprintf("- Home sales: %s.", upstream_branch_verdicts$verdict[upstream_branch_verdicts$branch == "home_sales"]),
  sprintf("- Rentals: %s.", upstream_branch_verdicts$verdict[upstream_branch_verdicts$branch == "rentals"]),
  "",
  "## Highest-Priority Findings",
  sprintf("- Permit manual review remains a live upstream fragility point if the permit branch cannot be rebuilt without `manual_permit_block_review.csv`."),
  sprintf("- Density branch topology / assignment issues should be checked against `assignment_consistency_summary.csv` and `distance_consistency_summary.csv` before trusting the RD and FE estimates as release-grade."),
  sprintf("- Home-sales and rental branches now have explicit independent ward/pair/block/segment audits instead of relying on downstream stability alone."),
  "",
  "## Propagation Readiness",
  "- `fix_candidate_propagation_queue.csv` lists the exact downstream rebuild chain for each non-green branch.",
  "- `manual_spotcheck_queue.csv` and `manual_spotcheck_layers.gpkg` provide a deterministic human review packet.",
  "",
  "## Default Interpretation",
  "- Treat any non-green branch verdict as a block on claiming the upstream spatial construction is airtight.",
  "- Use this memo with the manuscript hostile audit; if a branch is not green here, manuscript claims relying on that branch should be downgraded there."
)
writeLines(memo_lines, memo_path)
