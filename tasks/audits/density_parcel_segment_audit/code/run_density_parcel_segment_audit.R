# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_parcel_segment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(readr)
library(sf)
library(units)

sf_use_s2(FALSE)

segment_buffer_m <- 250
density_bandwidth_m <- 152.4
distance_tolerance_m <- 0.05

assert_unique <- function(df, key, label) {
  n_dup <- sum(duplicated(df[[key]]))
  if (n_dup > 0) {
    stop(sprintf("%s has %d duplicate %s values.", label, n_dup, key), call. = FALSE)
  }
}

format_pin <- function(pin) {
  pin <- gsub("[^0-9]", "", as.character(pin))
  ifelse(
    nchar(pin) == 14,
    paste(
      substr(pin, 1, 2),
      substr(pin, 3, 4),
      substr(pin, 5, 7),
      substr(pin, 8, 10),
      substr(pin, 11, 14),
      sep = "-"
    ),
    pin
  )
}

summary_row <- function(check, scope, n_checked, n_issue, detail) {
  tibble(
    check = check,
    scope = scope,
    n_checked = as.integer(n_checked),
    n_issue = as.integer(n_issue),
    pass = n_issue == 0,
    detail = detail
  )
}

diagnostic_row <- function(check, scope, n_checked, n_issue, detail) {
  out <- summary_row(check, scope, n_checked, n_issue, detail)
  out$pass <- TRUE
  out
}

same_or_both_na <- function(x, y) {
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

same_numeric_or_both_na <- function(x, y, tol) {
  (is.na(x) & is.na(y)) | (is.finite(x) & is.finite(y) & abs(x - y) <= tol)
}

pair_includes_ward <- function(pair_dash, ward) {
  pair_dash <- as.character(pair_dash)
  ward <- as.character(ward)
  out <- rep(FALSE, length(pair_dash))
  ok <- !is.na(pair_dash) & !is.na(ward)
  out[ok] <- mapply(
    function(pair_i, ward_i) ward_i %in% strsplit(pair_i, "-", fixed = TRUE)[[1]],
    pair_dash[ok],
    ward[ok],
    USE.NAMES = FALSE
  )
  out
}

recompute_boundary_assignment <- function(points_sf, era_values, ward_maps, boundary_layers) {
  ward_out <- rep(NA_integer_, nrow(points_sf))
  ward_match_count <- rep(NA_integer_, nrow(points_sf))
  pair_out <- rep(NA_character_, nrow(points_sf))
  neighbor_out <- rep(NA_integer_, nrow(points_sf))
  dist_m_out <- rep(NA_real_, nrow(points_sf))

  for (era_i in sort(unique(na.omit(as.character(era_values))))) {
    idx_era <- which(as.character(era_values) == era_i)
    ward_map <- ward_maps[[era_i]]
    boundary_layer <- boundary_layers[[era_i]]
    if (length(idx_era) == 0 || is.null(ward_map) || is.null(boundary_layer)) {
      next
    }

    pts_era <- st_transform(points_sf[idx_era, ], st_crs(ward_map))
    hits <- st_within(pts_era, ward_map)
    ward_match_count[idx_era] <- lengths(hits)
    ward_out[idx_era] <- vapply(hits, function(v) {
      if (length(v) == 0) {
        return(NA_integer_)
      }
      suppressWarnings(as.integer(ward_map$ward[v[1]]))
    }, integer(1))

    for (ward_i in sort(unique(na.omit(ward_out[idx_era])))) {
      idx_ward_global <- idx_era[ward_out[idx_era] == ward_i]
      idx_ward_local <- match(idx_ward_global, idx_era)
      candidate_lines <- boundary_layer[boundary_layer$ward_a == ward_i | boundary_layer$ward_b == ward_i, ]
      if (length(idx_ward_global) == 0 || nrow(candidate_lines) == 0) {
        next
      }

      nearest_idx <- st_nearest_feature(pts_era[idx_ward_local, ], candidate_lines)
      nearest_lines <- candidate_lines[nearest_idx, ]
      dists <- st_distance(st_geometry(pts_era[idx_ward_local, ]), st_geometry(nearest_lines), by_element = TRUE)
      neighbor <- ifelse(
        ward_i == nearest_lines$ward_a,
        nearest_lines$ward_b,
        nearest_lines$ward_a
      )

      neighbor_out[idx_ward_global] <- suppressWarnings(as.integer(neighbor))
      pair_out[idx_ward_global] <- normalize_pair_id(ward_i, neighbor, sep = "_")
      dist_m_out[idx_ward_global] <- as.numeric(set_units(dists, "m"))
    }
  }

  tibble(
    recomputed_ward = ward_out,
    recomputed_ward_match_count = ward_match_count,
    recomputed_neighbor_ward = neighbor_out,
    recomputed_ward_pair = pair_out,
    recomputed_dist_to_boundary_m = dist_m_out
  )
}

compute_boundary_near_ties <- function(points_sf, era_values, ward_values, boundary_layers) {
  incident_boundary_count <- rep(NA_integer_, nrow(points_sf))
  nearest_incident_pair_dash <- rep(NA_character_, nrow(points_sf))
  nearest_incident_dist_m <- rep(NA_real_, nrow(points_sf))
  second_nearest_incident_pair_dash <- rep(NA_character_, nrow(points_sf))
  second_nearest_incident_dist_m <- rep(NA_real_, nrow(points_sf))
  incident_boundary_gap_m <- rep(NA_real_, nrow(points_sf))

  for (era_i in sort(unique(na.omit(as.character(era_values))))) {
    idx_era <- which(as.character(era_values) == era_i)
    boundary_layer <- boundary_layers[[era_i]]
    if (length(idx_era) == 0 || is.null(boundary_layer) || nrow(boundary_layer) == 0) {
      next
    }

    pts_era <- st_transform(points_sf[idx_era, ], st_crs(boundary_layer))
    ward_era <- ward_values[idx_era]

    for (ward_i in sort(unique(na.omit(ward_era)))) {
      idx_ward_global <- idx_era[ward_era == ward_i]
      idx_ward_local <- match(idx_ward_global, idx_era)
      candidate_lines <- boundary_layer[boundary_layer$ward_a == ward_i | boundary_layer$ward_b == ward_i, ]
      if (length(idx_ward_global) == 0 || nrow(candidate_lines) == 0) {
        next
      }

      candidate_pair_dash <- normalize_pair_dash(paste(candidate_lines$ward_a, candidate_lines$ward_b, sep = "-"))
      distance_matrix <- st_distance(st_geometry(pts_era[idx_ward_local, ]), st_geometry(candidate_lines))
      distance_matrix_m <- matrix(
        as.numeric(set_units(distance_matrix, "m")),
        nrow = length(idx_ward_global),
        ncol = nrow(candidate_lines)
      )

      incident_boundary_count[idx_ward_global] <- nrow(candidate_lines)
      for (i in seq_along(idx_ward_global)) {
        sorted <- order(distance_matrix_m[i, ], candidate_pair_dash)
        nearest <- sorted[1]
        nearest_incident_pair_dash[idx_ward_global[i]] <- candidate_pair_dash[nearest]
        nearest_incident_dist_m[idx_ward_global[i]] <- distance_matrix_m[i, nearest]

        if (length(sorted) > 1) {
          second <- sorted[2]
          second_nearest_incident_pair_dash[idx_ward_global[i]] <- candidate_pair_dash[second]
          second_nearest_incident_dist_m[idx_ward_global[i]] <- distance_matrix_m[i, second]
          incident_boundary_gap_m[idx_ward_global[i]] <- distance_matrix_m[i, second] - distance_matrix_m[i, nearest]
        }
      }
    }
  }

  tibble(
    incident_boundary_count = incident_boundary_count,
    nearest_incident_pair_dash = nearest_incident_pair_dash,
    nearest_incident_dist_m = nearest_incident_dist_m,
    second_nearest_incident_pair_dash = second_nearest_incident_pair_dash,
    second_nearest_incident_dist_m = second_nearest_incident_dist_m,
    incident_boundary_gap_m = incident_boundary_gap_m
  )
}

recompute_segments <- function(points_sf, era_values, pair_values, segment_layers) {
  pair_dash <- normalize_pair_dash(pair_values)
  constrained_segment_id <- rep(NA_character_, nrow(points_sf))
  constrained_pair_dash <- rep(NA_character_, nrow(points_sf))
  constrained_dist_m <- rep(NA_real_, nrow(points_sf))
  global_segment_id <- rep(NA_character_, nrow(points_sf))
  global_pair_dash <- rep(NA_character_, nrow(points_sf))
  global_dist_m <- rep(NA_real_, nrow(points_sf))

  for (era_i in sort(unique(na.omit(as.character(era_values))))) {
    idx_era <- which(as.character(era_values) == era_i)
    seg_era <- segment_layers[[era_i]]
    if (length(idx_era) == 0 || is.null(seg_era) || nrow(seg_era) == 0) {
      next
    }

    pts_era <- st_transform(points_sf[idx_era, ], st_crs(seg_era))
    seg_era <- seg_era[order(seg_era$pair_dash, seg_era$segment_id), ]

    nearest_global_idx <- st_nearest_feature(pts_era, seg_era)
    global_segments <- seg_era[nearest_global_idx, ]
    global_dists <- st_distance(st_geometry(pts_era), st_geometry(global_segments), by_element = TRUE)
    global_dist_m[idx_era] <- as.numeric(set_units(global_dists, "m"))
    global_pair_dash[idx_era] <- global_segments$pair_dash
    global_segment_id[idx_era] <- ifelse(
      is.finite(global_dist_m[idx_era]) & global_dist_m[idx_era] <= segment_buffer_m,
      as.character(global_segments$segment_id),
      NA_character_
    )

    for (pair_i in sort(unique(na.omit(pair_dash[idx_era])))) {
      idx_pair_global <- idx_era[pair_dash[idx_era] == pair_i]
      idx_pair_local <- match(idx_pair_global, idx_era)
      seg_pair <- seg_era[seg_era$pair_dash == pair_i, ]
      if (length(idx_pair_global) == 0 || nrow(seg_pair) == 0) {
        next
      }

      nearest_pair_idx <- st_nearest_feature(pts_era[idx_pair_local, ], seg_pair)
      pair_segments <- seg_pair[nearest_pair_idx, ]
      pair_dists <- st_distance(
        st_geometry(pts_era[idx_pair_local, ]),
        st_geometry(pair_segments),
        by_element = TRUE
      )
      pair_dist_m <- as.numeric(set_units(pair_dists, "m"))
      ok <- is.finite(pair_dist_m) & pair_dist_m <= segment_buffer_m

      constrained_dist_m[idx_pair_global] <- pair_dist_m
      constrained_pair_dash[idx_pair_global] <- pair_i
      constrained_segment_id[idx_pair_global[ok]] <- as.character(pair_segments$segment_id[ok])
    }
  }

  tibble(
    recomputed_segment_id = constrained_segment_id,
    recomputed_segment_pair_dash = constrained_pair_dash,
    recomputed_segment_dist_m = constrained_dist_m,
    global_nearest_segment_id = global_segment_id,
    global_nearest_pair_dash = global_pair_dash,
    global_nearest_segment_dist_m = global_dist_m
  )
}

sample_rows <- function(df, label, n_take) {
  if (nrow(df) == 0) {
    return(df[0, ] %>% mutate(spotcheck_stratum = character()))
  }
  df %>%
    slice_sample(n = min(n_take, nrow(df))) %>%
    mutate(spotcheck_stratum = label)
}

cat("=== Density parcel/ward/segment audit ===\n")

st_layers("../input/parcels_with_geometry.gpkg")
geom <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  mutate(pin = as.character(pin))
scores <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    segment_reason = col_character(),
    dist_to_segment_m = col_double(),
    .default = col_guess()
  )
) %>%
  mutate(
    pin = as.character(pin),
    construction_date = as.Date(construction_date),
    boundary_year = as.integer(boundary_year),
    construction_year = as.integer(construction_year),
    ward = as.integer(ward),
    other_ward = as.integer(other_ward),
    pair_dash = normalize_pair_dash(ward_pair),
    era = canonical_era_from_boundary_year(boundary_year),
    segment_id = as.character(segment_id),
    segment_id = if_else(segment_id %in% c("", "NA", "NaN"), NA_character_, segment_id),
    valid_segment_flag = coerce_segment_valid_flag(valid_segment),
    invalid_reason = as.character(invalid_reason),
    dist_to_segment_m = if ("dist_to_segment_m" %in% names(.)) as.numeric(dist_to_segment_m) else NA_real_
  )
segment_lookup <- read_csv(
  "../input/parcel_segment_ids.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(),
    segment_id = col_character(),
    dist_to_segment_m = col_double(),
    segment_reason = col_character()
  )
) %>%
  mutate(
    pin = as.character(pin),
    lookup_segment_id = as.character(segment_id),
    lookup_segment_id = if_else(lookup_segment_id %in% c("", "NA", "NaN"), NA_character_, lookup_segment_id),
    lookup_dist_to_segment_m = if ("dist_to_segment_m" %in% names(.)) as.numeric(dist_to_segment_m) else NA_real_,
    lookup_segment_reason = as.character(segment_reason)
  ) %>%
  select(pin, lookup_segment_id, lookup_dist_to_segment_m, lookup_segment_reason)
pair_constraint_audit <- read_csv(
  "../input/parcel_segment_pair_constraint_audit.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  mutate(pin = as.character(pin))

assert_unique(geom, "pin", "parcels_with_geometry.gpkg")
assert_unique(scores, "pin", "parcels_with_ward_distances.csv")
assert_unique(segment_lookup, "pin", "parcel_segment_ids.csv")

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(3435)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_layers <- load_boundary_layers("../input/ward_pair_boundaries.gpkg", eras = sort(unique(na.omit(scores$era))))
segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg", eras = sort(unique(na.omit(scores$era))))
segment_pair_lookup <- bind_rows(lapply(names(segment_layers), function(era_i) {
  tibble::tibble(
    era = era_i,
    stored_segment_id = as.character(segment_layers[[era_i]]$segment_id),
    stored_segment_pair_dash = as.character(segment_layers[[era_i]]$pair_dash)
  )
}))
segment_layer_duplicate_count <- sum(duplicated(segment_pair_lookup[c("era", "stored_segment_id")]))
if (segment_layer_duplicate_count > 0) {
  stop("Segment pair lookup has duplicate era/segment_id rows.", call. = FALSE)
}

work <- geom %>%
  select(pin) %>%
  inner_join(scores, by = "pin") %>%
  left_join(segment_lookup, by = "pin")

coords <- st_coordinates(st_transform(work, 4326))
work$longitude <- coords[, "X"]
work$latitude <- coords[, "Y"]

boundary_recomputed <- recompute_boundary_assignment(work, work$era, ward_maps, boundary_layers)
boundary_near_ties <- compute_boundary_near_ties(
  work,
  work$era,
  boundary_recomputed$recomputed_ward,
  boundary_layers
)
segment_recomputed <- recompute_segments(work, work$era, work$pair_dash, segment_layers)

audit <- bind_cols(
  work %>%
    st_drop_geometry(),
  boundary_recomputed,
  boundary_near_ties,
  segment_recomputed
) %>%
  mutate(stored_segment_id = segment_id) %>%
  left_join(
    segment_pair_lookup,
    by = c("era", "stored_segment_id"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    ward_match = ward == recomputed_ward,
    ward_pair_match = normalize_pair_dash(ward_pair) == normalize_pair_dash(recomputed_ward_pair),
    distance_abs_diff_m = abs(dist_to_boundary_m - recomputed_dist_to_boundary_m),
    distance_match = distance_abs_diff_m <= distance_tolerance_m,
    other_ward_in_pair = !is.na(other_ward) & !is.na(ward) & !is.na(pair_dash) &
      normalize_pair_dash(paste(ward, other_ward, sep = "-")) == pair_dash,
    signed_distance_abs_match = abs(abs(signed_distance_m) - dist_to_boundary_m) <= distance_tolerance_m,
    final_segment_matches_lookup = same_or_both_na(stored_segment_id, lookup_segment_id),
    final_segment_distance_matches_lookup = (is.na(stored_segment_id) & is.na(lookup_segment_id)) |
      same_numeric_or_both_na(dist_to_segment_m, lookup_dist_to_segment_m, distance_tolerance_m),
    segment_matches_recomputed = same_or_both_na(stored_segment_id, recomputed_segment_id),
    segment_distance_matches_recomputed = (is.na(stored_segment_id) & is.na(recomputed_segment_id)) |
      same_numeric_or_both_na(dist_to_segment_m, recomputed_segment_dist_m, distance_tolerance_m),
    assigned_segment_distance_equals_boundary_distance = case_when(
      is.na(stored_segment_id) ~ TRUE,
      TRUE ~ is.finite(dist_to_segment_m) &
        abs(dist_to_segment_m - dist_to_boundary_m) <= distance_tolerance_m
    ),
    segment_pair_matches_ward_pair = case_when(
      is.na(stored_segment_id) ~ TRUE,
      TRUE ~ !is.na(stored_segment_pair_dash) & stored_segment_pair_dash == pair_dash
    ),
    assigned_segment_valid = case_when(
      is.na(stored_segment_id) ~ TRUE,
      TRUE ~ valid_segment_flag %in% TRUE
    ),
    assigned_segment_has_no_invalid_reason = case_when(
      is.na(stored_segment_id) ~ TRUE,
      TRUE ~ is.na(invalid_reason) | invalid_reason %in% c("", "NA", "none")
    ),
    assigned_segment_found_in_same_era_layer = case_when(
      is.na(stored_segment_id) ~ TRUE,
      TRUE ~ !is.na(stored_segment_pair_dash)
    ),
    segment_within_buffer = case_when(
      is.na(stored_segment_id) ~ TRUE,
      TRUE ~ is.finite(recomputed_segment_dist_m) & recomputed_segment_dist_m <= segment_buffer_m + distance_tolerance_m
    ),
    required_500ft_fields_present = !is.na(era) & !is.na(ward) & !is.na(other_ward) &
      !is.na(ward_pair) & is.finite(dist_to_boundary_m) &
      !is.na(stored_segment_id) & is.finite(dist_to_segment_m),
    global_pair_diff = !is.na(global_nearest_segment_id) & !is.na(stored_segment_id) &
      global_nearest_pair_dash != pair_dash,
    global_pair_includes_own_ward = pair_includes_ward(global_nearest_pair_dash, ward),
    global_nearest_segment_matches_current = same_or_both_na(stored_segment_id, global_nearest_segment_id),
    regression_base = construction_year >= 2006,
    regression_bw100m = construction_year >= 2006 & dist_to_boundary_m <= 100,
    regression_bw500ft = construction_year >= 2006 & dist_to_boundary_m <= density_bandwidth_m,
    regression_bw250m = construction_year >= 2006 & dist_to_boundary_m <= 250,
    main_density_sample = regression_bw500ft & unitscount > 0 & density_far > 0 & density_dupac > 0,
    main_multifamily_density_sample = main_density_sample & unitscount > 1,
    pin_formatted = format_pin(pin)
  )

audit_summary <- bind_rows(
  summary_row("geometry_crs_is_3435", "full", 1, ifelse(st_crs(geom)$epsg == 3435, 0, 1), "Parcel geometry CRS should be EPSG:3435."),
  summary_row("final_scores_unique_pin", "full", nrow(scores), sum(duplicated(scores$pin)), "Final density score output should be one row per PIN."),
  summary_row("geometry_unique_pin", "full", nrow(geom), sum(duplicated(geom$pin)), "Parcel geometry should be one row per PIN."),
  summary_row("segment_lookup_unique_pin", "full", nrow(segment_lookup), sum(duplicated(segment_lookup$pin)), "Segment lookup should be one row per PIN."),
  summary_row("segment_layer_unique_id_by_era", "full", nrow(segment_pair_lookup), segment_layer_duplicate_count, "Segment IDs should be unique within era in the canonical segment layer."),
  summary_row("ward_matches_recomputed", "full", nrow(audit), sum(!audit$ward_match, na.rm = TRUE), "Stored ward equals independent point-in-ward recomputation."),
  summary_row("ward_hit_count_exactly_one", "full", nrow(audit), sum(is.na(audit$recomputed_ward_match_count) | audit$recomputed_ward_match_count != 1, na.rm = TRUE), "Independent point-in-ward recomputation should find exactly one containing ward."),
  summary_row("ward_hit_count_exactly_one", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$regression_bw500ft & (is.na(audit$recomputed_ward_match_count) | audit$recomputed_ward_match_count != 1), na.rm = TRUE), "500ft rows should have exactly one containing ward."),
  summary_row("required_fields_present", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$regression_bw500ft & !audit$required_500ft_fields_present, na.rm = TRUE), "500ft rows should have nonmissing ward, pair, distance, segment, and era fields."),
  summary_row("ward_pair_matches_recomputed", "full", nrow(audit), sum(!audit$ward_pair_match, na.rm = TRUE), "Stored ward pair equals nearest adjacent boundary for stored ward/era."),
  summary_row("distance_m_matches_recomputed", "full", nrow(audit), sum(!audit$distance_match, na.rm = TRUE), sprintf("Stored distance-to-boundary in meters matches recomputed value within %.2fm.", distance_tolerance_m)),
  summary_row("other_ward_is_pair_partner", "full", nrow(audit), sum(!audit$other_ward_in_pair, na.rm = TRUE), "Stored other_ward is the non-own ward in ward_pair."),
  summary_row("signed_distance_abs_matches_distance", "full", nrow(audit), sum(!audit$signed_distance_abs_match, na.rm = TRUE), "Absolute signed distance equals unsigned distance in meters."),
  summary_row("final_segment_matches_lookup", "full", nrow(audit), sum(!audit$final_segment_matches_lookup, na.rm = TRUE), "Merged final score segment_id matches assign_segment_ids lookup."),
  summary_row("final_segment_distance_matches_lookup", "full", sum(!is.na(audit$stored_segment_id)), sum(!audit$final_segment_distance_matches_lookup, na.rm = TRUE), "Merged final score distance-to-segment matches assign_segment_ids lookup."),
  summary_row("segment_matches_recomputed", "full", nrow(audit), sum(!audit$segment_matches_recomputed, na.rm = TRUE), "Stored segment_id equals independent nearest segment within the same ward pair."),
  summary_row("segment_distance_matches_recomputed", "full", sum(!is.na(audit$stored_segment_id)), sum(!audit$segment_distance_matches_recomputed, na.rm = TRUE), "Stored distance-to-segment equals independent nearest-segment distance within the same ward pair."),
  summary_row("assigned_segment_distance_equals_boundary_distance", "full", sum(!is.na(audit$stored_segment_id)), sum(!audit$assigned_segment_distance_equals_boundary_distance, na.rm = TRUE), "Assigned segment distance equals nearest border distance."),
  summary_row("segment_pair_matches_ward_pair", "full", sum(!is.na(audit$stored_segment_id)), sum(!audit$segment_pair_matches_ward_pair, na.rm = TRUE), "Assigned segment belongs to the parcel's ward pair."),
  summary_row("assigned_segment_valid", "full", sum(!is.na(audit$stored_segment_id)), sum(!audit$assigned_segment_valid, na.rm = TRUE), "Assigned segment should be marked valid in the segment validity metadata."),
  summary_row("assigned_segment_valid", "regression_bw500ft", sum(audit$regression_bw500ft & !is.na(audit$stored_segment_id), na.rm = TRUE), sum(audit$regression_bw500ft & !audit$assigned_segment_valid, na.rm = TRUE), "500ft assigned segments should be marked valid in the segment validity metadata."),
  summary_row("assigned_segment_has_no_invalid_reason", "regression_bw500ft", sum(audit$regression_bw500ft & !is.na(audit$stored_segment_id), na.rm = TRUE), sum(audit$regression_bw500ft & !audit$assigned_segment_has_no_invalid_reason, na.rm = TRUE), "500ft assigned segments should not carry an invalid segment reason."),
  summary_row("assigned_segment_found_in_same_era_layer", "regression_bw500ft", sum(audit$regression_bw500ft & !is.na(audit$stored_segment_id), na.rm = TRUE), sum(audit$regression_bw500ft & !audit$assigned_segment_found_in_same_era_layer, na.rm = TRUE), "500ft assigned segments should resolve in the same canonical era segment layer."),
  summary_row("segment_within_250m_buffer", "full", sum(!is.na(audit$stored_segment_id)), sum(!audit$segment_within_buffer, na.rm = TRUE), "Assigned segment is within the configured 250m segment radius."),
  summary_row("audit_flags_no_na", "full", nrow(audit), sum(is.na(audit$final_segment_matches_lookup) | is.na(audit$segment_matches_recomputed) | is.na(audit$global_nearest_segment_matches_current)), "Core boolean audit flags should not be NA."),
  diagnostic_row("global_all_options_segment_matches_current", "full", sum(!is.na(audit$stored_segment_id)), sum(!audit$global_nearest_segment_matches_current & !is.na(audit$stored_segment_id), na.rm = TRUE), "Diagnostic only: purely geometric nearest segment among all same-era segments equals current assignment."),
  diagnostic_row("global_all_options_segment_matches_current", "regression_bw100m", sum(audit$regression_bw100m, na.rm = TRUE), sum(!audit$global_nearest_segment_matches_current & !is.na(audit$stored_segment_id) & audit$regression_bw100m, na.rm = TRUE), "Diagnostic only: purely geometric nearest segment among all same-era segments equals current assignment."),
  diagnostic_row("global_all_options_segment_matches_current", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(!audit$global_nearest_segment_matches_current & !is.na(audit$stored_segment_id) & audit$regression_bw500ft, na.rm = TRUE), "Diagnostic only: purely geometric nearest segment among all same-era segments equals current assignment."),
  diagnostic_row("global_all_options_segment_matches_current", "regression_bw250m", sum(audit$regression_bw250m, na.rm = TRUE), sum(!audit$global_nearest_segment_matches_current & !is.na(audit$stored_segment_id) & audit$regression_bw250m, na.rm = TRUE), "Diagnostic only: purely geometric nearest segment among all same-era segments equals current assignment."),
  diagnostic_row("global_nearest_pair_diff_includes_own_ward", "regression_bw100m", sum(audit$regression_bw100m, na.rm = TRUE), sum(audit$global_pair_diff & audit$global_pair_includes_own_ward & audit$regression_bw100m, na.rm = TRUE), "Diagnostic only: global nearest different-pair segment still includes the parcel's own ward."),
  diagnostic_row("global_nearest_pair_diff_includes_own_ward", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$global_pair_diff & audit$global_pair_includes_own_ward & audit$regression_bw500ft, na.rm = TRUE), "Diagnostic only: global nearest different-pair segment still includes the parcel's own ward."),
  diagnostic_row("global_nearest_pair_diff_includes_own_ward", "regression_bw250m", sum(audit$regression_bw250m, na.rm = TRUE), sum(audit$global_pair_diff & audit$global_pair_includes_own_ward & audit$regression_bw250m, na.rm = TRUE), "Diagnostic only: global nearest different-pair segment still includes the parcel's own ward."),
  diagnostic_row("global_nearest_pair_diff", "regression_bw100m", sum(audit$regression_bw100m, na.rm = TRUE), sum(audit$global_pair_diff & audit$regression_bw100m, na.rm = TRUE), "Diagnostic only: unconstrained global nearest segment belongs to a different ward pair."),
  diagnostic_row("global_nearest_pair_diff", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$global_pair_diff & audit$regression_bw500ft, na.rm = TRUE), "Diagnostic only: unconstrained global nearest segment belongs to a different ward pair."),
  diagnostic_row("global_nearest_pair_diff", "main_density_sample", sum(audit$main_density_sample, na.rm = TRUE), sum(audit$global_pair_diff & audit$main_density_sample, na.rm = TRUE), "Diagnostic only: unconstrained global nearest segment belongs to a different ward pair."),
  diagnostic_row("global_nearest_pair_diff", "main_multifamily_density_sample", sum(audit$main_multifamily_density_sample, na.rm = TRUE), sum(audit$global_pair_diff & audit$main_multifamily_density_sample, na.rm = TRUE), "Diagnostic only: unconstrained global nearest segment belongs to a different ward pair."),
  diagnostic_row("global_nearest_pair_diff", "regression_bw250m", sum(audit$regression_bw250m, na.rm = TRUE), sum(audit$global_pair_diff & audit$regression_bw250m, na.rm = TRUE), "Diagnostic only: unconstrained global nearest segment belongs to a different ward pair."),
  diagnostic_row("near_tie_incident_boundary_lt_1m", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$regression_bw500ft & !is.na(audit$incident_boundary_gap_m) & audit$incident_boundary_gap_m < 1, na.rm = TRUE), "Diagnostic only: nearest and second-nearest own-ward incident boundaries are within 1m."),
  diagnostic_row("near_tie_incident_boundary_lt_5m", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$regression_bw500ft & !is.na(audit$incident_boundary_gap_m) & audit$incident_boundary_gap_m < 5, na.rm = TRUE), "Diagnostic only: nearest and second-nearest own-ward incident boundaries are within 5m."),
  diagnostic_row("near_tie_incident_boundary_lt_10m", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$regression_bw500ft & !is.na(audit$incident_boundary_gap_m) & audit$incident_boundary_gap_m < 10, na.rm = TRUE), "Diagnostic only: nearest and second-nearest own-ward incident boundaries are within 10m."),
  summary_row("segment_coverage", "regression_bw100m", sum(audit$regression_bw100m, na.rm = TRUE), sum(audit$regression_bw100m & is.na(audit$stored_segment_id), na.rm = TRUE), "Rows in the 100m regression sample should have segment_id."),
  summary_row("segment_coverage", "regression_bw500ft", sum(audit$regression_bw500ft, na.rm = TRUE), sum(audit$regression_bw500ft & is.na(audit$stored_segment_id), na.rm = TRUE), "Rows in the 500ft regression sample should have segment_id."),
  summary_row("segment_coverage", "main_density_sample", sum(audit$main_density_sample, na.rm = TRUE), sum(audit$main_density_sample & is.na(audit$stored_segment_id), na.rm = TRUE), "Rows in the main 500ft all-construction density sample should have segment_id."),
  summary_row("segment_coverage", "main_multifamily_density_sample", sum(audit$main_multifamily_density_sample, na.rm = TRUE), sum(audit$main_multifamily_density_sample & is.na(audit$stored_segment_id), na.rm = TRUE), "Rows in the main 500ft multifamily density sample should have segment_id."),
  summary_row("segment_coverage", "regression_bw250m", sum(audit$regression_bw250m, na.rm = TRUE), sum(audit$regression_bw250m & is.na(audit$stored_segment_id), na.rm = TRUE), "Rows in the 250m regression sample should have segment_id.")
)

ward_pair_distance_issues <- audit %>%
  filter(
    !ward_match |
      !ward_pair_match |
      !distance_match |
      !other_ward_in_pair |
      !signed_distance_abs_match |
      recomputed_ward_match_count != 1
  ) %>%
  select(
    pin, pin_formatted, construction_year, construction_date, boundary_year, era,
    longitude, latitude, ward, recomputed_ward, recomputed_ward_match_count,
    ward_pair, recomputed_ward_pair, other_ward, recomputed_neighbor_ward,
    dist_to_boundary_m, recomputed_dist_to_boundary_m, distance_abs_diff_m,
    signed_distance_m, ward_match, ward_pair_match, distance_match,
    other_ward_in_pair, signed_distance_abs_match
  )

segment_assignment_issues <- audit %>%
  filter(
    !final_segment_matches_lookup |
      !final_segment_distance_matches_lookup |
      !segment_matches_recomputed |
      !segment_distance_matches_recomputed |
      !assigned_segment_distance_equals_boundary_distance |
      !segment_pair_matches_ward_pair |
      !assigned_segment_valid |
      !assigned_segment_has_no_invalid_reason |
      !assigned_segment_found_in_same_era_layer |
      !segment_within_buffer |
      (regression_bw500ft & !required_500ft_fields_present) |
      (regression_bw500ft & is.na(stored_segment_id))
  ) %>%
  select(
    pin, pin_formatted, construction_year, construction_date, boundary_year, era,
    longitude, latitude, ward, ward_pair, dist_to_boundary_m,
    stored_segment_id, stored_segment_pair_dash, lookup_segment_id, lookup_segment_reason,
    valid_segment_flag, invalid_reason,
    dist_to_segment_m, lookup_dist_to_segment_m,
    recomputed_segment_id, recomputed_segment_pair_dash, recomputed_segment_dist_m,
    global_nearest_segment_id, global_nearest_pair_dash, global_nearest_segment_dist_m,
    final_segment_matches_lookup, final_segment_distance_matches_lookup,
    segment_matches_recomputed, segment_distance_matches_recomputed,
    assigned_segment_distance_equals_boundary_distance,
    segment_pair_matches_ward_pair, assigned_segment_valid,
    assigned_segment_has_no_invalid_reason, assigned_segment_found_in_same_era_layer,
    required_500ft_fields_present, segment_within_buffer,
    regression_bw100m, regression_bw500ft, regression_bw250m
  )

all_options_segment_comparison <- audit %>%
  transmute(
    pin, pin_formatted, construction_year, construction_date, boundary_year, era,
    longitude, latitude, ward, other_ward, ward_pair, dist_to_boundary_m,
    current_segment_id = stored_segment_id,
    current_segment_pair_dash = stored_segment_pair_dash,
    current_segment_dist_m = dist_to_segment_m,
    nearest_assigned_pair_segment_id = recomputed_segment_id,
    nearest_assigned_pair_segment_dist_m = recomputed_segment_dist_m,
    global_nearest_segment_id,
    global_nearest_pair_dash,
    global_nearest_segment_dist_m,
    global_nearest_segment_matches_current,
    global_pair_diff,
    global_pair_includes_own_ward,
    regression_bw100m,
    regression_bw500ft,
    regression_bw250m
  )

global_nearest_segment_pair_differences <- audit %>%
  filter(global_pair_diff) %>%
  select(
    pin, pin_formatted, construction_year, construction_date, boundary_year, era,
    longitude, latitude, ward, ward_pair, dist_to_boundary_m,
    stored_segment_id, stored_segment_pair_dash, dist_to_segment_m, recomputed_segment_dist_m,
    global_nearest_segment_id, global_nearest_pair_dash, global_nearest_segment_dist_m,
    global_pair_includes_own_ward,
    regression_bw100m, regression_bw500ft, regression_bw250m
  )

near_tie_boundary_diagnostics <- audit %>%
  filter(regression_bw500ft, !is.na(incident_boundary_gap_m), incident_boundary_gap_m < 10) %>%
  arrange(incident_boundary_gap_m, pin) %>%
  select(
    pin, pin_formatted, construction_year, construction_date, boundary_year, era,
    longitude, latitude, ward, ward_pair, dist_to_boundary_m,
    nearest_incident_pair_dash, nearest_incident_dist_m,
    second_nearest_incident_pair_dash, second_nearest_incident_dist_m,
    incident_boundary_gap_m, stored_segment_id, dist_to_segment_m,
    main_density_sample, main_multifamily_density_sample
  )

count_pins <- function(pin, flag) {
  n_distinct(pin[flag %in% TRUE])
}

final_sample_attrition <- tibble(
  stage = c(
    "parcel_geometry",
    "final_score_rows",
    "final_score_rows_with_segment",
    "regression_bw500ft",
    "main_500ft_all_density_sample",
    "main_500ft_multifamily_density_sample"
  ),
  n_rows = c(
    nrow(geom),
    nrow(scores),
    sum(!is.na(audit$stored_segment_id)),
    sum(audit$regression_bw500ft, na.rm = TRUE),
    sum(audit$main_density_sample, na.rm = TRUE),
    sum(audit$main_multifamily_density_sample, na.rm = TRUE)
  ),
  n_unique_pin = c(
    n_distinct(geom$pin),
    n_distinct(scores$pin),
    n_distinct(audit$pin[!is.na(audit$stored_segment_id)]),
    count_pins(audit$pin, audit$regression_bw500ft),
    count_pins(audit$pin, audit$main_density_sample),
    count_pins(audit$pin, audit$main_multifamily_density_sample)
  ),
  n_with_geometry = c(
    nrow(geom),
    nrow(audit),
    sum(!is.na(audit$stored_segment_id)),
    sum(audit$regression_bw500ft, na.rm = TRUE),
    sum(audit$main_density_sample, na.rm = TRUE),
    sum(audit$main_multifamily_density_sample, na.rm = TRUE)
  ),
  n_with_ward = c(
    NA_integer_,
    sum(!is.na(audit$ward)),
    sum(!is.na(audit$ward) & !is.na(audit$stored_segment_id)),
    sum(audit$regression_bw500ft & !is.na(audit$ward), na.rm = TRUE),
    sum(audit$main_density_sample & !is.na(audit$ward), na.rm = TRUE),
    sum(audit$main_multifamily_density_sample & !is.na(audit$ward), na.rm = TRUE)
  ),
  n_with_ward_pair = c(
    NA_integer_,
    sum(!is.na(audit$ward_pair)),
    sum(!is.na(audit$ward_pair) & !is.na(audit$stored_segment_id)),
    sum(audit$regression_bw500ft & !is.na(audit$ward_pair), na.rm = TRUE),
    sum(audit$main_density_sample & !is.na(audit$ward_pair), na.rm = TRUE),
    sum(audit$main_multifamily_density_sample & !is.na(audit$ward_pair), na.rm = TRUE)
  ),
  n_with_segment = c(
    NA_integer_,
    sum(!is.na(audit$stored_segment_id)),
    sum(!is.na(audit$stored_segment_id)),
    sum(audit$regression_bw500ft & !is.na(audit$stored_segment_id), na.rm = TRUE),
    sum(audit$main_density_sample & !is.na(audit$stored_segment_id), na.rm = TRUE),
    sum(audit$main_multifamily_density_sample & !is.na(audit$stored_segment_id), na.rm = TRUE)
  ),
  n_inside_500ft = c(
    NA_integer_,
    sum(audit$regression_bw500ft, na.rm = TRUE),
    sum(audit$regression_bw500ft & !is.na(audit$stored_segment_id), na.rm = TRUE),
    sum(audit$regression_bw500ft, na.rm = TRUE),
    sum(audit$main_density_sample, na.rm = TRUE),
    sum(audit$main_multifamily_density_sample, na.rm = TRUE)
  ),
  reason_for_drop = c(
    "Raw parcel geometry universe before score/sign filters.",
    "Rows that survived score merge and nonmissing signed-distance construction.",
    "Final score rows with assigned segment within 250m.",
    "Final score rows within 500ft of assigned ward boundary.",
    "Paper all-construction density plot/regression rows: 500ft, positive units, positive FAR and DUPAC.",
    "Paper multifamily density plot/regression rows: 500ft, unitscount > 1, positive FAR and DUPAC."
  )
)

set.seed(20260513)
manual_spotcheck_sample <- bind_rows(
  sample_rows(audit %>% filter(main_density_sample), "main_500ft_density_sample", 8),
  sample_rows(audit %>% filter(main_density_sample, unitscount > 1), "main_500ft_multifamily_sample", 5),
  sample_rows(audit %>% filter(global_pair_diff), "global_nearest_pair_diff", 5),
  sample_rows(audit %>% filter(regression_bw500ft, !is.na(incident_boundary_gap_m), incident_boundary_gap_m < 10), "near_tie_boundary_under_10m", 5),
  sample_rows(audit %>% filter(regression_bw500ft, dist_to_boundary_m <= 10), "within_10m_of_boundary", 5),
  sample_rows(audit %>% filter(!regression_bw250m, is.na(stored_segment_id), dist_to_boundary_m > segment_buffer_m), "outside_segment_buffer", 5),
  sample_rows(audit %>% filter(boundary_year == 2024, !is.na(stored_segment_id)), "current_ward_external_checkable", 5)
) %>%
  distinct(pin, spotcheck_stratum, .keep_all = TRUE) %>%
  mutate(
    google_maps_url = paste0("https://www.google.com/maps/search/?api=1&query=", latitude, ",", longitude),
    external_check_scope = if_else(
      boundary_year == 2024,
      "Current ward boundary can be compared to official current ward lookup.",
      "Historical ward boundary; current online ward lookup is not comparable."
    )
  ) %>%
  select(
    spotcheck_stratum, pin, pin_formatted, construction_year, construction_date,
    boundary_year, era, longitude, latitude, google_maps_url, external_check_scope,
    ward, other_ward, ward_pair, dist_to_boundary_m,
    stored_segment_id, recomputed_segment_dist_m,
    global_nearest_segment_id, global_nearest_pair_dash, global_nearest_segment_dist_m,
    density_far, density_dupac, unitscount
  )

write_csv(audit_summary, "../output/audit_summary.csv")
write_csv(ward_pair_distance_issues, "../output/ward_pair_distance_issues.csv")
write_csv(segment_assignment_issues, "../output/segment_assignment_issues.csv")
write_csv(all_options_segment_comparison, "../output/all_options_segment_comparison.csv")
write_csv(global_nearest_segment_pair_differences, "../output/global_nearest_segment_pair_differences.csv")
write_csv(near_tie_boundary_diagnostics, "../output/near_tie_boundary_diagnostics.csv")
write_csv(final_sample_attrition, "../output/final_sample_attrition.csv")
write_csv(manual_spotcheck_sample, "../output/manual_spotcheck_sample.csv")

manual_spotcheck_sf <- manual_spotcheck_sample %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3435)
st_write(manual_spotcheck_sf, "../output/manual_spotcheck_sample.gpkg", delete_dsn = TRUE, quiet = TRUE)

cat("\nAudit summary:\n")
print(audit_summary)
cat("\nIssue row counts:\n")
cat("ward_pair_distance_issues:", nrow(ward_pair_distance_issues), "\n")
cat("segment_assignment_issues:", nrow(segment_assignment_issues), "\n")
cat("all_options_segment_comparison:", nrow(all_options_segment_comparison), "\n")
cat("global_nearest_segment_pair_differences:", nrow(global_nearest_segment_pair_differences), "\n")
cat("\nSaved outputs in ../output.\n")
