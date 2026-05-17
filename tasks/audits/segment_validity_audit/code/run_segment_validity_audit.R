# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/segment_validity_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

sf_use_s2(FALSE)

short_segment_ft <- 100
numeric_noise_ft <- 1
touch_tolerance_ft <- 1
near_tolerance_ft <- 10
offsets_ft <- c(5, 20, 50, 100)
support_bandwidths_ft <- c(500, 1000)

offset_point <- function(line_geom, side, offset_ft) {
  coords <- st_coordinates(line_geom)[, c("X", "Y"), drop = FALSE]
  if (nrow(coords) < 2) {
    return(NULL)
  }

  seg_len <- sqrt(rowSums((coords[-1, , drop = FALSE] - coords[-nrow(coords), , drop = FALSE])^2))
  keep <- is.finite(seg_len) & seg_len > 0
  if (!any(keep)) {
    return(NULL)
  }

  coords <- coords[c(TRUE, keep), , drop = FALSE]
  seg_len <- sqrt(rowSums((coords[-1, , drop = FALSE] - coords[-nrow(coords), , drop = FALSE])^2))
  cumdist <- c(0, cumsum(seg_len))
  target_dist <- cumdist[length(cumdist)] / 2
  end_idx <- which(cumdist >= target_dist)[1]
  start_idx <- max(1L, end_idx - 1L)
  denom <- cumdist[end_idx] - cumdist[start_idx]
  if (!is.finite(denom) || denom <= 0) {
    return(NULL)
  }

  weight <- (target_dist - cumdist[start_idx]) / denom
  mid <- coords[start_idx, ] + weight * (coords[end_idx, ] - coords[start_idx, ])
  tangent <- coords[end_idx, ] - coords[start_idx, ]
  tangent_len <- sqrt(sum(tangent^2))
  if (!is.finite(tangent_len) || tangent_len <= 0) {
    return(NULL)
  }

  normal <- c(-tangent[2], tangent[1]) / tangent_len * side
  st_point(as.numeric(mid + offset_ft * normal))
}

ward_at_point <- function(point_geom, ward_sf) {
  if (is.null(point_geom)) {
    return(NA_character_)
  }

  point_sf <- st_sf(geometry = st_sfc(point_geom, crs = st_crs(ward_sf)))
  hits <- st_within(point_sf, ward_sf)[[1]]
  if (length(hits) == 0) {
    return(NA_character_)
  }

  paste(sort(unique(as.integer(ward_sf$ward[hits]))), collapse = ";")
}

offset_pair_passes <- function(left_ward, right_ward, ward_a, ward_b) {
  if (is.na(left_ward) || is.na(right_ward)) {
    return(FALSE)
  }
  if (grepl(";", left_ward, fixed = TRUE) || grepl(";", right_ward, fixed = TRUE)) {
    return(FALSE)
  }

  observed <- sort(as.integer(c(left_ward, right_ward)))
  expected <- sort(as.integer(c(ward_a, ward_b)))
  identical(observed, expected)
}

read_segment_layers <- function() {
  eras <- canonical_era_levels()
  out <- lapply(eras, function(era_i) {
    st_read("../input/boundary_segments_1320ft.gpkg", layer = era_i, quiet = TRUE) %>%
      mutate(era = era_i)
  })
  names(out) <- eras
  bind_rows(out)
}

count_support <- function(df, id_col, side_col, distance_col, dataset_label, audit_segments) {
  base_segments <- audit_segments %>%
    st_drop_geometry() %>%
    select(raw_segment_id, analysis_segment_id, era, ward_pair_id, ward_a, ward_b, drop_confound) %>%
    distinct(raw_segment_id, .keep_all = TRUE)

  x <- df %>%
    transmute(
      raw_segment_id = as.character(.data[[id_col]]),
      own_ward = suppressWarnings(as.integer(.data[[side_col]])),
      distance_m = suppressWarnings(as.numeric(.data[[distance_col]]))
    ) %>%
    filter(!is.na(raw_segment_id), raw_segment_id != "")

  bind_rows(lapply(support_bandwidths_ft, function(bw_ft) {
    observed <- x %>%
      filter(is.finite(distance_m), distance_m <= bw_ft * 0.3048) %>%
      inner_join(
        base_segments %>% select(raw_segment_id, ward_a, ward_b),
        by = "raw_segment_id",
        relationship = "many-to-one"
      ) %>%
      group_by(raw_segment_id) %>%
      summarise(
        n_total = n(),
        n_side_a = sum(own_ward == ward_a, na.rm = TRUE),
        n_side_b = sum(own_ward == ward_b, na.rm = TRUE),
        n_other_side = sum(!is.na(own_ward) & !own_ward %in% c(ward_a, ward_b)),
        .groups = "drop"
      )

    base_segments %>%
      left_join(observed, by = "raw_segment_id", relationship = "one-to-one") %>%
      mutate(
        n_total = coalesce(n_total, 0L),
        n_side_a = coalesce(n_side_a, 0L),
        n_side_b = coalesce(n_side_b, 0L),
        n_other_side = coalesce(n_other_side, 0L)
      ) %>%
      mutate(
        dataset = dataset_label,
        bandwidth_ft = bw_ft,
        support_valid_main = n_side_a >= 1 & n_side_b >= 1,
        support_valid_strict = n_side_a >= 2 & n_side_b >= 2
      )
  }))
}

message("Loading segment layers and ward maps...")
segments <- read_segment_layers() %>%
  mutate(
    raw_segment_id = as.character(segment_id),
    ward_pair_id = as.character(ward_pair_id),
    ward_a = as.integer(ward_a),
    ward_b = as.integer(ward_b),
    segment_number = as.integer(segment_number),
    n_segments_in_pair = as.integer(n_segments_in_pair),
    segment_length_ft = as.numeric(segment_length_ft),
    short_segment = segment_length_ft < short_segment_ft,
    terminal_segment = segment_number == 1L | segment_number == n_segments_in_pair
  )

if (anyDuplicated(segments$raw_segment_id)) {
  stop("Segment IDs are not unique across canonical layers.", call. = FALSE)
}

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)

rows <- list()
for (i in seq_len(nrow(segments))) {
  segment_i <- segments[i, ]
  needs_topology_check <- segment_i$short_segment || segment_i$segment_length_ft < numeric_noise_ft

  nearest_segment_id <- NA_character_
  nearest_segment_number <- NA_integer_
  nearest_distance_ft <- NA_real_
  n_touching_1ft <- 0L
  n_near_10ft <- 0L
  touching_segment_id <- NA_character_

  if (needs_topology_check) {
    same_pair <- segments %>%
      filter(era == segment_i$era, ward_pair_id == segment_i$ward_pair_id, raw_segment_id != segment_i$raw_segment_id)
  } else {
    same_pair <- segments[0, ]
  }

  if (needs_topology_check && nrow(same_pair) > 0) {
    distances <- as.numeric(st_distance(st_geometry(segment_i), st_geometry(same_pair), by_element = FALSE)[1, ])
    nearest_idx <- which.min(distances)
    nearest_segment_id <- same_pair$raw_segment_id[nearest_idx]
    nearest_segment_number <- same_pair$segment_number[nearest_idx]
    nearest_distance_ft <- distances[nearest_idx]
    n_touching_1ft <- sum(distances <= touch_tolerance_ft, na.rm = TRUE)
    n_near_10ft <- sum(distances <= near_tolerance_ft, na.rm = TRUE)
    if (n_touching_1ft == 1L) {
      touching_segment_id <- same_pair$raw_segment_id[which(distances <= touch_tolerance_ft)[1]]
    }
  }

  offset_values <- list()
  pass_values <- rep(NA, length(offsets_ft))
  for (j in seq_along(offsets_ft)) {
    offset_ft <- offsets_ft[j]
    if (needs_topology_check) {
      left_ward <- ward_at_point(offset_point(st_geometry(segment_i)[[1]], -1, offset_ft), ward_maps[[segment_i$era]])
      right_ward <- ward_at_point(offset_point(st_geometry(segment_i)[[1]], 1, offset_ft), ward_maps[[segment_i$era]])
      pass_values[j] <- offset_pair_passes(left_ward, right_ward, segment_i$ward_a, segment_i$ward_b)
    } else {
      left_ward <- NA_character_
      right_ward <- NA_character_
    }
    offset_values[[paste0("left_offset_ward_", offset_ft, "ft")]] <- left_ward
    offset_values[[paste0("right_offset_ward_", offset_ft, "ft")]] <- right_ward
    offset_values[[paste0("two_sided_pass_", offset_ft, "ft")]] <- pass_values[j]
  }

  two_sided_all_offsets <- if (needs_topology_check) all(pass_values) else NA
  component_type <- case_when(
    segment_i$segment_length_ft < numeric_noise_ft ~ "suspected_artifact",
    segment_i$short_segment & segment_i$n_segments_in_pair == 1L ~ "short_only_piece",
    segment_i$short_segment & segment_i$terminal_segment & n_touching_1ft == 1L ~ "terminal_touching_remainder",
    segment_i$short_segment & segment_i$terminal_segment ~ "terminal_short_component",
    segment_i$short_segment ~ "short_disconnected_component",
    TRUE ~ "long_split_piece"
  )
  invalid_reason <- case_when(
    segment_i$segment_length_ft < numeric_noise_ft ~ "artifact_topology_noise",
    segment_i$short_segment & !isTRUE(two_sided_all_offsets) ~ "artifact_wrong_ward_offsets",
    TRUE ~ "none"
  )
  valid_segment <- invalid_reason == "none"
  analysis_segment_id <- case_when(
    !valid_segment ~ NA_character_,
    component_type == "terminal_touching_remainder" ~ touching_segment_id,
    TRUE ~ segment_i$raw_segment_id
  )
  merge_reason <- case_when(
    !valid_segment ~ "invalid_no_analysis_segment",
    component_type == "terminal_touching_remainder" ~ "touching_terminal_remainder",
    TRUE ~ "standalone_valid_component"
  )

  rows[[i]] <- bind_cols(
    st_drop_geometry(segment_i) %>%
      select(
        era, raw_segment_id, ward_pair_id, ward_a, ward_b, segment_number,
        n_segments_in_pair, segment_length_ft, target_length_ft
      ),
    tibble(
      short_segment = segment_i$short_segment,
      terminal_segment = segment_i$terminal_segment,
      component_type = component_type,
      nearest_same_pair_segment_id = nearest_segment_id,
      nearest_same_pair_segment_number = nearest_segment_number,
      nearest_same_pair_distance_ft = nearest_distance_ft,
      n_touching_same_pair_1ft = n_touching_1ft,
      n_near_same_pair_10ft = n_near_10ft,
      two_sided_all_offsets = two_sided_all_offsets,
      valid_segment = valid_segment,
      invalid_reason = invalid_reason,
      analysis_segment_id = analysis_segment_id,
      merge_reason = merge_reason
    ),
    as_tibble(offset_values)
  )
}

segment_audit <- bind_rows(rows)

confound_flags <- read_csv("../input/confounded_segment_flags.csv", show_col_types = FALSE) %>%
  transmute(
    raw_segment_id = as.character(segment_id),
    drop_confound = as.logical(drop_confound),
    confound_drop_reason = as.character(drop_reason)
  )

segment_audit <- segment_audit %>%
  left_join(confound_flags, by = "raw_segment_id", relationship = "one-to-one") %>%
  mutate(
    drop_confound = coalesce(drop_confound, FALSE),
    confound_drop_reason = coalesce(confound_drop_reason, "none")
  ) %>%
  arrange(era, ward_pair_id, segment_number)

message("Counting density and sales support...")
parcels <- read_csv("../input/parcels_with_ward_distances.csv", show_col_types = FALSE) %>%
  filter(segment_reason == "matched")
sales <- read_csv("../input/sales_pre_scores_with_segments.csv", show_col_types = FALSE) %>%
  filter(segment_reason == "matched")

density_support <- count_support(
  parcels, "segment_id", "ward", "dist_to_boundary_m", "density", segment_audit
)
sales_support <- count_support(
  sales, "segment_id", "ward", "dist_m", "sales", segment_audit
)

segment_support <- bind_rows(density_support, sales_support) %>%
  arrange(dataset, bandwidth_ft, era, ward_pair_id, raw_segment_id)

invalid_segments <- segment_audit %>%
  filter(!valid_segment) %>%
  transmute(
    raw_segment_id,
    analysis_segment_id,
    invalid_reason,
    segment_ward_pair_id = ward_pair_id,
    segment_era = era
  )

invalid_density <- parcels %>%
  inner_join(invalid_segments, by = c("segment_id" = "raw_segment_id"), relationship = "many-to-one") %>%
  transmute(
    dataset = "density",
    observation_id = as.character(pin),
    raw_segment_id = segment_id,
    analysis_segment_id,
    ward_pair_id = segment_ward_pair_id,
    era = segment_era,
    invalid_reason,
    distance_m = dist_to_boundary_m
  )

invalid_sales <- sales %>%
  mutate(observation_id = paste(pin, sale_date, sale_price_nominal, sep = "_")) %>%
  inner_join(invalid_segments, by = c("segment_id" = "raw_segment_id"), relationship = "many-to-one") %>%
  transmute(
    dataset = "sales",
    observation_id,
    raw_segment_id = segment_id,
    analysis_segment_id,
    ward_pair_id = segment_ward_pair_id,
    era = segment_era,
    invalid_reason,
    distance_m = dist_m
  )

invalid_observations <- bind_rows(invalid_density, invalid_sales) %>%
  arrange(dataset, raw_segment_id, observation_id)

support_wide <- segment_support %>%
  select(dataset, bandwidth_ft, raw_segment_id, n_total, n_side_a, n_side_b, support_valid_main, support_valid_strict) %>%
  pivot_wider(
    names_from = c(dataset, bandwidth_ft),
    values_from = c(n_total, n_side_a, n_side_b, support_valid_main, support_valid_strict),
    values_fill = list(n_total = 0, n_side_a = 0, n_side_b = 0, support_valid_main = FALSE, support_valid_strict = FALSE)
  )

segment_audit <- segment_audit %>%
  left_join(support_wide, by = "raw_segment_id", relationship = "one-to-one")

segment_map <- segment_audit %>%
  st_drop_geometry() %>%
  select(
    raw_segment_id, analysis_segment_id, valid_segment, invalid_reason, merge_reason,
    component_type, era, ward_pair_id, ward_a, ward_b, segment_number,
    n_segments_in_pair, segment_length_ft, nearest_same_pair_segment_id,
    nearest_same_pair_distance_ft, two_sided_all_offsets, drop_confound,
    confound_drop_reason
  )

thresholds <- c(1, 10, 25, 50, 100, 200, 500, 1000, 1320)
short_summary <- bind_rows(lapply(thresholds, function(threshold_ft) {
  ids <- segment_audit %>%
    filter(segment_length_ft < threshold_ft) %>%
    pull(raw_segment_id)
  density_rows <- segment_support %>%
    filter(dataset == "density", bandwidth_ft == 500, raw_segment_id %in% ids) %>%
    summarise(n = sum(n_total, na.rm = TRUE)) %>%
    pull(n)
  sales_rows <- segment_support %>%
    filter(dataset == "sales", bandwidth_ft == 1000, raw_segment_id %in% ids) %>%
    summarise(n = sum(n_total, na.rm = TRUE)) %>%
    pull(n)

  tibble(
    threshold_ft = threshold_ft,
    n_raw_segments_below_threshold = length(ids),
    n_valid_below_threshold = sum(segment_audit$raw_segment_id %in% ids & segment_audit$valid_segment),
    n_artifact_below_threshold = sum(segment_audit$raw_segment_id %in% ids & !segment_audit$valid_segment),
    n_confounded_below_threshold = sum(segment_audit$raw_segment_id %in% ids & segment_audit$drop_confound),
    n_density_rows_within_500ft = density_rows,
    n_sales_rows_within_1000ft = sales_rows
  )
}))

summary_lines <- c(
  "# Segment validity audit",
  "",
  sprintf("Raw segments audited: %s", format(nrow(segment_audit), big.mark = ",")),
  sprintf("Segments shorter than %sft: %s", short_segment_ft, sum(segment_audit$segment_length_ft < short_segment_ft)),
  sprintf("Invalid segments: %s", sum(!segment_audit$valid_segment)),
  sprintf("Invalid assigned observations: %s", nrow(invalid_observations)),
  "",
  "Production rule:",
  "",
  "- Keep full raw boundary coverage in `raw_segment_id`.",
  "- Use `analysis_segment_id` for econometric segment cells.",
  "- Keep disconnected short components when they pass the two-sided ward-offset tests.",
  "- Merge only true touching terminal remainders into adjacent same-pair segments.",
  "- Drop/flag topology noise and short pieces that fail two-sided ward tests.",
  "- Apply support restrictions in the estimation sample; do not use length alone as the main exclusion rule.",
  "",
  "Invalid segment counts by reason:",
  paste(capture.output(print(segment_audit %>% count(invalid_reason, name = "n"))), collapse = "\n")
)

message("Writing audit outputs...")
write_csv(segment_audit %>% st_drop_geometry(), "../output/segment_validity_audit.csv")
write_csv(segment_map, "../output/segment_analysis_id_map.csv")
write_csv(segment_support, "../output/segment_support_audit.csv")
write_csv(short_summary, "../output/short_segment_summary.csv")
write_csv(invalid_observations, "../output/invalid_segment_observation_audit.csv")
writeLines(summary_lines, "../output/segment_validity_summary.md")

message("Done.")
