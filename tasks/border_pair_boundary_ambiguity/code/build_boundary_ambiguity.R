# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_boundary_ambiguity/code")
# bandwidths <- "76.2 152.4 304.8"
# samples <- "all multifamily"

library(sf)
library(readr)
library(dplyr)

source("../../_lib/canonical_geometry_helpers.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(bandwidths, samples)
}

if (length(args) != 2) {
  stop(
    "FATAL: Script requires args: <bandwidths> <samples>",
    call. = FALSE
  )
}

bandwidths <- as.numeric(strsplit(trimws(args[1]), "\\s+")[[1]])
samples <- strsplit(trimws(args[2]), "\\s+")[[1]]

if (any(!is.finite(bandwidths))) {
  stop("bandwidths must parse to numeric values.", call. = FALSE)
}
if (!all(samples %in% c("all", "multifamily"))) {
  stop("samples must be drawn from: all, multifamily", call. = FALSE)
}
parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    ward = suppressWarnings(as.integer(ward)),
    boundary_year = suppressWarnings(as.integer(boundary_year)),
    era = canonical_era_from_boundary_year(boundary_year)
  ) %>%
  filter(
    construction_year >= 2006,
    construction_year <= 2022,
    arealotsf > 1,
    areabuilding > 1,
    !is.na(ward),
    !is.na(ward_pair),
    !is.na(era)
  )

if (anyDuplicated(parcels[c("pin", "construction_year")]) > 0) {
  stop("Merged parcel file has duplicate pin-construction_year keys.", call. = FALSE)
}

geometry_lookup <- st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE) %>%
  transmute(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(yearbuilt)),
    geometry = geom
  )

if (anyDuplicated(st_drop_geometry(geometry_lookup)[c("pin", "construction_year")]) > 0) {
  stop("Geometry source has duplicate pin-construction_year keys.", call. = FALSE)
}

parcel_sf <- parcels %>%
  left_join(geometry_lookup, by = c("pin", "construction_year"), relationship = "many-to-one") %>%
  st_as_sf()

if (sum(is.na(st_geometry(parcel_sf))) > 0) {
  stop("Missing point geometry for some merged parcels.", call. = FALSE)
}

parcel_sf <- parcel_sf %>%
  filter(!st_is_empty(geometry))

boundaries <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")

parcel_sf$nearest_other_pair_dist_m <- Inf
parcel_sf$nearest_other_pair_id <- NA_character_
parcel_sf$n_other_pairs <- 0L

for (era_i in unique(parcel_sf$era)) {
  idx_era <- which(parcel_sf$era == era_i)
  lines_era <- boundaries[[era_i]]
  if (length(idx_era) == 0 || is.null(lines_era) || nrow(lines_era) == 0) {
    next
  }

  points_era <- parcel_sf[idx_era, ]
  if (st_crs(points_era) != st_crs(lines_era)) {
    points_era <- st_transform(points_era, st_crs(lines_era))
  }

  for (ward_i in unique(points_era$ward)) {
    idx_ward <- which(points_era$ward == ward_i)
    edges_ward <- lines_era %>%
      filter(ward_a == ward_i | ward_b == ward_i)

    if (length(idx_ward) == 0 || nrow(edges_ward) == 0) {
      next
    }

    for (pair_i in unique(points_era$ward_pair[idx_ward])) {
      idx_pair <- idx_ward[points_era$ward_pair[idx_ward] == pair_i]
      candidate_edges <- edges_ward %>%
        filter(ward_pair_id != pair_i)

      parcel_sf$n_other_pairs[idx_era[idx_pair]] <- nrow(candidate_edges)

      if (length(idx_pair) == 0 || nrow(candidate_edges) == 0) {
        next
      }

      distance_matrix <- st_distance(points_era[idx_pair, ], candidate_edges)
      min_distance <- numeric(nrow(distance_matrix))
      min_index <- integer(nrow(distance_matrix))
      for (row_i in seq_len(nrow(distance_matrix))) {
        distance_row <- as.numeric(distance_matrix[row_i, ])
        min_distance[[row_i]] <- min(distance_row, na.rm = TRUE)
        min_index[[row_i]] <- which.min(distance_row)
      }

      parcel_sf$nearest_other_pair_dist_m[idx_era[idx_pair]] <- min_distance * 0.3048
      parcel_sf$nearest_other_pair_id[idx_era[idx_pair]] <- as.character(candidate_edges$ward_pair_id[min_index])
    }
  }
}

if (!"dist_to_boundary_m" %in% names(parcel_sf)) {
  parcel_sf$dist_to_boundary_m <- as.numeric(parcel_sf$dist_to_boundary) * 0.3048
}

parcel_other_pair_distance <- parcel_sf %>%
  st_drop_geometry() %>%
  select(
    pin,
    construction_year,
    boundary_year,
    era,
    ward,
    ward_pair,
    dist_to_boundary,
    dist_to_boundary_m,
    unitscount,
    nearest_other_pair_dist_m,
    nearest_other_pair_id,
    n_other_pairs
  ) %>%
  mutate(
    dist_to_boundary_m = if_else(
      is.finite(dist_to_boundary_m),
      as.numeric(dist_to_boundary_m),
      as.numeric(dist_to_boundary) * 0.3048
    )
  )

if (any(parcel_other_pair_distance$nearest_other_pair_dist_m < parcel_other_pair_distance$dist_to_boundary_m, na.rm = TRUE)) {
  stop("Found nearest_other_pair_dist_m smaller than dist_to_boundary_m.", call. = FALSE)
}
if (any(parcel_other_pair_distance$nearest_other_pair_id == parcel_other_pair_distance$ward_pair, na.rm = TRUE)) {
  stop("Found nearest_other_pair_id equal to assigned ward_pair.", call. = FALSE)
}

write_csv(parcel_other_pair_distance, "../output/parcel_other_pair_distance.csv")

ambiguity_rows <- list()
for (sample_i in samples) {
  sample_df <- if (sample_i == "all") {
    parcel_other_pair_distance %>% filter(unitscount > 0)
  } else {
    parcel_other_pair_distance %>% filter(unitscount > 1)
  }

  for (bw_i in bandwidths) {
    in_bandwidth <- sample_df %>% filter(dist_to_boundary_m <= bw_i)

    ambiguity_rows[[length(ambiguity_rows) + 1]] <- tibble(
      sample_filter = sample_i,
      bandwidth_m = bw_i,
      n_in_bw = nrow(in_bandwidth),
      n_ambiguous = sum(in_bandwidth$nearest_other_pair_dist_m <= bw_i, na.rm = TRUE),
      share_ambiguous = mean(in_bandwidth$nearest_other_pair_dist_m <= bw_i, na.rm = TRUE),
      median_other_pair_dist_m = median(in_bandwidth$nearest_other_pair_dist_m[is.finite(in_bandwidth$nearest_other_pair_dist_m)], na.rm = TRUE)
    )
  }
}

ambiguity_summary <- bind_rows(ambiguity_rows)

write_csv(ambiguity_summary, "../output/boundary_ambiguity_by_bw.csv")
