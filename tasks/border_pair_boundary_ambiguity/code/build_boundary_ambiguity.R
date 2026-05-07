library(sf)
library(readr)
library(dplyr)
library(ggplot2)

source("../../_lib/canonical_geometry_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_boundary_ambiguity/code")
# parcels_input <- "../input/parcels_with_ward_distances.csv"
# geometry_input <- "../input/geocoded_residential_data.gpkg"
# boundary_input <- "../input/ward_pair_boundaries.gpkg"
# parcel_output <- "../output/parcel_other_pair_distance.csv"
# summary_output <- "../output/boundary_ambiguity_by_bw.csv"
# top_pairs_output <- "../output/boundary_ambiguity_top_pairs.csv"
# plot_output <- "../output/boundary_ambiguity_share.pdf"
# bandwidths <- "164 246 328 410 492 574 656 738 820 902 984"
# samples <- "all multifamily"
# axis_units <- "meters"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    parcels_input,
    geometry_input,
    boundary_input,
    parcel_output,
    summary_output,
    top_pairs_output,
    plot_output,
    bandwidths,
    samples,
    axis_units
  )
}

if (!length(args) %in% c(9, 10)) {
  stop(
    "FATAL: Script requires args: <parcels_input> <geometry_input> <boundary_input> <parcel_output> <summary_output> <top_pairs_output> <plot_output> <bandwidths> <samples> [<axis_units>]",
    call. = FALSE
  )
}

parcels_input <- args[1]
geometry_input <- args[2]
boundary_input <- args[3]
parcel_output <- args[4]
summary_output <- args[5]
top_pairs_output <- args[6]
plot_output <- args[7]
bandwidths <- as.integer(strsplit(trimws(args[8]), "\\s+")[[1]])
samples <- strsplit(trimws(args[9]), "\\s+")[[1]]
axis_units <- ifelse(length(args) >= 10, args[10], "meters")

if (any(!is.finite(bandwidths))) {
  stop("bandwidths must parse to integers.", call. = FALSE)
}
if (!all(samples %in% c("all", "multifamily"))) {
  stop("samples must be drawn from: all, multifamily", call. = FALSE)
}
if (!axis_units %in% c("meters", "feet")) {
  stop("axis_units must be one of: meters, feet", call. = FALSE)
}

parcels <- read_csv(parcels_input, show_col_types = FALSE) %>%
  mutate(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(construction_year)),
    ward = suppressWarnings(as.integer(ward)),
    boundary_year = suppressWarnings(as.integer(boundary_year)),
    era = canonical_era_from_boundary_year(boundary_year)
  ) %>%
  filter(
    construction_year >= 2006,
    arealotsf > 1,
    areabuilding > 1,
    !is.na(ward),
    !is.na(ward_pair),
    !is.na(era)
  )

if (anyDuplicated(parcels[c("pin", "construction_year")]) > 0) {
  stop("Merged parcel file has duplicate pin-construction_year keys.", call. = FALSE)
}

geometry_lookup <- st_read(geometry_input, quiet = TRUE) %>%
  transmute(
    pin = as.character(pin),
    construction_year = suppressWarnings(as.integer(yearbuilt)),
    geometry = geom
  )

if (anyDuplicated(st_drop_geometry(geometry_lookup)[c("pin", "construction_year")]) > 0) {
  stop("Geometry source has duplicate pin-construction_year keys.", call. = FALSE)
}

parcel_sf <- parcels %>%
  left_join(geometry_lookup, by = c("pin", "construction_year")) %>%
  st_as_sf()

if (sum(is.na(st_geometry(parcel_sf))) > 0) {
  stop("Missing point geometry for some merged parcels.", call. = FALSE)
}

parcel_sf <- parcel_sf %>%
  filter(!st_is_empty(geometry))

boundaries <- load_boundary_layers(boundary_input)

parcel_sf$nearest_other_pair_dist_ft <- Inf
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
      min_distance <- apply(distance_matrix, 1, function(v) min(as.numeric(v), na.rm = TRUE))
      min_index <- apply(distance_matrix, 1, which.min)

      parcel_sf$nearest_other_pair_dist_ft[idx_era[idx_pair]] <- min_distance
      parcel_sf$nearest_other_pair_id[idx_era[idx_pair]] <- as.character(candidate_edges$ward_pair_id[min_index])
    }
  }
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
    unitscount,
    nearest_other_pair_dist_ft,
    nearest_other_pair_id,
    n_other_pairs
  )

if (any(parcel_other_pair_distance$nearest_other_pair_dist_ft < parcel_other_pair_distance$dist_to_boundary, na.rm = TRUE)) {
  stop("Found nearest_other_pair_dist_ft smaller than dist_to_boundary.", call. = FALSE)
}
if (any(parcel_other_pair_distance$nearest_other_pair_id == parcel_other_pair_distance$ward_pair, na.rm = TRUE)) {
  stop("Found nearest_other_pair_id equal to assigned ward_pair.", call. = FALSE)
}

write_csv(parcel_other_pair_distance, parcel_output)

ambiguity_summary <- bind_rows(lapply(samples, function(sample_i) {
  sample_df <- if (sample_i == "all") {
    parcel_other_pair_distance %>% filter(unitscount > 0)
  } else {
    parcel_other_pair_distance %>% filter(unitscount > 1)
  }

  bind_rows(lapply(bandwidths, function(bw_i) {
    in_bandwidth <- sample_df %>% filter(dist_to_boundary <= bw_i)

    tibble(
      sample_filter = sample_i,
      bw_ft = bw_i,
      n_in_bw = nrow(in_bandwidth),
      n_ambiguous = sum(in_bandwidth$nearest_other_pair_dist_ft <= bw_i, na.rm = TRUE),
      share_ambiguous = mean(in_bandwidth$nearest_other_pair_dist_ft <= bw_i, na.rm = TRUE),
      median_other_pair_dist_ft = median(in_bandwidth$nearest_other_pair_dist_ft[is.finite(in_bandwidth$nearest_other_pair_dist_ft)], na.rm = TRUE)
    )
  }))
}))

write_csv(ambiguity_summary, summary_output)

ambiguity_top_pairs <- bind_rows(lapply(samples, function(sample_i) {
  sample_df <- if (sample_i == "all") {
    parcel_other_pair_distance %>% filter(unitscount > 0)
  } else {
    parcel_other_pair_distance %>% filter(unitscount > 1)
  }

  bind_rows(lapply(bandwidths, function(bw_i) {
    sample_df %>%
      filter(dist_to_boundary <= bw_i, nearest_other_pair_dist_ft <= bw_i) %>%
      count(ward_pair, nearest_other_pair_id, sort = TRUE, name = "n_ambiguous") %>%
      mutate(sample_filter = sample_i, bw_ft = bw_i)
  }))
}))

write_csv(ambiguity_top_pairs, top_pairs_output)

ambiguity_plot <- ambiguity_summary %>%
  mutate(
    bw_display = if_else(axis_units == "meters", bw_ft * 0.3048, as.numeric(bw_ft)),
    sample_label = case_when(
      sample_filter == "all" ~ "All Construction",
      sample_filter == "multifamily" ~ "Multifamily"
    )
  ) %>%
  ggplot(aes(x = bw_display, y = share_ambiguous, color = sample_label, fill = sample_label)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c("All Construction" = "#1f77b4", "Multifamily" = "#d62728")) +
  scale_fill_manual(values = c("All Construction" = "#1f77b4", "Multifamily" = "#d62728")) +
  scale_x_continuous(breaks = pretty(bandwidths * ifelse(axis_units == "meters", 0.3048, 1), n = 8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Corner Ambiguity by Bandwidth",
    subtitle = "Share of parcels within the bandwidth of their assigned ward pair and another adjacent ward pair",
    x = ifelse(axis_units == "meters", "Bandwidth (m)", "Bandwidth (ft)"),
    y = "Ambiguity Share",
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold")
  )

ggsave(plot_output, ambiguity_plot, width = 8.4, height = 5.6, dpi = 300)

message(sprintf("Built %s", parcel_output))
message(sprintf("Built %s", summary_output))
message(sprintf("Built %s", top_pairs_output))
message(sprintf("Built %s", plot_output))
