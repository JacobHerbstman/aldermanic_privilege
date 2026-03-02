source("../../setup_environment/code/packages.R")

library(data.table)
library(sf)
library(ggplot2)

segments_gpkg <- "../input/boundary_segments_1320ft.gpkg"
ward_panel_gpkg <- "../input/ward_panel.gpkg"
flags_csv <- "../input/confounded_pair_era_flags.csv"
parcels_csv <- "../input/parcels_with_ward_distances.csv"
invalid_sales_csv <- "../input/border_verification_invalid_pairs_sales.csv"
invalid_rent_csv <- "../input/border_verification_invalid_pairs_rent.csv"
water_shp <- "../input/gis_osm_water_a_free_1.shp"
landuse_shp <- "../input/gis_osm_landuse_a_free_1.shp"
out_dir <- "../output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

stopifnot(
  file.exists(segments_gpkg),
  file.exists(ward_panel_gpkg),
  file.exists(flags_csv),
  file.exists(parcels_csv),
  file.exists(invalid_sales_csv),
  file.exists(invalid_rent_csv),
  file.exists(water_shp),
  file.exists(landuse_shp)
)

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  x <- x[grepl("^[0-9]+-[0-9]+$", x)]
  if (length(x) == 0) return(character())
  parts <- strsplit(x, "-", fixed = TRUE)
  vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) return(NA_character_)
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
}

era_from_year <- function(y) {
  fifelse(
    y < 2003L, "1998_2002",
    fifelse(y < 2015L, "2003_2014",
      fifelse(y < 2023L, "2015_2023", "post_2023")
    )
  )
}

eras <- c("1998_2002", "2003_2014", "2015_2023", "post_2023")
map_eras <- c("2003_2014", "2015_2023", "post_2023")
map_era_labels <- c(
  "2003_2014" = "2003-2015",
  "2015_2023" = "2015-2023",
  "post_2023" = "2023-present"
)

flags <- fread(flags_csv)
flags[, pair_dash := normalize_pair_dash(ward_pair_id_dash)]
flags[, era := as.character(era)]
flags[, drop_confound := as.logical(drop_confound)]
flags[is.na(drop_confound), drop_confound := FALSE]
flags[is.na(drop_reason) | drop_reason == "", drop_reason := "none"]
flags <- unique(flags[, .(
  pair_dash, era, drop_confound, drop_reason,
  share_park_water_length, arterial_overlap_share
)])
flags[, park_margin := as.numeric(share_park_water_length) - 0.50]
flags[, arterial_margin := as.numeric(arterial_overlap_share) - 0.70]
flags[, park_margin_pos := fifelse(is.finite(park_margin) & park_margin >= 0, park_margin, Inf)]
flags[, arterial_margin_pos := fifelse(is.finite(arterial_margin) & arterial_margin >= 0, arterial_margin, Inf)]
flags[, drop_margin := pmin(park_margin_pos, arterial_margin_pos)]
flags[, borderline_drop := drop_confound &
  ((is.finite(park_margin) & park_margin >= 0 & park_margin <= 0.05) |
    (is.finite(arterial_margin) & arterial_margin >= 0 & arterial_margin <= 0.05))]
flags[, high_conf_drop := drop_confound &
  ((is.finite(share_park_water_length) & share_park_water_length >= 0.70) |
    (is.finite(arterial_overlap_share) & arterial_overlap_share >= 0.85))]
flags[, near_keep_threshold := !drop_confound &
  ((is.finite(share_park_water_length) & (0.50 - share_park_water_length) <= 0.03 & (0.50 - share_park_water_length) >= 0) |
    (is.finite(arterial_overlap_share) & (0.70 - arterial_overlap_share) <= 0.03 & (0.70 - arterial_overlap_share) >= 0))]
flags[, confidence_tag := fifelse(
  high_conf_drop, "drop_high_conf",
  fifelse(
    borderline_drop, "drop_borderline",
    fifelse(
      drop_confound, "drop_moderate",
      fifelse(near_keep_threshold, "keep_near_threshold", "keep_far_threshold")
    )
  )
)]

parcels <- fread(parcels_csv, select = c("ward_pair", "construction_year", "strictness_own", "strictness_neighbor"))
parcels[, construction_year := as.integer(construction_year)]
parcels[, pair_dash := normalize_pair_dash(ward_pair)]
parcels[, era := era_from_year(construction_year)]
parcels <- parcels[
  !is.na(pair_dash) &
    !is.na(era) &
    is.finite(strictness_own) &
    is.finite(strictness_neighbor)
]
gap <- parcels[, .(
  strictness_gap = median(abs(strictness_own - strictness_neighbor), na.rm = TRUE),
  gap_obs = .N
), by = .(pair_dash, era)]

read_line_layer <- function(era_name) {
  x <- st_read(segments_gpkg, layer = era_name, quiet = TRUE)
  x$pair_dash <- normalize_pair_dash(x$ward_pair_id)
  x$era <- era_name
  x
}

lines_list <- lapply(eras, read_line_layer)
segments <- do.call(rbind, lines_list)

segments <- merge(
  segments,
  flags,
  by = c("pair_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
segments <- merge(
  segments,
  gap,
  by = c("pair_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)

segments$drop_confound[is.na(segments$drop_confound)] <- FALSE
segments$drop_reason[is.na(segments$drop_reason) | segments$drop_reason == ""] <- "none"
segments$status <- ifelse(segments$drop_confound, "Dropped", "Kept")
segments$era <- factor(segments$era, levels = eras)
segments_ll <- st_transform(segments, 4326)

ward_panel <- st_read(ward_panel_gpkg, quiet = TRUE)
ward_panel <- st_transform(ward_panel, st_crs(segments))
ward_year_ref <- max(as.integer(ward_panel$year), na.rm = TRUE)
city_outline_geom <- st_boundary(
  st_union(st_geometry(ward_panel[as.integer(ward_panel$year) == ward_year_ref, ]))
)
city_outline <- st_sf(
  era = map_eras,
  geometry = st_sfc(
    lapply(map_eras, function(x) city_outline_geom[[1]]),
    crs = st_crs(ward_panel)
  )
)
city_outline_ll <- st_transform(city_outline, 4326)

status_map_pdf <- file.path(out_dir, "pruned_boundaries_status_by_era.pdf")
all_segments_map_pdf <- file.path(out_dir, "pruned_boundaries_all_segments_by_era.pdf")
kept_segments_map_pdf <- file.path(out_dir, "pruned_boundaries_kept_segments_by_era.pdf")
gap_map_pdf <- file.path(out_dir, "pruned_boundaries_kept_strictness_gap_by_era.pdf")
reason_map_pdf <- file.path(out_dir, "pruned_boundaries_drop_reason_by_era.pdf")
summary_csv <- file.path(out_dir, "pruned_boundaries_map_summary.csv")
zoom_lakeview_status_pdf <- file.path(out_dir, "pruned_boundaries_zoom_lakeview_status.pdf")
zoom_lakeview_reason_pdf <- file.path(out_dir, "pruned_boundaries_zoom_lakeview_drop_reason.pdf")
zoom_downtown_status_pdf <- file.path(out_dir, "pruned_boundaries_zoom_downtown_status.pdf")
zoom_downtown_reason_pdf <- file.path(out_dir, "pruned_boundaries_zoom_downtown_drop_reason.pdf")
zoom_lakeview_pairs_csv <- file.path(out_dir, "pruned_boundaries_zoom_lakeview_pairs.csv")
zoom_downtown_pairs_csv <- file.path(out_dir, "pruned_boundaries_zoom_downtown_pairs.csv")
audit_pair_csv <- file.path(out_dir, "pruned_boundaries_audit_pair_era.csv")
audit_borderline_csv <- file.path(out_dir, "pruned_boundaries_audit_borderline_drop.csv")
audit_unchecked_csv <- file.path(out_dir, "pruned_boundaries_audit_unchecked_pair_era.csv")
audit_extra_flags_csv <- file.path(out_dir, "pruned_boundaries_audit_extra_flag_pair_era.csv")
segment_feature_audit_csv <- file.path(out_dir, "segment_feature_distance_audit_1320.csv")
zoom_lakeview_feature_context_csv <- file.path(out_dir, "pruned_boundaries_zoom_lakeview_feature_context.csv")
zoom_downtown_feature_context_csv <- file.path(out_dir, "pruned_boundaries_zoom_downtown_feature_context.csv")
uncertainty_priority_csv <- file.path(out_dir, "pruned_boundaries_uncertainty_priority.csv")
audit_md <- file.path(out_dir, "pruned_boundaries_audit.md")

summary_dt <- as.data.table(st_drop_geometry(segments))[, .(
  n_segments = .N,
  total_length_ft = sum(segment_length_ft, na.rm = TRUE),
  mean_strictness_gap = mean(strictness_gap, na.rm = TRUE),
  median_strictness_gap = median(strictness_gap, na.rm = TRUE),
  share_missing_gap = mean(!is.finite(strictness_gap))
), by = .(era, status)]
setorder(summary_dt, era, status)
fwrite(summary_dt, summary_csv)

# Segment index parity alternates along each ward-pair boundary and makes
# line-segment partitioning visible in citywide maps.
segments_ll$segment_parity <- factor(
  ifelse(as.integer(segments_ll$segment_number) %% 2L == 0L, "Even segment", "Odd segment"),
  levels = c("Odd segment", "Even segment")
)
segments_ll_map <- segments_ll[as.character(segments_ll$era) %in% map_eras, ]
segments_ll_map$era_facet <- factor(
  unname(map_era_labels[as.character(segments_ll_map$era)]),
  levels = unname(map_era_labels[map_eras])
)
city_outline_ll$era_facet <- factor(
  unname(map_era_labels[as.character(city_outline_ll$era)]),
  levels = unname(map_era_labels[map_eras])
)

p_all_segments <- ggplot() +
  geom_sf(
    data = city_outline_ll,
    color = "#111111",
    linewidth = 0.40,
    inherit.aes = FALSE
  ) +
  geom_sf(data = segments_ll_map, aes(color = segment_parity), linewidth = 0.25, alpha = 0.95) +
  facet_wrap(~era_facet, ncol = 3) +
  scale_color_manual(values = c("Odd segment" = "#1f77b4", "Even segment" = "#ff7f0e")) +
  labs(
    title = "Ward Boundaries Divided into Segments",
    subtitle = "Alternating segment parity highlights segment-level FE geography",
    color = NULL
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave(all_segments_map_pdf, p_all_segments, width = 11, height = 8.5, dpi = 300)

kept_segments_ll <- segments_ll[segments_ll$status == "Kept", ]
p_kept_segments <- ggplot(kept_segments_ll) +
  geom_sf(aes(color = segment_parity), linewidth = 0.25, alpha = 0.95) +
  facet_wrap(~era, ncol = 2) +
  scale_color_manual(values = c("Odd segment" = "#1f77b4", "Even segment" = "#ff7f0e")) +
  labs(
    title = "Pruned Sample: Remaining Boundary Segments",
    subtitle = "Chicago-wide map after pair-era pruning; segment geography shown using index parity",
    color = NULL
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave(kept_segments_map_pdf, p_kept_segments, width = 11, height = 8.5, dpi = 300)

p_status <- ggplot(segments_ll) +
  geom_sf(aes(color = status), linewidth = 0.2, alpha = 0.9) +
  facet_wrap(~era, ncol = 2) +
  scale_color_manual(values = c("Kept" = "#1f77b4", "Dropped" = "#d62728")) +
  labs(
    title = "Pruned Boundary Segments by Era",
    subtitle = "Kept vs dropped under confound pruning rule",
    color = NULL
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave(status_map_pdf, p_status, width = 11, height = 8.5, dpi = 300)

kept <- segments_ll[segments_ll$status == "Kept", ]
p_gap <- ggplot(kept) +
  geom_sf(aes(color = strictness_gap), linewidth = 0.2, alpha = 0.9) +
  facet_wrap(~era, ncol = 2) +
  scale_color_viridis_c(option = "C", na.value = "grey80") +
  labs(
    title = "Kept Boundary Segments: Strictness Gap Variation",
    subtitle = "Color = pair-era median |strictness_own - strictness_neighbor|",
    color = "Gap"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave(gap_map_pdf, p_gap, width = 11, height = 8.5, dpi = 300)

dropped <- segments_ll[segments_ll$status == "Dropped", ]
p_reason <- ggplot(dropped) +
  geom_sf(aes(color = drop_reason), linewidth = 0.22, alpha = 0.95) +
  facet_wrap(~era, ncol = 2) +
  labs(
    title = "Dropped Boundary Segments by Drop Reason",
    subtitle = "Reason inherited from pair-era pruning flags",
    color = "Drop reason"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )
ggsave(reason_map_pdf, p_reason, width = 11, height = 8.5, dpi = 300)

make_zoom <- function(seg_ll, bounds, title_text, out_status, out_reason, out_pairs) {
  bb <- st_bbox(c(
    xmin = bounds[1], ymin = bounds[2],
    xmax = bounds[3], ymax = bounds[4]
  ), crs = st_crs(4326))
  z <- st_crop(seg_ll, bb)

  pz_status <- ggplot(z) +
    geom_sf(aes(color = status), linewidth = 0.45, alpha = 0.95) +
    facet_wrap(~era, ncol = 2) +
    scale_color_manual(values = c("Kept" = "#1f77b4", "Dropped" = "#d62728")) +
    labs(
      title = sprintf("%s: kept vs dropped boundaries", title_text),
      subtitle = "Zoom view for manual verification",
      color = NULL
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
  ggsave(out_status, pz_status, width = 11, height = 8.5, dpi = 300)

  pz_reason <- ggplot(z[z$status == "Dropped", ]) +
    geom_sf(aes(color = drop_reason), linewidth = 0.55, alpha = 0.98) +
    facet_wrap(~era, ncol = 2) +
    labs(
      title = sprintf("%s: dropped boundaries by reason", title_text),
      subtitle = "Only dropped segments shown",
      color = "Drop reason"
    ) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom"
    )
  ggsave(out_reason, pz_reason, width = 11, height = 8.5, dpi = 300)

  z_dt <- as.data.table(st_drop_geometry(z))
  pairs <- z_dt[, .(
    n_segments = .N,
    dropped_segments = sum(status == "Dropped"),
    drop_share_segments = mean(status == "Dropped"),
    strictness_gap = median(strictness_gap, na.rm = TRUE),
    share_park_water_length = max(share_park_water_length, na.rm = TRUE),
    arterial_overlap_share = max(arterial_overlap_share, na.rm = TRUE),
    drop_reason = paste(sort(unique(drop_reason[status == "Dropped"])), collapse = "|")
  ), by = .(pair_dash, era)]
  pairs[drop_reason == "", drop_reason := "none"]
  setorder(pairs, -drop_share_segments, pair_dash, era)
  fwrite(pairs, out_pairs)
  pairs[]
}

lakeview_pairs <- make_zoom(
  segments_ll,
  c(-87.68, 41.92, -87.62, 41.97),
  "Lakeview",
  zoom_lakeview_status_pdf,
  zoom_lakeview_reason_pdf,
  zoom_lakeview_pairs_csv
)

downtown_pairs <- make_zoom(
  segments_ll,
  c(-87.66, 41.86, -87.60, 41.91),
  "Downtown / Near North",
  zoom_downtown_status_pdf,
  zoom_downtown_reason_pdf,
  zoom_downtown_pairs_csv
)

top_weighted_name <- function(names_vec, weights_vec, k = 3L) {
  dt <- data.table(
    name = as.character(names_vec),
    weight = as.numeric(weights_vec)
  )
  dt[is.na(name) | name == "", name := "(unnamed)"]
  dt[!is.finite(weight), weight := 0]
  top <- dt[, .(weight = sum(weight, na.rm = TRUE)), by = name][order(-weight)][seq_len(min(.N, k))]
  paste(top$name, collapse = " | ")
}

park_landuse_classes <- c(
  "park", "grass", "recreation_ground", "cemetery",
  "golf_course", "forest", "meadow", "allotments"
)

water_sf <- st_read(water_shp, quiet = TRUE)
water_sf <- st_transform(water_sf, st_crs(segments))
landuse_sf <- st_read(landuse_shp, quiet = TRUE)
landuse_sf <- st_transform(landuse_sf, st_crs(segments))
park_sf <- landuse_sf[tolower(as.character(landuse_sf$fclass)) %in% park_landuse_classes, ]

if (nrow(water_sf) == 0 || nrow(park_sf) == 0) {
  stop("Water/park layers are empty after reading/filtering.", call. = FALSE)
}

segment_core <- segments[, c(
  "segment_id", "ward_pair_id", "pair_dash", "era", "status", "segment_type",
  "segment_length_ft", "nearest_street_name", "water_area_share", "park_area_share",
  "waterway_overlap_ft"
)]
idx_water <- st_nearest_feature(segment_core, water_sf)
idx_park <- st_nearest_feature(segment_core, park_sf)
segment_feature_dt <- as.data.table(st_drop_geometry(segment_core))
segment_feature_dt[, nearest_water_fclass := as.character(water_sf$fclass[idx_water])]
segment_feature_dt[, nearest_water_name := as.character(water_sf$name[idx_water])]
segment_feature_dt[, nearest_park_fclass := as.character(park_sf$fclass[idx_park])]
segment_feature_dt[, nearest_park_name := as.character(park_sf$name[idx_park])]
segment_feature_dt[, dist_to_water_ft := as.numeric(st_distance(segment_core, water_sf[idx_water, ], by_element = TRUE))]
segment_feature_dt[, dist_to_park_ft := as.numeric(st_distance(segment_core, park_sf[idx_park, ], by_element = TRUE))]
segment_feature_dt[, ward_pair_id_dash := normalize_pair_dash(ward_pair_id)]
setorder(segment_feature_dt, era, ward_pair_id_dash, segment_id)
fwrite(segment_feature_dt, segment_feature_audit_csv)

build_feature_context <- function(pairs_dt, out_csv) {
  drop_pairs <- pairs_dt[drop_share_segments > 0, .(
    pair_dash, era,
    pair_drop_reason = drop_reason,
    pair_share_park_water_length = share_park_water_length,
    pair_arterial_overlap_share = arterial_overlap_share
  )]
  ctx <- merge(segment_feature_dt, drop_pairs, by = c("pair_dash", "era"), all = FALSE)
  if (nrow(ctx) == 0) {
    fwrite(data.table(), out_csv)
    return(invisible(NULL))
  }
  out <- ctx[, .(
    n_segments = .N,
    total_length_ft = sum(segment_length_ft, na.rm = TRUE),
    drop_reason = first(pair_drop_reason),
    share_park_water_length = first(pair_share_park_water_length),
    arterial_overlap_share = first(pair_arterial_overlap_share),
    med_dist_to_water_ft = median(dist_to_water_ft, na.rm = TRUE),
    med_dist_to_park_ft = median(dist_to_park_ft, na.rm = TRUE),
    p90_dist_to_water_ft = quantile(dist_to_water_ft, 0.9, na.rm = TRUE),
    p90_dist_to_park_ft = quantile(dist_to_park_ft, 0.9, na.rm = TRUE),
    share_segments_within_250ft_water = mean(dist_to_water_ft <= 250, na.rm = TRUE),
    share_segments_within_250ft_park = mean(dist_to_park_ft <= 250, na.rm = TRUE),
    top_nearest_streets = top_weighted_name(nearest_street_name, segment_length_ft),
    top_nearest_water_names = top_weighted_name(nearest_water_name, segment_length_ft),
    top_nearest_water_fclass = top_weighted_name(nearest_water_fclass, segment_length_ft),
    top_nearest_park_names = top_weighted_name(nearest_park_name, segment_length_ft),
    top_nearest_park_fclass = top_weighted_name(nearest_park_fclass, segment_length_ft)
  ), by = .(pair_dash, era)]
  setorder(out, era, pair_dash)
  fwrite(out, out_csv)
}

build_feature_context(lakeview_pairs, zoom_lakeview_feature_context_csv)
build_feature_context(downtown_pairs, zoom_downtown_feature_context_csv)

seg_pairs <- unique(as.data.table(st_drop_geometry(segments))[, .(pair_dash, era)])
flag_pairs <- unique(flags[, .(pair_dash, era)])
unchecked_pairs <- fsetdiff(seg_pairs, flag_pairs)
extra_flag_pairs <- fsetdiff(flag_pairs, seg_pairs)
fwrite(unchecked_pairs, audit_unchecked_csv)
fwrite(extra_flag_pairs, audit_extra_flags_csv)

audit_pair <- copy(flags)
audit_pair[, keep_margin := pmin(
  fifelse(is.finite(park_margin) & park_margin < 0, -park_margin, Inf),
  fifelse(is.finite(arterial_margin) & arterial_margin < 0, -arterial_margin, Inf)
)]
setorder(audit_pair, era, pair_dash)
audit_pair_out <- copy(audit_pair)
for (cc in c("park_margin_pos", "arterial_margin_pos", "drop_margin", "keep_margin")) {
  audit_pair_out[!is.finite(get(cc)), (cc) := NA_real_]
}
fwrite(audit_pair_out, audit_pair_csv)

borderline <- audit_pair[borderline_drop == TRUE]
setorder(borderline, drop_margin, era, pair_dash)
fwrite(borderline, audit_borderline_csv)

feature_pair_summary <- segment_feature_dt[, .(
  n_segments = .N,
  total_length_ft = sum(segment_length_ft, na.rm = TRUE),
  median_dist_to_water_ft = median(dist_to_water_ft, na.rm = TRUE),
  median_dist_to_park_ft = median(dist_to_park_ft, na.rm = TRUE),
  share_segments_within_250ft_water = mean(dist_to_water_ft <= 250, na.rm = TRUE),
  share_segments_within_250ft_park = mean(dist_to_park_ft <= 250, na.rm = TRUE),
  top_nearest_streets = top_weighted_name(nearest_street_name, segment_length_ft)
), by = .(pair_dash, era)]

uncertainty_priority <- merge(
  audit_pair[borderline_drop == TRUE | near_keep_threshold == TRUE, .(
    pair_dash, era, drop_confound, drop_reason, confidence_tag, drop_margin, keep_margin,
    share_park_water_length, arterial_overlap_share
  )],
  feature_pair_summary,
  by = c("pair_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
uncertainty_priority[, priority_margin := fifelse(drop_confound, drop_margin, keep_margin)]
setorder(
  uncertainty_priority,
  confidence_tag,
  priority_margin,
  era,
  pair_dash
)
uncertainty_priority[, priority_margin := NULL]
expected_uncertainty_n <- uniqueN(audit_pair[borderline_drop == TRUE | near_keep_threshold == TRUE, .(pair_dash, era)])
if (nrow(uncertainty_priority) != expected_uncertainty_n) {
  stop("Uncertainty priority coverage mismatch.", call. = FALSE)
}
uncertainty_priority_out <- copy(uncertainty_priority)
for (cc in c("drop_margin", "keep_margin")) {
  uncertainty_priority_out[!is.finite(get(cc)), (cc) := NA_real_]
}
fwrite(uncertainty_priority_out, uncertainty_priority_csv)

invalid_sales_n <- nrow(fread(invalid_sales_csv))
invalid_rent_n <- nrow(fread(invalid_rent_csv))
drop_counts <- audit_pair[, .N, by = .(confidence_tag)]
setorder(drop_counts, -N)
borderline_high_conf_n <- nrow(audit_pair[borderline_drop == TRUE & high_conf_drop == TRUE])

md_lines <- c(
  "# Pruning Audit Notes",
  "",
  sprintf("- generated: %s", as.character(Sys.time())),
  sprintf("- pair-era in segment layers: %d", nrow(seg_pairs)),
  sprintf("- pair-era in pruning flags: %d", nrow(flag_pairs)),
  sprintf("- pair-era missing flags (unchecked): %d", nrow(unchecked_pairs)),
  sprintf("- extra flag pair-era not in geometry: %d", nrow(extra_flag_pairs)),
  sprintf("- invalid-pair rows available (sales): %d", invalid_sales_n),
  sprintf("- invalid-pair rows available (rent): %d", invalid_rent_n),
  "",
  "## Confidence Tiers",
  "",
  "| Tier | Count |",
  "|---|---:|"
)
for (i in seq_len(nrow(drop_counts))) {
  md_lines <- c(md_lines, sprintf("| %s | %d |", drop_counts$confidence_tag[i], drop_counts$N[i]))
}
md_lines <- c(
  md_lines,
  "",
  "## Borderline Drops",
  sprintf("- Borderline dropped pair-era count (within 0.05 of threshold): %d", nrow(borderline)),
  "- See `pruned_boundaries_audit_borderline_drop.csv` for pair-level review.",
  sprintf("- Borderline rows also tagged high-confidence: %d", borderline_high_conf_n),
  sprintf("- Uncertainty review rows (borderline drops + near-threshold keeps): %d", nrow(uncertainty_priority)),
  "- See `pruned_boundaries_uncertainty_priority.csv` and neighborhood feature-context CSVs.",
  "",
  "## Neighborhood Verification Files",
  "- `pruned_boundaries_zoom_lakeview_status.pdf`",
  "- `pruned_boundaries_zoom_lakeview_drop_reason.pdf`",
  "- `pruned_boundaries_zoom_downtown_status.pdf`",
  "- `pruned_boundaries_zoom_downtown_drop_reason.pdf`",
  "- `pruned_boundaries_zoom_lakeview_pairs.csv`",
  "- `pruned_boundaries_zoom_downtown_pairs.csv`",
  "- `pruned_boundaries_zoom_lakeview_feature_context.csv`",
  "- `pruned_boundaries_zoom_downtown_feature_context.csv`",
  "- `segment_feature_distance_audit_1320.csv`"
)
writeLines(md_lines, audit_md)

message("Saved:")
message(sprintf("  - %s", status_map_pdf))
message(sprintf("  - %s", gap_map_pdf))
message(sprintf("  - %s", reason_map_pdf))
message(sprintf("  - %s", summary_csv))
message(sprintf("  - %s", zoom_lakeview_status_pdf))
message(sprintf("  - %s", zoom_lakeview_reason_pdf))
message(sprintf("  - %s", zoom_downtown_status_pdf))
message(sprintf("  - %s", zoom_downtown_reason_pdf))
message(sprintf("  - %s", zoom_lakeview_pairs_csv))
message(sprintf("  - %s", zoom_downtown_pairs_csv))
message(sprintf("  - %s", zoom_lakeview_feature_context_csv))
message(sprintf("  - %s", zoom_downtown_feature_context_csv))
message(sprintf("  - %s", segment_feature_audit_csv))
message(sprintf("  - %s", audit_pair_csv))
message(sprintf("  - %s", audit_borderline_csv))
message(sprintf("  - %s", uncertainty_priority_csv))
message(sprintf("  - %s", audit_unchecked_csv))
message(sprintf("  - %s", audit_extra_flags_csv))
message(sprintf("  - %s", audit_md))
