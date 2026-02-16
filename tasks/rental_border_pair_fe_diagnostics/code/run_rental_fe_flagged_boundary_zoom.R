source("../../setup_environment/code/packages.R")

library(data.table)
library(arrow)
library(sf)
library(ggplot2)
library(optparse)
library(patchwork)

sf_use_s2(FALSE)

option_list <- list(
  make_option("--input", type = "character", default = "../input/rent_with_ward_distances.parquet"),
  make_option("--ward_panel", type = "character", default = "../input/ward_panel.gpkg"),
  make_option("--flags_csv", type = "character", default = "../output/rent_fe_lop_flags_top_boundaries.csv"),
  make_option("--bw_ft", type = "integer", default = 500),
  make_option("--window", type = "character", default = "pre_2023"),
  make_option("--top_n_per_spec", type = "integer", default = 8),
  make_option("--map_year", type = "integer", default = 2015),
  make_option("--zoom_margin_ft", type = "double", default = 1500),
  make_option("--point_sample_per_boundary", type = "integer", default = 6000),
  make_option("--output_dir", type = "character", default = "../output"),
  make_option("--smoke", type = "logical", default = FALSE)
)
opt <- parse_args(OptionParser(option_list = option_list))

if (!opt$window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!is.finite(opt$bw_ft) || opt$bw_ft <= 0) {
  stop("--bw_ft must be positive", call. = FALSE)
}
if (!is.finite(opt$top_n_per_spec) || opt$top_n_per_spec <= 0) {
  stop("--top_n_per_spec must be positive", call. = FALSE)
}
if (!dir.exists(opt$output_dir)) {
  dir.create(opt$output_dir, recursive = TRUE)
}

apply_window_filter <- function(dt, window_name) {
  if (window_name == "full") return(dt)
  if (window_name == "pre_covid") return(dt[year <= 2019])
  if (window_name == "pre_2021") return(dt[year <= 2020])
  if (window_name == "pre_2023") return(dt[year <= 2022])
  if (window_name == "drop_mid") return(dt[year <= 2020 | year >= 2024])
  dt
}

build_boundary_lines <- function(ward_sf) {
  ward_sf <- st_buffer(ward_sf, 0)
  adj <- st_touches(ward_sf)

  edges <- lapply(seq_along(adj), function(i) {
    nb <- adj[[i]]
    if (length(nb) == 0) return(NULL)
    nb <- nb[nb > i]
    if (length(nb) == 0) return(NULL)

    pieces <- lapply(nb, function(j) {
      shared <- suppressWarnings(st_intersection(st_geometry(ward_sf[i, ]), st_geometry(ward_sf[j, ])))
      if (length(shared) == 0 || all(st_is_empty(shared))) return(NULL)

      gtypes <- unique(as.character(st_geometry_type(shared)))
      lines <- if (all(gtypes %in% c("LINESTRING", "MULTILINESTRING"))) {
        st_cast(shared, "LINESTRING")
      } else if ("GEOMETRYCOLLECTION" %in% gtypes) {
        suppressWarnings(st_collection_extract(shared, "LINESTRING"))
      } else {
        return(NULL)
      }

      if (length(lines) == 0 || all(st_is_empty(lines))) return(NULL)
      lines <- lines[as.numeric(st_length(lines)) > 0]
      if (length(lines) == 0) return(NULL)

      st_sf(
        ward_a = as.integer(ward_sf$ward[i]),
        ward_b = as.integer(ward_sf$ward[j]),
        geometry = lines
      )
    })

    pieces <- pieces[!vapply(pieces, is.null, logical(1))]
    if (length(pieces) == 0) return(NULL)
    do.call(rbind, pieces)
  })

  edges <- edges[!vapply(edges, is.null, logical(1))]
  if (length(edges) == 0) {
    return(st_sf(ward_a = integer(), ward_b = integer(), pair_id = character(), geometry = st_sfc(), crs = st_crs(ward_sf)))
  }

  edges <- do.call(rbind, edges)
  edges <- st_as_sf(edges, crs = st_crs(ward_sf))
  edges$ward_lo <- pmin(edges$ward_a, edges$ward_b)
  edges$ward_hi <- pmax(edges$ward_a, edges$ward_b)
  edges$pair_id <- sprintf("%d-%d", edges$ward_lo, edges$ward_hi)
  edges <- edges[, c("pair_id", "ward_lo", "ward_hi", "geometry")]
  names(edges)[names(edges) == "ward_lo"] <- "ward_a"
  names(edges)[names(edges) == "ward_hi"] <- "ward_b"
  edges <- edges |>
    dplyr::group_by(pair_id, ward_a, ward_b) |>
    dplyr::summarise(geometry = st_union(geometry), .groups = "drop") |>
    st_as_sf()

  edges
}

safe_quantile <- function(x, p) {
  x <- x[is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, names = FALSE, na.rm = TRUE))
}

message("=== Flagged Boundary Zoom Drilldown ===")
message("input: ", opt$input)
message("ward_panel: ", opt$ward_panel)
message("flags_csv: ", opt$flags_csv)
message("output_dir: ", opt$output_dir)
message("smoke: ", opt$smoke)

flags <- as.data.table(fread(opt$flags_csv))
if (!"severe_flag" %in% names(flags)) {
  stop("flags_csv must include severe_flag", call. = FALSE)
}
flags <- flags[severe_flag == TRUE & !is.na(ward_pair_id)]
if (nrow(flags) == 0) {
  stop("No severe_flag rows found in flags_csv", call. = FALSE)
}

flags[, sort_abs_infl := fifelse(!is.na(abs_influence_exact), abs_influence_exact, abs_influence_fast)]
setorder(flags, spec, -sort_abs_infl, -max_abs_implied_gap_pct, -max_abs_raw_gap_pct)
selected <- flags[, head(.SD, opt$top_n_per_spec), by = spec]
if (opt$smoke && nrow(selected) > 6) {
  selected <- selected[1:6]
}
selected_pairs <- unique(selected$ward_pair_id)

message("[diag] selected boundaries: ", length(selected_pairs))

keep_cols <- c(
  "id", "file_date", "rent_price", "ward_pair_id", "ward", "signed_dist",
  "strictness_own", "beds", "baths", "sqft", "building_type_clean",
  "longitude", "latitude"
)
parquet_cols <- names(read_parquet(opt$input, as_data_frame = FALSE))
col_select <- intersect(keep_cols, parquet_cols)
missing_cols <- setdiff(c("id", "file_date", "rent_price", "ward_pair_id", "ward", "signed_dist", "strictness_own"), col_select)
if (length(missing_cols) > 0) {
  stop(sprintf("Input is missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

dt <- as.data.table(read_parquet(opt$input, col_select = tidyselect::all_of(col_select)))
if (!"beds" %in% names(dt)) dt[, beds := NA_real_]
if (!"baths" %in% names(dt)) dt[, baths := NA_real_]
if (!"sqft" %in% names(dt)) dt[, sqft := NA_real_]
if (!"building_type_clean" %in% names(dt)) dt[, building_type_clean := NA_character_]
if (!"longitude" %in% names(dt)) dt[, longitude := NA_real_]
if (!"latitude" %in% names(dt)) dt[, latitude := NA_real_]

dt[, `:=`(
  file_date = as.Date(file_date),
  year = as.integer(format(as.Date(file_date), "%Y")),
  ward_pair_id = as.character(ward_pair_id),
  ward = as.integer(ward),
  abs_dist = abs(as.numeric(signed_dist)),
  beds = as.numeric(beds),
  baths = as.numeric(baths),
  sqft = as.numeric(sqft),
  strictness_own = as.numeric(strictness_own),
  building_type_clean = as.character(building_type_clean)
)]

dt <- dt[
  !is.na(file_date) &
    !is.na(rent_price) & rent_price > 0 &
    !is.na(ward_pair_id) &
    !is.na(ward) &
    !is.na(signed_dist) &
    !is.na(strictness_own) &
    abs_dist <= opt$bw_ft
]
dt <- apply_window_filter(dt, opt$window)
dt <- dt[ward_pair_id %in% selected_pairs]

if (nrow(dt) == 0) {
  stop("No observations remain for selected boundaries after base filters.", call. = FALSE)
}

dt[, building_type_group := fifelse(is.na(building_type_clean) | building_type_clean == "", "missing", building_type_clean)]
dt[, beds_bucket := fifelse(is.na(beds), "missing", fifelse(beds >= 4, "4plus", as.character(as.integer(pmax(beds, 0)))))]
dt[, baths_bucket := fifelse(is.na(baths), "missing", fifelse(baths >= 3, "3plus", sprintf("%.1f", pmax(baths, 0))))]

message("[diag] rows in selected boundaries: ", format(nrow(dt), big.mark = ","))

side_summary <- dt[, .(
  n_listings = .N,
  mean_rent = mean(rent_price, na.rm = TRUE),
  median_rent = median(rent_price, na.rm = TRUE),
  p25_rent = safe_quantile(rent_price, 0.25),
  p75_rent = safe_quantile(rent_price, 0.75),
  mean_sqft = mean(sqft, na.rm = TRUE),
  median_sqft = median(sqft, na.rm = TRUE),
  mean_beds = mean(beds, na.rm = TRUE),
  median_beds = median(beds, na.rm = TRUE),
  mean_baths = mean(baths, na.rm = TRUE),
  median_baths = median(baths, na.rm = TRUE),
  share_multifamily = mean(building_type_group == "multi_family", na.rm = TRUE),
  share_missing_sqft = mean(is.na(sqft) | sqft <= 0),
  share_near_100ft = mean(abs_dist <= 100),
  share_near_250ft = mean(abs_dist <= 250),
  strictness_side_mean = mean(strictness_own, na.rm = TRUE)
), by = .(ward_pair_id, ward)]

type_shares <- dt[, .N, by = .(ward_pair_id, ward, building_type_group)]
type_shares[, share := N / sum(N), by = .(ward_pair_id, ward)]

pair_summary_list <- lapply(split(side_summary, by = "ward_pair_id", keep.by = TRUE), function(x) {
  if (nrow(x) < 2) return(NULL)
  x <- x[order(-strictness_side_mean, -n_listings)]
  hi <- x[1]
  lo <- x[2]
  data.table(
    ward_pair_id = hi$ward_pair_id,
    ward_high = hi$ward,
    ward_low = lo$ward,
    n_total = hi$n_listings + lo$n_listings,
    n_high = hi$n_listings,
    n_low = lo$n_listings,
    strictness_gap = hi$strictness_side_mean - lo$strictness_side_mean,
    mean_rent_high = hi$mean_rent,
    mean_rent_low = lo$mean_rent,
    rent_gap_pct = exp(log(hi$mean_rent) - log(lo$mean_rent)) - 1,
    median_rent_gap = hi$median_rent - lo$median_rent,
    mean_sqft_gap = hi$mean_sqft - lo$mean_sqft,
    mean_beds_gap = hi$mean_beds - lo$mean_beds,
    mean_baths_gap = hi$mean_baths - lo$mean_baths,
    share_multifamily_gap = hi$share_multifamily - lo$share_multifamily,
    share_near_250ft_gap = hi$share_near_250ft - lo$share_near_250ft
  )
})
pair_summary <- rbindlist(pair_summary_list, fill = TRUE)

if (nrow(pair_summary) == 0) {
  stop("Could not build pair summaries for selected boundaries.", call. = FALSE)
}

type_tv <- type_shares[, .(types = list(building_type_group), shares = list(share)), by = .(ward_pair_id, ward)]
type_tv <- split(type_tv, by = "ward_pair_id", keep.by = TRUE)
type_tv_dt <- rbindlist(lapply(type_tv, function(x) {
  if (nrow(x) < 2) return(NULL)
  a <- x[1]
  b <- x[2]
  t_all <- sort(unique(c(unlist(a$types), unlist(b$types))))
  sa <- setNames(rep(0, length(t_all)), t_all)
  sb <- setNames(rep(0, length(t_all)), t_all)
  sa[unlist(a$types)] <- unlist(a$shares)
  sb[unlist(b$types)] <- unlist(b$shares)
  data.table(
    ward_pair_id = a$ward_pair_id,
    building_mix_tv = 0.5 * sum(abs(sa - sb))
  )
}), fill = TRUE)

pair_summary <- merge(pair_summary, type_tv_dt, by = "ward_pair_id", all.x = TRUE)
flag_cols <- c(
  "ward_pair_id", "spec", "max_abs_raw_gap_pct", "max_abs_implied_gap_pct",
  "n_pair_month", "total_listings", "influence_exact", "abs_influence_exact"
)
flag_keep <- intersect(flag_cols, names(selected))
pair_summary <- merge(pair_summary, selected[, ..flag_keep], by = "ward_pair_id", all.x = TRUE, allow.cartesian = TRUE)

selection_out <- selected[, .(
  spec, ward_pair_id, n_pair_month, total_listings,
  max_abs_raw_gap_pct, max_abs_implied_gap_pct,
  influence_exact, abs_influence_exact, rank_abs_exact, geometry_clean, geometry_flag
)]

selection_csv <- file.path(opt$output_dir, "rent_fe_flagged_boundary_selection.csv")
side_csv <- file.path(opt$output_dir, "rent_fe_flagged_boundary_side_summary.csv")
pair_csv <- file.path(opt$output_dir, "rent_fe_flagged_boundary_pair_summary.csv")
type_csv <- file.path(opt$output_dir, "rent_fe_flagged_boundary_type_shares.csv")
map_pdf <- file.path(opt$output_dir, "rent_fe_flagged_boundary_unit_maps.pdf")
char_pdf <- file.path(opt$output_dir, "rent_fe_flagged_boundary_characteristics.pdf")
summary_md <- file.path(opt$output_dir, "rent_fe_flagged_boundary_zoom_summary.md")

fwrite(selection_out, selection_csv)
fwrite(side_summary, side_csv)
fwrite(pair_summary, pair_csv)
fwrite(type_shares, type_csv)

ward_panel <- st_read(opt$ward_panel, quiet = TRUE)
ward_panel$ward <- as.integer(ward_panel$ward)
ward_panel$year <- as.integer(ward_panel$year)
if (isTRUE(st_is_longlat(ward_panel))) {
  ward_panel <- st_transform(ward_panel, 26971)
}
years <- sort(unique(ward_panel$year))
map_year <- if (opt$map_year %in% years) opt$map_year else years[which.min(abs(years - opt$map_year))]
ward_map <- ward_panel[ward_panel$year == map_year, c("ward")]
boundary_lines <- build_boundary_lines(ward_map)
boundary_lines <- boundary_lines[boundary_lines$pair_id %in% selected_pairs, ]

pair_meta <- unique(pair_summary[, .(ward_pair_id, ward_high, ward_low)])
pair_meta <- pair_meta[order(ward_pair_id)]

if (nrow(pair_meta) == 0) {
  stop("No boundary metadata found for mapping.", call. = FALSE)
}

make_pair_map <- function(pid) {
  meta <- pair_meta[ward_pair_id == pid][1]
  pair_dt <- dt[ward_pair_id == pid & is.finite(longitude) & is.finite(latitude)]
  if (nrow(pair_dt) == 0) return(NULL)

  if (nrow(pair_dt) > opt$point_sample_per_boundary) {
    set.seed(20260216)
    pair_dt <- pair_dt[sample(.N, opt$point_sample_per_boundary)]
  }

  pts <- st_as_sf(pair_dt, coords = c("longitude", "latitude"), crs = 4326)
  pts <- st_transform(pts, st_crs(ward_map))
  line <- boundary_lines[boundary_lines$pair_id == pid, ]

  bbox <- if (nrow(line) > 0) {
    st_bbox(st_union(st_geometry(line), st_geometry(pts)))
  } else {
    st_bbox(pts)
  }
  bbox_exp <- bbox
  bbox_exp["xmin"] <- bbox_exp["xmin"] - opt$zoom_margin_ft
  bbox_exp["xmax"] <- bbox_exp["xmax"] + opt$zoom_margin_ft
  bbox_exp["ymin"] <- bbox_exp["ymin"] - opt$zoom_margin_ft
  bbox_exp["ymax"] <- bbox_exp["ymax"] + opt$zoom_margin_ft

  wards_clip <- suppressWarnings(st_crop(ward_map, bbox_exp))
  if (nrow(wards_clip) == 0) wards_clip <- ward_map

  sub <- pair_summary[ward_pair_id == pid]
  sub <- sub[order(spec)]
  subtitle <- paste0(
    "N=", format(sum(sub$n_total, na.rm = TRUE), big.mark = ","),
    " | rent gap (high-low): ",
    paste(vapply(sub$spec, function(s) {
      ss <- sub[spec == s][1]
      paste0(s, "=", sprintf("%.1f%%", 100 * ss$rent_gap_pct))
    }, character(1)), collapse = "; ")
  )

  ggplot() +
    geom_sf(data = wards_clip, fill = "grey97", color = "grey80", linewidth = 0.2) +
    {
      if (nrow(line) > 0) geom_sf(data = line, color = "black", linewidth = 1.0)
    } +
    geom_sf(
      data = pts,
      aes(color = factor(ward)),
      alpha = 0.35,
      size = 0.6
    ) +
    coord_sf(
      xlim = c(bbox_exp["xmin"], bbox_exp["xmax"]),
      ylim = c(bbox_exp["ymin"], bbox_exp["ymax"]),
      expand = FALSE
    ) +
    scale_color_brewer(palette = "Set1") +
    labs(
      title = paste0("Boundary ", pid, " (Ward ", meta$ward_high, " vs ", meta$ward_low, ")"),
      subtitle = subtitle,
      color = "Ward side"
    ) +
    theme_minimal()
}

make_pair_chars <- function(pid) {
  d <- copy(dt[ward_pair_id == pid])
  if (nrow(d) == 0) return(NULL)
  d[, ward_f := paste0("Ward ", ward)]

  p_rent <- ggplot(d, aes(x = log(rent_price), color = ward_f, fill = ward_f)) +
    geom_density(alpha = 0.2) +
    labs(title = "Log Rent Distribution", x = "log(rent)", y = "Density", color = "Ward side", fill = "Ward side") +
    theme_minimal()

  ds <- d[is.finite(sqft) & sqft > 0]
  p_sqft <- if (nrow(ds) > 10) {
    ggplot(ds, aes(x = log(sqft), color = ward_f, fill = ward_f)) +
      geom_density(alpha = 0.2) +
      labs(title = "Log Sqft Distribution", x = "log(sqft)", y = "Density", color = "Ward side", fill = "Ward side") +
      theme_minimal()
  } else {
    ggplot() + annotate("text", x = 1, y = 1, label = "Not enough sqft data") + theme_void()
  }

  beds_share <- d[, .N, by = .(ward_f, beds_bucket)]
  beds_share[, share := N / sum(N), by = ward_f]
  p_beds <- ggplot(beds_share, aes(x = beds_bucket, y = share, fill = ward_f)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Beds Mix", x = "Beds bucket", y = "Share", fill = "Ward side") +
    theme_minimal()

  type_share <- d[, .N, by = .(ward_f, building_type_group)]
  type_share[, share := N / sum(N), by = ward_f]
  setorder(type_share, ward_f, -share)
  keep_types <- unique(type_share[order(-share)][1:min(6, .N), building_type_group])
  type_plot <- type_share[building_type_group %in% keep_types]
  p_type <- ggplot(type_plot, aes(x = building_type_group, y = share, fill = ward_f)) +
    geom_col(position = "dodge") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = "Building Type Mix (Top Categories)", x = "Type", y = "Share", fill = "Ward side") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1))

  (p_rent | p_sqft) / (p_beds | p_type) + patchwork::plot_annotation(title = paste0("Characteristics: Boundary ", pid))
}

pdf(map_pdf, width = 10, height = 8)
for (pid in pair_meta$ward_pair_id) {
  g <- make_pair_map(pid)
  if (!is.null(g)) print(g)
}
dev.off()

pdf(char_pdf, width = 12, height = 8)
for (pid in pair_meta$ward_pair_id) {
  g <- make_pair_chars(pid)
  if (!is.null(g)) print(g)
}
dev.off()

top_gap <- pair_summary[order(-abs(rent_gap_pct))]
top_gap <- top_gap[1:min(12, .N), .(
  ward_pair_id, spec, n_total, rent_gap_pct,
  mean_sqft_gap, mean_beds_gap, mean_baths_gap, share_multifamily_gap, building_mix_tv
)]

md <- c(
  "# Flagged Boundary Zoom Summary",
  "",
  sprintf("- generated: `%s`", as.character(Sys.time())),
  sprintf("- bw_ft: `%d`", opt$bw_ft),
  sprintf("- window: `%s`", opt$window),
  sprintf("- selected boundaries: `%d`", length(selected_pairs)),
  sprintf("- map_year: `%d`", map_year),
  "",
  "## What This Contains",
  "- zoom maps of listing points by ward side for selected severe-flag boundaries",
  "- side-by-side unit composition summaries for each selected boundary side",
  "- per-boundary characteristic plots (rent, sqft, beds mix, building type mix)",
  "",
  "## Top Absolute Rent-Gap Boundaries In Selection",
  "| Ward Pair | Spec | N | Rent Gap (high-low) | Sqft Gap | Beds Gap | Baths Gap | Multi-family Share Gap | Building Mix TV |",
  "|---|---|---:|---:|---:|---:|---:|---:|---:|"
)

for (i in seq_len(nrow(top_gap))) {
  r <- top_gap[i]
  md <- c(md, sprintf(
    "| %s | %s | %s | %.1f%% | %.1f | %.2f | %.2f | %.2fpp | %.2f |",
    r$ward_pair_id,
    r$spec,
    format(r$n_total, big.mark = ","),
    100 * r$rent_gap_pct,
    r$mean_sqft_gap,
    r$mean_beds_gap,
    r$mean_baths_gap,
    100 * r$share_multifamily_gap,
    r$building_mix_tv
  ))
}

md <- c(
  md,
  "",
  "## Output Files",
  "- `rent_fe_flagged_boundary_selection.csv`",
  "- `rent_fe_flagged_boundary_side_summary.csv`",
  "- `rent_fe_flagged_boundary_pair_summary.csv`",
  "- `rent_fe_flagged_boundary_type_shares.csv`",
  "- `rent_fe_flagged_boundary_unit_maps.pdf`",
  "- `rent_fe_flagged_boundary_characteristics.pdf`"
)
writeLines(md, summary_md)

message("Saved: ", selection_csv)
message("Saved: ", side_csv)
message("Saved: ", pair_csv)
message("Saved: ", type_csv)
message("Saved: ", map_pdf)
message("Saved: ", char_pdf)
message("Saved: ", summary_md)
message("Done.")
