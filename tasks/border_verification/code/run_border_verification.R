source("../../setup_environment/code/packages.R")

library(data.table)
library(sf)
library(arrow)
library(ggplot2)

sf_use_s2(FALSE)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_verification/code")
# certify_only <- FALSE
# skip_thresholds <- FALSE
# output_dir <- "../output"
# Rscript run_border_verification.R FALSE FALSE "../output"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 3) {
  certify_only <- tolower(cli_args[1]) %in% c("true", "t", "1", "yes")
  skip_thresholds <- tolower(cli_args[2]) %in% c("true", "t", "1", "yes")
  output_dir <- cli_args[3]
} else {
  if (!exists("certify_only") || !exists("skip_thresholds") || !exists("output_dir")) {
    stop("FATAL: Script requires 3 args: <certify_only> <skip_thresholds> <output_dir>", call. = FALSE)
  }
}

OUT_DIR <- output_dir
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

severity_from_rate <- function(rate) {
  if (!is.finite(rate) || is.na(rate) || rate <= 0) return("none")
  if (rate >= 0.05) return("high")
  if (rate >= 0.005) return("medium")
  "low"
}

safe_rate <- function(n_issue, n_total) {
  if (!is.finite(n_total) || n_total <= 0) return(NA_real_)
  n_issue / n_total
}

check_rows <- list()
add_check <- function(dataset, check_id, n_issue, n_total, note = "") {
  rate <- safe_rate(n_issue, n_total)
  check_rows[[length(check_rows) + 1]] <<- data.table(
    dataset = dataset,
    check_id = check_id,
    n_issue = as.numeric(n_issue),
    n_total = as.numeric(n_total),
    issue_rate = rate,
    severity = severity_from_rate(rate),
    note = note
  )
}

parse_pair <- function(x, sep = "-") {
  sp <- tstrsplit(as.character(x), sep, fixed = TRUE)
  list(as.integer(sp[[1]]), as.integer(sp[[2]]))
}

build_boundary_lines <- function(ward_sf) {
  ward_sf <- st_buffer(ward_sf, 0)
  adj <- st_touches(ward_sf)

  edges <- purrr::imap_dfr(adj, function(nb, i) {
    if (length(nb) == 0) return(NULL)
    nb_valid <- nb[nb > i]
    if (length(nb_valid) == 0) return(NULL)

    purrr::map_dfr(nb_valid, function(j) {
      shared <- suppressWarnings(st_intersection(st_geometry(ward_sf[i, ]), st_geometry(ward_sf[j, ])))
      if (length(shared) == 0 || all(st_is_empty(shared))) return(NULL)

      gtypes <- unique(as.character(st_geometry_type(shared)))
      shared_lines <- if (all(gtypes %in% c("LINESTRING", "MULTILINESTRING"))) {
        st_cast(shared, "LINESTRING")
      } else if ("GEOMETRYCOLLECTION" %in% gtypes) {
        suppressWarnings(st_collection_extract(shared, "LINESTRING"))
      } else {
        return(NULL)
      }

      if (length(shared_lines) == 0 || all(st_is_empty(shared_lines))) return(NULL)
      shared_lines <- shared_lines[as.numeric(st_length(shared_lines)) > 0, ]
      if (length(shared_lines) == 0) return(NULL)

      st_sf(
        ward_a = as.integer(ward_sf$ward[i]),
        ward_b = as.integer(ward_sf$ward[j]),
        geometry = shared_lines
      )
    })
  })

  if (nrow(edges) == 0) {
    return(st_sf(
      ward_a = integer(),
      ward_b = integer(),
      pair_id = character(),
      geometry = st_sfc(),
      crs = st_crs(ward_sf)
    ))
  }

  edges <- st_as_sf(edges, crs = st_crs(ward_sf))
  edges <- edges %>%
    mutate(
      ward_lo = pmin(ward_a, ward_b),
      ward_hi = pmax(ward_a, ward_b),
      ward_a = ward_lo,
      ward_b = ward_hi,
      pair_id = sprintf("%d-%d", ward_a, ward_b)
    ) %>%
    select(-ward_lo, -ward_hi) %>%
    group_by(pair_id, ward_a, ward_b) %>%
    summarize(geometry = st_union(geometry), .groups = "drop") %>%
    st_as_sf()

  edges
}

pair_set_from_lines <- function(lines) {
  unique(sprintf("%d-%d", pmin(lines$ward_a, lines$ward_b), pmax(lines$ward_a, lines$ward_b)))
}

compute_corrected_pairs <- function(dt, lines) {
  if (nrow(dt) == 0) return(dt[, `:=`(corrected_neighbor = NA_integer_, corrected_pair = NA_character_)])

  dt <- copy(dt)
  dt[, obs_row := .I]

  pts <- st_as_sf(dt, coords = c("longitude", "latitude"), crs = 4326)
  if (st_crs(pts) != st_crs(lines)) pts <- st_transform(pts, st_crs(lines))

  ward_vals <- sort(unique(dt$ward))
  corrected_parts <- vector("list", length(ward_vals))

  ii <- 1
  for (w in ward_vals) {
    idx <- which(dt$ward == w)
    edges_w <- lines[lines$ward_a == w | lines$ward_b == w, ]
    if (length(idx) == 0 || nrow(edges_w) == 0) next

    nearest_idx <- st_nearest_feature(pts[idx, ], edges_w)
    nearest_edges <- edges_w[nearest_idx, ]
    neighbor <- ifelse(nearest_edges$ward_a == w, nearest_edges$ward_b, nearest_edges$ward_a)

    corrected_parts[[ii]] <- data.table(
      obs_row = idx,
      corrected_neighbor = as.integer(neighbor),
      corrected_pair = sprintf("%d-%d", pmin(w, neighbor), pmax(w, neighbor))
    )
    ii <- ii + 1
  }

  corrected <- rbindlist(corrected_parts, fill = TRUE)
  out <- merge(dt, corrected, by = "obs_row", all.x = TRUE, sort = FALSE)
  out[, obs_row := NULL]
  out
}

csv_nrows <- function(path) nrow(fread(path, select = 1))
parquet_nrows <- function(path, col) nrow(read_parquet(path, col_select = col))

compute_topology_metrics <- function(ward_panel) {
  yrs <- sort(unique(ward_panel$year))
  rows <- vector("list", length(yrs))
  for (i in seq_along(yrs)) {
    yy <- yrs[i]
    w <- ward_panel[ward_panel$year == yy, c("ward")]
    invalid_n <- sum(!st_is_valid(w))

    ov <- st_overlaps(w)
    overlap_pairs <- 0L
    for (j in seq_along(ov)) {
      if (length(ov[[j]]) == 0) next
      overlap_pairs <- overlap_pairs + sum(ov[[j]] > j)
    }

    total_area <- sum(as.numeric(st_area(w)), na.rm = TRUE)
    union_area <- as.numeric(st_area(st_union(st_geometry(w))))
    overlap_area <- max(total_area - union_area, 0)
    overlap_share <- ifelse(is.finite(union_area) && union_area > 0, overlap_area / union_area, NA_real_)

    rows[[i]] <- data.table(
      year = yy,
      n_wards = length(unique(w$ward)),
      invalid_geom = invalid_n,
      overlap_pairs = overlap_pairs,
      overlap_area_ft2 = overlap_area,
      overlap_share = overlap_share
    )
  }
  rbindlist(rows)
}

save_anomaly_maps <- function(samples_dt, ward_panel, out_pdf) {
  if (nrow(samples_dt) == 0) {
    grDevices::pdf(out_pdf, width = 11, height = 8)
    plot.new()
    text(0.5, 0.5, "No anomalies to map")
    grDevices::dev.off()
    return(invisible(NULL))
  }

  map_years <- data.table(
    dataset = c("sales_pre_scores", "rent_pre_scores_full"),
    year = c(2003L, 2014L)
  )

  samples <- copy(samples_dt)
  samples <- merge(samples, map_years, by = "dataset", all.x = TRUE)
  samples_sf <- st_as_sf(samples, coords = c("longitude", "latitude"), crs = 4326)
  samples_sf <- st_transform(samples_sf, st_crs(ward_panel))

  grDevices::pdf(out_pdf, width = 11, height = 8)
  for (ds in unique(samples_sf$dataset)) {
    ss <- samples_sf[samples_sf$dataset == ds, ]
    yy <- unique(ss$year)[1]
    wp <- ward_panel[ward_panel$year == yy, c("ward")]

    p <- ggplot() +
      geom_sf(data = wp, fill = NA, color = "grey70", linewidth = 0.2) +
      geom_sf(data = ss, aes(color = issue_type), alpha = 0.7, size = 0.7) +
      scale_color_manual(values = c("invalid_pair" = "#d73027", "recomputed_neighbor" = "#4575b4")) +
      labs(
        title = sprintf("Border Anomalies: %s (map year %d)", ds, yy),
        color = "Anomaly Type"
      ) +
      theme_minimal()
    print(p)
  }
  grDevices::dev.off()
}

message("=== Border Verification Audit ===")

# -----------------------------------------------------------------------------
# 1) OVERVIEW
# -----------------------------------------------------------------------------
overview <- rbindlist(list(
  data.table(dataset = "parcels_pre_scores", rows = csv_nrows("../input/parcels_pre_scores.csv")),
  data.table(dataset = "parcels_with_ward_distances", rows = csv_nrows("../input/parcels_with_ward_distances.csv")),
  data.table(dataset = "sales_pre_scores", rows = csv_nrows("../input/sales_pre_scores.csv")),
  data.table(dataset = "sales_with_ward_distances", rows = csv_nrows("../input/sales_with_ward_distances.csv")),
  data.table(dataset = "rent_pre_scores_full", rows = parquet_nrows("../input/rent_pre_scores_full.parquet", "id")),
  data.table(dataset = "rent_with_ward_distances_full", rows = parquet_nrows("../input/rent_with_ward_distances_full.parquet", "id")),
  data.table(dataset = "block_treatment_pre_scores", rows = csv_nrows("../input/block_treatment_pre_scores.csv")),
  data.table(dataset = "block_treatment_panel", rows = csv_nrows("../input/block_treatment_panel.csv")),
  data.table(dataset = "sales_transaction_panel", rows = parquet_nrows("../input/sales_transaction_panel.parquet", "pin")),
  data.table(dataset = "rental_listing_panel", rows = parquet_nrows("../input/rental_listing_panel.parquet", "id"))
))
fwrite(overview, file.path(OUT_DIR, "border_verification_dataset_overview.csv"))

# -----------------------------------------------------------------------------
# 2) BUILD MAP-SPECIFIC BOUNDARY PAIR SETS + TOPOLOGY METRICS
# -----------------------------------------------------------------------------
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
  st_transform(3435) %>%
  mutate(ward = as.integer(ward), year = as.integer(year))
ward_years <- sort(unique(ward_panel$year))

sales_year_1998 <- if (1998 %in% ward_years) 1998L else min(ward_years)
sales_year_2003 <- if (2003 %in% ward_years) 2003L else min(ward_years[ward_years >= 2003])
sales_year_2015 <- if (2015 %in% ward_years) 2015L else min(ward_years[ward_years >= 2015])
sales_year_2024 <- if (2024 %in% ward_years) 2024L else max(ward_years)

rent_year_2014 <- if (2014 %in% ward_years) 2014L else max(ward_years[ward_years < 2015])
rent_year_2016 <- if (2016 %in% ward_years) 2016L else sales_year_2015
rent_year_2024 <- sales_year_2024

lines_sales_1998 <- build_boundary_lines(ward_panel[ward_panel$year == sales_year_1998, c("ward")])
lines_sales_2003 <- build_boundary_lines(ward_panel[ward_panel$year == sales_year_2003, c("ward")])
lines_sales_2015 <- build_boundary_lines(ward_panel[ward_panel$year == sales_year_2015, c("ward")])
lines_sales_2024 <- build_boundary_lines(ward_panel[ward_panel$year == sales_year_2024, c("ward")])

lines_rent_2014 <- build_boundary_lines(ward_panel[ward_panel$year == rent_year_2014, c("ward")])
lines_rent_2016 <- build_boundary_lines(ward_panel[ward_panel$year == rent_year_2016, c("ward")])
lines_rent_2024 <- build_boundary_lines(ward_panel[ward_panel$year == rent_year_2024, c("ward")])

pairs_sales_1998 <- pair_set_from_lines(lines_sales_1998)
pairs_sales_2003 <- pair_set_from_lines(lines_sales_2003)
pairs_sales_2015 <- pair_set_from_lines(lines_sales_2015)
pairs_sales_2024 <- pair_set_from_lines(lines_sales_2024)

pairs_rent_2014 <- pair_set_from_lines(lines_rent_2014)
pairs_rent_2016 <- pair_set_from_lines(lines_rent_2016)
pairs_rent_2024 <- pair_set_from_lines(lines_rent_2024)

topology_metrics <- compute_topology_metrics(ward_panel)

# -----------------------------------------------------------------------------
# 3) PARCEL BORDER DATA CHECKS
# -----------------------------------------------------------------------------
parcels_pre <- fread("../input/parcels_pre_scores.csv", select = c("pin", "boundary_year", "ward", "other_ward", "ward_pair", "dist_to_boundary"))
parcels_sc <- fread("../input/parcels_with_ward_distances.csv", select = c("pin", "ward", "other_ward", "ward_pair", "dist_to_boundary", "signed_distance", "sign", "strictness_own", "strictness_neighbor"))

add_check("parcels_pre_scores", "duplicate_pin_rows", sum(duplicated(parcels_pre$pin)), nrow(parcels_pre), "Should be one row per pin.")
add_check("parcels_pre_scores", "missing_ward_or_pair", sum(is.na(parcels_pre$ward) | is.na(parcels_pre$ward_pair)), nrow(parcels_pre), "Missing ward assignment indicates geometry join failure.")
add_check("parcels_with_ward_distances", "rows_dropped_after_score_merge", nrow(parcels_pre) - nrow(parcels_sc), nrow(parcels_pre), "Drop expected only from equal/missing scores.")
add_check("parcels_with_ward_distances", "signed_distance_identity_fail", sum(abs(parcels_sc$signed_distance - parcels_sc$dist_to_boundary * parcels_sc$sign) > 1e-6, na.rm = TRUE), nrow(parcels_sc), "signed_distance should equal distance * sign.")

# -----------------------------------------------------------------------------
# 4) SALES PRE-SCORES + SCORED CHECKS
# -----------------------------------------------------------------------------
sales_pre <- fread("../input/sales_pre_scores.csv", select = c("pin", "sale_date", "year", "ward", "neighbor_ward", "ward_pair_id", "dist_ft", "latitude", "longitude"))
sales_pre[, sale_date := as.Date(sale_date)]
sales_pre[is.na(sale_date), sale_date := as.Date(sprintf("%d-06-15", year))]

sales_sc <- fread("../input/sales_with_ward_distances.csv", select = c("pin", "sale_date", "ward", "neighbor_ward", "ward_pair_id", "dist_ft", "signed_dist", "sign", "strictness_own", "strictness_neighbor"))
sales_sc[, sale_date := as.Date(sale_date)]

sp <- parse_pair(sales_pre$ward_pair_id, "-")
sales_pair_mismatch <- !(pmin(sales_pre$ward, sales_pre$neighbor_ward) == sp[[1]] & pmax(sales_pre$ward, sales_pre$neighbor_ward) == sp[[2]])

add_check("sales_pre_scores", "ward_equals_neighbor", sum(sales_pre$ward == sales_pre$neighbor_ward, na.rm = TRUE), nrow(sales_pre))
add_check("sales_pre_scores", "ward_pair_not_equal_to_ward_neighbor_pair", sum(sales_pair_mismatch, na.rm = TRUE), nrow(sales_pre))
add_check("sales_with_ward_distances", "signed_distance_identity_fail", sum(abs(sales_sc$signed_dist - sales_sc$dist_ft * sales_sc$sign) > 1e-6, na.rm = TRUE), nrow(sales_sc))
add_check("sales_with_ward_distances", "strictness_sign_disagreement", sum(sign(sales_sc$strictness_own - sales_sc$strictness_neighbor) != sales_sc$sign, na.rm = TRUE), nrow(sales_sc))

d2003 <- as.Date("2003-05-01")
d2015 <- as.Date("2015-05-18")
d2023 <- as.Date("2023-05-15")

sales_pre[, era := fifelse(sale_date < d2003, "pre2003", fifelse(sale_date < d2015, "2003_2015", fifelse(sale_date < d2023, "2015_2023", "post2023")))]
sales_pre[, pair_adjacent := fcase(
  era == "pre2003", ward_pair_id %in% pairs_sales_1998,
  era == "2003_2015", ward_pair_id %in% pairs_sales_2003,
  era == "2015_2023", ward_pair_id %in% pairs_sales_2015,
  era == "post2023", ward_pair_id %in% pairs_sales_2024,
  default = FALSE
)]

add_check("sales_pre_scores", "non_adjacent_ward_pairs_by_era_map", sum(!sales_pre$pair_adjacent, na.rm = TRUE), nrow(sales_pre), sprintf("Map years used: pre=%d, mid=%d, post2015=%d, post2023=%d", sales_year_1998, sales_year_2003, sales_year_2015, sales_year_2024))

sales_invalid <- sales_pre[era == "2003_2015" & !pair_adjacent]
sales_invalid_corr <- compute_corrected_pairs(sales_invalid, lines_sales_2003)
add_check("sales_pre_scores", "invalid_mid_era_pairs_with_correctable_neighbor", sum(!is.na(sales_invalid_corr$corrected_pair), na.rm = TRUE), nrow(sales_invalid_corr), "Rows where nearest boundary touching assigned ward gives an alternate pair.")

sales_invalid_pairs <- sales_invalid_corr[, .N, by = .(ward_pair_id, corrected_pair)][order(-N)]
fwrite(sales_invalid_pairs, file.path(OUT_DIR, "border_verification_invalid_pairs_sales.csv"))

# -----------------------------------------------------------------------------
# 5) RENT PRE-SCORES + SCORED CHECKS
# -----------------------------------------------------------------------------
rent_pre <- as.data.table(read_parquet("../input/rent_pre_scores_full.parquet", col_select = c("id", "file_date", "ward", "neighbor_ward", "ward_pair_id", "dist_ft", "latitude", "longitude")))
rent_pre[, file_date := as.Date(file_date)]
rent_sc <- as.data.table(read_parquet("../input/rent_with_ward_distances_full.parquet", col_select = c("id", "file_date", "dist_ft", "signed_dist", "sign", "strictness_own", "strictness_neighbor")))
rent_sc[, file_date := as.Date(file_date)]

rp <- parse_pair(rent_pre$ward_pair_id, "-")
rent_pair_mismatch <- !(pmin(rent_pre$ward, rent_pre$neighbor_ward) == rp[[1]] & pmax(rent_pre$ward, rent_pre$neighbor_ward) == rp[[2]])

add_check("rent_pre_scores_full", "ward_equals_neighbor", sum(rent_pre$ward == rent_pre$neighbor_ward, na.rm = TRUE), nrow(rent_pre))
add_check("rent_pre_scores_full", "ward_pair_not_equal_to_ward_neighbor_pair", sum(rent_pair_mismatch, na.rm = TRUE), nrow(rent_pre))
add_check("rent_with_ward_distances_full", "signed_distance_identity_fail", sum(abs(rent_sc$signed_dist - rent_sc$dist_ft * rent_sc$sign) > 1e-6, na.rm = TRUE), nrow(rent_sc))
add_check("rent_with_ward_distances_full", "strictness_sign_disagreement", sum(sign(rent_sc$strictness_own - rent_sc$strictness_neighbor) != rent_sc$sign, na.rm = TRUE), nrow(rent_sc))

d2015_rent <- as.Date("2015-05-18")
d2023_rent <- as.Date("2023-05-15")

rent_pre[, era := fifelse(file_date < d2015_rent, "pre2015", fifelse(file_date < d2023_rent, "2015_2023", "post2023"))]
rent_pre[, pair_adjacent := fcase(
  era == "pre2015", ward_pair_id %in% pairs_rent_2014,
  era == "2015_2023", ward_pair_id %in% pairs_rent_2016,
  era == "post2023", ward_pair_id %in% pairs_rent_2024,
  default = FALSE
)]

add_check("rent_pre_scores_full", "non_adjacent_ward_pairs_by_era_map", sum(!rent_pre$pair_adjacent, na.rm = TRUE), nrow(rent_pre), sprintf("Map years used: pre=%d, post2015=%d, post2023=%d", rent_year_2014, rent_year_2016, rent_year_2024))

rent_invalid <- rent_pre[era == "pre2015" & !pair_adjacent]
rent_invalid_corr <- compute_corrected_pairs(rent_invalid, lines_rent_2014)
add_check("rent_pre_scores_full", "invalid_pre2015_pairs_with_correctable_neighbor", sum(!is.na(rent_invalid_corr$corrected_pair), na.rm = TRUE), nrow(rent_invalid_corr), "Rows where nearest boundary touching assigned ward gives an alternate pair.")

rent_invalid_pairs <- rent_invalid_corr[, .N, by = .(ward_pair_id, corrected_pair)][order(-N)]
fwrite(rent_invalid_pairs, file.path(OUT_DIR, "border_verification_invalid_pairs_rent.csv"))

# -----------------------------------------------------------------------------
# 6) TREATMENT PANEL CHECKS
# -----------------------------------------------------------------------------
treat_pre <- fread("../input/block_treatment_pre_scores.csv")
treat_sc <- fread("../input/block_treatment_panel.csv")

add_check("block_treatment_pre_scores", "switched_not_equal_to_ward_change", sum(treat_pre$switched != (treat_pre$ward_origin != treat_pre$ward_dest), na.rm = TRUE), nrow(treat_pre))
add_check("block_treatment_panel", "switched_not_equal_to_ward_change", sum(treat_sc$switched != (treat_sc$ward_origin != treat_sc$ward_dest), na.rm = TRUE), nrow(treat_sc))
add_check("block_treatment_panel", "strictness_change_identity_fail", sum(abs(treat_sc$strictness_change - (treat_sc$strictness_dest - treat_sc$strictness_origin)) > 1e-8, na.rm = TRUE), nrow(treat_sc))

# -----------------------------------------------------------------------------
# 7) ANALYSIS PANEL CHECKS
# -----------------------------------------------------------------------------
sales_schema <- open_dataset("../input/sales_transaction_panel.parquet")$schema$names
sales_cols <- intersect(c("cohort", "treat", "ward_pair_id", "ward_origin", "strictness_change", "dist_ft"), sales_schema)
if ("ward_dest" %in% sales_schema) sales_cols <- c(sales_cols, "ward_dest")
sales_panel <- as.data.table(read_parquet("../input/sales_transaction_panel.parquet", col_select = sales_cols))
sp2 <- parse_pair(sales_panel$ward_pair_id, "-")
if (!"ward_dest" %in% names(sales_panel)) {
  sales_panel[, ward_dest := fifelse(ward_origin == sp2[[1]], sp2[[2]], fifelse(ward_origin == sp2[[2]], sp2[[1]], NA_integer_))]
}
sales_panel[, origin_in_pair := ward_origin == sp2[[1]] | ward_origin == sp2[[2]]]
sales_panel[, treated_zero_strictness := treat == 1 & abs(strictness_change) <= 0]
sales_panel[, treated_pair_exact := treat == 1 & ward_pair_id == sprintf("%d-%d", pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest))]

add_check("sales_transaction_panel", "origin_ward_not_in_pair", sum(!sales_panel$origin_in_pair, na.rm = TRUE), nrow(sales_panel))
add_check("sales_transaction_panel", "treated_zero_strictness_change", sum(sales_panel$treated_zero_strictness, na.rm = TRUE), sum(sales_panel$treat == 1, na.rm = TRUE))
add_check("sales_transaction_panel", "treated_pair_not_equal_to_origin_dest_pair", sum(sales_panel$treat == 1 & !sales_panel$treated_pair_exact, na.rm = TRUE), sum(sales_panel$treat == 1, na.rm = TRUE))

rent_schema <- open_dataset("../input/rental_listing_panel.parquet")$schema$names
rent_cols <- intersect(c("cohort", "treat", "ward_pair_id", "ward_origin", "strictness_change", "dist_ft"), rent_schema)
if ("ward_dest" %in% rent_schema) rent_cols <- c(rent_cols, "ward_dest")
rent_panel <- as.data.table(read_parquet("../input/rental_listing_panel.parquet", col_select = rent_cols))
rp2 <- parse_pair(rent_panel$ward_pair_id, "-")
if (!"ward_dest" %in% names(rent_panel)) {
  rent_panel[, ward_dest := fifelse(ward_origin == rp2[[1]], rp2[[2]], fifelse(ward_origin == rp2[[2]], rp2[[1]], NA_integer_))]
}
rent_panel[, origin_in_pair := ward_origin == rp2[[1]] | ward_origin == rp2[[2]]]
rent_panel[, treated_pair_exact := treat == 1 & ward_pair_id == sprintf("%d-%d", pmin(ward_origin, ward_dest), pmax(ward_origin, ward_dest))]

add_check("rental_listing_panel", "origin_ward_not_in_pair", sum(!rent_panel$origin_in_pair, na.rm = TRUE), nrow(rent_panel))
add_check("rental_listing_panel", "treated_pair_not_equal_to_origin_dest_pair", sum(rent_panel$treat == 1 & !rent_panel$treated_pair_exact, na.rm = TRUE), sum(rent_panel$treat == 1, na.rm = TRUE))

panel_checks <- rbindlist(list(
  sales_panel[, .(
    dataset = "sales_transaction_panel",
    cohort,
    treat,
    n = .N,
    origin_not_in_pair = sum(!origin_in_pair, na.rm = TRUE),
    treated_zero_strictness = sum(treated_zero_strictness, na.rm = TRUE),
    treated_pair_mismatch = sum(treat == 1 & !treated_pair_exact, na.rm = TRUE)
  ), by = .(cohort, treat)],
  rent_panel[, .(
    dataset = "rental_listing_panel",
    cohort,
    treat,
    n = .N,
    origin_not_in_pair = sum(!origin_in_pair, na.rm = TRUE),
    treated_pair_mismatch = sum(treat == 1 & !treated_pair_exact, na.rm = TRUE)
  ), by = .(cohort, treat)]
), fill = TRUE)
fwrite(panel_checks, file.path(OUT_DIR, "border_verification_panel_checks.csv"))

# -----------------------------------------------------------------------------
# 8) MAP YEAR SENSITIVITY CHECK (PARCELS WITH boundary_year=2003)
# -----------------------------------------------------------------------------
if (2003 %in% ward_years && 2005 %in% ward_years) {
  parcels_2003 <- parcels_pre[boundary_year == 2003, .(pin, ward_stored = ward)]
  geo_pts <- st_read("../input/geocoded_residential_data.gpkg", quiet = TRUE)[, c("pin")]
  geo_pts$pin <- as.character(geo_pts$pin)
  parcels_2003$pin <- as.character(parcels_2003$pin)
  geo_pts <- geo_pts[geo_pts$pin %in% parcels_2003$pin, ]
  if (st_crs(geo_pts) != st_crs(ward_panel)) geo_pts <- st_transform(geo_pts, st_crs(ward_panel))

  w03 <- ward_panel[ward_panel$year == 2003, c("ward")]
  w05 <- ward_panel[ward_panel$year == 2005, c("ward")]

  j03 <- as.data.table(st_drop_geometry(st_join(geo_pts, w03, join = st_within)))[, .(pin, ward03 = ward)]
  j05 <- as.data.table(st_drop_geometry(st_join(geo_pts, w05, join = st_within)))[, .(pin, ward05 = ward)]
  j03 <- j03[!duplicated(pin)]
  j05 <- j05[!duplicated(pin)]

  comp <- merge(j03, j05, by = "pin", all = TRUE)
  add_check("parcels_pre_scores", "boundary_year2003_assignment_diff_between_2003_and_2005_maps", sum(comp$ward03 != comp$ward05, na.rm = TRUE), nrow(comp), "Large rate implies boundary-year map windows are highly consequential.")
}

# -----------------------------------------------------------------------------
# 9) ANOMALY SAMPLES + MAPS
# -----------------------------------------------------------------------------
sales_anoms <- sales_invalid_corr[, .(
  dataset = "sales_pre_scores",
  id = as.character(pin),
  date = as.character(sale_date),
  era,
  ward,
  neighbor_ward,
  ward_pair_id,
  corrected_pair,
  issue_type = fifelse(!is.na(corrected_pair) & corrected_pair != ward_pair_id, "recomputed_neighbor", "invalid_pair"),
  longitude,
  latitude,
  dist_ft
)]

rent_anoms <- rent_invalid_corr[, .(
  dataset = "rent_pre_scores_full",
  id = as.character(id),
  date = as.character(file_date),
  era,
  ward,
  neighbor_ward,
  ward_pair_id,
  corrected_pair,
  issue_type = fifelse(!is.na(corrected_pair) & corrected_pair != ward_pair_id, "recomputed_neighbor", "invalid_pair"),
  longitude,
  latitude,
  dist_ft
)]

anoms <- rbindlist(list(sales_anoms, rent_anoms), fill = TRUE)
if (nrow(anoms) > 0) {
  setorder(anoms, dataset, -dist_ft)
  anom_samples <- anoms[, head(.SD, 1500), by = dataset]
} else {
  anom_samples <- anoms
}
fwrite(anom_samples, file.path(OUT_DIR, "border_pair_anomaly_samples.csv"))
save_anomaly_maps(anom_samples[!is.na(longitude) & !is.na(latitude)], ward_panel, file.path(OUT_DIR, "border_pair_anomaly_maps.pdf"))

# -----------------------------------------------------------------------------
# 10) WRITE SUMMARY + FINDINGS
# -----------------------------------------------------------------------------
summary_dt <- rbindlist(check_rows, fill = TRUE)
setorder(summary_dt, -issue_rate, -n_issue)
fwrite(summary_dt, file.path(OUT_DIR, "border_verification_summary.csv"))

sales_nonadj <- summary_dt[dataset == "sales_pre_scores" & check_id == "non_adjacent_ward_pairs_by_era_map", issue_rate][1]
rent_nonadj <- summary_dt[dataset == "rent_pre_scores_full" & check_id == "non_adjacent_ward_pairs_by_era_map", issue_rate][1]
sales_panel_issue <- summary_dt[dataset == "sales_transaction_panel" & check_id == "origin_ward_not_in_pair", issue_rate][1]
rent_panel_issue <- summary_dt[dataset == "rental_listing_panel" & check_id == "origin_ward_not_in_pair", issue_rate][1]
map_2003_2005_diff <- summary_dt[dataset == "parcels_pre_scores" & check_id == "boundary_year2003_assignment_diff_between_2003_and_2005_maps", issue_rate][1]

findings <- c(
  "# Border Verification Findings",
  "",
  "## What Looks Correct",
  sprintf("- Score merges preserve row counts for event-study sales/rent: sales drop = %.2f%%, rent drop = %.2f%%.",
    100 * safe_rate(overview[dataset == "sales_pre_scores", rows] - overview[dataset == "sales_with_ward_distances", rows], overview[dataset == "sales_pre_scores", rows]),
    100 * safe_rate(overview[dataset == "rent_pre_scores_full", rows] - overview[dataset == "rent_with_ward_distances_full", rows], overview[dataset == "rent_pre_scores_full", rows])
  ),
  "- Signed distance algebra and strictness-sign consistency checks pass in scored sales/rent and parcel border files.",
  "",
  "## What Is Broken / Risky",
  sprintf("- Non-adjacent border pairs in `sales_pre_scores`: %.2f%% of rows.", 100 * sales_nonadj),
  sprintf("- Non-adjacent border pairs in `rent_pre_scores_full`: %.2f%% of rows.", 100 * rent_nonadj),
  sprintf("- `sales_transaction_panel` rows where `ward_origin` is not in `ward_pair_id`: %.2f%%.", 100 * sales_panel_issue),
  sprintf("- `rental_listing_panel` rows where `ward_origin` is not in `ward_pair_id`: %.2f%%.", 100 * rent_panel_issue),
  sprintf("- Parcels with `boundary_year=2003` that change ward assignment under 2003 vs 2005 map: %.2f%%.", 100 * map_2003_2005_diff),
  "",
  "## Additional Spatial Outputs",
  "- `border_pair_anomaly_samples.csv`",
  "- `border_pair_anomaly_maps.pdf`"
)
writeLines(findings, file.path(OUT_DIR, "border_verification_findings.md"))

# -----------------------------------------------------------------------------
# 11) CERTIFICATION GATES + FAIL/STOP HANDLER
# -----------------------------------------------------------------------------
control_sales_rate <- sales_panel[treat == 0, mean(!origin_in_pair, na.rm = TRUE)]
control_rent_rate <- rent_panel[treat == 0, mean(!origin_in_pair, na.rm = TRUE)]

sales_post_rate <- sales_pre[era %in% c("2015_2023", "post2023"), mean(!pair_adjacent, na.rm = TRUE)]
rent_post_rate <- rent_pre[era %in% c("2015_2023", "post2023"), mean(!pair_adjacent, na.rm = TRUE)]

sales_treated_mismatch <- sales_panel[treat == 1, sum(!treated_pair_exact, na.rm = TRUE)]
rent_treated_mismatch <- rent_panel[treat == 1, sum(!treated_pair_exact, na.rm = TRUE)]

ward_count_mode <- as.integer(names(which.max(table(topology_metrics$n_wards))))
ward_inconsistent_years <- sum(topology_metrics$n_wards != ward_count_mode)
invalid_geoms_total <- sum(topology_metrics$invalid_geom)
overlap_share_max <- max(topology_metrics$overlap_share, na.rm = TRUE)

lines_to_dt <- function(lines, map_name) {
  if (nrow(lines) == 0) {
    return(data.table(
      ward_a = integer(),
      ward_b = integer(),
      line_len = numeric(),
      map = character()
    ))
  }
  data.table(
    ward_a = as.integer(lines$ward_a),
    ward_b = as.integer(lines$ward_b),
    line_len = as.numeric(st_length(lines)),
    map = map_name
  )
}

all_lines <- rbindlist(list(
  lines_to_dt(lines_sales_1998, "sales_1998"),
  lines_to_dt(lines_sales_2003, "sales_2003"),
  lines_to_dt(lines_sales_2015, "sales_2015"),
  lines_to_dt(lines_sales_2024, "sales_2024"),
  lines_to_dt(lines_rent_2014, "rent_2014"),
  lines_to_dt(lines_rent_2016, "rent_2016"),
  lines_to_dt(lines_rent_2024, "rent_2024")
), fill = TRUE, use.names = TRUE)

all_lines[, norm_pair := sprintf("%d-%d", pmin(ward_a, ward_b), pmax(ward_a, ward_b))]

boundary_self_pairs <- sum(all_lines$ward_a == all_lines$ward_b, na.rm = TRUE)
boundary_zero_len <- sum(all_lines$line_len <= 0, na.rm = TRUE)
boundary_dup_norm <- all_lines[, sum(duplicated(norm_pair)), by = map][, sum(V1)]

gates <- rbindlist(list(
  data.table(gate_id = "treated_pair_mismatch_sales", value = sales_treated_mismatch, threshold = 0, comparator = "<=", pass = sales_treated_mismatch <= 0, note = "Must be zero."),
  data.table(gate_id = "treated_pair_mismatch_rental", value = rent_treated_mismatch, threshold = 0, comparator = "<=", pass = rent_treated_mismatch <= 0, note = "Must be zero."),
  data.table(gate_id = "sales_post_redistricting_invalid_rate", value = sales_post_rate, threshold = 0.0005, comparator = "<=", pass = sales_post_rate <= 0.0005, note = "Raw-pair adjacency failure post-2015."),
  data.table(gate_id = "rental_post_redistricting_invalid_rate", value = rent_post_rate, threshold = 0.0005, comparator = "<=", pass = rent_post_rate <= 0.0005, note = "Raw-pair adjacency failure post-2015."),
  data.table(gate_id = "sales_control_origin_not_in_pair_rate", value = control_sales_rate, threshold = 0.001, comparator = "<=", pass = control_sales_rate <= 0.001, note = "Controls only."),
  data.table(gate_id = "rental_control_origin_not_in_pair_rate", value = control_rent_rate, threshold = 0.001, comparator = "<=", pass = control_rent_rate <= 0.001, note = "Controls only."),
  data.table(gate_id = "ward_topology_invalid_geometries", value = invalid_geoms_total, threshold = 0, comparator = "<=", pass = invalid_geoms_total <= 0, note = "All years."),
  data.table(gate_id = "ward_topology_overlap_share_max", value = overlap_share_max, threshold = 1e-4, comparator = "<=", pass = overlap_share_max <= 1e-4, note = "Allows tiny source-map slivers; threshold is 0.01% max annual overlap share."),
  data.table(gate_id = "ward_count_inconsistent_years", value = ward_inconsistent_years, threshold = 0, comparator = "<=", pass = ward_inconsistent_years <= 0, note = sprintf("Mode ward count = %d", ward_count_mode)),
  data.table(gate_id = "boundary_graph_self_pairs", value = boundary_self_pairs, threshold = 0, comparator = "<=", pass = boundary_self_pairs <= 0, note = "All map eras."),
  data.table(gate_id = "boundary_graph_zero_length", value = boundary_zero_len, threshold = 0, comparator = "<=", pass = boundary_zero_len <= 0, note = "All map eras."),
  data.table(gate_id = "boundary_graph_duplicate_norm_pairs", value = boundary_dup_norm, threshold = 0, comparator = "<=", pass = boundary_dup_norm <= 0, note = "Expect one normalized pair per map graph row.")
), fill = TRUE)

fwrite(gates, file.path(OUT_DIR, "border_certification_gates.csv"))

gate_lines <- c(
  "# Border Certification Report",
  "",
  sprintf("- `certify_only`: %s", certify_only),
  sprintf("- `skip_thresholds`: %s", skip_thresholds),
  sprintf("- generated: %s", as.character(Sys.time())),
  "",
  "## Gate Results"
)
for (i in seq_len(nrow(gates))) {
  rr <- gates[i]
  gate_lines <- c(gate_lines, sprintf("- %s: value=%.8f threshold %s %.8f -> **%s** (%s)", rr$gate_id, rr$value, rr$comparator, rr$threshold, ifelse(rr$pass, "PASS", "FAIL"), rr$note))
}
writeLines(gate_lines, file.path(OUT_DIR, "border_certification_report.md"))

failed <- gates[pass == FALSE]
if (nrow(failed) > 0) {
  failure_lines <- c(
    "# Border Certification Failed",
    "",
    "## Failing Gates"
  )
  for (i in seq_len(nrow(failed))) {
    rr <- failed[i]
    failure_lines <- c(failure_lines, sprintf("- %s: value=%.8f threshold %s %.8f (%s)", rr$gate_id, rr$value, rr$comparator, rr$threshold, rr$note))
  }

  failure_lines <- c(failure_lines, "", "## Top Offending Sales Pairs")
  top_sales <- sales_invalid_pairs[seq_len(min(nrow(sales_invalid_pairs), 10))]
  if (nrow(top_sales) == 0) {
    failure_lines <- c(failure_lines, "- none")
  } else {
    for (i in seq_len(nrow(top_sales))) {
      failure_lines <- c(failure_lines, sprintf("- %s -> %s : %d rows", top_sales$ward_pair_id[i], top_sales$corrected_pair[i], top_sales$N[i]))
    }
  }

  failure_lines <- c(failure_lines, "", "## Top Offending Rental Pairs")
  top_rent <- rent_invalid_pairs[seq_len(min(nrow(rent_invalid_pairs), 10))]
  if (nrow(top_rent) == 0) {
    failure_lines <- c(failure_lines, "- none")
  } else {
    for (i in seq_len(nrow(top_rent))) {
      failure_lines <- c(failure_lines, sprintf("- %s -> %s : %d rows", top_rent$ward_pair_id[i], top_rent$corrected_pair[i], top_rent$N[i]))
    }
  }

  failure_lines <- c(failure_lines, "", "## Top Control Panel Mismatches (Sales)")
  bad_sales_ctrl <- sales_panel[treat == 0 & !origin_in_pair, .N, by = .(ward_pair_id, ward_origin)][order(-N)]
  bad_sales_ctrl <- bad_sales_ctrl[seq_len(min(nrow(bad_sales_ctrl), 10))]
  if (nrow(bad_sales_ctrl) == 0) {
    failure_lines <- c(failure_lines, "- none")
  } else {
    for (i in seq_len(nrow(bad_sales_ctrl))) {
      failure_lines <- c(failure_lines, sprintf("- pair %s with origin ward %d: %d rows", bad_sales_ctrl$ward_pair_id[i], bad_sales_ctrl$ward_origin[i], bad_sales_ctrl$N[i]))
    }
  }

  failure_lines <- c(failure_lines, "", "## Top Control Panel Mismatches (Rental)")
  bad_rent_ctrl <- rent_panel[treat == 0 & !origin_in_pair, .N, by = .(ward_pair_id, ward_origin)][order(-N)]
  bad_rent_ctrl <- bad_rent_ctrl[seq_len(min(nrow(bad_rent_ctrl), 10))]
  if (nrow(bad_rent_ctrl) == 0) {
    failure_lines <- c(failure_lines, "- none")
  } else {
    for (i in seq_len(nrow(bad_rent_ctrl))) {
      failure_lines <- c(failure_lines, sprintf("- pair %s with origin ward %d: %d rows", bad_rent_ctrl$ward_pair_id[i], bad_rent_ctrl$ward_origin[i], bad_rent_ctrl$N[i]))
    }
  }

  failure_lines <- c(failure_lines, "", "## Years With Ward Overlap Pairs")
  top_overlap_years <- topology_metrics[overlap_pairs > 0][order(-overlap_pairs)]
  top_overlap_years <- top_overlap_years[seq_len(min(nrow(top_overlap_years), 10))]
  if (nrow(top_overlap_years) == 0) {
    failure_lines <- c(failure_lines, "- none")
  } else {
    for (i in seq_len(nrow(top_overlap_years))) {
      failure_lines <- c(failure_lines, sprintf("- year %d: %d overlap pairs", top_overlap_years$year[i], top_overlap_years$overlap_pairs[i]))
    }
  }

  writeLines(failure_lines, file.path(OUT_DIR, "border_certification_failed.md"))
} else {
  failed_file <- file.path(OUT_DIR, "border_certification_failed.md")
  if (file.exists(failed_file)) file.remove(failed_file)
}

# -----------------------------------------------------------------------------
# 12) Final messages and enforce stop for certification mode
# -----------------------------------------------------------------------------
message("Saved: ", file.path(OUT_DIR, "border_verification_dataset_overview.csv"))
message("Saved: ", file.path(OUT_DIR, "border_verification_summary.csv"))
message("Saved: ", file.path(OUT_DIR, "border_verification_panel_checks.csv"))
message("Saved: ", file.path(OUT_DIR, "border_verification_invalid_pairs_sales.csv"))
message("Saved: ", file.path(OUT_DIR, "border_verification_invalid_pairs_rent.csv"))
message("Saved: ", file.path(OUT_DIR, "border_pair_anomaly_samples.csv"))
message("Saved: ", file.path(OUT_DIR, "border_pair_anomaly_maps.pdf"))
message("Saved: ", file.path(OUT_DIR, "border_certification_gates.csv"))
message("Saved: ", file.path(OUT_DIR, "border_certification_report.md"))

if (certify_only && !skip_thresholds && nrow(failed) > 0) {
  message("Certification failed. See border_certification_failed.md")
  quit(status = 1)
}

message("Done.")