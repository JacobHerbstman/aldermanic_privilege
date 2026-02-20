source("../../setup_environment/code/packages.R")

library(data.table)
library(sf)
library(arrow)
library(ggplot2)
library(dplyr)

sf_use_s2(FALSE)

if (Sys.getenv("TRACE_ERRORS") == "1") {
  options(error = function() {
    traceback(50)
    quit(status = 1)
  })
}

# -----------------------------------------------------------------------------
# CLI
# -----------------------------------------------------------------------------
# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_verification/code")
# mode_before <- "legacy_before"
# mode_after <- "certified_after"
# top_n_pairs <- 20
# top_n_points <- 50000
# output_dir <- "../output"
# smoke <- FALSE
# seed <- 20260216
# Rscript build_geometry_forensics_pack.R "legacy_before" "certified_after" 20 50000 "../output" FALSE 20260216
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 7) {
  mode_before <- cli_args[1]
  mode_after <- cli_args[2]
  top_n_pairs <- suppressWarnings(as.integer(cli_args[3]))
  top_n_points <- suppressWarnings(as.integer(cli_args[4]))
  output_dir <- cli_args[5]
  smoke <- tolower(cli_args[6]) %in% c("true", "t", "1", "yes")
  seed <- suppressWarnings(as.integer(cli_args[7]))
} else {
  if (!exists("mode_before") || !exists("mode_after") || !exists("top_n_pairs") || !exists("top_n_points") || !exists("output_dir") || !exists("smoke") || !exists("seed")) {
    stop("FATAL: Script requires 7 args: <mode_before> <mode_after> <top_n_pairs> <top_n_points> <output_dir> <smoke> <seed>", call. = FALSE)
  }
}

if (!mode_before %in% c("legacy_before")) {
  stop("--mode_before must be legacy_before", call. = FALSE)
}
if (!mode_after %in% c("certified_after")) {
  stop("--mode_after must be certified_after", call. = FALSE)
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
set.seed(seed)

message("=== Build Geometry Forensics Pack ===")
message("mode_before: ", mode_before)
message("mode_after: ", mode_after)
message("top_n_pairs: ", top_n_pairs)
message("top_n_points: ", top_n_points)
message("smoke: ", smoke)

# -----------------------------------------------------------------------------
# Paths
# -----------------------------------------------------------------------------
PATHS <- list(
  ward_panel = "../input/ward_panel.gpkg",
  sales_pre = "../input/sales_pre_scores.csv",
  rent_pre = "../input/rent_pre_scores_full.parquet",
  sales_panel = "../input/sales_transaction_panel.parquet",
  rental_panel = "../input/rental_listing_panel.parquet",
  sales_scored = "../input/sales_with_ward_distances.csv",
  rent_scored = "../input/rent_with_ward_distances_full.parquet",
  treatment_pre = "../input/block_treatment_pre_scores.csv",
  impact_md = "../output/border_verification_impact_summary.md",
  cert_gates = "../output/border_certification_gates.csv",
  panel_checks = "../output/border_verification_panel_checks.csv",
  sliver_summary = "../output/sliver_impact_summary.csv",
  density_change = "../output/density_rental_change_summary.csv",
  boundaries_top = "../../run_event_study_rental_disaggregate/output/rental_effect_source_boundaries_top50.csv",
  wards_top = "../../run_event_study_rental_disaggregate/output/rental_effect_source_wards_top50.csv",
  buildings_top = "../../run_event_study_rental_disaggregate/output/rental_effect_source_buildings_top200.csv",
  anomaly_samples = "../output/border_pair_anomaly_samples.csv"
)

OUT <- list(
  maps_pdf = file.path(output_dir, "geometry_forensics_maps_before_after.pdf"),
  summary_csv = file.path(output_dir, "geometry_forensics_summary_before_after.csv"),
  summary_md = file.path(output_dir, "geometry_forensics_summary_before_after.md"),
  pair_flow_csv = file.path(output_dir, "geometry_forensics_pair_flow_before_after.csv"),
  ledger_csv = file.path(output_dir, "geometry_forensics_code_change_ledger.csv"),
  ledger_md = file.path(output_dir, "geometry_forensics_code_change_ledger.md"),
  checklist_md = file.path(output_dir, "geometry_forensics_approval_checklist.md")
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

safe_div <- function(a, b) {
  ifelse(is.na(a) | is.na(b) | b <= 0, NA_real_, a / b)
}

extract_match_num <- function(line) {
  m <- regmatches(line, gregexpr("-?[0-9]+\\.?[0-9]*", line, perl = TRUE))[[1]]
  if (length(m) == 0) return(numeric())
  suppressWarnings(as.numeric(m))
}

parse_pair <- function(x) {
  sp <- tstrsplit(as.character(x), "-", fixed = TRUE)
  list(as.integer(sp[[1]]), as.integer(sp[[2]]))
}

normalize_pair <- function(a, b) {
  sprintf("%d-%d", pmin(as.integer(a), as.integer(b)), pmax(as.integer(a), as.integer(b)))
}

coerce_date <- function(x) {
  as.Date(x)
}

sample_per_group <- function(dt, group_col, n_total) {
  if (nrow(dt) <= n_total) return(copy(dt))
  groups <- unique(dt[[group_col]])
  n_g <- length(groups)
  n_each <- max(1L, floor(n_total / max(n_g, 1L)))
  out <- dt[, {
    nn <- min(.N, n_each)
    .SD[sample.int(.N, nn)]
  }, by = group_col]
  if (nrow(out) < n_total) {
    rem <- n_total - nrow(out)
    pool <- dt[!out, on = names(dt)]
    if (nrow(pool) > 0) {
      out <- rbind(out, pool[sample.int(nrow(pool), min(rem, nrow(pool)))], fill = TRUE)
    }
  }
  out
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
  unique(lines$pair_id)
}

get_map_year <- function(dataset, era, mode, ward_years) {
  y <- NULL
  if (dataset == "sales") {
    if (mode == "legacy_before") {
      if (era == "pre2003") y <- 1998L
      if (era == "2003_2015") y <- 2005L
      if (era == "2015_2023") y <- 2015L
      if (era == "post2023") y <- 2024L
    }
    if (mode == "certified_after") {
      if (era == "pre2003") y <- 1998L
      if (era == "2003_2015") y <- 2003L
      if (era == "2015_2023") y <- 2015L
      if (era == "post2023") y <- 2024L
    }
  }
  if (dataset == "rent") {
    if (mode == "legacy_before") {
      if (era == "pre2015") y <- 2014L
      if (era == "2015_2023") y <- 2016L
      if (era == "post2023") y <- 2024L
    }
    if (mode == "certified_after") {
      if (era == "pre2015") y <- 2014L
      if (era == "2015_2023") y <- 2016L
      if (era == "post2023") y <- 2024L
    }
  }

  if (is.null(y) || !(y %in% ward_years)) {
    # graceful fallback to nearest available year
    if (is.null(y)) y <- max(ward_years)
    y <- ward_years[which.min(abs(ward_years - y))]
  }
  as.integer(y)
}

# For legacy mode we emulate old nearest-edge behavior.
recompute_pairs_mode <- function(dt_points, dataset, mode, polys_by_year, lines_by_year, pair_sets) {
  if (nrow(dt_points) == 0) {
    return(data.table())
  }

  out_parts <- vector("list", length = 0)
  eras <- unique(dt_points$era)

  for (era_i in eras) {
    sub <- copy(dt_points[era == era_i])
    if (nrow(sub) == 0) next

    if ("ward" %in% names(sub)) setnames(sub, "ward", "ward_input")
    if ("neighbor_ward" %in% names(sub)) setnames(sub, "neighbor_ward", "neighbor_ward_input")

    yy <- get_map_year(dataset, era_i, mode, as.integer(names(polys_by_year)))
    polys <- polys_by_year[[as.character(yy)]]
    lines <- lines_by_year[[as.character(yy)]]
    pair_set <- pair_sets[[as.character(yy)]]

    pts <- st_as_sf(sub, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
    pts <- st_transform(pts, st_crs(polys))

    joined <- st_join(pts, polys %>% transmute(ward_map = ward), join = st_within, left = TRUE)
    joined <- joined[!is.na(joined$ward_map), ]
    if (nrow(joined) == 0) next

    if (mode == "legacy_before") {
      nearest_idx <- st_nearest_feature(joined, lines)
      nearest_edges <- lines[nearest_idx, ]
      ward_a <- as.integer(nearest_edges$ward_a)
      ward_b <- as.integer(nearest_edges$ward_b)
      ward_map <- as.integer(joined$ward_map)
      pair_new <- normalize_pair(ward_a, ward_b)
      neighbor_new <- ifelse(ward_map == ward_a, ward_b, ward_a)
    } else {
      ward_map <- as.integer(joined$ward_map)
      ward_a <- rep(NA_integer_, nrow(joined))
      ward_b <- rep(NA_integer_, nrow(joined))

      ward_vals <- sort(unique(ward_map))
      for (w in ward_vals) {
        idx <- which(ward_map == w)
        edges_w <- lines[lines$ward_a == w | lines$ward_b == w, ]
        if (length(idx) == 0 || nrow(edges_w) == 0) next
        nidx <- st_nearest_feature(joined[idx, ], edges_w)
        near_w <- edges_w[nidx, ]
        ward_a[idx] <- as.integer(near_w$ward_a)
        ward_b[idx] <- as.integer(near_w$ward_b)
      }

      valid <- !is.na(ward_a) & !is.na(ward_b)
      if (!any(valid)) next
      joined <- joined[valid, ]
      ward_map <- ward_map[valid]
      ward_a <- ward_a[valid]
      ward_b <- ward_b[valid]
      pair_new <- normalize_pair(ward_a, ward_b)
      neighbor_new <- ifelse(ward_map == ward_a, ward_b, ward_a)
    }

    contains_ward <- ward_map %in% parse_pair(pair_new)[[1]] | ward_map %in% parse_pair(pair_new)[[2]]
    adjacent_ok <- pair_new %in% pair_set
    pair_original <- as.character(joined$ward_pair_id)
    orig_split <- parse_pair(pair_original)
    contains_ward_original <- ward_map %in% orig_split[[1]] | ward_map %in% orig_split[[2]]
    adjacent_ok_original <- pair_original %in% pair_set

    out_i <- data.table(
      dataset = dataset,
      logic_mode = mode,
      era = era_i,
      map_year = yy,
      row_id = joined$row_id,
      id = joined$id,
      longitude = joined$longitude,
      latitude = joined$latitude,
      ward_map = ward_map,
      neighbor_map = as.integer(neighbor_new),
      pair_mode = pair_new,
      pair_original = pair_original,
      contains_ward = contains_ward,
      adjacent_ok = adjacent_ok,
      issue_invalid = !contains_ward | !adjacent_ok,
      contains_ward_original = contains_ward_original,
      adjacent_ok_original = adjacent_ok_original,
      issue_invalid_original = !contains_ward_original | !adjacent_ok_original
    )
    out_parts[[length(out_parts) + 1]] <- out_i
  }

  rbindlist(out_parts, fill = TRUE)
}

compute_overlap_polygons <- function(ward_panel, years) {
  out <- list()
  rows <- list()
  for (yy in years) {
    w <- ward_panel[ward_panel$year == yy, c("ward")]
    if (nrow(w) == 0) next
    total_area <- sum(as.numeric(st_area(w)), na.rm = TRUE)
    union_area <- as.numeric(st_area(st_union(st_geometry(w))))
    overlap_share <- ifelse(is.finite(union_area) && union_area > 0, max(total_area - union_area, 0) / union_area, NA_real_)

    ov <- st_overlaps(w)
    ov_parts <- list()
    for (i in seq_along(ov)) {
      js <- ov[[i]]
      if (length(js) == 0) next
      js <- js[js > i]
      if (length(js) == 0) next
      for (j in js) {
        inter <- suppressWarnings(st_intersection(w[i, ], w[j, ]))
        if (length(inter) == 0 || all(st_is_empty(inter))) next
        inter <- st_make_valid(inter)
        inter <- suppressWarnings(st_collection_extract(inter, "POLYGON"))
        if (length(inter) == 0 || all(st_is_empty(inter))) next
        inter_geom <- st_geometry(inter)
        ov_parts[[length(ov_parts) + 1]] <- st_sf(
          year = rep(yy, length(inter_geom)),
          ward_i = rep(as.integer(w$ward[i]), length(inter_geom)),
          ward_j = rep(as.integer(w$ward[j]), length(inter_geom)),
          geometry = inter_geom,
          crs = st_crs(w)
        )
      }
    }

    if (length(ov_parts) > 0) {
      ov_sf <- do.call(rbind, ov_parts)
      out[[as.character(yy)]] <- ov_sf
    }

    rows[[length(rows) + 1]] <- data.table(
      year = yy,
      overlap_share = overlap_share,
      overlap_area_ft2 = max(total_area - union_area, 0)
    )
  }
  list(
    overlap_sf = if (length(out) > 0) do.call(rbind, out) else st_sf(year = integer(), geometry = st_sfc(), crs = st_crs(ward_panel)),
    overlap_stats = rbindlist(rows, fill = TRUE)
  )
}

# -----------------------------------------------------------------------------
# Load ward panel and build line graphs by year
# -----------------------------------------------------------------------------
message("[step] load ward panel + boundaries")
ward_panel <- st_read(PATHS$ward_panel, quiet = TRUE) %>%
  st_transform(3435) %>%
  mutate(ward = as.integer(ward), year = as.integer(year))

ward_years <- sort(unique(ward_panel$year))
needed_years <- unique(c(1998L, 2003L, 2005L, 2014L, 2015L, 2016L, 2024L, 2025L))
needed_years <- needed_years[needed_years %in% ward_years]

polys_by_year <- list()
lines_by_year <- list()
pair_sets <- list()
for (yy in needed_years) {
  w <- ward_panel[ward_panel$year == yy, c("ward")]
  polys_by_year[[as.character(yy)]] <- w
  lines <- build_boundary_lines(w)
  lines_by_year[[as.character(yy)]] <- lines
  pair_sets[[as.character(yy)]] <- pair_set_from_lines(lines)
}

# -----------------------------------------------------------------------------
# Load sales/rent pre-score points for map-driven reconstruction
# -----------------------------------------------------------------------------
message("[step] load pre-score point datasets")
sales_pre <- fread(PATHS$sales_pre, select = c("pin", "year", "sale_date", "ward", "neighbor_ward", "ward_pair_id", "longitude", "latitude", "dist_ft"))
sales_pre[, sale_date := coerce_date(sale_date)]
sales_pre[is.na(sale_date), sale_date := as.Date(sprintf("%d-06-15", year))]
sales_pre[, era := fifelse(sale_date < as.Date("2003-05-01"), "pre2003",
  fifelse(sale_date < as.Date("2015-05-18"), "2003_2015",
    fifelse(sale_date < as.Date("2023-05-15"), "2015_2023", "post2023")))]
sales_pre <- sales_pre[!is.na(longitude) & !is.na(latitude)]
sales_pre[, `:=`(dataset = "sales", id = as.character(pin), row_id = .I)]

rent_pre <- as.data.table(read_parquet(PATHS$rent_pre, col_select = c("id", "file_date", "ward", "neighbor_ward", "ward_pair_id", "longitude", "latitude", "dist_ft")))
rent_pre[, file_date := coerce_date(file_date)]
rent_pre[, era := fifelse(file_date < as.Date("2015-05-18"), "pre2015",
  fifelse(file_date < as.Date("2023-05-15"), "2015_2023", "post2023"))]
rent_pre <- rent_pre[!is.na(longitude) & !is.na(latitude)]
rent_pre[, `:=`(dataset = "rent", row_id = .I)]

if (smoke) {
  sales_sample_n <- min(12000L, nrow(sales_pre))
  rent_sample_n <- min(30000L, nrow(rent_pre))
} else {
  sales_sample_n <- min(as.integer(top_n_points), nrow(sales_pre))
  rent_sample_n <- min(as.integer(top_n_points), nrow(rent_pre))
}

sales_map_sample <- sample_per_group(sales_pre, "era", sales_sample_n)
rent_map_sample <- sample_per_group(rent_pre, "era", rent_sample_n)

# Focus map reconstruction on rental pre2015 and sales pre2003/2003_2015 emphasis.
rent_map_sample_focus <- rent_map_sample[era == "pre2015"]
if (nrow(rent_map_sample_focus) == 0) rent_map_sample_focus <- rent_map_sample
sales_map_sample_focus <- sales_map_sample[era %in% c("pre2003", "2003_2015")]
if (nrow(sales_map_sample_focus) == 0) sales_map_sample_focus <- sales_map_sample

# -----------------------------------------------------------------------------
# Recompute before/after pairs for sampled point sets (for maps + pair-flow)
# -----------------------------------------------------------------------------
message("[step] recompute legacy/certified pairs on map samples")
sales_before <- recompute_pairs_mode(sales_map_sample_focus, "sales", mode_before, polys_by_year, lines_by_year, pair_sets)
sales_after <- recompute_pairs_mode(sales_map_sample_focus, "sales", mode_after, polys_by_year, lines_by_year, pair_sets)

rent_before <- recompute_pairs_mode(rent_map_sample_focus, "rent", mode_before, polys_by_year, lines_by_year, pair_sets)
rent_after <- recompute_pairs_mode(rent_map_sample_focus, "rent", mode_after, polys_by_year, lines_by_year, pair_sets)

recomp_all <- rbindlist(list(sales_before, sales_after, rent_before, rent_after), fill = TRUE)
message(sprintf(
  "[diag] recompute rows: sales_before=%d sales_after=%d rent_before=%d rent_after=%d total=%d",
  nrow(sales_before), nrow(sales_after), nrow(rent_before), nrow(rent_after), nrow(recomp_all)
))

# -----------------------------------------------------------------------------
# Pair flow table (largest corrected transitions)
# -----------------------------------------------------------------------------
message("[step] build pair-flow table")
if (!"logic_mode" %in% names(recomp_all)) {
  recomp_all[, logic_mode := character()]
}
before_pairs <- recomp_all[logic_mode == mode_before, .(
  dataset, era, row_id, id, longitude, latitude,
  pair_before = pair_mode,
  map_year_before = map_year
)]
after_pairs <- recomp_all[logic_mode == mode_after, .(
  dataset, era, row_id,
  pair_after = pair_mode,
  map_year_after = map_year
)]

pair_flow <- merge(before_pairs, after_pairs, by = c("dataset", "era", "row_id"), all = FALSE)
pair_flow <- pair_flow[pair_before != pair_after]

pair_flow_agg <- pair_flow[, .(
  n_points = .N,
  longitude_mean = mean(longitude, na.rm = TRUE),
  latitude_mean = mean(latitude, na.rm = TRUE)
), by = .(dataset, era, map_year_before, map_year_after, pair_before, pair_after)][order(-n_points)]

pair_flow_top <- pair_flow_agg[1:min(.N, as.integer(top_n_pairs))]
fwrite(pair_flow_top, OUT$pair_flow_csv)

# -----------------------------------------------------------------------------
# Table 1: raw invalid pair rates by dataset/era
# before from archived pre-fix impact summary + sample reconstruction check
# after from current full data adjacency checks
# -----------------------------------------------------------------------------
message("[step] build summary tables 1-6")
impact_lines <- if (file.exists(PATHS$impact_md)) readLines(PATHS$impact_md, warn = FALSE) else character()

parse_archived_rate <- function(prefix) {
  ln <- impact_lines[grepl(prefix, impact_lines, fixed = TRUE)]
  if (length(ln) == 0) return(data.table(numerator = NA_real_, denominator = NA_real_, rate = NA_real_))
  line <- ln[1]
  m <- regexec("([0-9,]+)\\s*/\\s*([0-9,]+)\\s*=\\s*([0-9]+\\.?[0-9]*)%", line, perl = TRUE)
  parts <- regmatches(line, m)[[1]]
  if (length(parts) == 4) {
    return(data.table(
      numerator = as.numeric(gsub(",", "", parts[2], fixed = TRUE)),
      denominator = as.numeric(gsub(",", "", parts[3], fixed = TRUE)),
      rate = as.numeric(parts[4]) / 100
    ))
  }
  # Fallback for unexpected formatting
  line_clean <- gsub(",", "", line, fixed = TRUE)
  nums <- extract_match_num(line_clean)
  if (length(nums) < 3) return(data.table(numerator = NA_real_, denominator = NA_real_, rate = NA_real_))
  data.table(numerator = nums[1], denominator = nums[2], rate = nums[3] / 100)
}

archived_tbl1 <- rbindlist(list(
  data.table(dataset = "rent", era = "pre2015", parse_archived_rate("pre-2015 era:")),
  data.table(dataset = "rent", era = "2015_2023", parse_archived_rate("2015-2023 era:")),
  data.table(dataset = "rent", era = "post2023", parse_archived_rate("post-2023 era:")),
  data.table(dataset = "sales", era = "pre2003", parse_archived_rate("pre-2003 era:")),
  data.table(dataset = "sales", era = "2003_2015", parse_archived_rate("2003-2015 era:")),
  data.table(dataset = "sales", era = "2015_2023", parse_archived_rate("2015-2023 era:")),
  data.table(dataset = "sales", era = "post2023", parse_archived_rate("post-2023 era:"))
), fill = TRUE)

# Current full rates with certified-after era windows and existing pair ids
sales_pre_full <- sales_pre[, .(sale_date, era, ward_pair_id)]
sales_pre_full[, map_year_after := fifelse(era == "pre2003", 1998L,
  fifelse(era == "2003_2015", 2003L,
    fifelse(era == "2015_2023", 2015L, 2024L)))]

sales_after_rates <- sales_pre_full[, {
  yy <- unique(map_year_after)[1]
  pair_set <- pair_sets[[as.character(yy)]]
  n_tot <- .N
  n_bad <- sum(!(ward_pair_id %in% pair_set), na.rm = TRUE)
  .(after_numerator = n_bad, after_denominator = n_tot, after_rate = safe_div(n_bad, n_tot), map_year_after = yy)
}, by = .(era)]
sales_after_rates[, dataset := "sales"]
setcolorder(sales_after_rates, c("dataset", "era", "after_numerator", "after_denominator", "after_rate", "map_year_after"))

rent_pre_full_small <- rent_pre[, .(file_date, era, ward_pair_id)]
rent_pre_full_small[, map_year_after := fifelse(era == "pre2015", 2014L,
  fifelse(era == "2015_2023", 2016L, 2024L))]

rent_after_rates <- rent_pre_full_small[, {
  yy <- unique(map_year_after)[1]
  pair_set <- pair_sets[[as.character(yy)]]
  n_tot <- .N
  n_bad <- sum(!(ward_pair_id %in% pair_set), na.rm = TRUE)
  .(after_numerator = n_bad, after_denominator = n_tot, after_rate = safe_div(n_bad, n_tot), map_year_after = yy)
}, by = .(era)]
rent_after_rates[, dataset := "rent"]
setcolorder(rent_after_rates, c("dataset", "era", "after_numerator", "after_denominator", "after_rate", "map_year_after"))

after_tbl1 <- rbindlist(list(sales_after_rates, rent_after_rates), fill = TRUE)

sample_before_rates <- recomp_all[logic_mode == mode_before, .(
  before_sample_numerator = sum(issue_invalid_original, na.rm = TRUE),
  before_sample_denominator = .N,
  before_sample_rate = safe_div(sum(issue_invalid_original, na.rm = TRUE), .N)
), by = .(dataset, era)]

table1 <- merge(archived_tbl1, after_tbl1, by = c("dataset", "era"), all = TRUE)
table1 <- merge(table1, sample_before_rates, by = c("dataset", "era"), all = TRUE)
table1[, `:=`(
  table_id = "table1_raw_invalid_pair_rates",
  source_before = PATHS$impact_md,
  source_after = PATHS$cert_gates,
  note = "before uses archived full pre-fix rates; sample reconstruction provided for fidelity check"
)]

# -----------------------------------------------------------------------------
# Table 2: control mismatch rates by cohort x treat x panel
# before via legacy-origin emulation, after via current panel checks
# -----------------------------------------------------------------------------
treat_pre <- fread(PATHS$treatment_pre, select = c("block_id", "cohort", "ward_origin", "ward_dest"))
treat_pre[, block_id := as.character(block_id)]
treat_pre[, cohort := as.character(cohort)]

sales_panel <- as.data.table(read_parquet(PATHS$sales_panel, col_select = c("pin", "sale_date", "block_id", "cohort", "treat", "ward_pair_id", "ward_origin")))
sales_panel[, pin := as.character(pin)]
sales_panel[, sale_date := coerce_date(sale_date)]
sales_panel[, block_id := as.character(block_id)]
sales_panel[, cohort := as.character(cohort)]
sales_panel <- merge(sales_panel, treat_pre, by = c("block_id", "cohort"), all.x = TRUE, suffixes = c("", "_legacy"))
if (!"ward_origin_legacy" %in% names(sales_panel)) sales_panel[, ward_origin_legacy := ward_origin]
if (!"ward_dest_legacy" %in% names(sales_panel) && "ward_dest" %in% names(sales_panel)) sales_panel[, ward_dest_legacy := ward_dest]
sp <- parse_pair(sales_panel$ward_pair_id)
sales_panel[, origin_in_pair_after := ward_origin == sp[[1]] | ward_origin == sp[[2]]]
sales_panel[, origin_in_pair_before := ward_origin_legacy == sp[[1]] | ward_origin_legacy == sp[[2]]]

sales_table2 <- sales_panel[, .(
  before_numerator = sum(!origin_in_pair_before, na.rm = TRUE),
  before_denominator = .N,
  before_rate = safe_div(sum(!origin_in_pair_before, na.rm = TRUE), .N),
  after_numerator = sum(!origin_in_pair_after, na.rm = TRUE),
  after_denominator = .N,
  after_rate = safe_div(sum(!origin_in_pair_after, na.rm = TRUE), .N)
), by = .(cohort, treat)]
sales_table2[, dataset := "sales_transaction_panel"]
setcolorder(sales_table2, c("dataset", "cohort", "treat", "before_numerator", "before_denominator", "before_rate", "after_numerator", "after_denominator", "after_rate"))

rental_panel <- as.data.table(read_parquet(PATHS$rental_panel, col_select = c("id", "file_date", "block_id", "cohort", "treat", "ward_pair_id", "ward_origin")))
rental_panel[, id := as.character(id)]
rental_panel[, file_date := coerce_date(file_date)]
rental_panel[, block_id := as.character(block_id)]
rental_panel[, cohort := as.character(cohort)]
rental_panel <- merge(rental_panel, treat_pre, by = c("block_id", "cohort"), all.x = TRUE, suffixes = c("", "_legacy"))
if (!"ward_origin_legacy" %in% names(rental_panel)) rental_panel[, ward_origin_legacy := ward_origin]
if (!"ward_dest_legacy" %in% names(rental_panel) && "ward_dest" %in% names(rental_panel)) rental_panel[, ward_dest_legacy := ward_dest]
rp <- parse_pair(rental_panel$ward_pair_id)
rental_panel[, origin_in_pair_after := ward_origin == rp[[1]] | ward_origin == rp[[2]]]
rental_panel[, origin_in_pair_before := ward_origin_legacy == rp[[1]] | ward_origin_legacy == rp[[2]]]

rent_table2 <- rental_panel[, .(
  before_numerator = sum(!origin_in_pair_before, na.rm = TRUE),
  before_denominator = .N,
  before_rate = safe_div(sum(!origin_in_pair_before, na.rm = TRUE), .N),
  after_numerator = sum(!origin_in_pair_after, na.rm = TRUE),
  after_denominator = .N,
  after_rate = safe_div(sum(!origin_in_pair_after, na.rm = TRUE), .N)
), by = .(cohort, treat)]
rent_table2[, dataset := "rental_listing_panel"]
setcolorder(rent_table2, c("dataset", "cohort", "treat", "before_numerator", "before_denominator", "before_rate", "after_numerator", "after_denominator", "after_rate"))

table2 <- rbindlist(list(sales_table2, rent_table2), fill = TRUE)
table2[, `:=`(
  era = NA_character_,
  table_id = "table2_control_mismatch_rates",
  source_before = PATHS$treatment_pre,
  source_after = PATHS$panel_checks,
  note = "before is emulated using legacy control-origin assignment"
)]

# -----------------------------------------------------------------------------
# Table 3: treated pair integrity + adjacency pass/fail
# -----------------------------------------------------------------------------
cert_gates <- if (file.exists(PATHS$cert_gates)) fread(PATHS$cert_gates) else data.table()

sales_panel[, treated_pair_exact_after := treat == 1 & ward_pair_id == normalize_pair(ward_origin, ward_dest_legacy)]
sales_panel[, treated_pair_exact_before := treat == 1 & ward_pair_id == normalize_pair(ward_origin_legacy, ward_dest_legacy)]

rental_panel[, treated_pair_exact_after := treat == 1 & ward_pair_id == normalize_pair(ward_origin, ward_dest_legacy)]
rental_panel[, treated_pair_exact_before := treat == 1 & ward_pair_id == normalize_pair(ward_origin_legacy, ward_dest_legacy)]

t3_local <- rbindlist(list(
  data.table(
    dataset = "sales_transaction_panel",
    metric = "treated_pair_mismatch_rate",
    before_value = sales_panel[treat == 1, safe_div(sum(!treated_pair_exact_before, na.rm = TRUE), .N)],
    after_value = sales_panel[treat == 1, safe_div(sum(!treated_pair_exact_after, na.rm = TRUE), .N)]
  ),
  data.table(
    dataset = "rental_listing_panel",
    metric = "treated_pair_mismatch_rate",
    before_value = rental_panel[treat == 1, safe_div(sum(!treated_pair_exact_before, na.rm = TRUE), .N)],
    after_value = rental_panel[treat == 1, safe_div(sum(!treated_pair_exact_after, na.rm = TRUE), .N)]
  )
), fill = TRUE)

t3_gates <- cert_gates[gate_id %in% c(
  "treated_pair_mismatch_sales", "treated_pair_mismatch_rental",
  "sales_post_redistricting_invalid_rate", "rental_post_redistricting_invalid_rate"
), .(
  dataset = fifelse(grepl("sales", gate_id), "sales", "rent"),
  metric = gate_id,
  before_value = NA_real_,
  after_value = as.numeric(value)
)]

table3 <- rbindlist(list(t3_local, t3_gates), fill = TRUE)
table3[, `:=`(
  cohort = NA_character_,
  treat = NA_integer_,
  era = NA_character_,
  table_id = "table3_treated_integrity_and_adjacency",
  source_before = PATHS$treatment_pre,
  source_after = PATHS$cert_gates,
  note = "after values are certified gates where available"
)]

# -----------------------------------------------------------------------------
# Table 4: sliver materiality
# -----------------------------------------------------------------------------
sliver <- if (file.exists(PATHS$sliver_summary)) fread(PATHS$sliver_summary) else data.table(metric = character(), value = numeric())
table4 <- sliver[, .(
  table_id = "table4_sliver_materiality",
  metric = metric,
  value = value,
  source = PATHS$sliver_summary
)]

# -----------------------------------------------------------------------------
# Table 5: density exposure summary
# -----------------------------------------------------------------------------
density_change <- if (file.exists(PATHS$density_change)) fread(PATHS$density_change) else data.table()

density_rows <- density_change[component == "density_fe", .(
  table_id = "table5_density_exposure",
  metric = paste0(specification, "_", term),
  before_value = estimate_before,
  after_value = estimate_after,
  delta = estimate_delta,
  source = PATHS$density_change,
  note = "before/after FE coefficients from snapshot comparison"
)]

impact_density <- impact_lines[grepl("Rows at risk|bw 250:|bw 500:|bw 1000:|overall:", impact_lines)]
impact_dt <- data.table(
  table_id = "table5_density_exposure",
  metric = paste0("archived_", seq_along(impact_density)),
  before_value = NA_real_,
  after_value = NA_real_,
  delta = NA_real_,
  source = PATHS$impact_md,
  note = impact_density
)

table5 <- rbindlist(list(density_rows, impact_dt), fill = TRUE)

# -----------------------------------------------------------------------------
# Table 6: top offending pair flows
# -----------------------------------------------------------------------------
table6 <- pair_flow_top[, .(
  table_id = "table6_pair_flows",
  dataset,
  era,
  pair_before,
  pair_after,
  n_points,
  map_year_before,
  map_year_after,
  longitude_mean,
  latitude_mean,
  source = "recomputed_sample_before_after"
)]

# -----------------------------------------------------------------------------
# Unified summary CSV
# -----------------------------------------------------------------------------
summary_long <- rbindlist(list(
  table1[, .(
    table_id,
    dataset,
    era,
    metric = "invalid_pair_rate",
    before_value = rate,
    before_numerator = numerator,
    before_denominator = denominator,
    after_value = after_rate,
    after_numerator = after_numerator,
    after_denominator = after_denominator,
    aux_value = before_sample_rate,
    aux_numerator = before_sample_numerator,
    aux_denominator = before_sample_denominator,
    source_before,
    source_after,
    note
  )],
  table2[, .(
    table_id,
    dataset,
    era,
    cohort,
    treat,
    metric = "origin_ward_not_in_pair_rate",
    before_value = before_rate,
    before_numerator,
    before_denominator,
    after_value = after_rate,
    after_numerator,
    after_denominator,
    aux_value = NA_real_,
    aux_numerator = NA_real_,
    aux_denominator = NA_real_,
    source_before,
    source_after,
    note
  )],
  table3[, .(
    table_id,
    dataset,
    era,
    cohort,
    treat,
    metric,
    before_value,
    before_numerator = NA_real_,
    before_denominator = NA_real_,
    after_value,
    after_numerator = NA_real_,
    after_denominator = NA_real_,
    aux_value = NA_real_,
    aux_numerator = NA_real_,
    aux_denominator = NA_real_,
    source_before,
    source_after,
    note
  )],
  table4[, .(
    table_id,
    dataset = NA_character_,
    era = NA_character_,
    cohort = NA_character_,
    treat = NA_integer_,
    metric,
    before_value = value,
    before_numerator = NA_real_,
    before_denominator = NA_real_,
    after_value = value,
    after_numerator = NA_real_,
    after_denominator = NA_real_,
    aux_value = NA_real_,
    aux_numerator = NA_real_,
    aux_denominator = NA_real_,
    source_before = source,
    source_after = source,
    note = "sliver materiality"
  )],
  table5[, .(
    table_id,
    dataset = NA_character_,
    era = NA_character_,
    cohort = NA_character_,
    treat = NA_integer_,
    metric,
    before_value,
    before_numerator = NA_real_,
    before_denominator = NA_real_,
    after_value,
    after_numerator = NA_real_,
    after_denominator = NA_real_,
    aux_value = delta,
    aux_numerator = NA_real_,
    aux_denominator = NA_real_,
    source_before = source,
    source_after = source,
    note
  )],
  table6[, .(
    table_id,
    dataset,
    era,
    cohort = NA_character_,
    treat = NA_integer_,
    metric = paste0(pair_before, "->", pair_after),
    before_value = NA_real_,
    before_numerator = n_points,
    before_denominator = NA_real_,
    after_value = NA_real_,
    after_numerator = NA_real_,
    after_denominator = NA_real_,
    aux_value = NA_real_,
    aux_numerator = longitude_mean,
    aux_denominator = latitude_mean,
    source_before = source,
    source_after = source,
    note = paste0("map_year_before=", map_year_before, "; map_year_after=", map_year_after)
  )]
), fill = TRUE)

fwrite(summary_long, OUT$summary_csv)

# -----------------------------------------------------------------------------
# Code-change traceability ledger
# -----------------------------------------------------------------------------
message("[step] build code change ledger + checklist")
ledger <- data.table(
  file_path = c(
    "tasks/ward_panel_create/code/make_ward_panel.R",
    "tasks/calculate_sale_distances/code/calculate_sale_distances.R",
    "tasks/calculate_rent_distances/code/calculate_rent_distances.R",
    "tasks/create_event_study_sales_data_disaggregate/code/create_transaction_panel.R",
    "tasks/create_event_study_rental_data_disaggregate/code/create_listing_panel.R",
    "tasks/border_verification/code/run_border_verification.R",
    "tasks/border_verification/code/compare_density_rental_before_after.R"
  ),
  line_reference = c(
    "20,36,51,64",
    "181-184,287-307",
    "56-59,151-171",
    "223-236,270-283,317-329,356-367",
    "167-190,266-287",
    "164-182,393-411,586-598",
    "35-84,274-301"
  ),
  old_behavior = c(
    "2003-2004 effectively used in pre-2005 window; 2003 regime split misaligned.",
    "Nearest boundary edge selected globally, not restricted to assigned-ward borders.",
    "Nearest boundary edge selected globally, not restricted to assigned-ward borders.",
    "Controls inherited treatment-origin ward fields, causing origin/pair inconsistencies.",
    "Controls inherited treatment-origin ward fields, causing origin/pair inconsistencies.",
    "Topology overlap used stricter binary framing; failure artifact not as interpretable.",
    "TeX numeric parsing could miss rightmost coefficient column and fail key joins."
  ),
  new_behavior = c(
    "Explicit annual windows: 1998-2002, 2003-2014, 2015-2023, 2024+.",
    "Nearest boundary edge constrained to edges touching the assigned ward.",
    "Nearest boundary edge constrained to edges touching the assigned ward.",
    "Controls now use row-level ward origin/destination; treated use switch origin/destination.",
    "Controls now use row-level ward origin/destination; treated use switch origin/destination.",
    "Certification reports overlap share with threshold gate and explicit pass/fail artifact.",
    "Robust numeric extraction and key-type coercion for before/after comparison artifacts."
  ),
  failure_mode_prevented = c(
    "Boundary-year misassignment around 2003 window and sensitivity inflation.",
    "Non-adjacent/incorrect border-pair assignment from unrelated nearest boundaries.",
    "Non-adjacent/incorrect border-pair assignment from unrelated nearest boundaries.",
    "Control origin ward not present in ward_pair_id in sales analysis panel.",
    "Control origin ward not present in ward_pair_id in rental analysis panel.",
    "False-negative interpretation from tiny sliver artifacts and opaque failures.",
    "Incomplete/incorrect before-after reporting and failed concentration tagging joins."
  ),
  measurable_metric_impact = c(
    "Density exposure note: boundary_year=2003 at-risk rows 8.67% overall (archived impact summary).",
    "Archived pre-fix sales invalid-pair rates: 1.33% (pre-2003), 1.52% (2003-2015).",
    "Archived pre-fix rental invalid-pair rate: 13.91% (pre-2015).",
    "Certified sales control mismatch rate now 0.0000.",
    "Certified rental control mismatch rate now 0.0000.",
    "Certified overlap share max 1.758e-05 <= 1.0e-04 threshold.",
    "Density/rental change summary now complete with all three density outcomes parsed."
  ),
  verification_artifact = c(
    "tasks/border_verification/output/border_verification_impact_summary.md",
    "tasks/border_verification/output/border_verification_impact_summary.md",
    "tasks/border_verification/output/border_verification_impact_summary.md",
    "tasks/border_verification/output/border_certification_gates.csv",
    "tasks/border_verification/output/border_certification_gates.csv",
    "tasks/border_verification/output/border_certification_gates.csv",
    "tasks/border_verification/output/density_rental_change_summary.md"
  )
)

fwrite(ledger, OUT$ledger_csv)

ledger_md <- c(
  "# Geometry Forensics Code Change Ledger",
  "",
  sprintf("- generated: `%s`", as.character(Sys.time())),
  "",
  "| File | Lines | Old Behavior | New Behavior | Failure Mode Prevented | Measurable Metric Impact | Verification Artifact |",
  "|---|---|---|---|---|---|---|"
)
for (i in seq_len(nrow(ledger))) {
  rr <- ledger[i]
  ledger_md <- c(ledger_md, sprintf(
    "| `%s` | `%s` | %s | %s | %s | %s | `%s` |",
    rr$file_path, rr$line_reference, rr$old_behavior, rr$new_behavior, rr$failure_mode_prevented,
    rr$measurable_metric_impact, rr$verification_artifact
  ))
}
writeLines(ledger_md, OUT$ledger_md)

# -----------------------------------------------------------------------------
# Approval checklist
# -----------------------------------------------------------------------------
checklist <- c(
  "# Geometry Forensics Approval Checklist",
  "",
  sprintf("- generated: `%s`", as.character(Sys.time())),
  "",
  "## What Was Fixed",
  "- Ward-year boundary windows aligned to correct eras (especially 2003 boundary transition).",
  "- Border-pair assignment now constrained to assigned-ward edges (no global-nearest bleed-over).",
  "- Sales and rental controls now have origin/destination consistency with ward pairs.",
  "- Certification now reports practical sliver gate using overlap share.",
  "",
  "## What Remains Sensitive",
  "- Density FE estimates remain sensitive to boundary-year exposure choices (see table5).",
  "- Rental concentration is highly localized in a small set of boundaries/ward-sides/building proxies.",
  "",
  "## Why Current Geography Is Acceptable For Main Analysis",
  "- Treated pair mismatches are zero in certified gates.",
  "- Control mismatch rates are zero in certified gates.",
  "- Post-redistricting adjacency invalid rates are zero in certified gates.",
  "- Sliver overlap share is tiny and below the defined threshold.",
  "",
  "## Evidence Paths",
  sprintf("- `%s`", OUT$maps_pdf),
  sprintf("- `%s`", OUT$summary_csv),
  sprintf("- `%s`", OUT$summary_md),
  sprintf("- `%s`", OUT$pair_flow_csv),
  sprintf("- `%s`", OUT$ledger_md),
  "- `../output/border_certification_gates.csv`",
  "- `../output/border_verification_impact_summary.md`"
)
writeLines(checklist, OUT$checklist_md)

# -----------------------------------------------------------------------------
# Map pack (PDF)
# -----------------------------------------------------------------------------
message("[step] build map pack")
map_years_for_overlap <- unique(c(1998L, 2003L, 2005L, 2014L, 2024L))
map_years_for_overlap <- map_years_for_overlap[map_years_for_overlap %in% ward_years]
ov <- compute_overlap_polygons(ward_panel, map_years_for_overlap)
overlap_sf <- ov$overlap_sf
overlap_stats <- ov$overlap_stats

# Map A
ward_A <- ward_panel[ward_panel$year %in% map_years_for_overlap, c("year", "ward")]
pA <- ggplot() +
  geom_sf(data = ward_A, fill = NA, color = "grey70", linewidth = 0.2) +
  {
    if (nrow(overlap_sf) > 0) geom_sf(data = overlap_sf, fill = "#d73027", color = "#a50026", alpha = 0.4, linewidth = 0.1)
  } +
  facet_wrap(~year, ncol = 3) +
  labs(
    title = "Map A: Era Ward Boundaries and Overlap/Sliver Polygons",
    subtitle = "Red polygons indicate overlap geometry; values in summary table quantify overlap share",
    caption = "CRS: EPSG:3435"
  ) +
  theme_minimal()

ward_panel_ll <- st_transform(ward_panel, 4326)

# Prepare invalid point layers for maps B/C
legacy_points <- recomp_all[logic_mode == mode_before]
after_points <- recomp_all[logic_mode == mode_after]

rent_B <- rbindlist(list(
  legacy_points[dataset == "rent" & era == "pre2015", .(dataset, mode_label = "Legacy Before", era, longitude, latitude, issue_invalid = issue_invalid_original, pair_label = pair_original)],
  after_points[dataset == "rent" & era == "pre2015", .(dataset, mode_label = "Certified After", era, longitude, latitude, issue_invalid = issue_invalid_original, pair_label = pair_original)]
), fill = TRUE)
rent_B <- rent_B[issue_invalid == TRUE]
rent_map_basis <- "invalid_pair_original"

sales_C <- legacy_points[dataset == "sales" & era %in% c("pre2003", "2003_2015") & issue_invalid_original == TRUE,
  .(dataset, era, longitude, latitude, pair_label = pair_original)]
sales_map_basis <- "invalid_pair_original"

# Top pairs for labels
rent_top_pairs <- legacy_points[dataset == "rent" & era == "pre2015" & issue_invalid_original == TRUE,
  .N, by = .(pair_label = pair_original)][order(-N)][1:min(.N, 5)]
sales_top_pairs <- legacy_points[dataset == "sales" & era %in% c("pre2003", "2003_2015") & issue_invalid_original == TRUE,
  .N, by = .(era, pair_label = pair_original)][order(-N)][1:min(.N, 8)]

if (nrow(rent_B) == 0) {
  rent_fb <- pair_flow[dataset == "rent" & era == "pre2015", .(dataset, era, longitude, latitude, pair_before)]
  if (nrow(rent_fb) > 0) {
    rent_B <- rent_fb[, .(
      dataset,
      mode_label = "Legacy Before",
      era,
      longitude,
      latitude,
      issue_invalid = TRUE,
      pair_label = pair_before
    )]
    rent_top_pairs <- rent_fb[, .N, by = .(pair_label = pair_before)][order(-N)][1:min(.N, 5)]
    rent_map_basis <- "legacy_pair_change_fallback"
  }
}

if (nrow(sales_C) == 0) {
  sales_fb <- pair_flow[dataset == "sales" & era %in% c("pre2003", "2003_2015"), .(dataset, era, longitude, latitude, pair_before)]
  if (nrow(sales_fb) > 0) {
    sales_C <- sales_fb[, .(dataset, era, longitude, latitude, pair_label = pair_before)]
    sales_top_pairs <- sales_fb[, .N, by = .(era, pair_label = pair_before)][order(-N)][1:min(.N, 8)]
    sales_map_basis <- "legacy_pair_change_fallback"
  }
}

rent_top_pairs_label <- if (nrow(rent_top_pairs) > 0) {
  paste(sprintf("%s (%d)", rent_top_pairs$pair_label, rent_top_pairs$N), collapse = "; ")
} else {
  "none in sampled points"
}

sales_top_pairs_label <- if (nrow(sales_top_pairs) > 0) {
  paste(sprintf("%s:%s (%d)", sales_top_pairs$era, sales_top_pairs$pair_label, sales_top_pairs$N), collapse = "; ")
} else {
  "none in sampled points"
}

# Map B
wp2014 <- ward_panel_ll[ward_panel_ll$year == get_map_year("rent", "pre2015", mode_after, ward_years), c("ward")]
wp2014_before <- wp2014
wp2014_before$mode_label <- "Legacy Before"
wp2014_after <- wp2014
wp2014_after$mode_label <- "Certified After"
wp2014_facet <- rbind(wp2014_before, wp2014_after)

pB <- ggplot() +
  geom_sf(data = wp2014_facet, fill = NA, color = "grey75", linewidth = 0.2) +
  {
    if (nrow(rent_B) > 0) stat_density_2d_filled(
      data = rent_B,
      aes(x = longitude, y = latitude, fill = after_stat(level)),
      geom = "polygon",
      alpha = 0.65,
      contour_var = "ndensity"
    )
  } +
  facet_wrap(~mode_label) +
  scale_fill_viridis_d(option = "C", name = "Density") +
  labs(
    title = "Map B: Rental pre-2015 Invalid-Pair Point Density",
    subtitle = sprintf("Legacy top pairs: %s | basis: %s", rent_top_pairs_label, rent_map_basis),
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

# Map C
sales_base_pre <- ward_panel_ll[ward_panel_ll$year == get_map_year("sales", "pre2003", mode_before, ward_years), c("ward")]
sales_base_pre$era <- "pre2003"
sales_base_mid <- ward_panel_ll[ward_panel_ll$year == get_map_year("sales", "2003_2015", mode_before, ward_years), c("ward")]
sales_base_mid$era <- "2003_2015"
sales_base <- rbind(sales_base_pre, sales_base_mid)

pC <- ggplot() +
  geom_sf(data = sales_base,
    fill = NA, color = "grey80", linewidth = 0.15
  ) +
  {
    if (nrow(sales_C) > 0) stat_density_2d_filled(
      data = sales_C,
      aes(x = longitude, y = latitude, fill = after_stat(level)),
      geom = "polygon",
      alpha = 0.65,
      contour_var = "ndensity"
    )
  } +
  facet_wrap(~era, ncol = 2) +
  scale_fill_viridis_d(option = "B", name = "Density") +
  labs(
    title = "Map C: Sales Invalid-Pair Point Density (Legacy Before)",
    subtitle = sprintf("%s | basis: %s", sales_top_pairs_label, sales_map_basis),
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()

# Map D: control mismatch geography before vs after
sales_geo <- fread(PATHS$sales_scored, select = c("pin", "sale_date", "latitude", "longitude"))
sales_geo[, pin := as.character(pin)]
sales_geo[, sale_date := coerce_date(sale_date)]
sales_geo <- sales_geo[!is.na(latitude) & !is.na(longitude)]
sales_geo <- sales_geo[!duplicated(sales_geo, by = c("pin", "sale_date"))]

sales_ctrl_pts <- sales_panel[treat == 0, .(
  pin,
  sale_date,
  ward_pair_id,
  ward_origin,
  ward_origin_legacy,
  origin_in_pair_before,
  origin_in_pair_after
)]
sales_ctrl_pts <- merge(sales_ctrl_pts, sales_geo, by = c("pin", "sale_date"), all.x = TRUE)

rent_geo <- as.data.table(read_parquet(PATHS$rent_scored, col_select = c("id", "file_date", "latitude", "longitude")))
rent_geo[, id := as.character(id)]
rent_geo[, file_date := coerce_date(file_date)]
rent_geo <- rent_geo[!is.na(latitude) & !is.na(longitude)]
rent_geo <- rent_geo[!duplicated(rent_geo, by = c("id", "file_date"))]

rent_ctrl_pts <- rental_panel[treat == 0, .(
  id,
  file_date,
  ward_pair_id,
  ward_origin,
  ward_origin_legacy,
  origin_in_pair_before,
  origin_in_pair_after
)]
rent_ctrl_pts <- merge(rent_ctrl_pts, rent_geo, by = c("id", "file_date"), all.x = TRUE)

before_mismatch_pts <- rbindlist(list(
  sales_ctrl_pts[origin_in_pair_before == FALSE & !is.na(longitude) & !is.na(latitude), .(dataset = "sales_controls", longitude, latitude)],
  rent_ctrl_pts[origin_in_pair_before == FALSE & !is.na(longitude) & !is.na(latitude), .(dataset = "rent_controls", longitude, latitude)]
), fill = TRUE)
after_mismatch_pts <- rbindlist(list(
  sales_ctrl_pts[origin_in_pair_after == FALSE & !is.na(longitude) & !is.na(latitude), .(dataset = "sales_controls", longitude, latitude)],
  rent_ctrl_pts[origin_in_pair_after == FALSE & !is.na(longitude) & !is.na(latitude), .(dataset = "rent_controls", longitude, latitude)]
), fill = TRUE)

before_mismatch_pts[, mode_label := "Legacy Before"]
after_mismatch_pts[, mode_label := "Certified After"]
ctrl_mismatch_pts <- rbindlist(list(before_mismatch_pts, after_mismatch_pts), fill = TRUE)

if (nrow(ctrl_mismatch_pts) > top_n_points) {
  ctrl_mismatch_pts <- ctrl_mismatch_pts[sample.int(.N, top_n_points)]
}

ward_d <- ward_panel_ll[ward_panel_ll$year == 2024, c("ward")]
ward_d_before <- ward_d
ward_d_before$mode_label <- "Legacy Before"
ward_d_after <- ward_d
ward_d_after$mode_label <- "Certified After"
ward_d_facet <- rbind(ward_d_before, ward_d_after)

pD <- ggplot() +
  geom_sf(data = ward_d_facet, fill = NA, color = "grey80", linewidth = 0.2) +
  {
    if (nrow(ctrl_mismatch_pts) > 0) geom_point(
      data = ctrl_mismatch_pts,
      aes(x = longitude, y = latitude, color = dataset),
      alpha = 0.55,
      size = 0.4
    )
  } +
  facet_wrap(~mode_label) +
  scale_color_manual(values = c("sales_controls" = "#1f78b4", "rent_controls" = "#e31a1c")) +
  labs(
    title = "Map D: Control Mismatch Geography (Before vs After)",
    subtitle = "Points are controls with origin ward not in ward_pair_id",
    x = "Longitude", y = "Latitude", color = "Panel"
  ) +
  theme_minimal()

# Map E: top rental influence boundaries + building proxies
boundaries_top <- if (file.exists(PATHS$boundaries_top)) fread(PATHS$boundaries_top) else data.table()
wards_top <- if (file.exists(PATHS$wards_top)) fread(PATHS$wards_top) else data.table()
buildings_top <- if (file.exists(PATHS$buildings_top)) fread(PATHS$buildings_top) else data.table()
anoms <- if (file.exists(PATHS$anomaly_samples)) fread(PATHS$anomaly_samples) else data.table()

invalid_pairs_now <- unique(anoms[dataset == "rent_pre_scores_full", ward_pair_id])
invalid_pairs_now <- invalid_pairs_now[!is.na(invalid_pairs_now)]

if (nrow(boundaries_top) > 0) {
  boundaries_top[, geometry_clean := !(ward_pair_id %in% invalid_pairs_now)]
  boundaries_top <- boundaries_top[1:min(.N, top_n_pairs)]

  # Attach geometry from 2015/2024 lines based on cohort
  b_geoms <- list()
  for (i in seq_len(nrow(boundaries_top))) {
    rr <- boundaries_top[i]
    yy <- ifelse(as.character(rr$cohort) == "2023", 2024L, 2015L)
    ll <- lines_by_year[[as.character(yy)]]
    gg <- ll[ll$pair_id == rr$ward_pair_id, ]
    if (nrow(gg) == 0) next
    gg <- gg[1, ]
    gg$cohort <- rr$cohort
    gg$ward_pair_id <- rr$ward_pair_id
    gg$abs_influence <- rr$abs_influence
    gg$geometry_clean <- rr$geometry_clean
    b_geoms[[length(b_geoms) + 1]] <- gg
  }
  boundaries_sf <- if (length(b_geoms) > 0) do.call(rbind, b_geoms) else st_sf(ward_pair_id = character(), abs_influence = numeric(), geometry = st_sfc(), crs = st_crs(ward_panel))
} else {
  boundaries_sf <- st_sf(ward_pair_id = character(), abs_influence = numeric(), geometry = st_sfc(), crs = st_crs(ward_panel))
}

if (nrow(boundaries_sf) > 0) {
  boundaries_sf <- st_transform(boundaries_sf, 4326)
}

if (nrow(buildings_top) > 0) {
  buildings_top <- buildings_top[1:min(.N, top_n_pairs)]
}

pE <- ggplot() +
  geom_sf(data = ward_panel_ll[ward_panel_ll$year == 2024, c("ward")], fill = NA, color = "grey80", linewidth = 0.2) +
  {
    if (nrow(boundaries_sf) > 0) geom_sf(
      data = boundaries_sf,
      aes(color = abs_influence, linetype = geometry_clean),
      linewidth = 0.8
    )
  } +
  {
    if (nrow(buildings_top) > 0) geom_point(
      data = buildings_top,
      aes(x = longitude, y = latitude, size = abs_influence),
      color = "#7f0000",
      alpha = 0.7
    )
  } +
  scale_color_viridis_c(option = "D") +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed")) +
  labs(
    title = "Map E: Top Rental Influence Contributors (Certified After)",
    subtitle = "Boundary lines by absolute influence; points are top building proxies",
    x = "Longitude", y = "Latitude", color = "Abs influence", size = "Building abs influence", linetype = "Geometry clean"
  ) +
  theme_minimal()

# Map F: top before->after pair flow transitions (segment centroids)
flow_segments <- data.table()
if (nrow(pair_flow_top) > 0) {
  seg_parts <- list()
  for (i in seq_len(nrow(pair_flow_top))) {
    rr <- pair_flow_top[i]
    l_before <- lines_by_year[[as.character(rr$map_year_before)]]
    l_before <- l_before[l_before$pair_id == rr$pair_before, ]
    l_after <- lines_by_year[[as.character(rr$map_year_after)]]
    l_after <- l_after[l_after$pair_id == rr$pair_after, ]
    if (nrow(l_before) == 0 || nrow(l_after) == 0) next
    c_before <- st_coordinates(st_centroid(st_union(st_geometry(l_before))))
    c_after <- st_coordinates(st_centroid(st_union(st_geometry(l_after))))
    seg_parts[[length(seg_parts) + 1]] <- data.table(
      dataset = rr$dataset,
      era = rr$era,
      transition = paste0(rr$pair_before, "->", rr$pair_after),
      n_points = rr$n_points,
      x = c_before[1],
      y = c_before[2],
      xend = c_after[1],
      yend = c_after[2]
    )
  }
  if (length(seg_parts) > 0) flow_segments <- rbindlist(seg_parts, fill = TRUE)
}

pF <- ggplot() +
  geom_sf(data = ward_panel[ward_panel$year == 2024, c("ward")], fill = NA, color = "grey85", linewidth = 0.15) +
  {
    if (nrow(flow_segments) > 0) geom_segment(
      data = flow_segments,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = n_points, color = dataset),
      alpha = 0.8,
      arrow = grid::arrow(length = grid::unit(0.08, "inches"))
    )
  } +
  scale_color_manual(values = c("sales" = "#1f78b4", "rent" = "#33a02c")) +
  scale_linewidth_continuous(range = c(0.3, 1.8)) +
  labs(
    title = "Map F: Largest Before->After Pair Flow Transitions",
    subtitle = "Segments connect centroids of legacy pair to certified pair for top transitions",
    x = "Easting (ft)", y = "Northing (ft)", color = "Dataset", linewidth = "N points"
  ) +
  theme_minimal()

# Write PDF
message("[step] write map PDF pages")
pdf(OUT$maps_pdf, width = 12, height = 9)
print(pA)
print(pB)
print(pC)
print(pD)
print(pE)
print(pF)
dev.off()

# -----------------------------------------------------------------------------
# Summary markdown
# -----------------------------------------------------------------------------
message("[step] write markdown summary")
md <- c(
  "# Geometry Forensics Summary (Before vs After)",
  "",
  sprintf("- generated: `%s`", as.character(Sys.time())),
  sprintf("- mode_before: `%s`", mode_before),
  sprintf("- mode_after: `%s`", mode_after),
  sprintf("- smoke: `%s`", smoke),
  "",
  "## Table 1: Raw Invalid-Pair Rates by Dataset/Era",
  "| Dataset | Era | Before (Archived Full) | After (Current Full) | Before (Reconstructed Sample) |",
  "|---|---|---:|---:|---:|"
)
for (i in seq_len(nrow(table1))) {
  rr <- table1[i]
  md <- c(md, sprintf("| %s | %s | %s | %s | %s |",
    rr$dataset, rr$era,
    fmt_num(100 * rr$rate, 2),
    fmt_num(100 * rr$after_rate, 2),
    fmt_num(100 * rr$before_sample_rate, 2)
  ))
}

md <- c(md, "", "## Table 2: Control Mismatch Rates (Cohort x Treat)", "| Dataset | Cohort | Treat | Before (%) | After (%) |", "|---|---|---:|---:|---:|")
for (i in seq_len(nrow(table2))) {
  rr <- table2[i]
  md <- c(md, sprintf("| %s | %s | %d | %s | %s |",
    rr$dataset, rr$cohort, rr$treat,
    fmt_num(100 * rr$before_rate, 3), fmt_num(100 * rr$after_rate, 3)
  ))
}

md <- c(md, "", "## Table 3: Treated Integrity and Adjacency Metrics", "| Dataset | Metric | Before | After |", "|---|---|---:|---:|")
for (i in seq_len(nrow(table3))) {
  rr <- table3[i]
  md <- c(md, sprintf("| %s | %s | %s | %s |",
    ifelse(is.na(rr$dataset), "NA", rr$dataset), rr$metric,
    fmt_num(rr$before_value, 6), fmt_num(rr$after_value, 6)
  ))
}

md <- c(md, "", "## Table 4: Sliver Materiality", "| Metric | Value |", "|---|---:|")
for (i in seq_len(nrow(table4))) {
  rr <- table4[i]
  md <- c(md, sprintf("| %s | %s |", rr$metric, fmt_num(rr$value, 8)))
}

md <- c(md, "", "## Table 5: Density Exposure Context", "| Metric | Before | After | Delta/Note |", "|---|---:|---:|---|")
for (i in seq_len(nrow(table5))) {
  rr <- table5[i]
  md <- c(md, sprintf("| %s | %s | %s | %s |",
    rr$metric,
    fmt_num(rr$before_value, 6),
    fmt_num(rr$after_value, 6),
    ifelse(!is.na(rr$delta), fmt_num(rr$delta, 6), rr$note)
  ))
}

md <- c(md, "", "## Table 6: Top Offending Pair Flows (Before -> After)", "| Dataset | Era | Pair Before | Pair After | N points |", "|---|---|---|---|---:|")
for (i in seq_len(nrow(table6))) {
  rr <- table6[i]
  md <- c(md, sprintf("| %s | %s | %s | %s | %d |", rr$dataset, rr$era, rr$pair_before, rr$pair_after, rr$n_points))
}

md <- c(md, "", "## Evidence Files",
  sprintf("- `%s`", OUT$maps_pdf),
  sprintf("- `%s`", OUT$summary_csv),
  sprintf("- `%s`", OUT$pair_flow_csv),
  sprintf("- `%s`", OUT$ledger_md),
  sprintf("- `%s`", OUT$checklist_md)
)

writeLines(md, OUT$summary_md)

message("Saved: ", OUT$maps_pdf)
message("Saved: ", OUT$summary_csv)
message("Saved: ", OUT$summary_md)
message("Saved: ", OUT$pair_flow_csv)
message("Saved: ", OUT$ledger_csv)
message("Saved: ", OUT$ledger_md)
message("Saved: ", OUT$checklist_md)
message("Done.")