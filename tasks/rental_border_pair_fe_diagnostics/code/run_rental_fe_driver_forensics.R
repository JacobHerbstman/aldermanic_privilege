source("../../setup_environment/code/packages.R")

library(data.table)
library(arrow)
library(fixest)
library(sf)
library(ggplot2)
library(dplyr)

sf_use_s2(FALSE)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/rental_border_pair_fe_diagnostics/code")
# input <- "../input/rent_with_ward_distances.parquet"
# ward_panel <- "../input/ward_panel.gpkg"
# bw_ft <- 500
# window <- "pre_2023"
# sample_filter <- "all"
# top_n_fast <- 200
# top_n_exact_boundary <- 30
# top_n_exact_ward_side <- 30
# top_n_exact_building <- 50
# building_round <- 5
# lop_bins <- "0.05,0.10,0.20"
# near_boundary_ft <- "250,500"
# reference_csv <- "../input/reference_fe_table_rental_bw500_pre_2023.csv"
# cert_gates <- "../input/border_certification_gates.csv"
# anomaly_samples <- "../input/border_pair_anomaly_samples.csv"
# output_dir <- "../output"
# smoke <- FALSE
# Rscript run_rental_fe_driver_forensics.R "../input/rent_with_ward_distances.parquet" "../input/ward_panel.gpkg" 500 "pre_2023" "all" 200 30 30 50 5 "0.05,0.10,0.20" "250,500" "../input/reference_fe_table_rental_bw500_pre_2023.csv" "../input/border_certification_gates.csv" "../input/border_pair_anomaly_samples.csv" "../output" FALSE
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 17) {
  input <- cli_args[1]
  ward_panel <- cli_args[2]
  bw_ft <- suppressWarnings(as.integer(cli_args[3]))
  window <- cli_args[4]
  sample_filter <- cli_args[5]
  top_n_fast <- suppressWarnings(as.integer(cli_args[6]))
  top_n_exact_boundary <- suppressWarnings(as.integer(cli_args[7]))
  top_n_exact_ward_side <- suppressWarnings(as.integer(cli_args[8]))
  top_n_exact_building <- suppressWarnings(as.integer(cli_args[9]))
  building_round <- suppressWarnings(as.integer(cli_args[10]))
  lop_bins <- cli_args[11]
  near_boundary_ft <- cli_args[12]
  reference_csv <- cli_args[13]
  cert_gates <- cli_args[14]
  anomaly_samples <- cli_args[15]
  output_dir <- cli_args[16]
  smoke <- tolower(cli_args[17]) %in% c("true", "t", "1", "yes")
} else {
  if (!exists("input") || !exists("ward_panel") || !exists("bw_ft") || !exists("window") || !exists("sample_filter") || !exists("top_n_fast") || !exists("top_n_exact_boundary") || !exists("top_n_exact_ward_side") || !exists("top_n_exact_building") || !exists("building_round") || !exists("lop_bins") || !exists("near_boundary_ft") || !exists("reference_csv") || !exists("cert_gates") || !exists("anomaly_samples") || !exists("output_dir") || !exists("smoke")) {
    stop("FATAL: Script requires 17 args: <input> <ward_panel> <bw_ft> <window> <sample_filter> <top_n_fast> <top_n_exact_boundary> <top_n_exact_ward_side> <top_n_exact_building> <building_round> <lop_bins> <near_boundary_ft> <reference_csv> <cert_gates> <anomaly_samples> <output_dir> <smoke>", call. = FALSE)
  }
}

if (!window %in% c("full", "pre_covid", "pre_2021", "pre_2023", "drop_mid")) {
  stop("--window must be one of: full, pre_covid, pre_2021, pre_2023, drop_mid", call. = FALSE)
}
if (!sample_filter %in% c("all", "multifamily_only")) {
  stop("--sample_filter must be one of: all, multifamily_only", call. = FALSE)
}
if (!is.finite(bw_ft) || bw_ft <= 0) {
  stop("--bw_ft must be a positive integer", call. = FALSE)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

parse_num_vec <- function(x) {
  vals <- as.numeric(trimws(unlist(strsplit(x, ",", fixed = TRUE))))
  vals <- vals[is.finite(vals)]
  vals
}

format_pct_label <- function(v) {
  sprintf("%.0f%%", 100 * v)
}

apply_window_filter <- function(dt, window_name) {
  if (window_name == "full") return(dt)
  if (window_name == "pre_covid") return(dt[year <= 2019])
  if (window_name == "pre_2021") return(dt[year <= 2020])
  if (window_name == "pre_2023") return(dt[year <= 2022])
  if (window_name == "drop_mid") return(dt[year <= 2020 | year >= 2024])
  dt
}

message("=== Rental Border-Pair FE Driver Forensics ===")
message("input: ", input)
message("ward_panel: ", ward_panel)
message("bw_ft: ", bw_ft)
message("window: ", window)
message("sample_filter: ", sample_filter)
message("smoke: ", smoke)

lop_bins <- sort(unique(parse_num_vec(lop_bins)))
near_ft <- sort(unique(as.integer(parse_num_vec(near_boundary_ft))))

if (length(lop_bins) < 3) {
  stop("--lop_bins must include at least three comma-separated thresholds", call. = FALSE)
}
if (length(near_ft) < 2) {
  stop("--near_boundary_ft must include at least two comma-separated distances", call. = FALSE)
}

bin_breaks <- c(-Inf, lop_bins, Inf)
bin_labels <- c(
  paste0("<", format_pct_label(lop_bins[1])),
  vapply(seq_len(length(lop_bins) - 1), function(i) {
    paste0(format_pct_label(lop_bins[i]), "-", format_pct_label(lop_bins[i + 1]))
  }, character(1)),
  paste0(">=", format_pct_label(lop_bins[length(lop_bins)]))
)

required_cols <- c(
  "id", "file_date", "rent_price", "ward_pair_id", "ward", "signed_dist", "strictness_own"
)
optional_cols <- c(
  "strictness_neighbor", "sqft", "beds", "baths", "building_type",
  "building_type_clean", "building_type_factor", "log_sqft", "log_beds",
  "log_baths", "latitude", "longitude"
)

message("[step] loading rental border data")
parquet_cols <- names(read_parquet(input, as_data_frame = FALSE))
missing_required <- setdiff(required_cols, parquet_cols)
if (length(missing_required) > 0) {
  stop(
    sprintf("Input parquet is missing required columns: %s", paste(missing_required, collapse = ", ")),
    call. = FALSE
  )
}
col_select <- c(required_cols, intersect(optional_cols, parquet_cols))
dt_raw <- as.data.table(read_parquet(input, col_select = tidyselect::all_of(col_select)))

if (!"sqft" %in% names(dt_raw)) dt_raw[, sqft := NA_real_]
if (!"beds" %in% names(dt_raw)) dt_raw[, beds := NA_real_]
if (!"baths" %in% names(dt_raw)) dt_raw[, baths := NA_real_]
if (!"building_type" %in% names(dt_raw)) dt_raw[, building_type := NA_character_]
if (!"building_type_clean" %in% names(dt_raw)) dt_raw[, building_type_clean := NA_character_]
if (!"building_type_factor" %in% names(dt_raw)) dt_raw[, building_type_factor := NA_character_]
if (!"log_sqft" %in% names(dt_raw)) dt_raw[, log_sqft := NA_real_]
if (!"log_beds" %in% names(dt_raw)) dt_raw[, log_beds := NA_real_]
if (!"log_baths" %in% names(dt_raw)) dt_raw[, log_baths := NA_real_]
if (!"latitude" %in% names(dt_raw)) dt_raw[, latitude := NA_real_]
if (!"longitude" %in% names(dt_raw)) dt_raw[, longitude := NA_real_]


dt_raw[, `:=`(
  file_date = as.Date(file_date),
  year = as.integer(format(as.Date(file_date), "%Y")),
  year_month = format(as.Date(file_date), "%Y-%m"),
  ward_pair_id = as.character(ward_pair_id),
  ward = as.integer(ward),
  abs_dist = abs(as.numeric(signed_dist)),
  strictness_own = as.numeric(strictness_own),
  sqft = as.numeric(sqft),
  beds = as.numeric(beds),
  baths = as.numeric(baths),
  building_type = as.character(building_type),
  building_type_clean = as.character(building_type_clean),
  building_type_factor = as.character(building_type_factor),
  log_sqft = as.numeric(log_sqft),
  log_beds = as.numeric(log_beds),
  log_baths = as.numeric(log_baths)
)]

# Mirror rental_border_pair_fe preprocessing exactly where possible.
dt_raw[, `:=`(
  log_sqft = fifelse(!is.na(sqft) & sqft > 0, log(sqft), log_sqft),
  log_beds = fifelse(!is.na(beds) & beds > 0, log(beds), log_beds),
  log_baths = fifelse(!is.na(baths) & baths > 0, log(baths), log_baths),
  building_type_factor = fifelse(
    !is.na(building_type_clean) & building_type_clean != "",
    building_type_clean,
    fifelse(
      !is.na(building_type_factor) & building_type_factor != "",
      building_type_factor,
      fifelse(!is.na(building_type) & building_type != "", building_type, "other")
    )
  )
)]

# Apply the same core filters as rental_border_pair_fe
base_dt <- dt_raw[
  !is.na(file_date) &
    !is.na(rent_price) & rent_price > 0 &
    !is.na(ward_pair_id) &
    !is.na(ward) &
    !is.na(signed_dist) &
    abs_dist <= bw_ft &
    !is.na(strictness_own)
]
base_dt <- apply_window_filter(base_dt, window)

if (sample_filter == "multifamily_only") {
  base_dt <- base_dt[building_type_clean == "multi_family"]
}

if (nrow(base_dt) == 0) {
  stop("No observations remain after applying base filters.", call. = FALSE)
}

if (smoke) {
  set.seed(20260216)
  smoke_n <- min(300000L, nrow(base_dt))
  base_dt <- base_dt[, .SD[sample.int(.N, min(.N, max(1L, smoke_n %/% max(1L, uniqueN(base_dt$year)))) )], by = year]
  if (nrow(base_dt) > smoke_n) {
    base_dt <- base_dt[sample.int(.N, smoke_n)]
  }
  message("[diag] smoke sample rows: ", nrow(base_dt))
}

message("[diag] base rows after filters: ", format(nrow(base_dt), big.mark = ","))
message("[diag] unique ward pairs: ", format(uniqueN(base_dt$ward_pair_id), big.mark = ","))

strictness_sd_base <- sd(base_dt$strictness_own, na.rm = TRUE)
if (!is.finite(strictness_sd_base) || strictness_sd_base <= 0) {
  stop("strictness_own SD invalid in base filtered sample", call. = FALSE)
}

prepare_spec_dt <- function(dt, spec_name, building_round, strictness_sd_reference) {
  x <- copy(dt)

  x[, building_type_factor := as.factor(fifelse(is.na(building_type_factor) | building_type_factor == "", "other", building_type_factor))]
  x[, ward_pair := ward_pair_id]
  x[, ward_side := paste0(ward_pair_id, "_", ward)]
  x[, building_proxy := fifelse(
    !is.na(latitude) & !is.na(longitude),
    sprintf(paste0("%.", building_round, "f_%.", building_round, "f"), round(latitude, building_round), round(longitude, building_round)),
    NA_character_
  )]

  if (spec_name == "with_hedonics") {
    x <- x[
      !is.na(log_sqft) &
        !is.na(log_beds) &
        !is.na(log_baths) &
        !is.na(building_type_factor)
    ]
  }

  if (nrow(x) == 0) {
    stop(sprintf("No observations left for spec '%s'", spec_name), call. = FALSE)
  }

  include_building_factor <- if (spec_name == "with_hedonics") {
    nlevels(droplevels(x$building_type_factor)) >= 2
  } else {
    FALSE
  }

  strict_sd <- strictness_sd_reference

  x[, strictness_std := strictness_own / strict_sd]
  list(dt = x, strict_sd = strict_sd, include_building_factor = include_building_factor)
}

build_formulas <- function(spec_name, include_building_factor = TRUE) {
  if (spec_name == "no_hedonics") {
    return(list(
      main = as.formula("log(rent_price) ~ strictness_std | ward_pair^year_month"),
      y = as.formula("log(rent_price) ~ 1 | ward_pair^year_month"),
      x = as.formula("strictness_std ~ 1 | ward_pair^year_month")
    ))
  }
  if (isTRUE(include_building_factor)) {
    return(list(
      main = as.formula("log(rent_price) ~ strictness_std + log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month"),
      y = as.formula("log(rent_price) ~ log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month"),
      x = as.formula("strictness_std ~ log_sqft + log_beds + log_baths + building_type_factor | ward_pair^year_month")
    ))
  }
  list(
    main = as.formula("log(rent_price) ~ strictness_std + log_sqft + log_beds + log_baths | ward_pair^year_month"),
    y = as.formula("log(rent_price) ~ log_sqft + log_beds + log_baths | ward_pair^year_month"),
    x = as.formula("strictness_std ~ log_sqft + log_beds + log_baths | ward_pair^year_month")
  )
}

aggregate_influence <- function(dt, by_cols, Sxy, Sxx, beta_fwl) {
  out <- dt[, .(
    n_obs = .N,
    n_listings = uniqueN(id),
    sxy = sum(sxy),
    sxx = sum(sxx),
    mean_rent = mean(rent_price),
    mean_abs_dist = mean(abs_dist)
  ), by = by_cols]

  if (isTRUE(abs(Sxy) > 0)) {
    out[, share_sxy := sxy / Sxy]
  } else {
    out[, share_sxy := NA_real_]
  }
  if (isTRUE(abs(Sxx) > 0)) {
    out[, share_sxx := sxx / Sxx]
  } else {
    out[, share_sxx := NA_real_]
  }
  out[, beta_without_group_fast := fifelse((Sxx - sxx) > 0, (Sxy - sxy) / (Sxx - sxx), NA_real_)]
  out[, influence_fast := beta_fwl - beta_without_group_fast]
  out[, abs_influence_fast := abs(influence_fast)]
  setorder(out, -abs_influence_fast)
  out[, rank_abs_fast := .I]
  out
}

run_exact_loo <- function(dt, formulas, spec_name, group_col, group_fast, top_n) {
  if (nrow(group_fast) == 0 || top_n <= 0) {
    return(data.table())
  }

  sel <- copy(group_fast[1:min(.N, top_n)])
  out <- vector("list", nrow(sel))

  m_full <- feols(formulas$main, data = dt, cluster = ~ward_pair, warn = FALSE)
  beta_full <- as.numeric(coef(m_full)["strictness_std"])

  message(sprintf("[step] exact LOO spec=%s group=%s top_n=%d", spec_name, group_col, nrow(sel)))

  for (i in seq_len(nrow(sel))) {
    gid <- sel[[group_col]][i]
    dt_drop <- dt[get(group_col) != gid]

    if (nrow(dt_drop) < 1000 || uniqueN(dt_drop$ward_pair) < 2) {
      out[[i]] <- data.table(group_id = as.character(gid), beta_full = beta_full, beta_drop = NA_real_, influence_exact = NA_real_, abs_influence_exact = NA_real_, n_obs_drop = nrow(dt_drop))
      next
    }

    m_drop <- tryCatch(
      feols(formulas$main, data = dt_drop, cluster = ~ward_pair, warn = FALSE),
      error = function(e) NULL
    )

    if (is.null(m_drop) || !"strictness_std" %in% names(coef(m_drop))) {
      out[[i]] <- data.table(group_id = as.character(gid), beta_full = beta_full, beta_drop = NA_real_, influence_exact = NA_real_, abs_influence_exact = NA_real_, n_obs_drop = nrow(dt_drop))
    } else {
      beta_drop <- as.numeric(coef(m_drop)["strictness_std"])
      infl <- beta_full - beta_drop
      out[[i]] <- data.table(group_id = as.character(gid), beta_full = beta_full, beta_drop = beta_drop, influence_exact = infl, abs_influence_exact = abs(infl), n_obs_drop = nobs(m_drop))
    }

    if (i %% 5 == 0 || i == nrow(sel)) {
      message(sprintf("  exact LOO %s %s: %d/%d", spec_name, group_col, i, nrow(sel)))
    }
  }

  exact <- rbindlist(out, fill = TRUE)
  sel_copy <- copy(sel)
  sel_copy[, group_id := as.character(get(group_col))]
  exact <- merge(sel_copy, exact, by = "group_id", all.y = TRUE)
  exact[, fast_vs_exact_delta := influence_exact - influence_fast]
  setorder(exact, -abs_influence_exact)
  exact[, rank_abs_exact := .I]
  exact[, spec := spec_name]
  exact
}

build_pair_month_metrics <- function(dt, beta_spec, near_ft, bin_breaks, bin_labels) {
  near_a <- near_ft[1]
  near_b <- near_ft[2]

  side <- dt[, .(
    n_listings = .N,
    mean_rent = mean(rent_price),
    strictness_side = mean(strictness_std),
    near_a = sum(abs_dist <= near_a),
    near_b = sum(abs_dist <= near_b)
  ), by = .(ward_pair_id, year_month, ward)]

  high <- side[order(ward_pair_id, year_month, -strictness_side, -n_listings, ward), .SD[1], by = .(ward_pair_id, year_month)]
  low <- side[order(ward_pair_id, year_month, strictness_side, -n_listings, ward), .SD[1], by = .(ward_pair_id, year_month)]

  pm <- merge(high, low, by = c("ward_pair_id", "year_month"), suffixes = c("_high", "_low"), all = FALSE)
  pm <- pm[strictness_side_high > strictness_side_low]

  if (nrow(pm) == 0) {
    return(data.table())
  }

  pm[, strictness_gap_std := strictness_side_high - strictness_side_low]
  pm[, raw_gap_pct := exp(log(mean_rent_high) - log(mean_rent_low)) - 1]
  pm[, implied_gap_pct := exp(beta_spec * strictness_gap_std) - 1]
  pm[, `:=`(
    abs_raw_gap_pct = abs(raw_gap_pct),
    abs_implied_gap_pct = abs(implied_gap_pct),
    listings_total = n_listings_high + n_listings_low,
    near_a_total = near_a_high + near_a_low,
    near_b_total = near_b_high + near_b_low
  )]

  pm[, raw_gap_bin := cut(abs_raw_gap_pct, breaks = bin_breaks, labels = bin_labels, right = FALSE)]
  pm[, implied_gap_bin := cut(abs_implied_gap_pct, breaks = bin_breaks, labels = bin_labels, right = FALSE)]

  setcolorder(pm, c(
    "ward_pair_id", "year_month", "ward_high", "ward_low",
    "n_listings_high", "n_listings_low", "listings_total",
    "near_a_total", "near_b_total", "mean_rent_high", "mean_rent_low",
    "strictness_side_high", "strictness_side_low", "strictness_gap_std",
    "raw_gap_pct", "abs_raw_gap_pct", "implied_gap_pct", "abs_implied_gap_pct",
    "raw_gap_bin", "implied_gap_bin"
  ))

  setnames(pm, c("near_a_total", "near_b_total"), c(paste0("near_", near_a, "ft_total"), paste0("near_", near_b, "ft_total")))
  pm
}

boundary_bin_counts <- function(pm, bin_col, prefix) {
  if (nrow(pm) == 0) {
    return(data.table())
  }
  x <- pm[, .N, by = .(ward_pair_id, bin = as.character(get(bin_col)))]
  x <- dcast(x, ward_pair_id ~ bin, value.var = "N", fill = 0)
  old <- setdiff(names(x), "ward_pair_id")
  if (length(old) > 0) {
    setnames(x, old, paste0(prefix, "_", gsub("[^A-Za-z0-9]+", "_", old)))
  }
  x
}

build_boundary_summary <- function(pm, boundary_fast, boundary_exact, lop_bins, near_ft) {
  if (nrow(pm) == 0) {
    return(data.table())
  }

  near_a_name <- paste0("near_", near_ft[1], "ft_total")
  near_b_name <- paste0("near_", near_ft[2], "ft_total")

  base <- pm[, .(
    n_pair_month = .N,
    total_listings = sum(listings_total),
    near_a_total = sum(get(near_a_name)),
    near_b_total = sum(get(near_b_name)),
    raw_gap_abs_wmean = weighted.mean(abs_raw_gap_pct, pmax(listings_total, 1), na.rm = TRUE),
    implied_gap_abs_wmean = weighted.mean(abs_implied_gap_pct, pmax(listings_total, 1), na.rm = TRUE),
    max_abs_raw_gap_pct = max(abs_raw_gap_pct, na.rm = TRUE),
    max_abs_implied_gap_pct = max(abs_implied_gap_pct, na.rm = TRUE),
    n_raw_ge_5 = sum(abs_raw_gap_pct >= lop_bins[1]),
    n_raw_ge_10 = sum(abs_raw_gap_pct >= lop_bins[2]),
    n_raw_ge_20 = sum(abs_raw_gap_pct >= lop_bins[3]),
    n_implied_ge_5 = sum(abs_implied_gap_pct >= lop_bins[1]),
    n_implied_ge_10 = sum(abs_implied_gap_pct >= lop_bins[2]),
    n_implied_ge_20 = sum(abs_implied_gap_pct >= lop_bins[3])
  ), by = ward_pair_id]

  raw_bins <- boundary_bin_counts(pm, "raw_gap_bin", "raw_bin")
  implied_bins <- boundary_bin_counts(pm, "implied_gap_bin", "implied_bin")

  out <- merge(base, raw_bins, by = "ward_pair_id", all.x = TRUE)
  out <- merge(out, implied_bins, by = "ward_pair_id", all.x = TRUE)

  if (nrow(boundary_fast) > 0) {
    out <- merge(out,
      boundary_fast[, .(ward_pair_id, influence_fast, abs_influence_fast, rank_abs_fast)],
      by = "ward_pair_id", all.x = TRUE
    )
  }

  if (nrow(boundary_exact) > 0) {
    out <- merge(out,
      boundary_exact[, .(ward_pair_id, influence_exact, abs_influence_exact, rank_abs_exact)],
      by = "ward_pair_id", all.x = TRUE
    )
  }

  out[, severe_flag := (
    (max_abs_raw_gap_pct >= lop_bins[2] | max_abs_implied_gap_pct >= lop_bins[2]) &
      n_pair_month >= 6 &
      total_listings >= 100
  )]

  setnames(out, c("near_a_total", "near_b_total"), c(near_a_name, near_b_name))
  out[, sort_abs_influence := fifelse(!is.na(abs_influence_exact), abs_influence_exact, abs_influence_fast)]
  setorder(out, -sort_abs_influence, -max_abs_implied_gap_pct, -max_abs_raw_gap_pct)
  out[, sort_abs_influence := NULL]
  out
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

attach_pair_geometry <- function(df, pair_col, lines_by_year, year_priority, crs_out = 4326) {
  if (nrow(df) == 0) {
    return(st_sf(df, geometry = st_sfc(crs = crs_out)))
  }

  geoms <- vector("list", nrow(df))
  year_used <- rep(NA_integer_, nrow(df))

  for (i in seq_len(nrow(df))) {
    pid <- as.character(df[[pair_col]][i])
    geom_i <- NULL

    for (yy in year_priority) {
      lines <- lines_by_year[[as.character(yy)]]
      if (is.null(lines) || nrow(lines) == 0) next
      hit <- lines[lines$pair_id == pid, ]
      if (nrow(hit) > 0) {
        geom_i <- st_geometry(hit)[[1]]
        year_used[i] <- yy
        break
      }
    }

    if (is.null(geom_i)) geom_i <- st_geometrycollection()
    geoms[[i]] <- geom_i
  }

  out <- st_sf(df, map_year = year_used, geometry = st_sfc(geoms, crs = crs_out))
  out$geometry_found <- !st_is_empty(out$geometry)
  out
}

run_spec <- function(base_dt, spec_name) {
  message("[step] fitting spec: ", spec_name)
  spec_prep <- prepare_spec_dt(base_dt, spec_name, building_round, strictness_sd_base)
  dt <- spec_prep$dt
  formulas <- build_formulas(spec_name, include_building_factor = spec_prep$include_building_factor)

  model <- feols(formulas$main, data = dt, cluster = ~ward_pair, warn = FALSE)

  beta <- as.numeric(coef(model)["strictness_std"])
  se <- as.numeric(se(model)["strictness_std"])
  pval <- as.numeric(pvalue(model)["strictness_std"])

  # FWL decomposition
  m_y <- feols(formulas$y, data = dt, warn = FALSE)
  m_x <- feols(formulas$x, data = dt, warn = FALSE)

  dt_fwl <- copy(dt)
  dt_fwl[, `:=`(
    y_tilde = as.numeric(resid(m_y)),
    x_tilde = as.numeric(resid(m_x))
  )]
  dt_fwl <- dt_fwl[is.finite(y_tilde) & is.finite(x_tilde)]
  dt_fwl[, `:=`(
    sxy = x_tilde * y_tilde,
    sxx = x_tilde * x_tilde
  )]

  Sxy <- dt_fwl[, sum(sxy)]
  Sxx <- dt_fwl[, sum(sxx)]
  beta_fwl <- Sxy / Sxx
  fwl_abs_diff <- abs(beta - beta_fwl)

  # Fast influence tables
  boundary_full <- aggregate_influence(dt_fwl, "ward_pair_id", Sxy, Sxx, beta_fwl)
  ward_side_full <- aggregate_influence(dt_fwl, c("ward_pair_id", "ward_side", "ward"), Sxy, Sxx, beta_fwl)

  building_full <- data.table()
  if (sum(!is.na(dt_fwl$building_proxy)) > 0) {
    building_full <- aggregate_influence(dt_fwl[!is.na(building_proxy)], "building_proxy", Sxy, Sxx, beta_fwl)
    loc <- dt_fwl[!is.na(building_proxy), .(
      latitude = median(latitude, na.rm = TRUE),
      longitude = median(longitude, na.rm = TRUE)
    ), by = building_proxy]
    building_full <- merge(building_full, loc, by = "building_proxy", all.x = TRUE)
  }

  boundary_fast <- copy(boundary_full[1:min(.N, top_n_fast)])
  ward_side_fast <- copy(ward_side_full[1:min(.N, top_n_fast)])
  building_fast <- copy(building_full[1:min(.N, top_n_fast)])

  # Exact leave-one-out for top groups
  top_b <- if (smoke) min(8L, top_n_exact_boundary) else top_n_exact_boundary
  top_w <- if (smoke) min(8L, top_n_exact_ward_side) else top_n_exact_ward_side
  top_g <- if (smoke) min(12L, top_n_exact_building) else top_n_exact_building

  boundary_exact <- run_exact_loo(dt_fwl, formulas, spec_name, "ward_pair_id", boundary_full, top_b)
  ward_side_exact <- run_exact_loo(dt_fwl, formulas, spec_name, "ward_side", ward_side_full, top_w)
  building_exact <- if (nrow(building_full) > 0) run_exact_loo(dt_fwl[!is.na(building_proxy)], formulas, spec_name, "building_proxy", building_full, top_g) else data.table()

  if (nrow(boundary_exact) > 0) {
    boundary_exact <- merge(
      boundary_exact,
      unique(boundary_full[, .(ward_pair_id)]),
      by.x = "group_id",
      by.y = "ward_pair_id",
      all.x = TRUE
    )
    boundary_exact[, ward_pair_id := group_id]
  }
  if (nrow(ward_side_exact) > 0) {
    ward_side_exact <- merge(
      ward_side_exact,
      unique(ward_side_full[, .(ward_side, ward_pair_id, ward)]),
      by.x = "group_id",
      by.y = "ward_side",
      all.x = TRUE
    )
    ward_side_exact[, ward_side := group_id]
  }
  if (nrow(building_exact) > 0) {
    building_exact <- merge(
      building_exact,
      unique(building_full[, .(building_proxy, latitude, longitude)]),
      by.x = "group_id",
      by.y = "building_proxy",
      all.x = TRUE
    )
    building_exact[, building_proxy := group_id]
  }

  # LOP diagnostics
  pair_month_metrics <- build_pair_month_metrics(dt_fwl, beta, near_ft, bin_breaks, bin_labels)
  boundary_lop <- build_boundary_summary(pair_month_metrics, boundary_full, boundary_exact, lop_bins, near_ft)

  list(
    spec = spec_name,
    dt = dt_fwl,
    model = model,
    beta = beta,
    se = se,
    pval = pval,
    n_obs = nobs(model),
    dep_var_mean = mean(dt_fwl$rent_price, na.rm = TRUE),
    ward_pairs = uniqueN(dt_fwl$ward_pair_id),
    beta_fwl = beta_fwl,
    fwl_abs_diff = fwl_abs_diff,
    strictness_sd = spec_prep$strict_sd,
    Sxy = Sxy,
    Sxx = Sxx,
    boundary_full = boundary_full,
    ward_side_full = ward_side_full,
    building_full = building_full,
    boundary_fast = boundary_fast,
    ward_side_fast = ward_side_fast,
    building_fast = building_fast,
    boundary_exact = boundary_exact,
    ward_side_exact = ward_side_exact,
    building_exact = building_exact,
    pair_month_metrics = pair_month_metrics,
    boundary_lop = boundary_lop
  )
}

specs <- c("no_hedonics", "with_hedonics")
res_list <- lapply(specs, function(s) run_spec(base_dt, s))
names(res_list) <- specs

# Geometry tags from certification + anomalies
cert_pass_global <- TRUE
if (file.exists(cert_gates)) {
  gates <- fread(cert_gates)
  if ("pass" %in% names(gates)) cert_pass_global <- all(as.logical(gates$pass))
}

anomaly_pairs <- character()
if (file.exists(anomaly_samples)) {
  anom <- fread(anomaly_samples)
  if ("ward_pair_id" %in% names(anom)) {
    anomaly_pairs <- unique(as.character(anom$ward_pair_id[!is.na(anom$ward_pair_id)]))
  }
}

apply_geometry_clean <- function(dt) {
  if (nrow(dt) == 0 || !"ward_pair_id" %in% names(dt)) return(dt)
  x <- copy(dt)
  x[, geometry_clean := cert_pass_global & !(ward_pair_id %in% anomaly_pairs)]
  x[, geometry_flag := fifelse(ward_pair_id %in% anomaly_pairs, "anomaly_pair", fifelse(cert_pass_global, "clean", "cert_gate_fail"))]
  x
}

for (s in specs) {
  res_list[[s]]$boundary_fast <- apply_geometry_clean(res_list[[s]]$boundary_fast)
  res_list[[s]]$boundary_exact <- apply_geometry_clean(res_list[[s]]$boundary_exact)
  res_list[[s]]$boundary_lop <- apply_geometry_clean(res_list[[s]]$boundary_lop)
}

message("[step] preparing ward boundary geometry")
ward_panel <- st_read(ward_panel, quiet = TRUE)
ward_panel <- ward_panel %>%
  mutate(ward = as.integer(ward), year = as.integer(year))

if (isTRUE(st_is_longlat(ward_panel))) {
  ward_panel <- st_transform(ward_panel, 26971)
}

available_years <- sort(unique(ward_panel$year))
year_priority <- c(2015L, 2014L, 2024L, 2003L, 1998L)
year_priority <- year_priority[year_priority %in% available_years]
if (length(year_priority) == 0) year_priority <- available_years

lines_by_year <- list()
for (yy in year_priority) {
  w <- ward_panel[ward_panel$year == yy, c("ward")]
  lines_by_year[[as.character(yy)]] <- st_transform(build_boundary_lines(w), 4326)
}

drawable_pairs <- unique(unlist(lapply(lines_by_year, function(x) {
  if (is.null(x) || nrow(x) == 0 || !"pair_id" %in% names(x)) return(character())
  as.character(x$pair_id)
})))

ward_base_year <- if (2015L %in% available_years) 2015L else max(available_years)
ward_base <- st_transform(ward_panel[ward_panel$year == ward_base_year, c("ward")], 4326)

# Replication check against current rental border pair FE output
ref_dt <- data.table()
if (file.exists(reference_csv)) {
  ref_dt <- fread(reference_csv)
  ref_dt[, spec := as.character(specification)]
}

main_rows <- rbindlist(lapply(specs, function(s) {
  rr <- res_list[[s]]
  ref_est <- NA_real_
  if (nrow(ref_dt) > 0) {
    idx <- which(ref_dt$spec == s)
    if (length(idx) > 0) ref_est <- as.numeric(ref_dt$estimate[idx[1]])
  }
  repl_abs_diff <- abs(rr$beta - ref_est)
  repl_pass <- ifelse(is.na(ref_est), NA, repl_abs_diff <= 1e-6)

  data.table(
    row_type = "model",
    spec = s,
    estimate = rr$beta,
    std_error = rr$se,
    p_value = rr$pval,
    n_obs = rr$n_obs,
    dep_var_mean = rr$dep_var_mean,
    ward_pairs = rr$ward_pairs,
    beta_fwl = rr$beta_fwl,
    fwl_abs_diff = rr$fwl_abs_diff,
    fwl_pass = rr$fwl_abs_diff <= 1e-8,
    reference_estimate = ref_est,
    replication_abs_diff = repl_abs_diff,
    replication_pass = repl_pass
  )
}), fill = TRUE)

# Test metrics
build_test_rows <- function() {
  test_rows <- list()

  for (s in specs) {
    rr <- res_list[[s]]

    # Exact vs fast overlap on top-10 boundaries
    fast_top <- rr$boundary_full[1:min(.N, 10), ward_pair_id]
    exact_top <- rr$boundary_exact[order(-abs_influence_exact)][1:min(.N, 10), ward_pair_id]
    overlap <- length(intersect(fast_top, exact_top))
    overlap_pass <- ifelse(length(fast_top) >= 10 && length(exact_top) >= 10, overlap >= 7, NA)

    # LOP pair-month consistency
    pm <- rr$pair_month_metrics
    side <- rr$dt[, .(
      n_listings = .N,
      strictness_side = mean(strictness_std)
    ), by = .(ward_pair_id, year_month, ward)]
    high <- side[order(ward_pair_id, year_month, -strictness_side, -n_listings, ward), .SD[1], by = .(ward_pair_id, year_month)]
    low <- side[order(ward_pair_id, year_month, strictness_side, -n_listings, ward), .SD[1], by = .(ward_pair_id, year_month)]
    expected_pm <- merge(high, low, by = c("ward_pair_id", "year_month"), suffixes = c("_high", "_low"), all = FALSE)[
      strictness_side_high > strictness_side_low,
      .N
    ]
    actual_pm <- nrow(pm)
    pm_pass <- identical(as.integer(expected_pm), as.integer(actual_pm))

    # Geometry coverage on top boundaries
    top_for_coverage <- unique(rr$boundary_full[1:min(.N, 50), ward_pair_id])
    coverage <- if (length(top_for_coverage) > 0 && length(drawable_pairs) > 0) {
      mean(top_for_coverage %in% drawable_pairs)
    } else {
      NA_real_
    }

    test_rows[[length(test_rows) + 1]] <- data.table(
      row_type = "test",
      spec = s,
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      n_obs = NA_real_,
      dep_var_mean = NA_real_,
      ward_pairs = NA_real_,
      beta_fwl = NA_real_,
      fwl_abs_diff = NA_real_,
      fwl_pass = rr$fwl_abs_diff <= 1e-8,
      reference_estimate = NA_real_,
      replication_abs_diff = NA_real_,
      replication_pass = main_rows[spec == s, replication_pass][1],
      metric = "exact_fast_top10_overlap_boundaries",
      value = overlap,
      threshold = 7,
      pass = overlap_pass
    )

    test_rows[[length(test_rows) + 1]] <- data.table(
      row_type = "test",
      spec = s,
      metric = "lop_pair_month_consistency",
      value = actual_pm,
      threshold = expected_pm,
      pass = pm_pass
    )

    test_rows[[length(test_rows) + 1]] <- data.table(
      row_type = "test",
      spec = s,
      metric = "geometry_join_coverage_top50",
      value = coverage,
      threshold = 0.95,
      pass = ifelse(is.na(coverage), NA, coverage >= 0.95)
    )
  }

  rbindlist(test_rows, fill = TRUE)
}

test_rows <- build_test_rows()
main_summary <- rbindlist(list(main_rows, test_rows), fill = TRUE)

# Combine output tables across specs
boundary_fast_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$boundary_fast)
  x[, spec := s]
  x
}), fill = TRUE)

ward_side_fast_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$ward_side_fast)
  x[, spec := s]
  x
}), fill = TRUE)

building_fast_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$building_fast)
  x[, spec := s]
  x
}), fill = TRUE)

boundary_exact_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$boundary_exact)
  x[, spec := s]
  x
}), fill = TRUE)

ward_side_exact_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$ward_side_exact)
  x[, spec := s]
  x
}), fill = TRUE)

building_exact_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$building_exact)
  x[, spec := s]
  x
}), fill = TRUE)

lop_pair_month_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$pair_month_metrics)
  x[, spec := s]
  x
}), fill = TRUE)

lop_boundary_all <- rbindlist(lapply(specs, function(s) {
  x <- copy(res_list[[s]]$boundary_lop)
  x[, spec := s]
  x
}), fill = TRUE)

# LOP flags table
lop_flags <- copy(lop_boundary_all[severe_flag == TRUE])
if (nrow(lop_flags) > 0) {
  lop_flags[, sort_abs_influence := fifelse(!is.na(abs_influence_exact), abs_influence_exact, abs_influence_fast)]
  setorder(lop_flags, -sort_abs_influence, -max_abs_implied_gap_pct, -max_abs_raw_gap_pct)
  lop_flags[, sort_abs_influence := NULL]
  lop_flags <- lop_flags[1:min(.N, top_n_fast)]
}

# Build maps
message("[step] building map artifacts")

make_contrib_map_df <- function(spec_name) {
  ex <- res_list[[spec_name]]$boundary_exact
  fa <- res_list[[spec_name]]$boundary_fast

  if (nrow(ex) > 0) {
    src <- ex[, .(ward_pair_id, influence = influence_exact, abs_influence = abs_influence_exact, geometry_clean)]
  } else {
    src <- fa[, .(ward_pair_id, influence = influence_fast, abs_influence = abs_influence_fast, geometry_clean)]
  }

  pos <- src[influence > 0][order(-abs_influence)][1:min(.N, 12)]
  neg <- src[influence < 0][order(-abs_influence)][1:min(.N, 12)]

  if (nrow(pos) > 0) pos[, direction := "Positive drivers"]
  if (nrow(neg) > 0) neg[, direction := "Negative drivers"]

  out <- rbindlist(list(pos, neg), fill = TRUE)
  if (nrow(out) > 0) out[, spec := spec_name]
  out
}

contrib_map_df <- rbindlist(lapply(specs, make_contrib_map_df), fill = TRUE)
contrib_map_sf <- attach_pair_geometry(contrib_map_df, "ward_pair_id", lines_by_year, year_priority, 4326)

lop_map_df <- lop_boundary_all[severe_flag == TRUE, .(
  spec, ward_pair_id, geometry_clean, total_listings,
  severity = pmax(max_abs_raw_gap_pct, max_abs_implied_gap_pct)
)]
lop_map_sf <- attach_pair_geometry(lop_map_df, "ward_pair_id", lines_by_year, year_priority, 4326)

build_map_df <- rbindlist(lapply(specs, function(s) {
  ex <- res_list[[s]]$building_exact
  fa <- res_list[[s]]$building_fast
  if (nrow(ex) > 0) {
    if (!"latitude" %in% names(ex)) ex[, latitude := NA_real_]
    if (!"longitude" %in% names(ex)) ex[, longitude := NA_real_]
    x <- ex[order(-abs_influence_exact)][1:min(.N, 80), .(
      spec = s,
      building_proxy,
      latitude,
      longitude,
      influence = influence_exact,
      abs_influence = abs_influence_exact
    )]
  } else {
    x <- fa[order(-abs_influence_fast)][1:min(.N, 80), .(
      spec = s,
      building_proxy,
      latitude,
      longitude,
      influence = influence_fast,
      abs_influence = abs_influence_fast
    )]
  }
  x[!is.na(latitude) & !is.na(longitude)]
}), fill = TRUE)

ward_base_by_spec <- do.call(rbind, lapply(specs, function(s) {
  wb <- ward_base
  wb$spec <- s
  wb
}))

ward_base_by_dir_spec <- do.call(rbind, lapply(specs, function(s) {
  do.call(rbind, lapply(c("Positive drivers", "Negative drivers"), function(d) {
    wb <- ward_base
    wb$spec <- s
    wb$direction <- d
    wb
  }))
}))

p1 <- ggplot() +
  geom_sf(data = ward_base_by_dir_spec, fill = NA, color = "grey80", linewidth = 0.2) +
  {
    if (nrow(contrib_map_sf) > 0) geom_sf(
      data = contrib_map_sf,
      aes(color = influence, linewidth = abs_influence, linetype = geometry_clean)
    )
  } +
  scale_color_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
  scale_linewidth_continuous(range = c(0.4, 1.8)) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), drop = FALSE) +
  facet_grid(direction ~ spec) +
  labs(
    title = "Top Boundary Contributors to Rental Border-Pair FE",
    subtitle = "Positive and negative driver boundaries by core specification",
    color = "Influence",
    linewidth = "Abs influence",
    linetype = "Geometry clean"
  ) +
  theme_minimal()

p2 <- ggplot() +
  geom_sf(data = ward_base_by_spec, fill = NA, color = "grey85", linewidth = 0.2) +
  {
    if (nrow(lop_map_sf) > 0) geom_sf(
      data = lop_map_sf,
      aes(color = severity, linewidth = sqrt(pmax(total_listings, 1)), linetype = geometry_clean)
    )
  } +
  scale_color_viridis_c(option = "C") +
  scale_linewidth_continuous(range = c(0.3, 1.6)) +
  scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), drop = FALSE) +
  facet_wrap(~spec) +
  labs(
    title = "Potential LOP Severity by Boundary",
    subtitle = "Severity = max(|raw gap|, |model-implied gap|), severe-flag support thresholds applied",
    color = "Severity",
    linewidth = "Sqrt listings",
    linetype = "Geometry clean"
  ) +
  theme_minimal()

p3 <- ggplot() +
  geom_sf(data = ward_base_by_spec, fill = NA, color = "grey85", linewidth = 0.2) +
  {
    if (nrow(build_map_df) > 0) geom_point(
      data = build_map_df,
      aes(x = longitude, y = latitude, color = influence, size = abs_influence),
      alpha = 0.75
    )
  } +
  scale_color_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b", midpoint = 0) +
  scale_size_continuous(range = c(1.0, 3.8)) +
  facet_wrap(~spec) +
  labs(
    title = "Top Building-Cluster Contributors",
    subtitle = sprintf("Building proxy = rounded lat/lon (%d decimals)", building_round),
    color = "Influence",
    size = "Abs influence"
  ) +
  theme_minimal()

maps_pdf <- file.path(output_dir, "rent_fe_driver_maps.pdf")
pdf(maps_pdf, width = 12, height = 9)
print(p1)
print(p2)
print(p3)
dev.off()

# Write CSV outputs
fwrite(main_summary, file.path(output_dir, "rent_fe_driver_main_summary.csv"))
fwrite(boundary_fast_all, file.path(output_dir, "rent_fe_boundary_influence_fast.csv"))
fwrite(ward_side_fast_all, file.path(output_dir, "rent_fe_ward_side_influence_fast.csv"))
fwrite(building_fast_all, file.path(output_dir, "rent_fe_building_influence_fast.csv"))
fwrite(boundary_exact_all, file.path(output_dir, "rent_fe_boundary_influence_exact_loo.csv"))
fwrite(ward_side_exact_all, file.path(output_dir, "rent_fe_ward_side_influence_exact_loo.csv"))
fwrite(building_exact_all, file.path(output_dir, "rent_fe_building_influence_exact_loo.csv"))
fwrite(lop_pair_month_all, file.path(output_dir, "rent_fe_lop_pair_month_metrics.csv"))
fwrite(lop_boundary_all, file.path(output_dir, "rent_fe_lop_boundary_summary.csv"))
fwrite(lop_flags, file.path(output_dir, "rent_fe_lop_flags_top_boundaries.csv"))

# Main summary markdown
summary_md <- c(
  "# Rental Border-Pair FE Driver Main Summary",
  "",
  sprintf("- generated: `%s`", as.character(Sys.time())),
  sprintf("- bw_ft: `%d`", bw_ft),
  sprintf("- window: `%s`", window),
  sprintf("- sample_filter: `%s`", sample_filter),
  sprintf("- smoke: `%s`", smoke),
  "",
  "## Core FE Estimates",
  "| Spec | Estimate | SE | p-value | N | Ward Pairs | Mean Rent | FWL Diff | Replication Diff |",
  "|---|---:|---:|---:|---:|---:|---:|---:|---:|"
)

for (s in specs) {
  rr <- main_rows[spec == s][1]
  summary_md <- c(summary_md, sprintf(
    "| %s | %.6f | %.6f | %.6f | %s | %s | %.2f | %.10f | %s |",
    s,
    rr$estimate,
    rr$std_error,
    rr$p_value,
    format(rr$n_obs, big.mark = ","),
    format(rr$ward_pairs, big.mark = ","),
    rr$dep_var_mean,
    rr$fwl_abs_diff,
    ifelse(is.na(rr$replication_abs_diff), "NA", sprintf("%.10f", rr$replication_abs_diff))
  ))
}

summary_md <- c(summary_md, "", "## Diagnostics Tests", "| Spec | Metric | Value | Threshold | Pass |", "|---|---|---:|---:|---|")
for (i in seq_len(nrow(test_rows))) {
  tr <- test_rows[i]
  summary_md <- c(summary_md, sprintf(
    "| %s | %s | %s | %s | %s |",
    tr$spec,
    tr$metric,
    ifelse(is.na(tr$value), "NA", format(round(tr$value, 6), nsmall = 6)),
    ifelse(is.na(tr$threshold), "NA", format(round(tr$threshold, 6), nsmall = 6)),
    ifelse(is.na(tr$pass), "NA", ifelse(tr$pass, "PASS", "FAIL"))
  ))
}

summary_md <- c(summary_md, "", "## Output Files",
  "- `rent_fe_boundary_influence_fast.csv`",
  "- `rent_fe_boundary_influence_exact_loo.csv`",
  "- `rent_fe_lop_pair_month_metrics.csv`",
  "- `rent_fe_lop_boundary_summary.csv`",
  "- `rent_fe_lop_flags_top_boundaries.csv`",
  "- `rent_fe_driver_maps.pdf`"
)

writeLines(summary_md, file.path(output_dir, "rent_fe_driver_main_summary.md"))

# Takeaways markdown
concentration_line <- function(dt, value_col, top_n = 10L) {
  if (nrow(dt) == 0) return(NA_real_)
  vals <- abs(dt[[value_col]])
  vals <- vals[is.finite(vals)]
  if (length(vals) == 0) return(NA_real_)
  top <- sum(head(sort(vals, decreasing = TRUE), min(top_n, length(vals))))
  tot <- sum(vals)
  if (!is.finite(tot) || tot <= 0) return(NA_real_)
  top / tot
}

takeaways <- c(
  "# Rental FE Driver Takeaways",
  "",
  sprintf("- generated: `%s`", as.character(Sys.time())),
  "",
  "## Concentration",
  "| Spec | Top-10 Boundary Influence Share | Top-10 Ward-Side Influence Share | Top-10 Building Influence Share |",
  "|---|---:|---:|---:|"
)

for (s in specs) {
  rr <- res_list[[s]]
  boundary_share <- concentration_line(rr$boundary_full, "influence_fast", 10)
  ward_share <- concentration_line(rr$ward_side_full, "influence_fast", 10)
  build_share <- concentration_line(rr$building_full, "influence_fast", 10)

  takeaways <- c(takeaways, sprintf(
    "| %s | %s | %s | %s |",
    s,
    ifelse(is.na(boundary_share), "NA", sprintf("%.2f%%", 100 * boundary_share)),
    ifelse(is.na(ward_share), "NA", sprintf("%.2f%%", 100 * ward_share)),
    ifelse(is.na(build_share), "NA", sprintf("%.2f%%", 100 * build_share))
  ))
}

takeaways <- c(takeaways, "", "## Top Boundaries (Severe LOP Flags)")
if (nrow(lop_flags) == 0) {
  takeaways <- c(takeaways, "- No boundaries met severe-flag thresholds.")
} else {
  takeaways <- c(takeaways, "| Spec | Ward Pair | Max Raw Gap | Max Implied Gap | Pair-Month Cells | Listings | Geometry Clean |", "|---|---|---:|---:|---:|---:|---|")
  for (i in seq_len(nrow(lop_flags))) {
    lf <- lop_flags[i]
    takeaways <- c(takeaways, sprintf(
      "| %s | %s | %.2f%% | %.2f%% | %d | %s | %s |",
      lf$spec,
      lf$ward_pair_id,
      100 * lf$max_abs_raw_gap_pct,
      100 * lf$max_abs_implied_gap_pct,
      lf$n_pair_month,
      format(lf$total_listings, big.mark = ","),
      ifelse(is.na(lf$geometry_clean), "NA", ifelse(lf$geometry_clean, "TRUE", "FALSE"))
    ))
  }
}

for (s in specs) {
  lf_s <- lop_boundary_all[spec == s]
  n_flag <- lf_s[severe_flag == TRUE, .N]
  n_all <- nrow(lf_s)
  localized <- ifelse(n_all > 0, n_flag <= max(10, ceiling(0.1 * n_all)), NA)
  takeaways <- c(
    takeaways,
    "",
    sprintf("## Localized vs Widespread (%s)", s),
    sprintf("- flagged boundaries: %d / %d", n_flag, n_all),
    sprintf("- localized classification: %s", ifelse(is.na(localized), "NA", ifelse(localized, "localized", "widespread")))
  )
}

writeLines(takeaways, file.path(output_dir, "rent_fe_driver_takeaways.md"))

message("Saved: ", file.path(output_dir, "rent_fe_driver_main_summary.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_driver_main_summary.md"))
message("Saved: ", file.path(output_dir, "rent_fe_boundary_influence_fast.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_ward_side_influence_fast.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_building_influence_fast.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_boundary_influence_exact_loo.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_ward_side_influence_exact_loo.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_building_influence_exact_loo.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_lop_pair_month_metrics.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_lop_boundary_summary.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_lop_flags_top_boundaries.csv"))
message("Saved: ", file.path(output_dir, "rent_fe_driver_maps.pdf"))
message("Saved: ", file.path(output_dir, "rent_fe_driver_takeaways.md"))
message("Done.")