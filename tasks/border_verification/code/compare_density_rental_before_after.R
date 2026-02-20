source("../../setup_environment/code/packages.R")

library(data.table)

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_verification/code")
# snapshot_dir <- NULL
# output_dir <- "../output"
# Rscript compare_density_rental_before_after.R NULL "../output"
# =======================================================================================

# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────
cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 2) {
  snapshot_dir <- cli_args[1]
  output_dir <- cli_args[2]
} else {
  if (!exists("snapshot_dir") || !exists("output_dir")) {
    stop("FATAL: Script requires 2 args: <snapshot_dir> <output_dir>", call. = FALSE)
  }
}

if (is.null(snapshot_dir) || snapshot_dir == "") {
  stop("--snapshot_dir is required")
}
if (!dir.exists(snapshot_dir)) {
  stop(sprintf("Snapshot directory does not exist: %s", snapshot_dir))
}
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

strip_num <- function(x) {
  vals <- regmatches(as.character(x), gregexpr("-?[0-9]+\\.?[0-9]*", as.character(x), perl = TRUE))
  vapply(vals, function(v) {
    if (length(v) == 0) return(NA_real_)
    suppressWarnings(as.numeric(v[[1]]))
  }, numeric(1))
}

safe_read_lines <- function(path) {
  if (!file.exists(path)) return(character())
  readLines(path, warn = FALSE)
}

parse_density_tex <- function(path, bw_label, when_label) {
  lines <- safe_read_lines(path)
  if (length(lines) == 0) {
    return(data.table(
      component = "density_fe",
      specification = bw_label,
      term = c("ln_FAR", "ln_DUPAC", "ln_Units"),
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  idx <- grep("Uncertainty Index", lines, fixed = TRUE)
  if (length(idx) == 0 || idx[1] >= length(lines)) {
    return(data.table(
      component = "density_fe",
      specification = bw_label,
      term = c("ln_FAR", "ln_DUPAC", "ln_Units"),
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  coef_line <- lines[idx[1]]
  se_line <- lines[idx[1] + 1]

  coef_parts <- trimws(strsplit(coef_line, "&", fixed = TRUE)[[1]])
  se_parts <- trimws(strsplit(se_line, "&", fixed = TRUE)[[1]])

  if (length(coef_parts) < 4 || length(se_parts) < 4) {
    return(data.table(
      component = "density_fe",
      specification = bw_label,
      term = c("ln_FAR", "ln_DUPAC", "ln_Units"),
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  data.table(
    component = "density_fe",
    specification = bw_label,
    term = c("ln_FAR", "ln_DUPAC", "ln_Units"),
    estimate = c(strip_num(coef_parts[2]), strip_num(coef_parts[3]), strip_num(coef_parts[4])),
    std_error = c(strip_num(se_parts[2]), strip_num(se_parts[3]), strip_num(se_parts[4])),
    when = when_label,
    source_file = path,
    read_ok = TRUE
  )
}

read_rental_did <- function(path, when_label) {
  if (!file.exists(path)) {
    return(data.table(
      component = "rental_did",
      specification = c("No controls", "With controls"),
      term = "post_treat",
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  dt <- fread(path)
  out <- NULL

  if (all(c("specification", "estimate", "std_error") %in% names(dt))) {
    out <- dt[specification %in% c("No controls", "With controls"), .(
      component = "rental_did",
      specification,
      term = "post_treat",
      estimate = as.numeric(estimate),
      std_error = as.numeric(std_error)
    )]
  } else if (all(c("variable", "estimate_no_ctrl", "se_no_ctrl", "estimate_ctrl", "se_ctrl") %in% names(dt))) {
    row <- dt[variable == "post_treat"]
    if (nrow(row) > 0) {
      out <- rbindlist(list(
        data.table(
          component = "rental_did",
          specification = "No controls",
          term = "post_treat",
          estimate = as.numeric(row$estimate_no_ctrl[1]),
          std_error = as.numeric(row$se_no_ctrl[1])
        ),
        data.table(
          component = "rental_did",
          specification = "With controls",
          term = "post_treat",
          estimate = as.numeric(row$estimate_ctrl[1]),
          std_error = as.numeric(row$se_ctrl[1])
        )
      ))
    }
  }

  if (is.null(out) || nrow(out) == 0) {
    out <- data.table(
      component = "rental_did",
      specification = c("No controls", "With controls"),
      term = "post_treat",
      estimate = NA_real_,
      std_error = NA_real_,
      read_ok = FALSE
    )
  } else {
    out[, read_ok := TRUE]
  }

  out[, `:=`(when = when_label, source_file = path)]
  out
}

read_rental_effect_summary <- function(path, when_label) {
  keep_metrics <- c(
    "n_obs", "n_blocks", "n_listings", "n_boundaries", "n_ward_sides", "n_building_proxies",
    "beta_direct", "se_direct", "beta_fwl", "direct_minus_fwl",
    "event_t2_continuous_estimate", "event_t2_continuous_se",
    "event_t2_stricter_estimate", "event_t2_stricter_se",
    "event_t2_lenient_estimate", "event_t2_lenient_se",
    "beta_without_top1_boundary_abs", "beta_without_top5_boundary_abs", "beta_without_top10_boundary_abs"
  )

  if (!file.exists(path)) {
    return(data.table(
      component = "rental_effect_source",
      specification = "summary",
      term = keep_metrics,
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  dt <- fread(path)
  if (!all(c("metric", "value") %in% names(dt))) {
    return(data.table(
      component = "rental_effect_source",
      specification = "summary",
      term = keep_metrics,
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  out <- dt[metric %in% keep_metrics, .(
    component = "rental_effect_source",
    specification = "summary",
    term = metric,
    estimate = suppressWarnings(as.numeric(value)),
    std_error = NA_real_
  )]

  missing_terms <- setdiff(keep_metrics, out$term)
  if (length(missing_terms) > 0) {
    out <- rbind(out, data.table(
      component = "rental_effect_source",
      specification = "summary",
      term = missing_terms,
      estimate = NA_real_,
      std_error = NA_real_
    ), fill = TRUE)
  }

  out[, `:=`(when = when_label, source_file = path, read_ok = TRUE)]
  out
}

read_sales_did <- function(path, when_label) {
  if (!file.exists(path)) {
    return(data.table(
      component = "sales_did_sanity",
      specification = c("2012_no_ctrl", "2012_ctrl", "2015_no_ctrl", "2015_ctrl"),
      term = "post_treat",
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  dt <- fread(path)
  if (!all(c("variable", "specification", "estimate", "se") %in% names(dt))) {
    return(data.table(
      component = "sales_did_sanity",
      specification = c("2012_no_ctrl", "2012_ctrl", "2015_no_ctrl", "2015_ctrl"),
      term = "post_treat",
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ))
  }

  out <- dt[variable == "post_treat", .(
    component = "sales_did_sanity",
    specification = as.character(specification),
    term = "post_treat",
    estimate = as.numeric(estimate),
    std_error = as.numeric(se)
  )]
  out[, `:=`(when = when_label, source_file = path, read_ok = TRUE)]

  needed_specs <- c("2012_no_ctrl", "2012_ctrl", "2015_no_ctrl", "2015_ctrl")
  missing_specs <- setdiff(needed_specs, out$specification)
  if (length(missing_specs) > 0) {
    out <- rbind(out, data.table(
      component = "sales_did_sanity",
      specification = missing_specs,
      term = "post_treat",
      estimate = NA_real_,
      std_error = NA_real_,
      when = when_label,
      source_file = path,
      read_ok = FALSE
    ), fill = TRUE)
  }

  out
}

collect_side <- function(snapshot_dir, when_label) {
  density_paths <- data.table(
    bw = c("bw250", "bw500", "bw1000"),
    file = c(
      "fe_table_bw250_pair_x_year.tex",
      "fe_table_bw500_pair_x_year.tex",
      "fe_table_bw1000_pair_x_year.tex"
    )
  )

  density_dt <- rbindlist(lapply(seq_len(nrow(density_paths)), function(i) {
    rr <- density_paths[i]
    root <- if (when_label == "before") snapshot_dir else "../../border_pair_FE_regressions/output"
    parse_density_tex(file.path(root, rr$file), rr$bw, when_label)
  }), fill = TRUE)

  rental_did_root <- if (when_label == "before") snapshot_dir else "../../run_event_study_rental_disaggregate/output"
  sales_did_root <- if (when_label == "before") snapshot_dir else "../../run_event_study_sales_disaggregate/output"

  rental_did_dt <- read_rental_did(file.path(rental_did_root, "did_table_rental.csv"), when_label)
  rental_effect_dt <- read_rental_effect_summary(file.path(rental_did_root, "rental_effect_source_summary.csv"), when_label)
  sales_dt <- read_sales_did(file.path(sales_did_root, "did_coefficients_sales.csv"), when_label)

  rbindlist(list(density_dt, rental_did_dt, rental_effect_dt, sales_dt), fill = TRUE)
}

before_dt <- collect_side(snapshot_dir, "before")
after_dt <- collect_side(snapshot_dir, "after")

cmp <- merge(
  before_dt,
  after_dt,
  by = c("component", "specification", "term"),
  all = TRUE,
  suffixes = c("_before", "_after")
)

cmp[, `:=`(
  estimate_delta = estimate_after - estimate_before,
  std_error_delta = std_error_after - std_error_before,
  pct_change = fifelse(!is.na(estimate_before) & abs(estimate_before) > 1e-12,
    100 * (estimate_after - estimate_before) / abs(estimate_before),
    NA_real_
  )
)]

cmp[, comparison_status := fifelse(
  is.na(estimate_before) & is.na(estimate_after), "missing_both",
  fifelse(is.na(estimate_before), "missing_before",
    fifelse(is.na(estimate_after), "missing_after", "ok")
  )
)]

setorder(cmp, component, specification, term)
fwrite(cmp, file.path(output_dir, "density_rental_change_summary.csv"))

fmt <- function(x, digits = 4) {
  ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits, trim = TRUE))
}

md <- c(
  "# Density and Rental Before/After Change Summary",
  "",
  sprintf("- snapshot_dir: `%s`", snapshot_dir),
  sprintf("- generated: `%s`", as.character(Sys.time())),
  ""
)

# Density block
md <- c(md, "## Density FE (Border Pair x Year)")
density_cmp <- cmp[component == "density_fe"]
if (nrow(density_cmp) == 0) {
  md <- c(md, "- No density comparison rows found.", "")
} else {
  md <- c(md, "| Bandwidth | Outcome | Before | After | Delta | Before SE | After SE |", "|---|---|---:|---:|---:|---:|---:|")
  for (i in seq_len(nrow(density_cmp))) {
    rr <- density_cmp[i]
    md <- c(md, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
      rr$specification, rr$term,
      fmt(rr$estimate_before), fmt(rr$estimate_after), fmt(rr$estimate_delta),
      fmt(rr$std_error_before), fmt(rr$std_error_after)
    ))
  }
  md <- c(md, "")
}

# Rental DID block
md <- c(md, "## Rental DiD")
rental_did_cmp <- cmp[component == "rental_did"]
if (nrow(rental_did_cmp) == 0) {
  md <- c(md, "- No rental DiD comparison rows found.", "")
} else {
  md <- c(md, "| Spec | Before | After | Delta | Before SE | After SE |", "|---|---:|---:|---:|---:|---:|")
  for (i in seq_len(nrow(rental_did_cmp))) {
    rr <- rental_did_cmp[i]
    md <- c(md, sprintf("| %s | %s | %s | %s | %s | %s |",
      rr$specification,
      fmt(rr$estimate_before), fmt(rr$estimate_after), fmt(rr$estimate_delta),
      fmt(rr$std_error_before), fmt(rr$std_error_after)
    ))
  }
  md <- c(md, "")
}

# Rental effect summary block
md <- c(md, "## Rental Effect Source Metrics")
effect_cmp <- cmp[component == "rental_effect_source"]
if (nrow(effect_cmp) == 0) {
  md <- c(md, "- No rental effect-source comparison rows found.", "")
} else {
  md <- c(md, "| Metric | Before | After | Delta | % change vs |before| |", "|---|---:|---:|---:|---:|")
  for (i in seq_len(nrow(effect_cmp))) {
    rr <- effect_cmp[i]
    md <- c(md, sprintf("| %s | %s | %s | %s | %s |",
      rr$term,
      fmt(rr$estimate_before, 6), fmt(rr$estimate_after, 6), fmt(rr$estimate_delta, 6), fmt(rr$pct_change, 2)
    ))
  }
  md <- c(md, "")
}

# Sales sanity block
md <- c(md, "## Sales DiD Sanity Check")
sales_cmp <- cmp[component == "sales_did_sanity"]
if (nrow(sales_cmp) == 0) {
  md <- c(md, "- No sales sanity rows found.", "")
} else {
  md <- c(md, "| Spec | Before | After | Delta | Before SE | After SE | Status |", "|---|---:|---:|---:|---:|---:|---|")
  for (i in seq_len(nrow(sales_cmp))) {
    rr <- sales_cmp[i]
    md <- c(md, sprintf("| %s | %s | %s | %s | %s | %s | %s |",
      rr$specification,
      fmt(rr$estimate_before), fmt(rr$estimate_after), fmt(rr$estimate_delta),
      fmt(rr$std_error_before), fmt(rr$std_error_after), rr$comparison_status
    ))
  }
  md <- c(md, "")
}

writeLines(md, file.path(output_dir, "density_rental_change_summary.md"))

# -----------------------------------------------------------------------------
# Rental concentration explanation report with certification cleanliness tags
# -----------------------------------------------------------------------------
bound_path <- "../../run_event_study_rental_disaggregate/output/rental_effect_source_boundaries_top50.csv"
ward_path <- "../../run_event_study_rental_disaggregate/output/rental_effect_source_wards_top50.csv"
build_path <- "../../run_event_study_rental_disaggregate/output/rental_effect_source_buildings_top200.csv"
anom_path <- file.path(output_dir, "border_pair_anomaly_samples.csv")
gates_path <- file.path(output_dir, "border_certification_gates.csv")

bounds <- if (file.exists(bound_path)) fread(bound_path) else data.table()
wards <- if (file.exists(ward_path)) fread(ward_path) else data.table()
buildings <- if (file.exists(build_path)) fread(build_path) else data.table()
anoms <- if (file.exists(anom_path)) fread(anom_path) else data.table()
gates <- if (file.exists(gates_path)) fread(gates_path) else data.table()

rent_anoms <- anoms[dataset == "rent_pre_scores_full"]
if (!("ward_pair_id" %in% names(rent_anoms))) {
  rent_anoms[, ward_pair_id := NA_character_]
}
rent_anoms[, ward_pair_id := as.character(ward_pair_id)]
invalid_pairs <- unique(rent_anoms$ward_pair_id)
invalid_pairs <- invalid_pairs[!is.na(invalid_pairs) & nzchar(invalid_pairs)]

invalid_ward_counts <- data.table(ward = integer(), anomaly_n = integer())
if (nrow(rent_anoms) > 0) {
  ww <- rbind(
    data.table(ward = as.integer(rent_anoms$ward)),
    data.table(ward = as.integer(rent_anoms$neighbor_ward))
  )
  ww <- ww[!is.na(ward)]
  invalid_ward_counts <- ww[, .(anomaly_n = .N), by = ward]
}

anom_building_keys <- character()
if (nrow(rent_anoms) > 0 && all(c("latitude", "longitude") %in% names(rent_anoms))) {
  valid_geo <- rent_anoms[!is.na(latitude) & !is.na(longitude)]
  anom_building_keys <- unique(sprintf("%.5f_%.5f", round(valid_geo$latitude, 5), round(valid_geo$longitude, 5)))
}

if (nrow(bounds) > 0) {
  if (!("ward_pair_id" %in% names(bounds))) {
    bounds[, ward_pair_id := NA_character_]
  }
  bounds[, ward_pair_id := as.character(ward_pair_id)]
  bounds[, geometry_clean := !(ward_pair_id %in% invalid_pairs)]
  pair_counts <- rent_anoms[, .(anomaly_n = .N), by = .(ward_pair_id = as.character(ward_pair_id))]
  bounds <- merge(bounds, pair_counts, by = "ward_pair_id", all.x = TRUE)
  bounds[is.na(anomaly_n), anomaly_n := 0L]
  setorder(bounds, -abs_influence)
  bounds <- bounds[1:min(.N, 20)]
}

if (nrow(wards) > 0) {
  wards[, ward_origin := as.integer(ward_origin)]
  wards <- merge(wards, invalid_ward_counts, by.x = "ward_origin", by.y = "ward", all.x = TRUE)
  wards[is.na(anomaly_n), anomaly_n := 0L]
  wards[, geometry_clean := anomaly_n == 0L]
  setorder(wards, -abs_influence)
  wards <- wards[1:min(.N, 20)]
}

if (nrow(buildings) > 0) {
  bb <- copy(buildings)
  sp <- tstrsplit(bb$building_proxy, "_", fixed = TRUE)
  bb[, `:=`(lat_proxy = suppressWarnings(as.numeric(sp[[1]])), lon_proxy = suppressWarnings(as.numeric(sp[[2]])))]
  bb[, proxy_key_5 := sprintf("%.5f_%.5f", round(lat_proxy, 5), round(lon_proxy, 5))]
  bb[, geometry_clean := !(proxy_key_5 %in% anom_building_keys)]
  setorder(bb, -abs_influence)
  buildings <- bb[1:min(.N, 20)]
}

share_top <- function(dt, n = 5) {
  if (nrow(dt) == 0 || !"abs_influence" %in% names(dt)) return(NA_real_)
  denom <- sum(dt$abs_influence, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  topn <- sum(head(dt$abs_influence, n), na.rm = TRUE)
  topn / denom
}

conc_lines <- c(
  "# Rental Effect Concentration Report",
  "",
  sprintf("- generated: `%s`", as.character(Sys.time())),
  "",
  "## Certification Gate Context"
)

if (nrow(gates) == 0) {
  conc_lines <- c(conc_lines, "- Certification gates file not found in border_verification output.", "")
} else {
  for (i in seq_len(nrow(gates))) {
    rr <- gates[i]
    conc_lines <- c(conc_lines, sprintf("- %s: value=%.8f threshold %s %.8f -> %s",
      rr$gate_id, rr$value, rr$comparator, rr$threshold, ifelse(rr$pass, "PASS", "FAIL")
    ))
  }
  conc_lines <- c(conc_lines, "")
}

conc_lines <- c(conc_lines, "## Concentration Metrics")
if (nrow(bounds) > 0) {
  conc_lines <- c(conc_lines, sprintf("- Top 5 boundary share of abs influence (within top20 table): %.2f%%", 100 * share_top(bounds, 5)))
}
if (nrow(wards) > 0) {
  conc_lines <- c(conc_lines, sprintf("- Top 5 ward-side share of abs influence (within top20 table): %.2f%%", 100 * share_top(wards, 5)))
}
if (nrow(buildings) > 0) {
  conc_lines <- c(conc_lines, sprintf("- Top 5 building-proxy share of abs influence (within top20 table): %.2f%%", 100 * share_top(buildings, 5)))
}
conc_lines <- c(conc_lines, "")

conc_lines <- c(conc_lines, "## Top Boundaries (with Geometry Clean Flag)")
if (nrow(bounds) == 0) {
  conc_lines <- c(conc_lines, "- No boundary contribution file found.", "")
} else {
  conc_lines <- c(conc_lines, "| Rank | Cohort | Ward Pair | Abs Influence | Geometry Clean | Anomaly Rows |",
    "|---:|---|---|---:|---|---:|")
  for (i in seq_len(nrow(bounds))) {
    rr <- bounds[i]
    conc_lines <- c(conc_lines, sprintf("| %d | %s | %s | %.6f | %s | %d |",
      i, as.character(rr$cohort), rr$ward_pair_id, rr$abs_influence,
      ifelse(rr$geometry_clean, "clean", "flagged"), rr$anomaly_n
    ))
  }
  conc_lines <- c(conc_lines, "")
}

conc_lines <- c(conc_lines, "## Top Ward Sides (with Geometry Clean Flag)")
if (nrow(wards) == 0) {
  conc_lines <- c(conc_lines, "- No ward contribution file found.", "")
} else {
  conc_lines <- c(conc_lines, "| Rank | Cohort | Ward Side | Abs Influence | Geometry Clean | Anomaly Rows |",
    "|---:|---|---:|---:|---|---:|")
  for (i in seq_len(nrow(wards))) {
    rr <- wards[i]
    conc_lines <- c(conc_lines, sprintf("| %d | %s | %d | %.6f | %s | %d |",
      i, as.character(rr$cohort), rr$ward_origin, rr$abs_influence,
      ifelse(rr$geometry_clean, "clean", "flagged"), rr$anomaly_n
    ))
  }
  conc_lines <- c(conc_lines, "")
}

conc_lines <- c(conc_lines, "## Top Building Proxies (with Geometry Clean Flag)")
if (nrow(buildings) == 0) {
  conc_lines <- c(conc_lines, "- No building contribution file found.", "")
} else {
  conc_lines <- c(conc_lines, "| Rank | Building Proxy | Abs Influence | Geometry Clean |",
    "|---:|---|---:|---|")
  for (i in seq_len(nrow(buildings))) {
    rr <- buildings[i]
    conc_lines <- c(conc_lines, sprintf("| %d | %s | %.6f | %s |",
      i, rr$building_proxy, rr$abs_influence,
      ifelse(rr$geometry_clean, "clean", "flagged")
    ))
  }
  conc_lines <- c(conc_lines, "")
}

writeLines(conc_lines, file.path(output_dir, "rental_effect_concentration_report.md"))

message("Saved: ", file.path(output_dir, "density_rental_change_summary.csv"))
message("Saved: ", file.path(output_dir, "density_rental_change_summary.md"))
message("Saved: ", file.path(output_dir, "rental_effect_concentration_report.md"))
message("Done.")