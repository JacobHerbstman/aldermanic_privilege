source("../../setup_environment/code/packages.R")
library(data.table)
library(fixest)
library(arrow)


# ── 1) CLI ARGS ───────────────────────────────────────────────────────────────

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/run_event_study_rental_disaggregate/code")
# input <- "../input/rental_listing_panel.parquet"
# geo_input <- "../input/rent_with_ward_distances.parquet"
# bandwidth <- 1000
# weighting <- "triangular"
# sample_filter <- "multifamily_only"
# fe_type <- "strict_pair_x_year"
# top_n <- 50
# top_blocks <- 200
# top_buildings <- 200
# out_summary <- "../output/rental_effect_source_summary.csv"
# out_event_coeffs <- "../output/rental_effect_source_event_study_coefficients.csv"
# out_counterfactual <- "../output/rental_effect_source_counterfactuals.csv"
# out_boundaries <- "../output/rental_effect_source_boundaries.csv"
# out_boundaries_top <- "../output/rental_effect_source_boundaries_top50.csv"
# out_wards <- "../output/rental_effect_source_wards.csv"
# out_wards_top <- "../output/rental_effect_source_wards_top50.csv"
# out_blocks_top <- "../output/rental_effect_source_blocks_top200.csv"
# out_buildings <- "../output/rental_effect_source_buildings.csv"
# out_buildings_top <- "../output/rental_effect_source_buildings_top200.csv"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(input, geo_input, bandwidth, weighting, sample_filter, fe_type, top_n, top_blocks, top_buildings, out_summary, out_event_coeffs, out_counterfactual, out_boundaries, out_boundaries_top, out_wards, out_wards_top, out_blocks_top, out_buildings, out_buildings_top)
}

if (length(cli_args) >= 19) {
  input <- cli_args[1]
  geo_input <- cli_args[2]
  bandwidth <- suppressWarnings(as.integer(cli_args[3]))
  weighting <- cli_args[4]
  sample_filter <- cli_args[5]
  fe_type <- cli_args[6]
  top_n <- suppressWarnings(as.integer(cli_args[7]))
  top_blocks <- suppressWarnings(as.integer(cli_args[8]))
  top_buildings <- suppressWarnings(as.integer(cli_args[9]))
  out_summary <- cli_args[10]
  out_event_coeffs <- cli_args[11]
  out_counterfactual <- cli_args[12]
  out_boundaries <- cli_args[13]
  out_boundaries_top <- cli_args[14]
  out_wards <- cli_args[15]
  out_wards_top <- cli_args[16]
  out_blocks_top <- cli_args[17]
  out_buildings <- cli_args[18]
  out_buildings_top <- cli_args[19]
} else {
  if (!exists("input") || !exists("geo_input") || !exists("bandwidth") || !exists("weighting") || !exists("sample_filter") || !exists("fe_type") || !exists("top_n") || !exists("top_blocks") || !exists("top_buildings") || !exists("out_summary") || !exists("out_event_coeffs") || !exists("out_counterfactual") || !exists("out_boundaries") || !exists("out_boundaries_top") || !exists("out_wards") || !exists("out_wards_top") || !exists("out_blocks_top") || !exists("out_buildings") || !exists("out_buildings_top")) {
    stop("FATAL: Script requires 19 args: <input> <geo_input> <bandwidth> <weighting> <sample_filter> <fe_type> <top_n> <top_blocks> <top_buildings> <out_summary> <out_event_coeffs> <out_counterfactual> <out_boundaries> <out_boundaries_top> <out_wards> <out_wards_top> <out_blocks_top> <out_buildings> <out_buildings_top>", call. = FALSE)
  }
}

if (!weighting %in% c("triangular", "uniform")) {
  stop("--weighting must be one of: triangular, uniform", call. = FALSE)
}
if (!sample_filter %in% c("multifamily_only", "full_sample")) {
  stop("--sample_filter must be one of: multifamily_only, full_sample", call. = FALSE)
}
if (!fe_type %in% c("strict_pair_x_year", "pair_trend_plus_year", "side_plus_year")) {
  stop("--fe_type must be one of: strict_pair_x_year, pair_trend_plus_year, side_plus_year", call. = FALSE)
}

message("=== Diagnose Rental Effect Sources ===")
message("Input: ", input)
message("Geo input: ", geo_input)
message("Bandwidth: ", bandwidth)
message("Weighting: ", weighting)
message("Sample filter: ", sample_filter)
message("FE type: ", fe_type)

keep_cols <- c(
  "id", "block_id", "cohort", "year", "relative_year_capped",
  "dist_ft", "rent_price", "strictness_change",
  "building_type_clean", "building_type_factor",
  "log_sqft", "log_beds", "log_baths",
  "ward_pair_id", "ward_origin", "ward_dest",
  "cohort_block_id", "cohort_ward_pair", "cohort_ward_pair_side"
)

message("Loading rental listing panel...")
dt <- as.data.table(read_parquet(input, col_select = keep_cols))
message("Rows loaded: ", format(nrow(dt), big.mark = ","))

if (!is.factor(dt$building_type_factor)) {
  dt[, building_type_factor := as.factor(building_type_factor)]
}

dt <- dt[
  !is.na(strictness_change) &
    !is.na(rent_price) & rent_price > 0 &
    dist_ft <= bandwidth &
    !is.na(log_sqft) &
    !is.na(log_beds) &
    !is.na(log_baths) &
    !is.na(building_type_factor)
]

if (sample_filter == "multifamily_only") {
  dt <- dt[building_type_clean == "multi_family"]
}

if (nrow(dt) == 0) {
  stop("No observations remain after filtering.", call. = FALSE)
}

message("Rows after filters: ", format(nrow(dt), big.mark = ","))
message("Unique blocks: ", format(uniqueN(dt$block_id), big.mark = ","))
message("Unique listings: ", format(uniqueN(dt$id), big.mark = ","))

dt[, `:=`(
  post = as.integer(relative_year_capped >= 0),
  weight = if (weighting == "triangular") pmax(0, 1 - dist_ft / bandwidth) else 1
)]
dt[, post_treat := post * strictness_change]

# Ensure ward pair variable used by FE formulas exists and is stable
if (!"cohort_ward_pair" %in% names(dt) || any(is.na(dt$cohort_ward_pair))) {
  dt[, ward_pair_side_tmp := sub("^[0-9]+_", "", cohort_ward_pair_side)]
  dt[, ward_pair := sub("_[0-9]+$", "", ward_pair_side_tmp)]
  dt[, cohort_ward_pair := paste(cohort, ward_pair, sep = "_")]
} else {
  dt[, ward_pair_side_tmp := sub("^[0-9]+_", "", cohort_ward_pair_side)]
  dt[, ward_pair := sub("_[0-9]+$", "", ward_pair_side_tmp)]
}

# Bring in coordinates so we can aggregate to building proxies
if (file.exists(geo_input)) {
  message("Loading listing coordinates for building-level attribution...")
  geo <- as.data.table(read_parquet(geo_input, col_select = c("id", "latitude", "longitude")))
  geo <- geo[!is.na(latitude) & !is.na(longitude)]
  if (nrow(geo) > 0) {
    setorder(geo, id)
    geo <- geo[, .SD[1], by = id]
    dt <- merge(dt, geo, by = "id", all.x = TRUE, sort = FALSE)
    dt[, building_proxy := fifelse(
      !is.na(latitude) & !is.na(longitude),
      sprintf("%.5f_%.5f", round(latitude, 5), round(longitude, 5)),
      NA_character_
    )]
    message("Rows with coordinates: ", format(sum(!is.na(dt$building_proxy)), big.mark = ","))
  } else {
    dt[, `:=`(latitude = NA_real_, longitude = NA_real_, building_proxy = NA_character_)]
    message("No usable coordinates found in geo input.")
  }
} else {
  dt[, `:=`(latitude = NA_real_, longitude = NA_real_, building_proxy = NA_character_)]
  message("Geo input file not found. Building-level outputs will be empty.")
}

fe_formula <- switch(fe_type,
  "strict_pair_x_year" = "cohort_ward_pair_side + cohort_ward_pair^year",
  "pair_trend_plus_year" = "cohort_ward_pair_side + cohort^year + cohort_ward_pair[year]",
  "side_plus_year" = "cohort_ward_pair_side + cohort^year"
)

controls <- "building_type_factor + log_sqft + log_beds + log_baths"
full_formula <- as.formula(sprintf("log(rent_price) ~ post_treat + %s | %s", controls, fe_formula))
y_formula <- as.formula(sprintf("log(rent_price) ~ %s | %s", controls, fe_formula))
x_formula <- as.formula(sprintf("post_treat ~ %s | %s", controls, fe_formula))

message("Estimating full DiD model...")
m_full <- feols(
  full_formula,
  data = dt,
  weights = ~weight,
  cluster = ~cohort_block_id,
  warn = FALSE
)

beta_direct <- as.numeric(coef(m_full)["post_treat"])
se_direct <- as.numeric(se(m_full)["post_treat"])

message(sprintf("Direct beta: %.5f (SE %.5f)", beta_direct, se_direct))

message("Residualizing for FWL decomposition...")
m_y <- feols(y_formula, data = dt, weights = ~weight, warn = FALSE)
m_x <- feols(x_formula, data = dt, weights = ~weight, warn = FALSE)

dt[, `:=`(
  y_tilde = resid(m_y),
  x_tilde = resid(m_x)
)]
dt[, `:=`(
  sxy = weight * x_tilde * y_tilde,
  sxx = weight * x_tilde * x_tilde
)]

Sxy <- dt[, sum(sxy)]
Sxx <- dt[, sum(sxx)]
beta_fwl <- Sxy / Sxx

message(sprintf("FWL beta: %.5f", beta_fwl))
message(sprintf("Direct - FWL difference: %.6f", beta_direct - beta_fwl))

message("Estimating event-study models for period-specific diagnostics...")
dt[, `:=`(
  treatment_continuous = strictness_change,
  treatment_stricter_continuous = pmax(strictness_change, 0),
  treatment_lenient_continuous = pmax(-strictness_change, 0)
)]

es_formula_cont <- as.formula(sprintf(
  "log(rent_price) ~ i(relative_year_capped, treatment_continuous, ref = -1) + %s | %s",
  controls, fe_formula
))
es_formula_strict <- as.formula(sprintf(
  "log(rent_price) ~ i(relative_year_capped, treatment_stricter_continuous, ref = -1) + %s | %s",
  controls, fe_formula
))
es_formula_lenient <- as.formula(sprintf(
  "log(rent_price) ~ i(relative_year_capped, treatment_lenient_continuous, ref = -1) + %s | %s",
  controls, fe_formula
))

m_es_cont <- feols(es_formula_cont, data = dt, weights = ~weight, cluster = ~cohort_block_id, warn = FALSE)
m_es_strict <- feols(es_formula_strict, data = dt, weights = ~weight, cluster = ~cohort_block_id, warn = FALSE)
m_es_lenient <- feols(es_formula_lenient, data = dt, weights = ~weight, cluster = ~cohort_block_id, warn = FALSE)

extract_event_coefs <- function(model, spec_name) {
  out <- data.table(
    term = names(coef(model)),
    estimate = as.numeric(coef(model)),
    se = as.numeric(se(model))
  )
  out <- out[grepl("^relative_year_capped::", term)]
  out[, relative_year := as.integer(sub("^relative_year_capped::(-?[0-9]+):.*$", "\\1", term))]
  out[, `:=`(
    spec = spec_name,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se
  )]
  setorder(out, relative_year)
  out
}

event_coefs <- rbindlist(list(
  extract_event_coefs(m_es_cont, "continuous"),
  extract_event_coefs(m_es_strict, "stricter_continuous"),
  extract_event_coefs(m_es_lenient, "lenient_continuous")
), use.names = TRUE)

t2_cont <- event_coefs[spec == "continuous" & relative_year == 2]
t2_strict <- event_coefs[spec == "stricter_continuous" & relative_year == 2]
t2_lenient <- event_coefs[spec == "lenient_continuous" & relative_year == 2]

compute_group_stats <- function(data, by_vars) {
  out <- data[, .(
    n_obs = .N,
    n_post = sum(post),
    n_blocks = uniqueN(block_id),
    n_listings = uniqueN(id),
    sum_weight = sum(weight),
    sxy = sum(sxy),
    sxx = sum(sxx),
    mean_dist_ft = mean(dist_ft),
    mean_strictness_change = mean(strictness_change),
    mean_post_treat = mean(post_treat)
  ), by = by_vars]

  out[, `:=`(
    share_sxy = sxy / Sxy,
    share_sxx = sxx / Sxx,
    beta_leave_out = fifelse((Sxx - sxx) > 0, (Sxy - sxy) / (Sxx - sxx), NA_real_)
  )]
  out[, influence := beta_fwl - beta_leave_out]
  out[, abs_influence := abs(influence)]
  setorder(out, -abs_influence)
  out
}

counterfactual_table <- function(group_dt, group_level, ks = c(1L, 5L, 10L, 20L, 50L, 100L)) {
  out <- rbindlist(list(
    {
      tmp <- copy(group_dt)
      setorder(tmp, -abs_influence)
      tmp[, `:=`(cum_sxy = cumsum(sxy), cum_sxx = cumsum(sxx), rank = .I)]
      tmp[rank %in% ks & rank <= .N, .(
        group_level = group_level,
        ranking = "abs_influence",
        k = rank,
        beta_without_top_k = (Sxy - cum_sxy) / (Sxx - cum_sxx)
      )]
    },
    {
      tmp <- copy(group_dt)
      setorder(tmp, -influence)
      tmp[, `:=`(cum_sxy = cumsum(sxy), cum_sxx = cumsum(sxx), rank = .I)]
      tmp[rank %in% ks & rank <= .N, .(
        group_level = group_level,
        ranking = "pushes_beta_up",
        k = rank,
        beta_without_top_k = (Sxy - cum_sxy) / (Sxx - cum_sxx)
      )]
    },
    {
      tmp <- copy(group_dt)
      setorder(tmp, influence)
      tmp[, `:=`(cum_sxy = cumsum(sxy), cum_sxx = cumsum(sxx), rank = .I)]
      tmp[rank %in% ks & rank <= .N, .(
        group_level = group_level,
        ranking = "pushes_beta_down",
        k = rank,
        beta_without_top_k = (Sxy - cum_sxy) / (Sxx - cum_sxx)
      )]
    }
  ), use.names = TRUE)

  out
}

message("Computing group-level attribution tables...")

boundaries <- compute_group_stats(dt, c("cohort", "ward_pair_id"))
boundaries[, c("ward_a", "ward_b") := lapply(tstrsplit(ward_pair_id, "-", fixed = TRUE), as.integer)]
setcolorder(boundaries, c("cohort", "ward_pair_id", "ward_a", "ward_b", setdiff(names(boundaries), c("cohort", "ward_pair_id", "ward_a", "ward_b"))))
wards <- compute_group_stats(dt, c("cohort", "ward_origin"))
blocks <- compute_group_stats(dt, c("cohort", "block_id", "ward_pair_id"))

if (sum(!is.na(dt$building_proxy)) > 0) {
  buildings <- compute_group_stats(dt[!is.na(building_proxy)], c("building_proxy"))
  geo_meta <- dt[!is.na(building_proxy), .(
    latitude = median(latitude),
    longitude = median(longitude),
    n_unique_ids = uniqueN(id),
    n_cohorts = uniqueN(cohort)
  ), by = building_proxy]
  buildings <- merge(buildings, geo_meta, by = "building_proxy", all.x = TRUE, sort = FALSE)
  setorder(buildings, -abs_influence)
} else {
  buildings <- data.table()
}

boundary_top <- head(boundaries, top_n)
ward_top <- head(wards, top_n)
block_top <- head(blocks, top_blocks)
building_top <- if (nrow(buildings) > 0) head(buildings, top_buildings) else buildings

counterfactuals <- rbindlist(list(
  counterfactual_table(boundaries, "boundary"),
  counterfactual_table(wards, "ward_side"),
  counterfactual_table(blocks, "block"),
  if (nrow(buildings) > 0) counterfactual_table(buildings, "building_proxy") else data.table()
), use.names = TRUE, fill = TRUE)

get_flip_k <- function(group_dt, order_col) {
  tmp <- copy(group_dt)
  if (order_col == "abs") setorder(tmp, -abs_influence)
  if (order_col == "up") setorder(tmp, -influence)
  if (order_col == "down") setorder(tmp, influence)
  tmp[, beta_without := (Sxy - cumsum(sxy)) / (Sxx - cumsum(sxx))]
  idx <- if (beta_fwl > 0) {
    which(tmp$beta_without <= 0)
  } else if (beta_fwl < 0) {
    which(tmp$beta_without >= 0)
  } else {
    integer(0)
  }
  if (length(idx) == 0) {
    return(NA_integer_)
  }
  idx[1]
}

summary_dt <- data.table(
  metric = c(
    "n_obs",
    "n_blocks",
    "n_listings",
    "n_boundaries",
    "n_ward_sides",
    "n_building_proxies",
    "beta_direct",
    "se_direct",
    "beta_fwl",
    "direct_minus_fwl",
    "event_t2_continuous_estimate",
    "event_t2_continuous_se",
    "event_t2_stricter_estimate",
    "event_t2_stricter_se",
    "event_t2_lenient_estimate",
    "event_t2_lenient_se",
    "beta_without_top1_boundary_abs",
    "beta_without_top5_boundary_abs",
    "beta_without_top10_boundary_abs",
    "k_to_flip_sign_drop_up_boundaries",
    "k_to_flip_sign_drop_up_wards",
    "k_to_flip_sign_drop_up_blocks",
    "k_to_flip_sign_drop_up_buildings"
  ),
  value = c(
    nrow(dt),
    uniqueN(dt$block_id),
    uniqueN(dt$id),
    nrow(boundaries),
    nrow(wards),
    nrow(buildings),
    beta_direct,
    se_direct,
    beta_fwl,
    beta_direct - beta_fwl,
    if (nrow(t2_cont) > 0) t2_cont$estimate[1] else NA_real_,
    if (nrow(t2_cont) > 0) t2_cont$se[1] else NA_real_,
    if (nrow(t2_strict) > 0) t2_strict$estimate[1] else NA_real_,
    if (nrow(t2_strict) > 0) t2_strict$se[1] else NA_real_,
    if (nrow(t2_lenient) > 0) t2_lenient$estimate[1] else NA_real_,
    if (nrow(t2_lenient) > 0) t2_lenient$se[1] else NA_real_,
    if (nrow(counterfactuals[group_level == "boundary" & ranking == "abs_influence" & k == 1]) > 0) {
      counterfactuals[group_level == "boundary" & ranking == "abs_influence" & k == 1]$beta_without_top_k[1]
    } else {
      NA_real_
    },
    if (nrow(counterfactuals[group_level == "boundary" & ranking == "abs_influence" & k == 5]) > 0) {
      counterfactuals[group_level == "boundary" & ranking == "abs_influence" & k == 5]$beta_without_top_k[1]
    } else {
      NA_real_
    },
    if (nrow(counterfactuals[group_level == "boundary" & ranking == "abs_influence" & k == 10]) > 0) {
      counterfactuals[group_level == "boundary" & ranking == "abs_influence" & k == 10]$beta_without_top_k[1]
    } else {
      NA_real_
    },
    get_flip_k(boundaries, "up"),
    get_flip_k(wards, "up"),
    get_flip_k(blocks, "up"),
    if (nrow(buildings) > 0) get_flip_k(buildings, "up") else NA_real_
  )
)

fwrite(summary_dt, out_summary)
fwrite(event_coefs, out_event_coeffs)
fwrite(counterfactuals, out_counterfactual)
fwrite(boundaries, out_boundaries)
fwrite(boundary_top, out_boundaries_top)
fwrite(wards, out_wards)
fwrite(ward_top, out_wards_top)
fwrite(block_top, out_blocks_top)

if (nrow(buildings) > 0) {
  fwrite(buildings, out_buildings)
  fwrite(building_top, out_buildings_top)
} else {
  fwrite(data.table(), out_buildings)
  fwrite(data.table(), out_buildings_top)
}

message("Saved summary: ", out_summary)
message("Saved event-study coefficients: ", out_event_coeffs)
message("Saved counterfactuals: ", out_counterfactual)
message("Saved boundaries: ", out_boundaries)
message("Saved boundary top table: ", out_boundaries_top)
message("Saved wards: ", out_wards)
message("Saved ward top table: ", out_wards_top)
message("Saved block top table: ", out_blocks_top)
message("Saved buildings: ", out_buildings)
message("Saved building top table: ", out_buildings_top)

message("\nTop 10 boundaries by absolute influence:")
print(boundary_top[1:min(10L, .N), .(
  cohort, ward_pair_id, ward_a, ward_b, n_obs,
  influence, beta_leave_out, share_sxy, share_sxx
)])

message("\nTop 10 ward-sides by absolute influence:")
print(ward_top[1:min(10L, .N), .(
  cohort, ward_origin, n_obs,
  influence, beta_leave_out, share_sxy, share_sxx
)])

if (nrow(building_top) > 0) {
  message("\nTop 10 building proxies by absolute influence:")
  print(building_top[1:min(10L, .N), .(
    building_proxy, latitude, longitude, n_obs,
    influence, beta_leave_out, share_sxy, share_sxx
  )])
}

message("\nDone.")
