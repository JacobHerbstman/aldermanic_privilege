source("../../setup_environment/code/packages.R")

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

name_key <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- gsub("[^A-Z ]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

extract_fe_term <- function(model, term) {
  out <- data.table(
    estimate = NA_real_,
    se = NA_real_,
    p_value = NA_real_,
    t_stat = NA_real_,
    n_obs = NA_real_
  )
  if (is.null(model)) return(out)
  ct <- tryCatch(coeftable(model), error = function(e) NULL)
  if (is.null(ct) || !term %in% rownames(ct)) {
    out$n_obs <- model$nobs
    return(out)
  }
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  out$estimate <- ct[term, "Estimate"]
  out$se <- ct[term, "Std. Error"]
  out$p_value <- ct[term, p_col]
  out$t_stat <- ct[term, "t value"]
  out$n_obs <- model$nobs
  out
}

safe_rdrobust <- function(y, x, cluster, bw = 500, kernel = "triangular") {
  out <- data.table(
    conv_estimate = NA_real_,
    conv_se = NA_real_,
    conv_p_value = NA_real_,
    conv_t_stat = NA_real_,
    robust_estimate = NA_real_,
    robust_se = NA_real_,
    robust_p_value = NA_real_,
    robust_t_stat = NA_real_,
    n_obs = length(y)
  )
  fit <- tryCatch(
    rdrobust(y = y, x = x, c = 0, kernel = kernel, p = 1, h = bw, cluster = cluster),
    error = function(e) NULL
  )
  if (is.null(fit)) return(out)
  out$conv_estimate <- fit$coef[1]
  out$conv_se <- fit$se[1]
  out$conv_p_value <- fit$pv[1]
  out$conv_t_stat <- fit$coef[1] / fit$se[1]
  out$robust_estimate <- fit$coef[3]
  out$robust_se <- fit$se[3]
  out$robust_p_value <- fit$pv[3]
  out$robust_t_stat <- fit$coef[3] / fit$se[3]
  out
}

score_configs <- list(
  list(
    score_id = "strictness_month",
    file = "../input/alderman_restrictiveness_scores_month_FEs.csv",
    column = "strictness_index"
  ),
  list(
    score_id = "strictness_ward_month",
    file = "../input/alderman_restrictiveness_scores_ward_month_FEs.csv",
    column = "strictness_index"
  ),
  list(
    score_id = "uncertainty_ft_rt_pt_cf_2stage",
    file = "../input/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv",
    column = "uncertainty_index"
  )
)

parcels_file <- "../input/parcels_pre_scores.csv"
alderman_panel_file <- "../input/chicago_alderman_panel.csv"
block_treat_file <- "../input/block_treatment_pre_scores.csv"
sales_2012_file <- "../input/sales_transaction_panel_2012.parquet"
sales_2015_file <- "../input/sales_transaction_panel_2015.parquet"
rental_file <- "../input/rental_listing_panel.parquet"

results_file <- "../output/main_outcome_score_results.csv"
summary_file <- "../output/main_outcome_score_summary.csv"
vs_unc_file <- "../output/main_outcome_strictness_vs_uncertainty.csv"

cat("Loading core inputs...\n")
parcels_pre <- data.table::fread(parcels_file)
setDT(parcels_pre)
parcels_pre[, `:=`(
  alderman_own_key = name_key(alderman_own),
  alderman_neighbor_key = name_key(alderman_neighbor)
)]

alderman_panel <- data.table::fread(alderman_panel_file)
setDT(alderman_panel)
alderman_panel[, month_date := as.Date(paste("01", month), format = "%d %b %Y")]
alderman_lookup <- alderman_panel[
  !is.na(month_date) & as.integer(format(month_date, "%m")) == 6,
  .(
    score_year = as.integer(format(month_date, "%Y")),
    ward = as.integer(ward),
    alderman_key = name_key(alderman)
  )
]

treat_pre <- data.table::fread(block_treat_file)
setDT(treat_pre)
treat_pre[, `:=`(
  cohort = as.character(cohort),
  block_id = as.character(block_id),
  score_year = fifelse(cohort == "2015", 2014L, fifelse(cohort == "2023", 2022L, NA_integer_)),
  ward_origin = as.integer(ward_origin),
  ward_dest = as.integer(ward_dest)
)]

treat_base <- merge(
  treat_pre,
  alderman_lookup,
  by.x = c("score_year", "ward_origin"),
  by.y = c("score_year", "ward"),
  all.x = TRUE,
  sort = FALSE
)
setnames(treat_base, "alderman_key", "alderman_origin_key")

treat_base <- merge(
  treat_base,
  alderman_lookup,
  by.x = c("score_year", "ward_dest"),
  by.y = c("score_year", "ward"),
  all.x = TRUE,
  sort = FALSE
)
setnames(treat_base, "alderman_key", "alderman_dest_key")

sales_2012_base <- as.data.table(read_parquet(sales_2012_file))
sales_2015_base <- as.data.table(read_parquet(sales_2015_file))
rental_base <- as.data.table(read_parquet(rental_file))

sales_prep <- function(dt, bw = 1000) {
  dt <- copy(dt)
  dt[, block_id := as.character(block_id)]
  if (!"ward_pair" %in% names(dt)) {
    dt[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
  }
  dt <- dt[dist_ft <= bw]
  dt[, weight := pmax(0, 1 - dist_ft / bw)]
  dt[, post := as.integer(relative_year >= 0)]
  hedonics <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms", "log_baths", "has_garage")
  dt[, cc_hedonic := complete.cases(.SD), .SDcols = hedonics]
  dt
}

rental_prep <- function(dt, bw = 1000) {
  dt <- copy(dt)
  dt[, `:=`(
    block_id = as.character(block_id),
    cohort = as.character(cohort)
  )]
  if (!"cohort_block_id" %in% names(dt)) {
    dt[, cohort_block_id := paste(cohort, block_id, sep = "_")]
  }
  if (!"cohort_ward_pair" %in% names(dt)) {
    dt[, ward_pair_side := sub("^[0-9]+_", "", cohort_ward_pair_side)]
    dt[, ward_pair := sub("_[0-9]+$", "", ward_pair_side)]
    dt[, cohort_ward_pair := paste(cohort, ward_pair, sep = "_")]
  }
  dt <- dt[dist_ft <= bw]
  dt[, weight := pmax(0, 1 - dist_ft / bw)]
  dt[, post := as.integer(relative_year_capped >= 0)]
  if (!"building_type_factor" %in% names(dt)) {
    dt[, building_type_factor := as.factor(building_type_clean)]
  }
  dt[, cc_hedonic := !is.na(log_sqft) & !is.na(log_beds) & !is.na(log_baths) & !is.na(building_type_factor)]
  dt
}

sales_2012_base <- sales_prep(sales_2012_base)
sales_2015_base <- sales_prep(sales_2015_base)
rental_base <- rental_prep(rental_base)

outcome_yvars <- c("density_far", "density_dupac", "unitscount")
results <- list()
idx <- 1L

for (cfg in score_configs) {
  cat("\n=== Running score:", cfg$score_id, "===\n")
  scores_raw <- data.table::fread(cfg$file)
  setDT(scores_raw)
  if (!cfg$column %in% names(scores_raw)) {
    stop("Missing score column ", cfg$column, " in ", cfg$file, call. = FALSE)
  }
  scores <- unique(scores_raw[, .(
    alderman_key = name_key(alderman),
    score = to_numeric(get(cfg$column))
  )])
  scores <- scores[!is.na(alderman_key) & alderman_key != "" & !is.na(score)]

  # ---------------------------------------------------------------------------
  # PARCEL SCORE MERGE (for density and RD outcomes)
  # ---------------------------------------------------------------------------
  parcels <- copy(parcels_pre)
  parcels <- merge(
    parcels,
    scores,
    by.x = "alderman_own_key",
    by.y = "alderman_key",
    all.x = TRUE,
    sort = FALSE
  )
  setnames(parcels, "score", "strictness_own")
  parcels <- merge(
    parcels,
    scores,
    by.x = "alderman_neighbor_key",
    by.y = "alderman_key",
    all.x = TRUE,
    sort = FALSE
  )
  setnames(parcels, "score", "strictness_neighbor")
  parcels[, sign := fifelse(
    strictness_own > strictness_neighbor, 1,
    fifelse(strictness_own < strictness_neighbor, -1, NA_real_)
  )]
  parcels[, signed_distance := dist_to_boundary * sign]
  parcels <- parcels[!is.na(sign) & is.finite(signed_distance)]

  # ---------------------------------------------------------------------------
  # DENSITY FE (border-pair FE style; main spec)
  # ---------------------------------------------------------------------------
  dens <- copy(parcels)[
    arealotsf > 1 &
      areabuilding > 1 &
      unitscount > 1 &
      construction_year >= 2006 &
      dist_to_boundary <= 500 &
      !is.na(ward_pair) &
      !is.na(zone_code) &
      !is.na(construction_year)
  ]
  dens[, strictness_own_scaled := strictness_own / sd(strictness_own, na.rm = TRUE)]

  for (yv in outcome_yvars) {
    dt <- dens[get(yv) > 0 & is.finite(get(yv))]
    m <- tryCatch(
      feols(
        as.formula(paste0(
          "log(", yv, ") ~ strictness_own_scaled + share_white_own + share_black_own + ",
          "median_hh_income_own + share_bach_plus_own + homeownership_rate_own | ",
          "zone_code^ward_pair + construction_year"
        )),
        data = dt,
        cluster = ~ward_pair
      ),
      error = function(e) NULL
    )
    est <- extract_fe_term(m, "strictness_own_scaled")
    results[[idx]] <- data.table(
      score_id = cfg$score_id,
      family = "density_fe",
      model = "border_pair_main",
      outcome = yv,
      estimate = est$estimate,
      se = est$se,
      p_value = est$p_value,
      t_stat = est$t_stat,
      n_obs = est$n_obs
    )
    idx <- idx + 1L
  }

  # ---------------------------------------------------------------------------
  # SPATIAL RD (non-FE, robust rdrobust estimate)
  # ---------------------------------------------------------------------------
  rd <- copy(parcels)[
    arealotsf > 1 &
      areabuilding > 1 &
      unitscount > 1 &
      unitscount <= 100 &
      construction_year >= 2006 &
      abs(signed_distance) <= 500
  ]
  rd[, cluster_id := paste(boundary_year, ward_pair, zone_code, sep = "_")]

  for (yv in outcome_yvars) {
    dt <- rd[get(yv) > 0 & is.finite(get(yv))]
    rdres <- safe_rdrobust(
      y = log(dt[[yv]]),
      x = dt$signed_distance,
      cluster = dt$cluster_id,
      bw = 500,
      kernel = "triangular"
    )
    results[[idx]] <- data.table(
      score_id = cfg$score_id,
      family = "spatial_rd",
      model = "rdrobust_robust",
      outcome = yv,
      estimate = rdres$robust_estimate,
      se = rdres$robust_se,
      p_value = rdres$robust_p_value,
      t_stat = rdres$robust_t_stat,
      n_obs = rdres$n_obs
    )
    idx <- idx + 1L
  }

  # ---------------------------------------------------------------------------
  # SPATIAL RD FE (pair x year FE jump)
  # ---------------------------------------------------------------------------
  rdf <- copy(parcels)[
    arealotsf > 1 &
      areabuilding > 1 &
      unitscount > 1 &
      construction_year >= 2006 &
      dist_to_boundary <= 500 &
      !is.na(ward_pair) &
      !is.na(construction_year)
  ]
  rdf[, strictness_own_scaled := strictness_own / sd(strictness_own, na.rm = TRUE)]
  rdf[, side := as.integer(signed_distance > 0)]

  for (yv in outcome_yvars) {
    dt <- rdf[get(yv) > 0 & is.finite(get(yv))]
    dt[, outcome := log(get(yv))]
    m <- tryCatch(
      feols(
        outcome ~ side + signed_distance + side:signed_distance +
          share_white_own + share_black_own + median_hh_income_own +
          share_bach_plus_own + homeownership_rate_own | ward_pair^construction_year,
        data = dt,
        cluster = ~ward_pair
      ),
      error = function(e) NULL
    )
    est <- extract_fe_term(m, "side")
    results[[idx]] <- data.table(
      score_id = cfg$score_id,
      family = "spatial_rd_fe",
      model = "pair_x_year",
      outcome = yv,
      estimate = est$estimate,
      se = est$se,
      p_value = est$p_value,
      t_stat = est$t_stat,
      n_obs = est$n_obs
    )
    idx <- idx + 1L
  }

  # ---------------------------------------------------------------------------
  # EVENT STUDY DIDs (controlled specs only)
  # ---------------------------------------------------------------------------
  treat_spec <- merge(
    treat_base,
    scores,
    by.x = "alderman_origin_key",
    by.y = "alderman_key",
    all.x = TRUE,
    sort = FALSE
  )
  setnames(treat_spec, "score", "strictness_origin")
  treat_spec <- merge(
    treat_spec,
    scores,
    by.x = "alderman_dest_key",
    by.y = "alderman_key",
    all.x = TRUE,
    sort = FALSE
  )
  setnames(treat_spec, "score", "strictness_dest")
  treat_spec[, strictness_change := strictness_dest - strictness_origin]

  sales_lookup_dt <- unique(treat_spec[cohort == "2015", .(key = block_id, strictness_change)], by = "key")
  sales_lookup <- setNames(sales_lookup_dt$strictness_change, sales_lookup_dt$key)

  rent_lookup_dt <- unique(treat_spec[, .(key = paste(block_id, cohort, sep = "||"), strictness_change)], by = "key")
  rent_lookup <- setNames(rent_lookup_dt$strictness_change, rent_lookup_dt$key)

  s12 <- copy(sales_2012_base)
  s15 <- copy(sales_2015_base)
  rnt <- copy(rental_base)

  s12[, strictness_change := as.numeric(sales_lookup[block_id])]
  s15[, strictness_change := as.numeric(sales_lookup[block_id])]
  rnt[, strictness_change := as.numeric(rent_lookup[paste(block_id, cohort, sep = "||")])]

  s12[, `:=`(post_treat = post * strictness_change, analysis_keep = cc_hedonic & !is.na(strictness_change))]
  s15[, `:=`(post_treat = post * strictness_change, analysis_keep = cc_hedonic & !is.na(strictness_change))]
  rnt[, `:=`(post_treat = post * strictness_change, analysis_keep = cc_hedonic & !is.na(strictness_change))]

  m_sales_2012 <- tryCatch(
    feols(
      log(sale_price) ~ post_treat + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage |
        ward_pair_side + ward_pair^sale_year,
      data = s12,
      subset = ~analysis_keep,
      weights = ~weight,
      cluster = ~block_id
    ),
    error = function(e) NULL
  )

  m_sales_2015 <- tryCatch(
    feols(
      log(sale_price) ~ post_treat + log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage |
        ward_pair_side + ward_pair^sale_year,
      data = s15,
      subset = ~analysis_keep,
      weights = ~weight,
      cluster = ~block_id
    ),
    error = function(e) NULL
  )

  m_rent <- tryCatch(
    feols(
      log(rent_price) ~ post_treat + building_type_factor + log_sqft + log_beds + log_baths |
        cohort_ward_pair_side + cohort_ward_pair^year,
      data = rnt,
      subset = ~analysis_keep,
      weights = ~weight,
      cluster = ~cohort_block_id
    ),
    error = function(e) NULL
  )

  est_s12 <- extract_fe_term(m_sales_2012, "post_treat")
  est_s15 <- extract_fe_term(m_sales_2015, "post_treat")
  est_rnt <- extract_fe_term(m_rent, "post_treat")

  results[[idx]] <- data.table(
    score_id = cfg$score_id,
    family = "event_study_did",
    model = "sales_2012_ctrl",
    outcome = "sale_price",
    estimate = est_s12$estimate,
    se = est_s12$se,
    p_value = est_s12$p_value,
    t_stat = est_s12$t_stat,
    n_obs = est_s12$n_obs
  )
  idx <- idx + 1L

  results[[idx]] <- data.table(
    score_id = cfg$score_id,
    family = "event_study_did",
    model = "sales_2015_ctrl",
    outcome = "sale_price",
    estimate = est_s15$estimate,
    se = est_s15$se,
    p_value = est_s15$p_value,
    t_stat = est_s15$t_stat,
    n_obs = est_s15$n_obs
  )
  idx <- idx + 1L

  results[[idx]] <- data.table(
    score_id = cfg$score_id,
    family = "event_study_did",
    model = "rent_ctrl",
    outcome = "rent_price",
    estimate = est_rnt$estimate,
    se = est_rnt$se,
    p_value = est_rnt$p_value,
    t_stat = est_rnt$t_stat,
    n_obs = est_rnt$n_obs
  )
  idx <- idx + 1L
}

results_dt <- rbindlist(results, fill = TRUE)
results_dt[, abs_t_stat := abs(t_stat)]
fwrite(results_dt, results_file)

winner_dt <- results_dt[
  !is.na(abs_t_stat),
  .(
    best_abs_t = max(abs_t_stat, na.rm = TRUE)
  ),
  by = .(family, model, outcome)
]
winner_dt <- merge(results_dt, winner_dt, by = c("family", "model", "outcome"), all.x = TRUE)
winner_dt[, is_winner := as.integer(!is.na(abs_t_stat) & abs(abs_t_stat - best_abs_t) < 1e-10)]

summary_dt <- winner_dt[, .(
  tests = .N,
  wins = sum(is_winner, na.rm = TRUE),
  win_share = mean(is_winner, na.rm = TRUE),
  mean_abs_t = mean(abs_t_stat, na.rm = TRUE),
  median_abs_t = median(abs_t_stat, na.rm = TRUE)
), by = .(score_id, family)]
setorder(summary_dt, family, -win_share, -mean_abs_t)
fwrite(summary_dt, summary_file)

strictness_ids <- c("strictness_month", "strictness_ward_month")
vs_rows <- list()
v_idx <- 1L
for (sid in strictness_ids) {
  s_dt <- winner_dt[score_id == sid, .(family, model, outcome, strict_abs_t = abs_t_stat)]
  u_dt <- winner_dt[score_id == "uncertainty_ft_rt_pt_cf_2stage", .(family, model, outcome, uncertainty_abs_t = abs_t_stat)]
  cmp <- merge(s_dt, u_dt, by = c("family", "model", "outcome"), all = TRUE)
  cmp[, strictness_id := sid]
  cmp[, better_than_uncertainty := fifelse(
    is.na(strict_abs_t) | is.na(uncertainty_abs_t), NA_character_,
    fifelse(strict_abs_t > uncertainty_abs_t, "strictness_better",
      fifelse(strict_abs_t < uncertainty_abs_t, "uncertainty_better", "tie")
    )
  )]
  vs_rows[[v_idx]] <- cmp
  v_idx <- v_idx + 1L
}
vs_unc <- rbindlist(vs_rows, fill = TRUE)
fwrite(vs_unc, vs_unc_file)

cat("\nSaved:\n")
cat(" -", results_file, "\n")
cat(" -", summary_file, "\n")
cat(" -", vs_unc_file, "\n")

cat("\nTop-line wins by family:\n")
print(summary_dt)
