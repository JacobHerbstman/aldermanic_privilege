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

ward_key <- function(x) {
  out <- suppressWarnings(as.integer(as.character(x)))
  out_chr <- as.character(out)
  out_chr[is.na(out)] <- NA_character_
  out_chr
}

zscore <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(NA_real_, length(x)))
  }
  (x - mean(x, na.rm = TRUE)) / s
}

zscore_by_group <- function(x, g) {
  out <- rep(NA_real_, length(x))
  idx_split <- split(seq_along(x), g)
  for (idx in idx_split) {
    vals <- x[idx]
    s <- sd(vals, na.rm = TRUE)
    if (!is.na(s) && s > 0) {
      out[idx] <- (vals - mean(vals, na.rm = TRUE)) / s
    }
  }
  out
}

panel_file <- "../output/alderman_year_score_panel.csv"
alderman_panel_file <- "../input/chicago_alderman_panel.csv"

regression_file <- "../output/score_outcome_regressions.csv"
wins_file <- "../output/score_model_wins.csv"
winner_detail_file <- "../output/score_model_outcome_winners.csv"

cat("Loading alderman-year score panel...\n")
panel <- data.table::fread(panel_file)
setDT(panel)

num_cols <- c(
  "year", "n_permits", "n_parcels", "n_permits_status_known",
  "permit_issued_share", "permit_failed_share",
  "processing_time_mean", "processing_time_median", "processing_time_p90",
  "revision_rate", "zoning_relief_rate",
  "unit_change_signal_rate", "unit_reduction_signal_rate", "unit_increase_signal_rate",
  "unit_mentions_rate", "mean_unit_change_text", "mean_units_delta_explicit",
  "stories_signal_rate", "sqft_signal_rate", "parking_signal_rate",
  "residential_share", "residential_permits_n", "residential_unit_reduction_rate",
  "strictness_index", "uncertainty_index"
)
for (col in intersect(num_cols, names(panel))) {
  panel[, (col) := to_numeric(get(col))]
}

panel[, alderman_key := name_key(alderman)]
panel <- panel[!is.na(year) & !is.na(alderman_key)]

cat("Loading alderman monthly panel for ward mapping...\n")
alderman_panel <- data.table::fread(alderman_panel_file)
setDT(alderman_panel)
alderman_panel[, month := zoo::as.yearmon(month, "%b %Y")]
alderman_panel[, year := as.integer(format(as.Date(month), "%Y"))]
alderman_panel[, ward := ward_key(ward)]
alderman_panel[, alderman_key := name_key(alderman)]

ward_year_map <- alderman_panel[
  !is.na(alderman_key) & !is.na(year) & !is.na(ward),
  .N,
  by = .(alderman_key, year, ward)
]
setorder(ward_year_map, alderman_key, year, -N, ward)
ward_year_map <- ward_year_map[, .SD[1], by = .(alderman_key, year)][, .(alderman_key, year, ward_mode = ward)]

panel <- merge(panel, ward_year_map, by = c("alderman_key", "year"), all.x = TRUE)
panel <- panel[!is.na(strictness_index) | !is.na(uncertainty_index)]

panel[, strictness_z := zscore(strictness_index)]
panel[, uncertainty_z := zscore(uncertainty_index)]
panel[, pf_processing_p90_z := zscore_by_group(processing_time_p90, year)]
panel[, pf_revision_z := zscore_by_group(revision_rate, year)]
panel[, pf_relief_z := zscore_by_group(zoning_relief_rate, year)]
panel[, pf_reduction_z := zscore_by_group(unit_reduction_signal_rate, year)]

panel[, permit_friction_raw := rowMeans(
  cbind(pf_processing_p90_z, pf_revision_z, pf_relief_z, pf_reduction_z),
  na.rm = TRUE
)]
panel[is.nan(permit_friction_raw), permit_friction_raw := NA_real_]
panel[, permit_friction_index := zscore(permit_friction_raw)]

panel[, hybrid_equal_raw := rowMeans(
  cbind(strictness_z, uncertainty_z, permit_friction_index),
  na.rm = TRUE
)]
panel[is.nan(hybrid_equal_raw), hybrid_equal_raw := NA_real_]
panel[, hybrid_equal_index := zscore(hybrid_equal_raw)]

panel[, hybrid_pca_raw := NA_real_]
pca_cols <- c("strictness_z", "uncertainty_z", "permit_friction_index")
cc <- panel[complete.cases(panel[, ..pca_cols])]
if (nrow(cc) >= 30) {
  pca <- prcomp(cc[, ..pca_cols], center = TRUE, scale. = FALSE)
  pc1 <- pca$x[, 1]
  orient <- sign(suppressWarnings(cor(pc1, cc$strictness_z)))
  if (is.na(orient) || orient == 0) {
    orient <- 1
  }
  panel[complete.cases(panel[, ..pca_cols]), hybrid_pca_raw := pc1 * orient]
}
panel[, hybrid_pca_index := zscore(hybrid_pca_raw)]

outcomes <- c(
  "n_permits", "n_parcels", "permit_issued_share", "permit_failed_share",
  "processing_time_mean", "processing_time_median", "processing_time_p90",
  "revision_rate", "zoning_relief_rate", "unit_change_signal_rate",
  "unit_reduction_signal_rate", "unit_increase_signal_rate", "unit_mentions_rate",
  "mean_unit_change_text", "mean_units_delta_explicit", "stories_signal_rate",
  "sqft_signal_rate", "parking_signal_rate", "residential_share",
  "residential_unit_reduction_rate"
)

scores <- c("strictness_index", "uncertainty_index", "hybrid_equal_index", "hybrid_pca_index")

specs <- data.table::rbindlist(list(
  data.table(spec_id = "ols", description = "No FE, unweighted", fe = "", use_weight = FALSE, require_ward = FALSE),
  data.table(spec_id = "year_fe", description = "Year FE, unweighted", fe = "year", use_weight = FALSE, require_ward = FALSE),
  data.table(spec_id = "year_ward_fe", description = "Year + ward FE, unweighted", fe = "year + ward_mode", use_weight = FALSE, require_ward = TRUE),
  data.table(spec_id = "year_fe_w", description = "Year FE, weighted by permits", fe = "year", use_weight = TRUE, require_ward = FALSE),
  data.table(spec_id = "year_ward_fe_w", description = "Year + ward FE, weighted by permits", fe = "year + ward_mode", use_weight = TRUE, require_ward = TRUE)
))

results <- list()
i <- 1L

for (outcome in outcomes) {
  for (score in scores) {
    for (s in seq_len(nrow(specs))) {
      spec <- specs[s]

      dt <- panel[!is.na(get(outcome)) & !is.na(get(score)) & !is.na(year)]
      if (spec$require_ward) {
        dt <- dt[!is.na(ward_mode)]
      }

      if (nrow(dt) < 30) {
        results[[i]] <- data.table(
          outcome = outcome,
          score_name = score,
          spec_id = spec$spec_id,
          description = spec$description,
          n_obs = nrow(dt),
          n_years = uniqueN(dt$year),
          n_wards = uniqueN(dt$ward_mode),
          beta_std = NA_real_,
          se_std = NA_real_,
          t_stat = NA_real_,
          p_value = NA_real_,
          adj_r2 = NA_real_
        )
        i <- i + 1L
        next
      }

      dt <- copy(dt)
      dt[, y_std := zscore(get(outcome))]
      dt[, x_std := zscore(get(score))]
      dt <- dt[!is.na(y_std) & !is.na(x_std)]

      if (nrow(dt) < 30) {
        results[[i]] <- data.table(
          outcome = outcome,
          score_name = score,
          spec_id = spec$spec_id,
          description = spec$description,
          n_obs = nrow(dt),
          n_years = uniqueN(dt$year),
          n_wards = uniqueN(dt$ward_mode),
          beta_std = NA_real_,
          se_std = NA_real_,
          t_stat = NA_real_,
          p_value = NA_real_,
          adj_r2 = NA_real_
        )
        i <- i + 1L
        next
      }

      formula_str <- if (spec$fe == "") {
        "y_std ~ x_std"
      } else {
        paste0("y_std ~ x_std | ", spec$fe)
      }
      fml <- as.formula(formula_str)

      fit <- tryCatch(
        {
          if (spec$use_weight) {
            feols(fml, data = dt, weights = ~ n_permits)
          } else {
            feols(fml, data = dt)
          }
        },
        error = function(e) NULL
      )

      beta <- se <- tstat <- pval <- adjr2 <- NA_real_
      if (!is.null(fit)) {
        ctab <- coeftable(fit)
        if ("x_std" %in% rownames(ctab)) {
          beta <- ctab["x_std", "Estimate"]
          se <- ctab["x_std", "Std. Error"]
          tstat <- ctab["x_std", "t value"]
          pval <- ctab["x_std", "Pr(>|t|)"]
          adjr2 <- tryCatch(as.numeric(r2(fit, type = "ar2")), error = function(e) NA_real_)
        }
      }

      results[[i]] <- data.table(
        outcome = outcome,
        score_name = score,
        spec_id = spec$spec_id,
        description = spec$description,
        n_obs = nrow(dt),
        n_years = uniqueN(dt$year),
        n_wards = uniqueN(dt$ward_mode),
        beta_std = beta,
        se_std = se,
        t_stat = tstat,
        p_value = pval,
        abs_t_stat = abs(tstat),
        adj_r2 = adjr2
      )
      i <- i + 1L
    }
  }
}

regression_results <- rbindlist(results, fill = TRUE)
fwrite(regression_results, regression_file)

best_abs_t <- regression_results[!is.na(abs_t_stat), .(
  best_abs_t = max(abs_t_stat, na.rm = TRUE)
), by = .(spec_id, outcome)]

winner_detail <- merge(
  regression_results,
  best_abs_t,
  by = c("spec_id", "outcome"),
  all.x = TRUE
)
winner_detail[, is_winner_abs_t := as.integer(!is.na(abs_t_stat) & !is.na(best_abs_t) & abs(abs_t_stat - best_abs_t) < 1e-10)]
fwrite(winner_detail, winner_detail_file)

wins <- winner_detail[, .(
  outcomes_compared = uniqueN(outcome),
  wins_abs_t = sum(is_winner_abs_t, na.rm = TRUE),
  win_share_abs_t = mean(is_winner_abs_t, na.rm = TRUE),
  mean_abs_t = mean(abs_t_stat, na.rm = TRUE),
  median_abs_t = median(abs_t_stat, na.rm = TRUE),
  mean_adj_r2 = mean(adj_r2, na.rm = TRUE)
), by = .(spec_id, score_name)]

fwrite(wins, wins_file)

cat("\nDone.\n")
cat("Regression results:", regression_file, "\n")
cat("Model wins:", wins_file, "\n")
cat("Winner detail:", winner_detail_file, "\n")
