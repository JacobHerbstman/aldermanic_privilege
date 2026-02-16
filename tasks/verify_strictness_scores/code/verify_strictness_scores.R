source("../../setup_environment/code/packages.R")

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_quantile <- function(x, p = 0.9) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  as.numeric(quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
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

permit_file <- "../input/building_permits_text_features.csv.gz"
alderman_panel_file <- "../input/chicago_alderman_panel.csv"
strictness_file <- "../input/alderman_restrictiveness_scores_month_FEs.csv"
uncertainty_file <- "../input/alderman_uncertainty_index_ptfeFALSE_rtfeTRUE_porchTRUE_cafeFALSE_2stage.csv"

outcomes_file <- "../output/alderman_year_permit_outcomes.csv"
panel_file <- "../output/alderman_year_score_panel.csv"
correlations_file <- "../output/score_outcome_correlations.csv"
summary_file <- "../output/score_comparison_summary.csv"
overall_file <- "../output/score_comparison_overall.csv"

cat("Loading permit features...\n")
permits <- as.data.table(readr::read_csv(
  permit_file,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
))
setDT(permits)

permits[, application_start_date := as.Date(application_start_date)]
permits[, ward := ward_key(ward)]
permits[, application_year := as.integer(format(application_start_date, "%Y"))]
permits[, application_month := zoo::as.yearmon(application_start_date)]

numeric_cols <- c(
  "processing_time_days", "permit_issued", "flag_revision", "flag_zoning_relief",
  "unit_change_signal", "unit_reduction_signal", "unit_increase_signal",
  "unit_mentions_n", "units_delta_last", "unit_change_text", "flag_residential",
  "stories_max_mention", "sqft_max_mention", "parking_max_mention"
)
for (col in intersect(numeric_cols, names(permits))) {
  permits[, (col) := to_numeric(get(col))]
}

permits <- permits[!is.na(application_start_date) & !is.na(ward) & !is.na(application_year)]

cat("Loading alderman panel...\n")
alderman_panel <- data.table::fread(alderman_panel_file)
setDT(alderman_panel)
alderman_panel[, ward := ward_key(ward)]
alderman_panel[, month := zoo::as.yearmon(month, "%b %Y")]
alderman_panel <- unique(alderman_panel[!is.na(ward) & !is.na(month) & !is.na(alderman), .(ward, month, alderman)])

permits <- merge(
  permits,
  alderman_panel,
  by.x = c("ward", "application_month"),
  by.y = c("ward", "month"),
  all.x = TRUE
)

permits <- permits[!is.na(alderman)]
permits[, permit_issued_clean := fifelse(permit_issued %in% c(0, 1), permit_issued, NA_real_)]
permits[, permit_failed_clean := fifelse(!is.na(permit_issued_clean), as.numeric(permit_issued_clean == 0), NA_real_)]

cat("Aggregating outcomes to alderman-year...\n")
alderman_year_outcomes <- permits[, .(
  n_permits = .N,
  n_parcels = uniqueN(parcel_id),
  n_permits_status_known = sum(!is.na(permit_issued_clean)),
  permit_issued_share = safe_mean(permit_issued_clean),
  permit_failed_share = safe_mean(permit_failed_clean),
  processing_time_mean = safe_mean(processing_time_days),
  processing_time_median = median(processing_time_days, na.rm = TRUE),
  processing_time_p90 = safe_quantile(processing_time_days, p = 0.9),
  revision_rate = safe_mean(flag_revision),
  zoning_relief_rate = safe_mean(flag_zoning_relief),
  unit_change_signal_rate = safe_mean(unit_change_signal),
  unit_reduction_signal_rate = safe_mean(unit_reduction_signal),
  unit_increase_signal_rate = safe_mean(unit_increase_signal),
  unit_mentions_rate = safe_mean(unit_mentions_n > 0),
  mean_unit_change_text = safe_mean(unit_change_text),
  mean_units_delta_explicit = safe_mean(units_delta_last),
  stories_signal_rate = safe_mean(!is.na(stories_max_mention)),
  sqft_signal_rate = safe_mean(!is.na(sqft_max_mention)),
  parking_signal_rate = safe_mean(!is.na(parking_max_mention)),
  residential_share = safe_mean(flag_residential),
  residential_permits_n = sum(flag_residential == 1, na.rm = TRUE),
  residential_unit_reduction_rate = ifelse(
    sum(flag_residential == 1, na.rm = TRUE) > 0,
    sum(flag_residential == 1 & unit_reduction_signal == 1, na.rm = TRUE) /
      sum(flag_residential == 1, na.rm = TRUE),
    NA_real_
  )
), by = .(alderman, year = application_year)]

fwrite(alderman_year_outcomes, outcomes_file)

cat("Loading strictness and uncertainty scores...\n")
strictness <- data.table::fread(strictness_file)
setDT(strictness)
strictness[, strictness_index := to_numeric(strictness_index)]
strictness[, alderman_key := name_key(alderman)]
strictness <- strictness[!is.na(strictness_index), .(
  alderman_score_name = first(alderman),
  strictness_index = mean(strictness_index, na.rm = TRUE)
), by = alderman_key]

uncertainty <- data.table::fread(uncertainty_file)
setDT(uncertainty)
uncertainty[, uncertainty_index := to_numeric(uncertainty_index)]
uncertainty[, alderman_key := name_key(alderman)]
uncertainty <- uncertainty[!is.na(uncertainty_index), .(
  alderman_uncert_name = first(alderman),
  uncertainty_index = mean(uncertainty_index, na.rm = TRUE)
), by = alderman_key]

score_lookup <- merge(strictness, uncertainty, by = "alderman_key", all = TRUE)

alderman_year_outcomes[, alderman_key := name_key(alderman)]
alderman_year_panel <- merge(alderman_year_outcomes, score_lookup, by = "alderman_key", all.x = TRUE)

fwrite(alderman_year_panel, panel_file)

outcome_vars <- c(
  "n_permits", "n_parcels", "permit_issued_share", "permit_failed_share",
  "processing_time_mean", "processing_time_median", "processing_time_p90",
  "revision_rate", "zoning_relief_rate", "unit_change_signal_rate",
  "unit_reduction_signal_rate", "unit_increase_signal_rate", "unit_mentions_rate",
  "mean_unit_change_text", "mean_units_delta_explicit", "stories_signal_rate",
  "sqft_signal_rate", "parking_signal_rate", "residential_share",
  "residential_unit_reduction_rate"
)
score_vars <- c("strictness_index", "uncertainty_index")

correlation_rows <- list()
row_i <- 1L

for (outcome in outcome_vars) {
  for (score in score_vars) {
    dt <- alderman_year_panel[
      !is.na(get(outcome)) & !is.na(get(score)) & !is.na(year),
      .(y = get(outcome), x = get(score), year = year)
    ]

    n_obs <- nrow(dt)
    n_years <- uniqueN(dt$year)
    pearson <- NA_real_
    spearman <- NA_real_
    beta_year_fe <- NA_real_
    t_year_fe <- NA_real_
    p_year_fe <- NA_real_
    r2_year_fe <- NA_real_

    if (n_obs >= 10 && stats::sd(dt$x) > 0 && stats::sd(dt$y) > 0) {
      pearson <- suppressWarnings(cor(dt$x, dt$y, method = "pearson"))
      spearman <- suppressWarnings(cor(dt$x, dt$y, method = "spearman"))

      if (n_years >= 2) {
        dt[, x_std := as.numeric(scale(x))]
        dt[, y_std := as.numeric(scale(y))]
        if (stats::sd(dt$x_std, na.rm = TRUE) > 0 && stats::sd(dt$y_std, na.rm = TRUE) > 0) {
          fit <- tryCatch(
            lm(y_std ~ x_std + factor(year), data = dt),
            error = function(e) NULL
          )
          if (!is.null(fit) && "x_std" %in% rownames(summary(fit)$coefficients)) {
            fit_coef <- summary(fit)$coefficients["x_std", ]
            beta_year_fe <- unname(fit_coef["Estimate"])
            t_year_fe <- unname(fit_coef["t value"])
            p_year_fe <- unname(fit_coef["Pr(>|t|)"])
            r2_year_fe <- summary(fit)$adj.r.squared
          }
        }
      }
    }

    correlation_rows[[row_i]] <- data.table(
      outcome = outcome,
      score_name = score,
      n_obs = n_obs,
      n_years = n_years,
      pearson_cor = pearson,
      spearman_cor = spearman,
      abs_pearson_cor = abs(pearson),
      abs_spearman_cor = abs(spearman),
      beta_std_year_fe = beta_year_fe,
      t_stat_year_fe = t_year_fe,
      p_value_year_fe = p_year_fe,
      abs_t_stat_year_fe = abs(t_year_fe),
      adj_r2_year_fe = r2_year_fe
    )
    row_i <- row_i + 1L
  }
}

correlations <- rbindlist(correlation_rows, fill = TRUE)
fwrite(correlations, correlations_file)

strict_cols <- correlations[score_name == "strictness_index"][
  , .(
    outcome,
    strictness_pearson = pearson_cor,
    strictness_abs_pearson = abs_pearson_cor,
    strictness_abs_t_year_fe = abs_t_stat_year_fe,
    strictness_adj_r2_year_fe = adj_r2_year_fe
  )
]
uncert_cols <- correlations[score_name == "uncertainty_index"][
  , .(
    outcome,
    uncertainty_pearson = pearson_cor,
    uncertainty_abs_pearson = abs_pearson_cor,
    uncertainty_abs_t_year_fe = abs_t_stat_year_fe,
    uncertainty_adj_r2_year_fe = adj_r2_year_fe
  )
]

summary_table <- merge(strict_cols, uncert_cols, by = "outcome", all = TRUE)
summary_table[, better_by_abs_pearson := fifelse(
  is.na(strictness_abs_pearson) | is.na(uncertainty_abs_pearson), NA_character_,
  fifelse(strictness_abs_pearson > uncertainty_abs_pearson, "strictness_index",
          fifelse(strictness_abs_pearson < uncertainty_abs_pearson, "uncertainty_index", "tie"))
)]
summary_table[, better_by_abs_t_year_fe := fifelse(
  is.na(strictness_abs_t_year_fe) | is.na(uncertainty_abs_t_year_fe), NA_character_,
  fifelse(strictness_abs_t_year_fe > uncertainty_abs_t_year_fe, "strictness_index",
          fifelse(strictness_abs_t_year_fe < uncertainty_abs_t_year_fe, "uncertainty_index", "tie"))
)]

fwrite(summary_table, summary_file)

overall <- rbindlist(list(
  correlations[score_name == "strictness_index", .(
    score_name = "strictness_index",
    mean_abs_pearson = mean(abs_pearson_cor, na.rm = TRUE),
    median_abs_pearson = median(abs_pearson_cor, na.rm = TRUE),
    mean_abs_t_year_fe = mean(abs_t_stat_year_fe, na.rm = TRUE),
    median_abs_t_year_fe = median(abs_t_stat_year_fe, na.rm = TRUE)
  )],
  correlations[score_name == "uncertainty_index", .(
    score_name = "uncertainty_index",
    mean_abs_pearson = mean(abs_pearson_cor, na.rm = TRUE),
    median_abs_pearson = median(abs_pearson_cor, na.rm = TRUE),
    mean_abs_t_year_fe = mean(abs_t_stat_year_fe, na.rm = TRUE),
    median_abs_t_year_fe = median(abs_t_stat_year_fe, na.rm = TRUE)
  )]
))

wins <- summary_table[, .(
  outcome_count = .N,
  wins_by_abs_pearson_strictness = sum(better_by_abs_pearson == "strictness_index", na.rm = TRUE),
  wins_by_abs_pearson_uncertainty = sum(better_by_abs_pearson == "uncertainty_index", na.rm = TRUE),
  ties_by_abs_pearson = sum(better_by_abs_pearson == "tie", na.rm = TRUE),
  wins_by_abs_t_year_fe_strictness = sum(better_by_abs_t_year_fe == "strictness_index", na.rm = TRUE),
  wins_by_abs_t_year_fe_uncertainty = sum(better_by_abs_t_year_fe == "uncertainty_index", na.rm = TRUE),
  ties_by_abs_t_year_fe = sum(better_by_abs_t_year_fe == "tie", na.rm = TRUE)
)]

overall <- cbind(overall, wins[, .(
  outcome_count,
  wins_by_abs_pearson_strictness,
  wins_by_abs_pearson_uncertainty,
  ties_by_abs_pearson,
  wins_by_abs_t_year_fe_strictness,
  wins_by_abs_t_year_fe_uncertainty,
  ties_by_abs_t_year_fe
)])

fwrite(overall, overall_file)

cat("\nDone.\n")
cat("Alderman-year rows:", nrow(alderman_year_panel), "\n")
cat("Outcomes written:", outcomes_file, "\n")
cat("Correlations written:", correlations_file, "\n")
cat("Summary written:", summary_file, "\n")
cat("Overall written:", overall_file, "\n")
