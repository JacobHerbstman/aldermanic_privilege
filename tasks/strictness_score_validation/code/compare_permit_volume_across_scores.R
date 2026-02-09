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

read_dt <- function(path) {
  if (grepl("\\.gz$", path)) {
    data.table::fread(cmd = paste("gzip -dc", shQuote(path)))
  } else {
    data.table::fread(path)
  }
}

weighted_cor <- function(x, y, w) {
  keep <- !is.na(x) & !is.na(y) & !is.na(w) & w > 0
  x <- x[keep]
  y <- y[keep]
  w <- w[keep]
  if (length(x) < 2) return(NA_real_)
  w <- w / sum(w)
  mx <- sum(w * x)
  my <- sum(w * y)
  vx <- sum(w * (x - mx)^2)
  vy <- sum(w * (y - my)^2)
  if (vx <= 0 || vy <= 0) return(NA_real_)
  cov_xy <- sum(w * (x - mx) * (y - my))
  cov_xy / sqrt(vx * vy)
}

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
}

permit_file <- "../input/building_permits_text_features.csv.gz"
alderman_panel_file <- "../input/chicago_alderman_panel.csv"
ward_month_controls_file <- "../input/ward_monthly_panel_for_alderman_fe.csv"
strictness_file <- "../input/alderman_restrictiveness_scores_month_FEs.csv"
uncertainty_dir <- "../input/uncertainty_scores"

output_file <- "../output/ward_month_validation_permit_volume_by_score.csv"
summary_file <- "../output/ward_month_validation_permit_volume_by_score_summary.csv"

cat("Loading permit-level data to build ward-month permit volume outcomes...\n")
permits <- read_dt(permit_file)
setDT(permits)
permits[, application_start_date := as.Date(application_start_date)]
bad_dates <- is.na(permits$application_start_date)
if (any(bad_dates)) {
  permits[bad_dates, application_start_date := as.Date(application_start_date, format = "%m/%d/%Y")]
}
permits[, month := as.yearmon(application_start_date)]
permits[, ward := ward_key(ward)]

high_discretion_permits <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION",
  "PERMIT - WRECKING/DEMOLITION",
  "PERMIT - PORCH CONSTRUCTION",
  "PERMIT - REINSTATE REVOKED PMT"
)

permits <- permits[
  permit_type_clean %in% high_discretion_permits &
    !is.na(month) &
    !is.na(ward)
]

alderman_panel <- data.table::fread(alderman_panel_file)
setDT(alderman_panel)
alderman_panel[, ward := ward_key(ward)]
alderman_panel[, month_ym := zoo::as.yearmon(month, "%b %Y")]
alderman_panel[is.na(month_ym), month_ym := zoo::as.yearmon(month)]
alderman_panel <- unique(alderman_panel[!is.na(ward) & !is.na(month_ym) & !is.na(alderman), .(ward, month = month_ym, alderman)])

permits <- merge(permits, alderman_panel, by = c("ward", "month"), all.x = TRUE)
permits <- permits[!is.na(alderman)]
permits[, alderman_key := name_key(alderman)]

new_counts <- permits[
  permit_type_clean == "PERMIT - NEW CONSTRUCTION",
  .(n_new_construction = .N),
  by = .(ward, month)
]

ward_month <- permits[, .(
  alderman = first(alderman),
  alderman_key = first(alderman_key),
  n_permits_all = .N
), by = .(ward, month)]

ward_month <- merge(ward_month, new_counts, by = c("ward", "month"), all.x = TRUE)
ward_month[is.na(n_new_construction), n_new_construction := 0]

controls_dt <- data.table::fread(
  ward_month_controls_file,
  select = c("ward", "month", "pop_total")
)
setDT(controls_dt)
controls_dt[, ward := ward_key(ward)]
controls_dt[, month := as.yearmon(month)]
controls_dt[, pop_total := to_numeric(pop_total)]
controls_dt <- unique(controls_dt[!is.na(ward) & !is.na(month), .(ward, month, pop_total)])

ward_month <- merge(ward_month, controls_dt, by = c("ward", "month"), all.x = TRUE)
ward_month[, permits_per_10k_pop_all := fifelse(!is.na(pop_total) & pop_total > 0, n_permits_all / (pop_total / 10000), NA_real_)]
ward_month[, permits_per_10k_pop_new := fifelse(!is.na(pop_total) & pop_total > 0, n_new_construction / (pop_total / 10000), NA_real_)]
ward_month[, share_new_construction := fifelse(n_permits_all > 0, n_new_construction / n_permits_all, NA_real_)]

outcome_specs <- data.table(
  outcome_var = c(
    "n_permits_all",
    "n_new_construction",
    "permits_per_10k_pop_all",
    "permits_per_10k_pop_new",
    "share_new_construction"
  ),
  outcome_label = c(
    "Permits per Ward-Month (All High-Discretion)",
    "Permits per Ward-Month (New Construction)",
    "Permits per 10,000 Residents (All High-Discretion)",
    "Permits per 10,000 Residents (New Construction)",
    "Share New Construction Among High-Discretion Permits"
  )
)

score_files <- c(strictness_file, sort(list.files(uncertainty_dir, pattern = "^alderman_uncertainty_index_.*_2stage\\.csv$", full.names = TRUE)))

extract_score <- function(file_path) {
  dt <- data.table::fread(file_path)
  setDT(dt)
  dt[, alderman_key := name_key(alderman)]
  score_col <- NULL
  if ("strictness_index" %in% names(dt) && grepl("restrictiveness_scores", basename(file_path))) {
    score_col <- "strictness_index"
  } else if ("uncertainty_index" %in% names(dt)) {
    score_col <- "uncertainty_index"
  } else if ("strictness_index" %in% names(dt)) {
    score_col <- "strictness_index"
  }
  if (is.null(score_col)) {
    stop(paste("No score column found in", file_path))
  }
  dt[, score_value := to_numeric(get(score_col))]
  out <- dt[!is.na(score_value), .(score_value = mean(score_value, na.rm = TRUE)), by = alderman_key]
  out
}

source_label <- function(file_path) {
  b <- basename(file_path)
  if (grepl("restrictiveness_scores_month_FEs\\.csv$", b)) return("aggregated_strictness_month_FE")
  sub("\\.csv$", "", b)
}

rows <- list()
idx <- 1L

for (score_file in score_files) {
  score_dt <- extract_score(score_file)
  src <- source_label(score_file)
  dt <- merge(ward_month, score_dt, by = "alderman_key", all.x = FALSE)
  dt <- dt[!is.na(score_value)]
  if (nrow(dt) < 50) next

  for (i in seq_len(nrow(outcome_specs))) {
    yvar <- outcome_specs$outcome_var[i]
    ylbl <- outcome_specs$outcome_label[i]
    d <- dt[!is.na(get(yvar)) & !is.na(score_value) & !is.na(n_permits_all)]
    if (nrow(d) < 50) next
    if (stats::sd(d[[yvar]], na.rm = TRUE) <= 0 || stats::sd(d$score_value, na.rm = TRUE) <= 0) next

    m_level <- feols(
      as.formula(paste0(yvar, " ~ score_value")),
      data = d,
      weights = ~n_permits_all,
      warn = FALSE
    )
    ctab_level <- coeftable(m_level)
    level_est <- ctab_level["score_value", "Estimate"]
    level_se <- ctab_level["score_value", "Std. Error"]
    level_p <- ctab_level["score_value", "Pr(>|t|)"]

    d_log <- d[get(yvar) > 0]
    if (nrow(d_log) >= 50 && stats::sd(log(d_log[[yvar]]), na.rm = TRUE) > 0) {
      m_log <- feols(
        as.formula(paste0("log(", yvar, ") ~ score_value")),
        data = d_log,
        weights = ~n_permits_all,
        warn = FALSE
      )
      ctab_log <- coeftable(m_log)
      log_est <- ctab_log["score_value", "Estimate"]
      log_se <- ctab_log["score_value", "Std. Error"]
      log_p <- ctab_log["score_value", "Pr(>|t|)"]
      n_log <- nobs(m_log)
    } else {
      log_est <- NA_real_
      log_se <- NA_real_
      log_p <- NA_real_
      n_log <- NA_integer_
    }

    rows[[idx]] <- data.table(
      score_source = src,
      outcome = ylbl,
      n_ward_month_cells = nrow(d),
      mean_outcome = mean(d[[yvar]], na.rm = TRUE),
      share_zero = mean(d[[yvar]] == 0, na.rm = TRUE),
      cor_unweighted = suppressWarnings(cor(d[[yvar]], d$score_value, use = "complete.obs")),
      cor_weighted = weighted_cor(d[[yvar]], d$score_value, d$n_permits_all),
      level_estimate = level_est,
      level_std_error = level_se,
      level_p_value = level_p,
      level_stars = star_code(level_p),
      log_estimate = log_est,
      log_std_error = log_se,
      log_p_value = log_p,
      log_stars = star_code(log_p),
      n_log_sample = n_log
    )
    idx <- idx + 1L
  }
}

out_dt <- rbindlist(rows, fill = TRUE)
write_csv(out_dt, output_file)

summary_dt <- out_dt[, .(
  n_outcomes = .N,
  n_negative_weighted_corr = sum(cor_weighted < 0, na.rm = TRUE),
  n_positive_weighted_corr = sum(cor_weighted > 0, na.rm = TRUE),
  n_negative_level = sum(level_estimate < 0 & level_p_value < 0.05, na.rm = TRUE),
  n_positive_level = sum(level_estimate > 0 & level_p_value < 0.05, na.rm = TRUE)
), by = score_source][order(-n_negative_level, n_positive_level)]
write_csv(summary_dt, summary_file)

cat("Wrote:\n")
cat(" -", output_file, "\n")
cat(" -", summary_file, "\n")
