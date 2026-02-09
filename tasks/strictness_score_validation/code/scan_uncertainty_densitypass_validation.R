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

star_code <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.1) return("*")
  ""
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

permit_file <- "../input/building_permits_text_features.csv.gz"
alderman_panel_file <- "../input/chicago_alderman_panel.csv"
ward_month_controls_file <- "../input/ward_monthly_panel_for_alderman_fe.csv"
spec_scan_file <- "../../create_alderman_uncertainty_index/output/spec_scan_pairxyear_bw500.csv"
uncertainty_dir <- "../../create_alderman_uncertainty_index/output"

metrics_output <- "../output/uncertainty_nocafe_densitypass_validation_metrics.csv"
scorecard_output <- "../output/uncertainty_nocafe_densitypass_validation_scorecard.csv"

cat("Loading permits and building ward-month outcomes...\n")
permits <- read_dt(permit_file)
setDT(permits)

num_cols <- c(
  "unit_change_signal", "unit_reduction_signal", "unit_increase_signal",
  "flag_deconversion", "units_delta_last", "unit_change_text",
  "stories_first_mention", "stories_last_mention", "from_to_pair_n"
)
for (col in intersect(num_cols, names(permits))) {
  permits[, (col) := to_numeric(get(col))]
}

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
    !is.na(application_start_date) &
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

permits[, story_reduction_signal := as.integer(
  !is.na(stories_first_mention) &
    !is.na(stories_last_mention) &
    stories_last_mention < stories_first_mention
)]
permits[, explicit_unit_reduction_signal := as.integer(
  !is.na(units_delta_last) & units_delta_last < 0
)]
permits[, units_reduced_text := fifelse(!is.na(unit_change_text) & unit_change_text < 0, -unit_change_text, 0)]
permits[, units_reduced_explicit := fifelse(!is.na(units_delta_last) & units_delta_last < 0, -units_delta_last, 0)]

build_ward_month <- function(dt) {
  out <- dt[, .(
    n_permits = .N,
    n_unit_reduction = sum(unit_reduction_signal == 1, na.rm = TRUE),
    share_unit_reduction = mean(unit_reduction_signal == 1, na.rm = TRUE),
    share_unit_change = mean(unit_change_signal == 1, na.rm = TRUE),
    share_deconversion = mean(flag_deconversion == 1, na.rm = TRUE),
    share_explicit_unit_reduction = mean(explicit_unit_reduction_signal == 1, na.rm = TRUE),
    share_story_reduction = mean(story_reduction_signal == 1, na.rm = TRUE),
    units_reduced_text_permit = sum(units_reduced_text, na.rm = TRUE) / .N,
    units_reduced_explicit_permit = sum(units_reduced_explicit, na.rm = TRUE) / .N,
    n_new_construction = sum(permit_type_clean == "PERMIT - NEW CONSTRUCTION", na.rm = TRUE)
  ), by = .(ward, month, alderman_key)]
  out[, units_reduced_text_given_reduction := fifelse(n_unit_reduction > 0, (units_reduced_text_permit * n_permits) / n_unit_reduction, NA_real_)]
  out
}

ward_month <- build_ward_month(permits)

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
ward_month[, n_permits_all := n_permits]
ward_month[, permits_per_10k_pop_all := fifelse(!is.na(pop_total) & pop_total > 0, n_permits_all / (pop_total / 10000), NA_real_)]
ward_month[, permits_per_10k_pop_new := fifelse(!is.na(pop_total) & pop_total > 0, n_new_construction / (pop_total / 10000), NA_real_)]
ward_month[, share_new_construction := fifelse(n_permits_all > 0, n_new_construction / n_permits_all, NA_real_)]

scan <- data.table::fread(spec_scan_file)
setDT(scan)
scan <- scan[all_neg_sig_05 == TRUE & grepl("cafeFALSE", spec)]
if (nrow(scan) == 0) {
  stop("No no-CAFE density-pass specs found in spec scan.")
}

outcome_specs <- data.table(
  family = c(rep("unit_change", 7), rep("permit_frequency", 5)),
  outcome = c(
    "share_unit_reduction",
    "units_reduced_text_given_reduction",
    "units_reduced_text_permit",
    "share_unit_change",
    "share_story_reduction",
    "share_explicit_unit_reduction",
    "units_reduced_explicit_permit",
    "n_permits_all",
    "n_new_construction",
    "permits_per_10k_pop_all",
    "permits_per_10k_pop_new",
    "share_new_construction"
  ),
  desired_sign = c(rep(1, 7), -1, -1, -1, -1, -1)
)

rows <- list()
idx <- 1L
for (i in seq_len(nrow(scan))) {
  spec <- scan$spec[i]
  score_file <- file.path(uncertainty_dir, paste0("alderman_uncertainty_index_", spec, ".csv"))
  if (!file.exists(score_file)) next

  score_dt <- data.table::fread(score_file)
  setDT(score_dt)
  if (!"uncertainty_index" %in% names(score_dt)) next
  score_dt[, alderman_key := name_key(alderman)]
  score_dt[, score := to_numeric(uncertainty_index)]
  score_dt <- score_dt[!is.na(score), .(score = mean(score, na.rm = TRUE)), by = alderman_key]

  dt_spec <- merge(ward_month, score_dt, by = "alderman_key", all.x = FALSE)
  dt_spec <- dt_spec[!is.na(score)]

  for (j in seq_len(nrow(outcome_specs))) {
    y <- outcome_specs$outcome[j]
    desired <- outcome_specs$desired_sign[j]
    fam <- outcome_specs$family[j]

    dt <- dt_spec[!is.na(get(y)) & !is.na(score) & !is.na(n_permits)]
    if (nrow(dt) < 50) next
    if (stats::sd(dt[[y]], na.rm = TRUE) <= 0) next
    if (stats::sd(dt$score, na.rm = TRUE) <= 0) next

    m <- feols(
      as.formula(paste0(y, " ~ score")),
      data = dt,
      weights = ~n_permits,
      warn = FALSE
    )
    ctab <- coeftable(m)
    est <- ctab["score", "Estimate"]
    se <- ctab["score", "Std. Error"]
    p <- ctab["score", "Pr(>|t|)"]

    rows[[idx]] <- data.table(
      spec = spec,
      family = fam,
      outcome = y,
      n = nobs(m),
      estimate = est,
      se = se,
      p = p,
      cor_weighted = weighted_cor(dt[[y]], dt$score, dt$n_permits),
      stars = star_code(p),
      desired_sign = desired,
      sign_match = as.integer(sign(est) == sign(desired)),
      sig_match_05 = as.integer(sign(est) == sign(desired) & p < 0.05)
    )
    idx <- idx + 1L
  }
}

metrics_dt <- rbindlist(rows, fill = TRUE)
if (nrow(metrics_dt) == 0) {
  stop("No validation metrics estimated.")
}

scorecard <- metrics_dt[, .(
  n_metrics = .N,
  n_sign_match = sum(sign_match, na.rm = TRUE),
  n_sig_match_05 = sum(sig_match_05, na.rm = TRUE),
  mean_abs_t = mean(abs(estimate / se), na.rm = TRUE)
), by = spec][order(-n_sig_match_05, -n_sign_match, -mean_abs_t)]

write_csv(metrics_dt, metrics_output)
write_csv(scorecard, scorecard_output)

cat("Wrote:\n")
cat(" -", metrics_output, "\n")
cat(" -", scorecard_output, "\n")
