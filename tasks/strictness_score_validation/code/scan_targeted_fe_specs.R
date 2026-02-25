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

target_names <- list(
  lenient = c("Walter Burnett, Jr.", "Bill Conway"),
  strict = c("Marty Quinn", "Anthony Napolitano", "Toni Preckwinkle")
)
target_keys <- lapply(target_names, name_key)

permit_file <- "../input/building_permits_text_features.csv.gz"
alderman_panel_file <- "../input/chicago_alderman_panel.csv"
uncertainty_dir <- "../input/uncertainty_scores"

detail_output <- "../output/targeted_fe_sweep_detail.csv"
scorecard_output <- "../output/targeted_fe_sweep_scorecard.csv"

cat("Loading permit-level data...\n")
permits <- read_dt(permit_file)
setDT(permits)

permits[, application_start_date := as.Date(application_start_date)]
bad_dates <- is.na(permits$application_start_date)
if (any(bad_dates)) {
  permits[bad_dates, application_start_date := as.Date(application_start_date, format = "%m/%d/%Y")]
}
permits[, month := as.yearmon(application_start_date)]
permits[, ward := ward_key(ward)]

num_cols <- c("unit_change_signal", "unit_change_text")
for (col in intersect(num_cols, names(permits))) {
  permits[, (col) := to_numeric(get(col))]
}

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

cat("Loading alderman panel...\n")
alderman_panel <- data.table::fread(alderman_panel_file)
setDT(alderman_panel)
alderman_panel[, ward := ward_key(ward)]
alderman_panel[, month_ym := zoo::as.yearmon(month, "%b %Y")]
alderman_panel[is.na(month_ym), month_ym := zoo::as.yearmon(month)]
alderman_panel <- unique(alderman_panel[!is.na(ward) & !is.na(month_ym) & !is.na(alderman), .(ward, month = month_ym, alderman)])

permits <- merge(permits, alderman_panel, by = c("ward", "month"), all.x = TRUE)
permits <- permits[!is.na(alderman)]
permits[, alderman_key := name_key(alderman)]

permits[, units_reduced_text := fifelse(!is.na(unit_change_text) & unit_change_text < 0, -unit_change_text, 0)]

ward_month <- permits[, .(
  n_permits = .N,
  n_permits_all = .N,
  share_unit_change = mean(unit_change_signal == 1, na.rm = TRUE),
  units_reduced_text_permit = sum(units_reduced_text, na.rm = TRUE) / .N
), by = .(ward, month, alderman_key)]

outcome_specs <- data.table(
  outcome = c("n_permits_all", "share_unit_change", "units_reduced_text_permit"),
  desired_sign = c(-1, 1, 1)
)

score_files <- sort(list.files(
  uncertainty_dir,
  pattern = "^alderman_uncertainty_index_.*_2stage_volLAG1_BOTH\\.csv$",
  full.names = TRUE
))

cat("Found", length(score_files), "target score files.\n")
if (length(score_files) == 0) {
  stop("No _volLAG1_BOTH uncertainty score files found.", call. = FALSE)
}

detail_rows <- list()
detail_idx <- 1L

for (score_file in score_files) {
  score_source <- basename(score_file)
  spec <- sub("\\.csv$", "", sub("^alderman_uncertainty_index_", "", score_source))

  score_dt <- data.table::fread(score_file)
  setDT(score_dt)
  if (!"uncertainty_index" %in% names(score_dt) || !"alderman" %in% names(score_dt)) {
    next
  }

  score_dt[, alderman_key := name_key(alderman)]
  score_dt[, score := to_numeric(uncertainty_index)]
  score_map <- score_dt[!is.na(score), .(score = mean(score, na.rm = TRUE)), by = alderman_key]

  named_scores <- sapply(c(target_names$lenient, target_names$strict), function(nm) {
    key <- name_key(nm)
    val <- score_map[alderman_key == key, score]
    if (length(val) == 0) NA_real_ else val[1]
  })

  lenient_vals <- named_scores[target_names$lenient]
  strict_vals <- named_scores[target_names$strict]

  pair_matrix <- outer(strict_vals, lenient_vals, `>`)
  pair_ok <- sum(pair_matrix, na.rm = TRUE)
  pair_total <- sum(!is.na(pair_matrix))
  target_pass <- !is.na(pair_total) && pair_total == 6 && pair_ok == 6

  target_gap <- NA_real_
  if (all(is.finite(strict_vals)) && all(is.finite(lenient_vals))) {
    target_gap <- min(strict_vals) - max(lenient_vals)
  }

  dt <- merge(ward_month, score_map[, .(alderman_key, score_value = score)], by = "alderman_key", all.x = FALSE)
  dt <- dt[!is.na(score_value)]

  for (i in seq_len(nrow(outcome_specs))) {
    y <- outcome_specs$outcome[i]
    desired <- outcome_specs$desired_sign[i]

    sub_dt <- dt[!is.na(get(y)) & !is.na(score_value) & !is.na(n_permits)]
    if (nrow(sub_dt) < 50 || stats::sd(sub_dt[[y]], na.rm = TRUE) <= 0 || stats::sd(sub_dt$score_value, na.rm = TRUE) <= 0) {
      detail_rows[[detail_idx]] <- data.table(
        score_file = score_source,
        spec = spec,
        outcome = y,
        desired_sign = desired,
        n = nrow(sub_dt),
        estimate = NA_real_,
        se = NA_real_,
        p = NA_real_,
        abs_t = NA_real_,
        cor_weighted = weighted_cor(sub_dt[[y]], sub_dt$score_value, sub_dt$n_permits),
        sign_match = NA_integer_,
        sig_match_05 = NA_integer_,
        pair_ok = pair_ok,
        pair_total = pair_total,
        target_pass = target_pass,
        target_gap = target_gap,
        burnett_score = named_scores["Walter Burnett, Jr."],
        conway_score = named_scores["Bill Conway"],
        quinn_score = named_scores["Marty Quinn"],
        napolitano_score = named_scores["Anthony Napolitano"],
        preckwinkle_score = named_scores["Toni Preckwinkle"]
      )
      detail_idx <- detail_idx + 1L
      next
    }

    fit <- tryCatch(
      feols(as.formula(paste0(y, " ~ score_value")), data = sub_dt, weights = ~n_permits, warn = FALSE),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      detail_rows[[detail_idx]] <- data.table(
        score_file = score_source,
        spec = spec,
        outcome = y,
        desired_sign = desired,
        n = nrow(sub_dt),
        estimate = NA_real_,
        se = NA_real_,
        p = NA_real_,
        abs_t = NA_real_,
        cor_weighted = weighted_cor(sub_dt[[y]], sub_dt$score_value, sub_dt$n_permits),
        sign_match = NA_integer_,
        sig_match_05 = NA_integer_,
        pair_ok = pair_ok,
        pair_total = pair_total,
        target_pass = target_pass,
        target_gap = target_gap,
        burnett_score = named_scores["Walter Burnett, Jr."],
        conway_score = named_scores["Bill Conway"],
        quinn_score = named_scores["Marty Quinn"],
        napolitano_score = named_scores["Anthony Napolitano"],
        preckwinkle_score = named_scores["Toni Preckwinkle"]
      )
      detail_idx <- detail_idx + 1L
      next
    }

    ctab <- coeftable(fit)
    est <- ctab["score_value", "Estimate"]
    se <- ctab["score_value", "Std. Error"]
    p <- ctab["score_value", "Pr(>|t|)"]
    abs_t <- abs(est / se)
    sign_match <- as.integer(sign(est) == sign(desired))
    sig_match_05 <- as.integer(sign(est) == sign(desired) & p < 0.05)

    detail_rows[[detail_idx]] <- data.table(
      score_file = score_source,
      spec = spec,
      outcome = y,
      desired_sign = desired,
      n = nobs(fit),
      estimate = est,
      se = se,
      p = p,
      abs_t = abs_t,
      cor_weighted = weighted_cor(sub_dt[[y]], sub_dt$score_value, sub_dt$n_permits),
      sign_match = sign_match,
      sig_match_05 = sig_match_05,
      pair_ok = pair_ok,
      pair_total = pair_total,
      target_pass = target_pass,
      target_gap = target_gap,
      burnett_score = named_scores["Walter Burnett, Jr."],
      conway_score = named_scores["Bill Conway"],
      quinn_score = named_scores["Marty Quinn"],
      napolitano_score = named_scores["Anthony Napolitano"],
      preckwinkle_score = named_scores["Toni Preckwinkle"]
    )
    detail_idx <- detail_idx + 1L
  }
}

detail_dt <- rbindlist(detail_rows, fill = TRUE)
if (nrow(detail_dt) == 0) {
  stop("No rows generated for targeted FE sweep.", call. = FALSE)
}

scorecard <- detail_dt[, .(
  n_metrics_available = sum(!is.na(estimate)),
  n_sign_match = sum(sign_match, na.rm = TRUE),
  n_sig_match_05 = sum(sig_match_05, na.rm = TRUE),
  mean_abs_t = mean(abs_t, na.rm = TRUE),
  pair_ok = first(pair_ok),
  pair_total = first(pair_total),
  target_pass = first(target_pass),
  target_gap = first(target_gap),
  burnett_score = first(burnett_score),
  conway_score = first(conway_score),
  quinn_score = first(quinn_score),
  napolitano_score = first(napolitano_score),
  preckwinkle_score = first(preckwinkle_score)
), by = .(score_file, spec)]

scorecard[, rank_pass := fifelse(target_pass, 1L, 0L)]
scorecard[, rank_n_sig := fifelse(is.na(n_sig_match_05), -Inf, n_sig_match_05)]
scorecard[, rank_n_sign := fifelse(is.na(n_sign_match), -Inf, n_sign_match)]
scorecard[, rank_gap := fifelse(is.na(target_gap), -Inf, target_gap)]
scorecard[, rank_abs_t := fifelse(is.na(mean_abs_t), -Inf, mean_abs_t)]
scorecard[, rank_burnett := fifelse(is.na(burnett_score), Inf, burnett_score)]

setorder(
  scorecard,
  -rank_pass,
  -rank_n_sig,
  -rank_n_sign,
  -rank_gap,
  -rank_abs_t,
  rank_burnett,
  spec
)

scorecard[, rank := .I]

scorecard[, `:=`(
  rank_pass = NULL,
  rank_n_sig = NULL,
  rank_n_sign = NULL,
  rank_gap = NULL,
  rank_abs_t = NULL,
  rank_burnett = NULL
)]

write_csv(detail_dt, detail_output)
write_csv(scorecard, scorecard_output)

cat("Wrote:\n")
cat(" -", detail_output, "\n")
cat(" -", scorecard_output, "\n")
