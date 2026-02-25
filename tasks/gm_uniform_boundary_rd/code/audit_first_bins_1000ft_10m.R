source("../../setup_environment/code/packages.R")

library(data.table)
library(readr)
library(fixest)

parcels_path <- "../input/parcels_with_ward_distances.csv"
flags_path <- "../input/confounded_pair_era_flags.csv"

out_dir <- "../output/bayer_kulka_construction/first_bin_audit_bw1000ft_bin10m"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

summary_csv <- file.path(out_dir, "first_bin_summary.csv")
summary_md <- file.path(out_dir, "first_bin_summary.md")

stopifnot(file.exists(parcels_path), file.exists(flags_path))

bw_m <- 304.8
bin_w_m <- 10
target_bins <- c(-1L, 0L)
outcomes <- c("density_far", "density_dupac")
fe_formula <- "zone_code + ward_pair + construction_year"

normalize_pair_dash <- function(x) {
  x <- as.character(x)
  x <- gsub("_", "-", x, fixed = TRUE)
  x <- trimws(x)
  out <- rep(NA_character_, length(x))
  ok <- grepl("^[0-9]+-[0-9]+$", x)
  if (!any(ok)) {
    return(out)
  }
  parts <- strsplit(x[ok], "-", fixed = TRUE)
  out[ok] <- vapply(parts, function(v) {
    a <- suppressWarnings(as.integer(v[1]))
    b <- suppressWarnings(as.integer(v[2]))
    if (!is.finite(a) || !is.finite(b)) {
      return(NA_character_)
    }
    paste(min(a, b), max(a, b), sep = "-")
  }, character(1))
  out
}

era_from_year <- function(y) {
  y <- as.integer(y)
  ifelse(
    y < 2003L, "1998_2002",
    ifelse(y < 2015L, "2003_2014", ifelse(y < 2023L, "2015_2023", "post_2023"))
  )
}

extract_extremes <- function(dt, n_each = 5L) {
  if (nrow(dt) == 0) {
    return(dt[0])
  }
  top <- dt[order(-y_plot)][1:min(n_each, .N)]
  bot <- dt[order(y_plot)][1:min(n_each, .N)]
  out <- unique(rbindlist(list(top, bot), fill = TRUE))
  setorder(out, side, -y_plot)
  out
}

message("Loading parcels...")
dat <- as.data.table(read_csv(parcels_path, show_col_types = FALSE))

required_cols <- c(
  "pin", "ward_pair", "construction_year", "zone_code", "signed_distance",
  "arealotsf", "areabuilding", "unitscount", "density_far", "density_dupac"
)
missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

dat[, pin := as.character(pin)]
dat[, ward_pair := as.character(ward_pair)]
dat[, construction_year := as.integer(construction_year)]
dat[, zone_code := as.character(zone_code)]
dat[, signed_distance_m := as.numeric(signed_distance) * 0.3048]

dat <- dat[
  arealotsf > 1 &
    areabuilding > 1 &
    construction_year >= 2006 &
    unitscount > 0 &
    is.finite(signed_distance_m) &
    !is.na(ward_pair) &
    !is.na(construction_year)
]

dat[, pair_dash := normalize_pair_dash(ward_pair)]
dat[, era := era_from_year(construction_year)]
dat <- dat[!is.na(pair_dash) & !is.na(era)]

message("Loading confound flags...")
flags <- as.data.table(read_csv(
  flags_path,
  show_col_types = FALSE,
  col_select = c("ward_pair_id_dash", "era", "drop_confound", "drop_reason")
))
flags[, pair_dash := normalize_pair_dash(ward_pair_id_dash)]
flags[, era := as.character(era)]
flags[, drop_confound := as.logical(drop_confound)]
flags <- unique(flags[!is.na(pair_dash) & !is.na(era), .(pair_dash, era, drop_confound, drop_reason)])

dat <- merge(
  dat,
  flags,
  by = c("pair_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
dat[is.na(drop_confound), `:=`(drop_confound = FALSE, drop_reason = "unmatched_default_keep")]
dat <- dat[drop_confound == FALSE]

message("Auditing first bins...")
summary_rows <- list()

for (outcome in outcomes) {
  d_loc <- copy(dat[
    abs(signed_distance_m) <= bw_m &
      is.finite(get(outcome)) &
      get(outcome) > 0
  ])
  d_loc[, outcome_val := log(get(outcome))]

  model <- tryCatch(
    feols(as.formula(sprintf("outcome_val ~ 1 | %s", fe_formula)), data = d_loc),
    error = function(e) NULL
  )
  if (is.null(model)) {
    stop(sprintf("Failed FE residual model for %s", outcome), call. = FALSE)
  }

  removed <- model$obs_selection$obsRemoved
  if (is.null(removed)) {
    keep_idx <- seq_len(nrow(d_loc))
  } else {
    keep_idx <- setdiff(seq_len(nrow(d_loc)), abs(as.integer(removed)))
  }
  d_use <- d_loc[keep_idx]
  if (nrow(d_use) == nobs(model)) {
    d_use[, y_plot := as.numeric(resid(model)) + mean(outcome_val, na.rm = TRUE)]
  } else {
    d_use <- d_loc
    d_use[, y_plot := outcome_val]
  }

  d_use[, bin_id := floor(signed_distance_m / bin_w_m)]
  d_bin <- d_use[bin_id %in% target_bins]
  d_bin[, side := fifelse(bin_id < 0, "lenient_left_-10_to_0", "strict_right_0_to_10")]
  setorder(d_bin, side, -y_plot)

  obs_out <- file.path(out_dir, sprintf("first_bin_obs_%s.csv", outcome))
  obs_cols <- c(
    "outcome", "side", "bin_id", "pin", "ward_pair", "pair_dash", "era",
    "zone_code", "construction_year", "signed_distance_m",
    outcome, "outcome_val", "y_plot", "drop_reason"
  )
  d_export <- copy(d_bin)
  d_export[, outcome := outcome]
  setcolorder(d_export, obs_cols)
  fwrite(d_export, obs_out)

  side_stats <- d_bin[, .(
    n_obs = .N,
    mean_y_plot = mean(y_plot),
    median_y_plot = median(y_plot),
    sd_y_plot = sd(y_plot),
    min_y_plot = min(y_plot),
    max_y_plot = max(y_plot),
    mean_log_outcome = mean(outcome_val),
    median_log_outcome = median(outcome_val),
    mean_raw_outcome = mean(get(outcome)),
    median_raw_outcome = median(get(outcome))
  ), by = .(side, bin_id)]

  pair_comp <- d_bin[, .(
    n_obs = .N,
    mean_y_plot = mean(y_plot),
    median_y_plot = median(y_plot)
  ), by = .(side, ward_pair)][order(side, -n_obs, -abs(mean_y_plot))]
  pair_comp[, share_side := n_obs / sum(n_obs), by = side]

  year_comp <- d_bin[, .(
    n_obs = .N,
    mean_y_plot = mean(y_plot)
  ), by = .(side, construction_year)][order(side, -n_obs, construction_year)]

  zone_comp <- d_bin[, .(
    n_obs = .N,
    mean_y_plot = mean(y_plot)
  ), by = .(side, zone_code)][order(side, -n_obs, zone_code)]

  pair_out <- file.path(out_dir, sprintf("first_bin_pair_composition_%s.csv", outcome))
  year_out <- file.path(out_dir, sprintf("first_bin_year_composition_%s.csv", outcome))
  zone_out <- file.path(out_dir, sprintf("first_bin_zone_composition_%s.csv", outcome))
  extremes_out <- file.path(out_dir, sprintf("first_bin_extremes_%s.csv", outcome))

  fwrite(pair_comp, pair_out)
  fwrite(year_comp, year_out)
  fwrite(zone_comp, zone_out)
  fwrite(extract_extremes(d_bin), extremes_out)

  for (i in seq_len(nrow(side_stats))) {
    rr <- side_stats[i]
    summary_rows[[length(summary_rows) + 1L]] <- data.table(
      outcome = outcome,
      side = rr$side,
      bin_id = rr$bin_id,
      n_obs = rr$n_obs,
      mean_y_plot = rr$mean_y_plot,
      median_y_plot = rr$median_y_plot,
      sd_y_plot = rr$sd_y_plot,
      min_y_plot = rr$min_y_plot,
      max_y_plot = rr$max_y_plot,
      mean_log_outcome = rr$mean_log_outcome,
      median_log_outcome = rr$median_log_outcome,
      mean_raw_outcome = rr$mean_raw_outcome,
      median_raw_outcome = rr$median_raw_outcome,
      obs_csv = obs_out,
      pair_csv = pair_out,
      year_csv = year_out,
      zone_csv = zone_out,
      extremes_csv = extremes_out
    )
  }
}

summary_dt <- rbindlist(summary_rows, fill = TRUE)
setorder(summary_dt, outcome, bin_id)
fwrite(summary_dt, summary_csv)

md <- c(
  "# First-Bin Audit (1000ft, 10m, pruned, additive FE)",
  "",
  "- Fixed specification:",
  "  - sample: `pruned`",
  "  - FE residualization: `zone_code + ward_pair + construction_year`",
  "  - bandwidth: `1000ft` (304.8m)",
  "  - target bins: `[-10,0)` and `[0,10)`",
  "",
  "## Summary",
  ""
)

for (outcome in outcomes) {
  md <- c(md, paste0("### ", outcome))
  outcome_key <- outcome
  ss <- summary_dt[outcome == outcome_key]
  for (i in seq_len(nrow(ss))) {
    rr <- ss[i]
    md <- c(
      md,
      sprintf(
        "- %s (bin_id=%d): n=%d, mean_y_plot=%.6f, median_y_plot=%.6f, min=%.6f, max=%.6f",
        rr$side, rr$bin_id, rr$n_obs, rr$mean_y_plot, rr$median_y_plot, rr$min_y_plot, rr$max_y_plot
      ),
      sprintf("  - obs: `%s`", rr$obs_csv),
      sprintf("  - pair composition: `%s`", rr$pair_csv),
      sprintf("  - year composition: `%s`", rr$year_csv),
      sprintf("  - zone composition: `%s`", rr$zone_csv),
      sprintf("  - extremes: `%s`", rr$extremes_csv)
    )
  }
  md <- c(md, "")
}

writeLines(md, summary_md)

message("Saved:")
message(sprintf("  - %s", summary_csv))
message(sprintf("  - %s", summary_md))
message(sprintf("  - %s", out_dir))
