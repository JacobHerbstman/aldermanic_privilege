source("../../setup_environment/code/packages.R")

library(data.table)
library(readr)
library(fixest)

parcels_path <- "../input/parcels_with_ward_distances.csv"
flags_path <- "../input/confounded_pair_era_flags.csv"
out_dir <- "../output/bayer_kulka_construction"

stopifnot(file.exists(parcels_path), file.exists(flags_path))

outcomes <- c("density_far", "density_dupac")
transforms <- c("level", "log")
sample_tags <- c("all", "pruned")
kernels <- c("uniform", "triangular")
bin_width_m <- 25

fe_map <- list(
  pair_only = "ward_pair",
  pair_year = "ward_pair + construction_year",
  zone_pair_year_additive = "zone_code + ward_pair + construction_year"
)

sanitize_token <- function(x) {
  x <- trimws(as.character(x))
  if (!nzchar(x)) {
    return("")
  }
  gsub("[^A-Za-z0-9_-]", "_", x)
}

parse_num_list <- function(x) {
  x <- trimws(as.character(x))
  if (!nzchar(x)) {
    return(numeric())
  }
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  vals <- suppressWarnings(as.numeric(trimws(parts)))
  vals[is.finite(vals) & vals > 0]
}

format_meter_label <- function(x) {
  if (abs(x - round(x)) < 1e-8) {
    return(sprintf("%dm", as.integer(round(x))))
  }
  paste0(gsub("\\.$", "", gsub("0+$", "", sprintf("%.1f", x))), "m")
}

bw_ft_input <- parse_num_list(Sys.getenv("BK_BANDWIDTHS_FT", ""))
bw_m_input <- parse_num_list(Sys.getenv("BK_BANDWIDTHS_M", ""))

if (length(bw_ft_input) > 0) {
  bw_ft_vals <- sort(unique(bw_ft_input), decreasing = TRUE)
  bandwidths <- data.table(
    bandwidth_ft = as.numeric(bw_ft_vals),
    bandwidth_m = as.numeric(bw_ft_vals) * 0.3048
  )
  bandwidths[, bandwidth_label := sprintf("%dft", as.integer(round(bandwidth_ft)))]
} else if (length(bw_m_input) > 0) {
  bw_m_vals <- sort(unique(bw_m_input), decreasing = TRUE)
  bandwidths <- data.table(
    bandwidth_ft = as.numeric(bw_m_vals) / 0.3048,
    bandwidth_m = as.numeric(bw_m_vals)
  )
  bandwidths[, bandwidth_label := vapply(bandwidth_m, format_meter_label, character(1))]
} else {
  bandwidths <- data.table(
    bandwidth_m = c(1000, 500, 300, 175)
  )
  bandwidths[, bandwidth_ft := bandwidth_m / 0.3048]
  bandwidths[, bandwidth_label := vapply(bandwidth_m, format_meter_label, character(1))]
}

run_tag <- sanitize_token(Sys.getenv("BK_RUN_TAG", ""))
run_suffix <- if (nzchar(run_tag)) paste0("_", run_tag) else ""
bins_dir <- file.path(out_dir, paste0("bins", run_suffix))
plots_dir <- file.path(out_dir, paste0("plots", run_suffix))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bins_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

expected_inference_rows <- length(outcomes) * length(transforms) * length(sample_tags) * length(fe_map) * nrow(bandwidths) * length(kernels)
expected_plot_rows <- length(outcomes) * length(transforms) * length(sample_tags) * length(fe_map) * nrow(bandwidths)

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

kernel_weights <- function(signed_distance_m, bw_m, kernel) {
  if (kernel == "uniform") {
    return(rep(1, length(signed_distance_m)))
  }
  if (kernel == "triangular") {
    return(pmax(0, 1 - abs(signed_distance_m) / bw_m))
  }
  stop(sprintf("Unsupported kernel: %s", kernel), call. = FALSE)
}

extract_side_term <- function(model) {
  out <- list(
    estimate = NA_real_,
    std_error = NA_real_,
    p_value = NA_real_,
    t_value = NA_real_,
    n_obs = NA_integer_
  )
  if (is.null(model)) {
    return(out)
  }
  ct <- tryCatch(coeftable(model), error = function(e) NULL)
  out$n_obs <- as.integer(model$nobs)
  if (is.null(ct) || !("side" %in% rownames(ct))) {
    return(out)
  }
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  t_col <- grep("t value", colnames(ct), value = TRUE)[1]
  out$estimate <- as.numeric(ct["side", "Estimate"])
  out$std_error <- as.numeric(ct["side", "Std. Error"])
  out$p_value <- if (!is.na(p_col)) as.numeric(ct["side", p_col]) else NA_real_
  out$t_value <- if (!is.na(t_col)) as.numeric(ct["side", t_col]) else NA_real_
  out
}

fit_inference <- function(d, fe_formula, bw_m, kernel_name) {
  d_loc <- copy(d[abs(signed_distance_m) <= bw_m])
  if (nrow(d_loc) == 0) {
    return(list(
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      t_value = NA_real_,
      n_obs = 0L,
      n_pairs = 0L
    ))
  }

  d_loc[, kernel_weight := kernel_weights(signed_distance_m, bw_m, kernel_name)]
  d_loc <- d_loc[is.finite(kernel_weight) & kernel_weight > 0]

  n_obs_input <- nrow(d_loc)
  n_pairs <- uniqueN(d_loc$ward_pair)
  if (n_obs_input < 25 || uniqueN(d_loc$side) < 2 || n_pairs < 2) {
    return(list(
      estimate = NA_real_,
      std_error = NA_real_,
      p_value = NA_real_,
      t_value = NA_real_,
      n_obs = as.integer(n_obs_input),
      n_pairs = as.integer(n_pairs)
    ))
  }

  fml <- as.formula(sprintf(
    "outcome_val ~ side + signed_distance_m + side:signed_distance_m | %s",
    fe_formula
  ))

  model <- tryCatch(
    feols(
      fml,
      data = d_loc,
      cluster = ~ward_pair,
      weights = ~kernel_weight
    ),
    error = function(e) NULL
  )

  est <- extract_side_term(model)
  n_obs_used <- if (is.finite(est$n_obs)) as.integer(est$n_obs) else as.integer(n_obs_input)
  list(
    estimate = est$estimate,
    std_error = est$std_error,
    p_value = est$p_value,
    t_value = est$t_value,
    n_obs = n_obs_used,
    n_pairs = as.integer(n_pairs)
  )
}

build_plot_bins <- function(d, fe_formula, bw_m, bin_width_m) {
  empty_bins <- data.table(
    bin_id = integer(),
    bin_center_m = numeric(),
    n = integer(),
    mean_y = numeric(),
    side = character()
  )

  d_loc <- copy(d[abs(signed_distance_m) <= bw_m])
  if (nrow(d_loc) == 0) {
    return(list(
      bins = empty_bins,
      n_bins_total = 0L,
      n_bins_left = 0L,
      n_bins_right = 0L,
      cutoff_bin_present = FALSE,
      rmse_piecewise_linear_bins = NA_real_,
      cutoff_gap_bin = NA_real_,
      n_obs_plot = 0L,
      n_pairs_plot = 0L
    ))
  }

  fe_resid_model <- tryCatch(
    feols(as.formula(sprintf("outcome_val ~ 1 | %s", fe_formula)), data = d_loc),
    error = function(e) NULL
  )

  if (!is.null(fe_resid_model)) {
    removed <- fe_resid_model$obs_selection$obsRemoved
    if (is.null(removed)) {
      keep_idx <- seq_len(nrow(d_loc))
    } else {
      keep_idx <- setdiff(seq_len(nrow(d_loc)), abs(as.integer(removed)))
    }
    d_use <- d_loc[keep_idx]
    if (nrow(d_use) == nobs(fe_resid_model)) {
      d_use[, y_plot := as.numeric(resid(fe_resid_model)) + mean(outcome_val, na.rm = TRUE)]
    } else {
      d_use <- d_loc
      d_use[, y_plot := outcome_val]
    }
  } else {
    d_use <- d_loc
    d_use[, y_plot := outcome_val]
  }

  d_use[, bin_id := floor(signed_distance_m / bin_width_m)]
  bins <- d_use[, .(
    n = .N,
    mean_y = mean(y_plot, na.rm = TRUE)
  ), by = .(bin_id)]

  bins <- bins[n >= 5]
  bins[, bin_center_m := (bin_id + 0.5) * bin_width_m]
  bins[, side := fifelse(bin_center_m < 0, "lenient", "strict")]
  setorder(bins, bin_id)

  n_bins_total <- nrow(bins)
  n_bins_left <- bins[bin_center_m < 0, .N]
  n_bins_right <- bins[bin_center_m >= 0, .N]
  cutoff_bin_present <- any(bins$bin_id == -1L) && any(bins$bin_id == 0L)

  cutoff_gap_bin <- NA_real_
  if (cutoff_bin_present) {
    left_val <- bins[bin_id == -1L, mean_y][1]
    right_val <- bins[bin_id == 0L, mean_y][1]
    if (is.finite(left_val) && is.finite(right_val)) {
      cutoff_gap_bin <- right_val - left_val
    }
  }

  rmse_piecewise_linear_bins <- NA_real_
  if (n_bins_left >= 2 && n_bins_right >= 2) {
    left <- bins[bin_center_m < 0]
    right <- bins[bin_center_m >= 0]
    fit_left <- tryCatch(
      lm(mean_y ~ bin_center_m, data = left, weights = n),
      error = function(e) NULL
    )
    fit_right <- tryCatch(
      lm(mean_y ~ bin_center_m, data = right, weights = n),
      error = function(e) NULL
    )
    if (!is.null(fit_left) && !is.null(fit_right)) {
      left_res <- left$mean_y - as.numeric(predict(fit_left, newdata = left))
      right_res <- right$mean_y - as.numeric(predict(fit_right, newdata = right))
      all_res <- c(left_res, right_res)
      if (length(all_res) > 0 && any(is.finite(all_res))) {
        rmse_piecewise_linear_bins <- sqrt(mean(all_res^2, na.rm = TRUE))
      }
    }
  }

  list(
    bins = bins,
    n_bins_total = as.integer(n_bins_total),
    n_bins_left = as.integer(n_bins_left),
    n_bins_right = as.integer(n_bins_right),
    cutoff_bin_present = isTRUE(cutoff_bin_present),
    rmse_piecewise_linear_bins = rmse_piecewise_linear_bins,
    cutoff_gap_bin = cutoff_gap_bin,
    n_obs_plot = as.integer(nrow(d_use)),
    n_pairs_plot = as.integer(uniqueN(d_use$ward_pair))
  )
}

bandwidth_file_tag <- function(bw_label) {
  gsub("[^A-Za-z0-9]", "", as.character(bw_label))
}

make_spec_id <- function(sample_tag, outcome, transform, bw_label, fe_spec) {
  sprintf(
    "%s__%s__%s__bw%s__%s",
    sample_tag, outcome, transform, bandwidth_file_tag(bw_label), fe_spec
  )
}

message("Loading construction border data...")
dat <- as.data.table(read_csv(parcels_path, show_col_types = FALSE))

required_cols <- c(
  "ward_pair", "construction_year", "zone_code", "signed_distance",
  "arealotsf", "areabuilding", "unitscount", "density_far", "density_dupac"
)
missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols) > 0) {
  stop(sprintf("Missing required columns: %s", paste(missing_cols, collapse = ", ")), call. = FALSE)
}

dat[, construction_year := as.integer(construction_year)]
dat[, ward_pair := as.character(ward_pair)]
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
dat[, side := as.integer(signed_distance_m > 0)]

flags <- as.data.table(read_csv(
  flags_path,
  show_col_types = FALSE,
  col_select = c("ward_pair_id_dash", "era", "drop_confound")
))
flags[, pair_dash := normalize_pair_dash(ward_pair_id_dash)]
flags[, era := as.character(era)]
flags[, drop_confound := as.logical(drop_confound)]
flags <- unique(flags[!is.na(pair_dash) & !is.na(era), .(pair_dash, era, drop_confound)])

dat <- merge(
  dat,
  flags,
  by = c("pair_dash", "era"),
  all.x = TRUE,
  sort = FALSE
)
dat[is.na(drop_confound), drop_confound := FALSE]

data_cache <- new.env(parent = emptyenv())
get_transformed_data <- function(sample_tag, outcome, transform) {
  key <- paste(sample_tag, outcome, transform, sep = "__")
  if (exists(key, envir = data_cache, inherits = FALSE)) {
    return(get(key, envir = data_cache, inherits = FALSE))
  }

  d <- if (sample_tag == "pruned") copy(dat[drop_confound == FALSE]) else copy(dat)
  if (transform == "log") {
    d <- d[is.finite(get(outcome)) & get(outcome) > 0]
    d[, outcome_val := log(get(outcome))]
  } else {
    d <- d[is.finite(get(outcome))]
    d[, outcome_val := as.numeric(get(outcome))]
  }
  d <- d[is.finite(outcome_val)]
  assign(key, d, envir = data_cache)
  d
}

message("Running inference grid...")
inference_rows <- vector("list", expected_inference_rows)
row_idx <- 1L

for (sample_tag in sample_tags) {
  for (outcome in outcomes) {
    for (transform in transforms) {
      d_base <- get_transformed_data(sample_tag, outcome, transform)
      for (bw_i in seq_len(nrow(bandwidths))) {
        bw_m <- bandwidths$bandwidth_m[bw_i]
        bw_ft <- bandwidths$bandwidth_ft[bw_i]
        bw_label <- bandwidths$bandwidth_label[bw_i]
        for (fe_spec in names(fe_map)) {
          for (kernel_name in kernels) {
            est <- fit_inference(d_base, fe_map[[fe_spec]], bw_m, kernel_name)
            spec_id <- make_spec_id(sample_tag, outcome, transform, bw_label, fe_spec)
            inference_rows[[row_idx]] <- data.table(
              spec_id = spec_id,
              sample_tag = sample_tag,
              outcome = outcome,
              transform = transform,
              bandwidth_m = as.numeric(bw_m),
              bandwidth_ft = as.numeric(bw_ft),
              bandwidth_label = as.character(bw_label),
              fe_spec = fe_spec,
              kernel = kernel_name,
              estimate = est$estimate,
              std_error = est$std_error,
              p_value = est$p_value,
              t_value = est$t_value,
              n_obs = as.integer(est$n_obs),
              n_pairs = as.integer(est$n_pairs)
            )
            row_idx <- row_idx + 1L
          }
        }
      }
    }
  }
}

inference_dt <- rbindlist(inference_rows, fill = TRUE)
setorder(inference_dt, sample_tag, outcome, transform, bandwidth_m, fe_spec, kernel)

if (nrow(inference_dt) != expected_inference_rows) {
  stop(sprintf("Inference grid incomplete: got %d, expected %d.", nrow(inference_dt), expected_inference_rows), call. = FALSE)
}

kernel_choice_dt <- copy(inference_dt)
kernel_choice_dt[, p_rank := fifelse(is.na(p_value), Inf, p_value)]
kernel_choice_dt[, kernel_rank := fifelse(kernel == "uniform", 0L, 1L)]
setorder(kernel_choice_dt, sample_tag, outcome, transform, bandwidth_m, fe_spec, p_rank, kernel_rank)

plot_specs <- kernel_choice_dt[, .SD[1], by = .(
  spec_id, sample_tag, outcome, transform, bandwidth_m, bandwidth_ft, bandwidth_label, fe_spec
)]
setorder(plot_specs, sample_tag, outcome, transform, bandwidth_m, fe_spec)

message("Computing plot diagnostics and writing bins...")
plot_rows <- vector("list", nrow(plot_specs))
for (i in seq_len(nrow(plot_specs))) {
  rr <- plot_specs[i]
  d_base <- get_transformed_data(rr$sample_tag, rr$outcome, rr$transform)
  plot_fit <- build_plot_bins(d_base, fe_map[[rr$fe_spec]], rr$bandwidth_m, bin_width_m)

  bins_path <- file.path(
    bins_dir,
    sprintf(
      "gm_bayer_kulka_bins_%s_%s_%s_bw%s_%s_%s.csv",
      rr$sample_tag, rr$outcome, rr$transform, bandwidth_file_tag(rr$bandwidth_label), rr$fe_spec, rr$kernel
    )
  )

  bins_write <- copy(plot_fit$bins)
  bins_write[, `:=`(
    spec_id = rr$spec_id,
    sample_tag = rr$sample_tag,
    outcome = rr$outcome,
    transform = rr$transform,
    bandwidth_m = as.numeric(rr$bandwidth_m),
    bandwidth_ft = as.numeric(rr$bandwidth_ft),
    bandwidth_label = as.character(rr$bandwidth_label),
    fe_spec = rr$fe_spec,
    kernel_selected = rr$kernel
  )]
  setcolorder(bins_write, c(
    "spec_id", "sample_tag", "outcome", "transform",
    "bandwidth_m", "bandwidth_ft", "bandwidth_label", "fe_spec", "kernel_selected",
    "bin_id", "bin_center_m", "side", "n", "mean_y"
  ))
  fwrite(bins_write, bins_path)

  jump_over_rmse <- NA_real_
  if (is.finite(rr$estimate) && is.finite(plot_fit$rmse_piecewise_linear_bins) && plot_fit$rmse_piecewise_linear_bins > 0) {
    jump_over_rmse <- abs(rr$estimate) / plot_fit$rmse_piecewise_linear_bins
  }

  usable_visual <- isTRUE(plot_fit$cutoff_bin_present) &&
    plot_fit$n_bins_left >= 10L &&
    plot_fit$n_bins_right >= 10L

  plot_rows[[i]] <- data.table(
    spec_id = rr$spec_id,
    sample_tag = rr$sample_tag,
    outcome = rr$outcome,
    transform = rr$transform,
    bandwidth_m = as.numeric(rr$bandwidth_m),
    bandwidth_ft = as.numeric(rr$bandwidth_ft),
    bandwidth_label = as.character(rr$bandwidth_label),
    fe_spec = rr$fe_spec,
    kernel_selected = rr$kernel,
    estimate = rr$estimate,
    std_error = rr$std_error,
    p_value = rr$p_value,
    t_value = rr$t_value,
    n_obs = as.integer(rr$n_obs),
    n_pairs = as.integer(rr$n_pairs),
    n_obs_plot = as.integer(plot_fit$n_obs_plot),
    n_pairs_plot = as.integer(plot_fit$n_pairs_plot),
    n_bins_total = as.integer(plot_fit$n_bins_total),
    n_bins_left = as.integer(plot_fit$n_bins_left),
    n_bins_right = as.integer(plot_fit$n_bins_right),
    cutoff_bin_present = isTRUE(plot_fit$cutoff_bin_present),
    rmse_piecewise_linear_bins = plot_fit$rmse_piecewise_linear_bins,
    cutoff_gap_bin = plot_fit$cutoff_gap_bin,
    jump_over_rmse = jump_over_rmse,
    usable_visual = isTRUE(usable_visual),
    bins_csv = bins_path
  )
}

plot_dt <- rbindlist(plot_rows, fill = TRUE)
setorder(plot_dt, sample_tag, outcome, transform, bandwidth_m, fe_spec)

if (nrow(plot_dt) != expected_plot_rows) {
  stop(sprintf("Plot-spec grid incomplete: got %d, expected %d.", nrow(plot_dt), expected_plot_rows), call. = FALSE)
}

inference_out <- file.path(out_dir, paste0("gm_bayer_kulka_construction_inference_detail", run_suffix, ".csv"))
plot_out <- file.path(out_dir, paste0("gm_bayer_kulka_construction_plot_detail", run_suffix, ".csv"))

fwrite(inference_dt, inference_out)
fwrite(plot_dt, plot_out)

message("Saved:")
message(sprintf("  - %s", inference_out))
message(sprintf("  - %s", plot_out))
