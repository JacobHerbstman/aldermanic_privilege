source("../../setup_environment/code/packages.R")
source("rd_label_utils.R")

library(data.table)
library(readr)
library(fixest)
library(ggplot2)
library(patchwork)

parcels_path <- "../input/parcels_with_ward_distances.csv"
flags_path <- "../input/confounded_pair_era_flags.csv"

out_dir <- "../output/bayer_kulka_construction/donut_robustness_1000ft_pruned_log_zone_pair_year_additive"
bins_dir <- file.path(out_dir, "bins")
plots_dir <- file.path(out_dir, "plots")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bins_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(parcels_path), file.exists(flags_path))

outcomes <- c("density_far", "density_dupac")
outcome_label <- c(density_far = "FAR", density_dupac = "DUPAC")

bandwidth_ft <- 1000
bandwidth_m <- 304.8
bin_width_m <- 10
donuts_m <- c(0L, 10L, 20L)

sample_tag <- "pruned"
transform_tag <- "log"
fe_spec <- "zone_pair_year_additive"
fe_formula <- "zone_code + ward_pair + construction_year"
kernel <- "triangular"

detail_csv <- file.path(out_dir, "gm_bayer_kulka_donut_robustness_detail.csv")
summary_md <- file.path(out_dir, "gm_bayer_kulka_donut_robustness_summary.md")
comparison_pdf <- file.path(plots_dir, "gm_bayer_kulka_donut_robustness_stacked_comparison.pdf")

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

kernel_weight_triangular <- function(signed_distance_m, bw_m) {
  pmax(0, 1 - abs(signed_distance_m) / bw_m)
}

extract_side <- function(model) {
  out <- list(
    estimate = NA_real_,
    std_error = NA_real_,
    p_value = NA_real_,
    t_value = NA_real_,
    n_obs_model = NA_integer_
  )
  if (is.null(model)) {
    return(out)
  }
  ct <- tryCatch(coeftable(model), error = function(e) NULL)
  if (is.null(ct) || !("side" %in% rownames(ct))) {
    out$n_obs_model <- as.integer(model$nobs)
    return(out)
  }
  p_col <- grep("^Pr\\(", colnames(ct), value = TRUE)[1]
  t_col <- grep("t value", colnames(ct), value = TRUE)[1]
  out$estimate <- as.numeric(ct["side", "Estimate"])
  out$std_error <- as.numeric(ct["side", "Std. Error"])
  out$p_value <- if (!is.na(p_col)) as.numeric(ct["side", p_col]) else NA_real_
  out$t_value <- if (!is.na(t_col)) as.numeric(ct["side", t_col]) else NA_real_
  out$n_obs_model <- as.integer(model$nobs)
  out
}

build_bins <- function(d, bin_width_m, fe_formula) {
  empty_bins <- data.table(
    bin_id = integer(),
    bin_center_m = numeric(),
    n = integer(),
    mean_y = numeric()
  )
  if (nrow(d) == 0) {
    return(list(bins = empty_bins, n_bins_left = 0L, n_bins_right = 0L))
  }

  fe_resid <- tryCatch(
    feols(as.formula(sprintf("outcome_val ~ 1 | %s", fe_formula)), data = d),
    error = function(e) NULL
  )

  if (!is.null(fe_resid)) {
    removed <- fe_resid$obs_selection$obsRemoved
    if (is.null(removed)) {
      keep_idx <- seq_len(nrow(d))
    } else {
      keep_idx <- setdiff(seq_len(nrow(d)), abs(as.integer(removed)))
    }
    d_use <- d[keep_idx]
    if (nrow(d_use) == nobs(fe_resid)) {
      d_use[, y_plot := as.numeric(resid(fe_resid)) + mean(outcome_val, na.rm = TRUE)]
    } else {
      d_use <- copy(d)
      d_use[, y_plot := outcome_val]
    }
  } else {
    d_use <- copy(d)
    d_use[, y_plot := outcome_val]
  }

  d_use[, bin_id := floor(signed_distance_m / bin_width_m)]
  bins <- d_use[, .(
    n = .N,
    mean_y = mean(y_plot, na.rm = TRUE)
  ), by = .(bin_id)]
  bins <- bins[n >= 5]
  bins[, bin_center_m := (bin_id + 0.5) * bin_width_m]
  setorder(bins, bin_id)

  list(
    bins = bins,
    n_bins_left = as.integer(bins[bin_center_m < 0, .N]),
    n_bins_right = as.integer(bins[bin_center_m >= 0, .N])
  )
}

plot_bins <- function(bins, outcome, donut_m, estimate, std_error, p_value) {
  ggplot(bins, aes(x = bin_center_m, y = mean_y)) +
    geom_point(size = 1.8, alpha = 0.95, color = "#1f4e79") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.6) +
    coord_cartesian(xlim = c(-bandwidth_m, bandwidth_m)) +
    labs(
      title = sprintf(
        "%s | bw=%dft | donut=%dm | %s",
        outcome_label[[outcome]],
        as.integer(bandwidth_ft),
        as.integer(donut_m),
        gm_jump_label(estimate, std_error, p_value)
      ),
      x = "Distance to border (meters)",
      y = paste0("Log(", outcome_label[[outcome]], ")")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 10.5)
    )
}

message("Loading and filtering data...")
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
dat <- dat[drop_confound == FALSE]

message("Estimating donut robustness...")
rows <- list()
plots <- list()

for (outcome in outcomes) {
  d_base <- dat[
    abs(signed_distance_m) <= bandwidth_m &
      is.finite(get(outcome)) &
      get(outcome) > 0
  ]
  d_base <- copy(d_base)
  d_base[, outcome_val := log(get(outcome))]

  for (donut_m in donuts_m) {
    d <- copy(d_base[abs(signed_distance_m) >= donut_m])
    d[, side := as.integer(signed_distance_m > 0)]
    d[, kernel_weight := kernel_weight_triangular(signed_distance_m, bandwidth_m)]
    d <- d[kernel_weight > 0]

    model <- tryCatch(
      feols(
        as.formula(sprintf("outcome_val ~ side + signed_distance_m + side:signed_distance_m | %s", fe_formula)),
        data = d,
        cluster = ~ward_pair,
        weights = ~kernel_weight
      ),
      error = function(e) NULL
    )

    est <- extract_side(model)

    bins_info <- build_bins(d, bin_width_m, fe_formula)
    bins <- bins_info$bins
    bins_out <- file.path(
      bins_dir,
      sprintf("gm_bayer_kulka_donut_bins_%s_donut%dm.csv", outcome, as.integer(donut_m))
    )
    bins_write <- copy(bins)
    bins_write[, `:=`(
      sample_tag = sample_tag,
      transform = transform_tag,
      fe_spec = fe_spec,
      kernel = kernel,
      bandwidth_ft = as.integer(bandwidth_ft),
      bandwidth_m = as.numeric(bandwidth_m),
      donut_m = as.integer(donut_m),
      outcome = outcome
    )]
    setcolorder(bins_write, c(
      "sample_tag", "transform", "fe_spec", "kernel",
      "bandwidth_ft", "bandwidth_m", "donut_m", "outcome",
      "bin_id", "bin_center_m", "n", "mean_y"
    ))
    fwrite(bins_write, bins_out)

    n_left <- d[signed_distance_m < 0, .N]
    n_right <- d[signed_distance_m > 0, .N]
    n_pairs <- uniqueN(d$ward_pair)

    rows[[length(rows) + 1L]] <- data.table(
      sample_tag = sample_tag,
      transform = transform_tag,
      fe_spec = fe_spec,
      kernel = kernel,
      bandwidth_ft = as.integer(bandwidth_ft),
      bandwidth_m = as.numeric(bandwidth_m),
      donut_m = as.integer(donut_m),
      outcome = outcome,
      estimate = est$estimate,
      std_error = est$std_error,
      p_value = est$p_value,
      t_value = est$t_value,
      n_obs_model = est$n_obs_model,
      n_obs_side_left = as.integer(n_left),
      n_obs_side_right = as.integer(n_right),
      n_pairs = as.integer(n_pairs),
      n_bins_left = bins_info$n_bins_left,
      n_bins_right = bins_info$n_bins_right,
      bins_csv = bins_out
    )

    plots[[paste(outcome, donut_m, sep = "__")]] <- plot_bins(bins, outcome, donut_m, est$estimate, est$std_error, est$p_value)
  }
}

res <- rbindlist(rows, fill = TRUE)
setorder(res, outcome, donut_m)

res[, baseline_estimate := estimate[donut_m == 0L][1], by = outcome]
res[, baseline_p_value := p_value[donut_m == 0L][1], by = outcome]
res[, diff_vs_baseline := estimate - baseline_estimate]
res[, pct_change_vs_baseline := fifelse(is.finite(baseline_estimate) & baseline_estimate != 0, 100 * diff_vs_baseline / abs(baseline_estimate), NA_real_)]

fwrite(res, detail_csv)

stack_rows <- list()
for (donut_m in donuts_m) {
  p_far <- plots[[paste("density_far", donut_m, sep = "__")]]
  p_dup <- plots[[paste("density_dupac", donut_m, sep = "__")]]
  stacked <- p_far / p_dup + plot_annotation(
    title = sprintf("Donut robustness | pruned log additive FE | bw=%dft | donut=%dm", as.integer(bandwidth_ft), as.integer(donut_m))
  )
  out_pdf <- file.path(
    plots_dir,
    sprintf("gm_bayer_kulka_donut_stacked_1000ft_donut%dm.pdf", as.integer(donut_m))
  )
  ggsave(out_pdf, stacked, width = 8.8, height = 10.4, dpi = 300)
  stack_rows[[length(stack_rows) + 1L]] <- stacked + plot_annotation(tag_levels = "A")
}

if (length(stack_rows) == 3L) {
  cmp <- wrap_plots(stack_rows, ncol = 3)
  ggsave(comparison_pdf, cmp, width = 24, height = 10.4, dpi = 300)
}

summary_lines <- c(
  "# Donut Robustness (1000ft)",
  "",
  "- Fixed specification:",
  paste0("  - sample: `", sample_tag, "`"),
  paste0("  - transform: `", transform_tag, "`"),
  paste0("  - FE: `", fe_spec, "`"),
  paste0("  - kernel: `", kernel, "`"),
  paste0("  - bandwidth: `", as.integer(bandwidth_ft), "ft`"),
  paste0("  - donut values (m): ", paste(donuts_m, collapse = ", ")),
  "",
  paste0("- detail csv: `", detail_csv, "`"),
  paste0("- comparison pdf: `", comparison_pdf, "`"),
  ""
)

for (outcome in outcomes) {
  summary_lines <- c(summary_lines, paste0("## ", outcome))
  outcome_key <- outcome
  rr <- res[outcome == outcome_key]
  for (i in seq_len(nrow(rr))) {
    r <- rr[i]
    summary_lines <- c(
      summary_lines,
      sprintf(
        "- donut=%dm: est=%.6f, se=%.6f, p %.4f, t=%.3f, n=%d, pairs=%d, bins(L/R)=%d/%d, diff_vs_0m=%.6f (%s%%)",
        as.integer(r$donut_m),
        r$estimate,
        r$std_error,
        r$p_value,
        r$t_value,
        as.integer(r$n_obs_model),
        as.integer(r$n_pairs),
        as.integer(r$n_bins_left),
        as.integer(r$n_bins_right),
        r$diff_vs_baseline,
        ifelse(is.finite(r$pct_change_vs_baseline), sprintf("%.2f", r$pct_change_vs_baseline), "NA")
      ),
      sprintf("  - bins: `%s`", r$bins_csv)
    )
  }
  summary_lines <- c(summary_lines, "")
}

writeLines(summary_lines, summary_md)

message("Saved:")
message(sprintf("  - %s", detail_csv))
message(sprintf("  - %s", summary_md))
message(sprintf("  - %s", plots_dir))
