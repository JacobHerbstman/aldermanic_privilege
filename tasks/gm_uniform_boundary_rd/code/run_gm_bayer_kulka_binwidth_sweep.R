source("../../setup_environment/code/packages.R")
source("rd_label_utils.R")

library(data.table)
library(readr)
library(fixest)
library(ggplot2)
library(patchwork)

parcels_path <- "../input/parcels_with_ward_distances.csv"
flags_path <- "../input/confounded_pair_era_flags.csv"
ref_plot_path <- "../output/bayer_kulka_construction/gm_bayer_kulka_construction_plot_detail_ft500_1000.csv"

out_dir <- "../output/bayer_kulka_construction/binwidth_sweep_ft500_1000"
bins_dir <- file.path(out_dir, "bins")
plots_dir <- file.path(out_dir, "plots")

detail_out <- file.path(out_dir, "gm_bayer_kulka_binwidth_sweep_detail.csv")
summary_out <- file.path(out_dir, "gm_bayer_kulka_binwidth_sweep_summary.md")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(bins_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(parcels_path), file.exists(flags_path), file.exists(ref_plot_path))

outcomes <- c("density_far", "density_dupac")
outcome_label <- c(density_far = "FAR", density_dupac = "DUPAC")

bandwidths <- data.table(
  bandwidth_label = c("500ft", "1000ft"),
  bandwidth_m = c(152.4, 304.8)
)
bin_widths_m <- c(10L, 15L, 20L, 25L, 35L, 50L)
min_bin_n <- 5L

fixed_sample_tag <- "pruned"
fixed_transform_tag <- "log"
fixed_fe_spec <- "zone_pair_year_additive"
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

panel_y_label <- function(outcome) {
  if (outcome == "density_far") {
    "Log(FAR)"
  } else {
    "Log(DUPAC)"
  }
}

panel_title <- function(outcome, bw_label, bin_width_m, estimate, std_error, p_value) {
  base <- sprintf("%s | bw=%s | bin=%dm", outcome_label[[outcome]], bw_label, as.integer(bin_width_m))
  paste0(base, " | ", gm_jump_label(estimate, std_error, p_value))
}

build_panel <- function(bin_dt, outcome, bw_m, bw_label, bin_width_m, estimate, std_error, p_value) {
  ggplot(bin_dt, aes(x = bin_center_m, y = mean_y)) +
    geom_point(size = 1.8, alpha = 0.95, color = "#1f4e79") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.6) +
    coord_cartesian(xlim = c(-bw_m, bw_m)) +
    labs(
      title = panel_title(outcome, bw_label, bin_width_m, estimate, std_error, p_value),
      x = "Distance to border (meters)",
      y = panel_y_label(outcome)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 10.5)
    )
}

build_bins <- function(dt, outcome, bw_m, bin_width_m) {
  empty_bins <- data.table(
    bin_id = integer(),
    bin_center_m = numeric(),
    n = integer(),
    mean_y = numeric(),
    side = character()
  )

  d_loc <- copy(dt[
    abs(signed_distance_m) <= bw_m &
      is.finite(get(outcome)) &
      get(outcome) > 0
  ])

  if (nrow(d_loc) == 0) {
    return(list(
      bins = empty_bins,
      n_obs_plot = 0L,
      n_pairs_plot = 0L,
      n_bins_total = 0L,
      n_bins_left = 0L,
      n_bins_right = 0L
    ))
  }

  d_loc[, outcome_val := log(get(outcome))]

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

  bins <- bins[n >= min_bin_n]
  bins[, bin_center_m := (bin_id + 0.5) * bin_width_m]
  bins[, side := fifelse(bin_center_m < 0, "lenient", "strict")]
  setorder(bins, bin_id)

  list(
    bins = bins,
    n_obs_plot = as.integer(nrow(d_use)),
    n_pairs_plot = as.integer(uniqueN(d_use$ward_pair)),
    n_bins_total = as.integer(nrow(bins)),
    n_bins_left = as.integer(bins[bin_center_m < 0, .N]),
    n_bins_right = as.integer(bins[bin_center_m >= 0, .N])
  )
}

message("Loading reference winner-spec inference rows...")
ref_plot <- fread(ref_plot_path)
ref_plot <- ref_plot[
  sample_tag == fixed_sample_tag &
    transform == fixed_transform_tag &
    fe_spec == fixed_fe_spec &
    outcome %in% outcomes &
    bandwidth_label %in% bandwidths$bandwidth_label,
  .(outcome, bandwidth_label, estimate, std_error, p_value, t_value, kernel_selected)
]

if (nrow(ref_plot) != 4L) {
  stop(sprintf("Expected 4 reference rows (2 outcomes x 2 bandwidths), got %d.", nrow(ref_plot)), call. = FALSE)
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

message("Sweeping bin widths...")
detail_rows <- list()
row_idx <- 1L

for (bw_i in seq_len(nrow(bandwidths))) {
  bw_label <- bandwidths$bandwidth_label[bw_i]
  bw_m <- bandwidths$bandwidth_m[bw_i]

  for (bin_w in bin_widths_m) {
    for (outcome in outcomes) {
      b <- build_bins(dat, outcome, bw_m, bin_w)

      bins_out <- file.path(
        bins_dir,
        sprintf(
          "gm_bayer_kulka_binsweep_%s_bw%s_bin%dm.csv",
          outcome, bw_label, as.integer(bin_w)
        )
      )

      bins_write <- copy(b$bins)
      bins_write[, `:=`(
        sample_tag = fixed_sample_tag,
        transform = fixed_transform_tag,
        fe_spec = fixed_fe_spec,
        outcome = outcome,
        bandwidth_label = bw_label,
        bandwidth_m = bw_m,
        bin_width_m = as.integer(bin_w)
      )]
      setcolorder(bins_write, c(
        "sample_tag", "transform", "fe_spec", "outcome",
        "bandwidth_label", "bandwidth_m", "bin_width_m",
        "bin_id", "bin_center_m", "side", "n", "mean_y"
      ))
      fwrite(bins_write, bins_out)

      outcome_key <- outcome
      bw_key <- bw_label
      ref_row <- ref_plot[outcome == outcome_key & bandwidth_label == bw_key][1]

      detail_rows[[row_idx]] <- data.table(
        sample_tag = fixed_sample_tag,
        transform = fixed_transform_tag,
        fe_spec = fixed_fe_spec,
        outcome = outcome,
        bandwidth_label = bw_label,
        bandwidth_m = as.numeric(bw_m),
        bin_width_m = as.integer(bin_w),
        estimate = as.numeric(ref_row$estimate),
        std_error = as.numeric(ref_row$std_error),
        p_value = as.numeric(ref_row$p_value),
        t_value = as.numeric(ref_row$t_value),
        kernel_selected = as.character(ref_row$kernel_selected),
        n_obs_plot = b$n_obs_plot,
        n_pairs_plot = b$n_pairs_plot,
        n_bins_total = b$n_bins_total,
        n_bins_left = b$n_bins_left,
        n_bins_right = b$n_bins_right,
        bins_csv = bins_out,
        stacked_plot_pdf = NA_character_
      )
      row_idx <- row_idx + 1L
    }
  }
}

detail_dt <- rbindlist(detail_rows, fill = TRUE)
setorder(detail_dt, bandwidth_m, bin_width_m, outcome)

message("Rendering stacked plots...")
for (bw_i in seq_len(nrow(bandwidths))) {
  bw_label <- bandwidths$bandwidth_label[bw_i]
  bw_m <- bandwidths$bandwidth_m[bw_i]

  for (bin_w in bin_widths_m) {
    bw_key <- bw_label
    bin_key <- as.integer(bin_w)
    d_sub <- detail_dt[bandwidth_label == bw_key & bin_width_m == bin_key]
    if (nrow(d_sub) != 2L) {
      next
    }

    far_row <- d_sub[outcome == "density_far"][1]
    dupac_row <- d_sub[outcome == "density_dupac"][1]
    if (nrow(far_row) != 1L || nrow(dupac_row) != 1L) {
      next
    }

    far_bins <- fread(far_row$bins_csv)
    dupac_bins <- fread(dupac_row$bins_csv)
    if (nrow(far_bins) == 0 || nrow(dupac_bins) == 0) {
      next
    }

    p_far <- build_panel(far_bins, "density_far", bw_m, bw_label, bin_w, far_row$estimate, far_row$std_error, far_row$p_value)
    p_dupac <- build_panel(dupac_bins, "density_dupac", bw_m, bw_label, bin_w, dupac_row$estimate, dupac_row$std_error, dupac_row$p_value)

    stacked <- p_far / p_dupac + plot_annotation(
      title = sprintf("Pruned log additive FE (zone + pair + year), %s", bw_label)
    )

    out_pdf <- file.path(
      plots_dir,
      sprintf("gm_bayer_kulka_binsweep_bw%s_bin%dm_stacked_far_dupac.pdf", bw_label, as.integer(bin_w))
    )
    ggsave(out_pdf, stacked, width = 8.6, height = 10.4, dpi = 300)

    detail_dt[
      bandwidth_label == bw_key & bin_width_m == bin_key,
      stacked_plot_pdf := out_pdf
    ]
  }
}

message("Rendering side-by-side bin width comparison plots...")
for (bw_i in seq_len(nrow(bandwidths))) {
  bw_label <- bandwidths$bandwidth_label[bw_i]
  bw_m <- bandwidths$bandwidth_m[bw_i]

  far_panels <- vector("list", length(bin_widths_m))
  dupac_panels <- vector("list", length(bin_widths_m))

  for (j in seq_along(bin_widths_m)) {
    bin_w <- bin_widths_m[j]
    bw_key <- bw_label
    bin_key <- as.integer(bin_w)

    far_row <- detail_dt[
      bandwidth_label == bw_key &
        bin_width_m == bin_key &
        outcome == "density_far"
    ][1]
    dupac_row <- detail_dt[
      bandwidth_label == bw_key &
        bin_width_m == bin_key &
        outcome == "density_dupac"
    ][1]

    if (nrow(far_row) == 1L && file.exists(far_row$bins_csv)) {
      far_bins <- fread(far_row$bins_csv)
      far_panels[[j]] <- build_panel(far_bins, "density_far", bw_m, bw_label, bin_w, far_row$estimate, far_row$std_error, far_row$p_value)
    } else {
      far_panels[[j]] <- ggplot() + theme_void()
    }

    if (nrow(dupac_row) == 1L && file.exists(dupac_row$bins_csv)) {
      dupac_bins <- fread(dupac_row$bins_csv)
      dupac_panels[[j]] <- build_panel(dupac_bins, "density_dupac", bw_m, bw_label, bin_w, dupac_row$estimate, dupac_row$std_error, dupac_row$p_value)
    } else {
      dupac_panels[[j]] <- ggplot() + theme_void()
    }
  }

  comp_plot <- wrap_plots(far_panels, nrow = 1) / wrap_plots(dupac_panels, nrow = 1) +
    plot_annotation(
      title = sprintf("Bin width sensitivity | pruned log additive FE | %s", bw_label)
    )

  comp_out <- file.path(
    plots_dir,
    sprintf("gm_bayer_kulka_binsweep_comparison_bw%s.pdf", bw_label)
  )
  ggsave(comp_out, comp_plot, width = 20, height = 8.5, dpi = 300)
}

fwrite(detail_dt, detail_out)

summary_lines <- c(
  "# Bin Width Sweep Summary",
  "",
  "- Fixed specification:",
  paste0("  - sample: `", fixed_sample_tag, "`"),
  paste0("  - transform: `", fixed_transform_tag, "`"),
  paste0("  - FE: `", fixed_fe_spec, "`"),
  "  - outcomes: `density_far`, `density_dupac`",
  "  - bandwidths: `500ft`, `1000ft`",
  paste0("  - bin widths (meters): ", paste(bin_widths_m, collapse = ", ")),
  paste0("  - min bin mass: n >= ", min_bin_n),
  "",
  paste0("- Detail CSV: `", detail_out, "`"),
  paste0("- Plot directory: `", plots_dir, "`"),
  "",
  "## Plot Counts",
  paste0("- Stacked two-panel PDFs: ", uniqueN(detail_dt[!is.na(stacked_plot_pdf), stacked_plot_pdf])),
  paste0("- Comparison PDFs: ", nrow(bandwidths)),
  ""
)

for (bw_i in seq_len(nrow(bandwidths))) {
  bw_label <- bandwidths$bandwidth_label[bw_i]
  summary_lines <- c(summary_lines, paste0("## ", bw_label))
  bw_key <- bw_label
  d_bw <- detail_dt[bandwidth_label == bw_key]
  for (j in seq_along(bin_widths_m)) {
    bin_w <- bin_widths_m[j]
    bin_key <- as.integer(bin_w)
    rr_far <- d_bw[bin_width_m == bin_key & outcome == "density_far"][1]
    rr_dup <- d_bw[bin_width_m == bin_key & outcome == "density_dupac"][1]
    summary_lines <- c(
      summary_lines,
      sprintf(
        "- bin=%dm: FAR bins(L/R)=%d/%d, DUPAC bins(L/R)=%d/%d | `%s`",
        as.integer(bin_w),
        rr_far$n_bins_left, rr_far$n_bins_right,
        rr_dup$n_bins_left, rr_dup$n_bins_right,
        rr_far$stacked_plot_pdf
      )
    )
  }
  summary_lines <- c(
    summary_lines,
    sprintf(
      "- comparison: `%s`",
      file.path(plots_dir, sprintf("gm_bayer_kulka_binsweep_comparison_bw%s.pdf", bw_label))
    ),
    ""
  )
}

writeLines(summary_lines, summary_out)

message("Saved:")
message(sprintf("  - %s", detail_out))
message(sprintf("  - %s", summary_out))
message(sprintf("  - %s", plots_dir))
