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

out_dir <- "../output/bayer_kulka_construction/binwidth_sweep_ft500_1000_multispec"
bins_dir <- file.path(out_dir, "bins")
plots_dir <- file.path(out_dir, "plots")

detail_out <- file.path(out_dir, "gm_bayer_kulka_binwidth_sweep_multispec_detail.csv")
scorecard_out <- file.path(out_dir, "gm_bayer_kulka_binwidth_sweep_multispec_scorecard.csv")
summary_out <- file.path(out_dir, "gm_bayer_kulka_binwidth_sweep_multispec_summary.md")

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
fixed_transform <- "log"

sample_tags <- c("all", "pruned")
fe_map <- list(
  pair_only = "ward_pair",
  pair_year = "ward_pair + construction_year",
  zone_pair_year_additive = "zone_code + ward_pair + construction_year"
)

sanitize <- function(x) {
  x <- as.character(x)
  gsub("[^A-Za-z0-9_-]", "", x)
}

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

panel_title <- function(outcome, sample_tag, fe_spec, bw_label, bin_width_m, estimate, std_error, p_value) {
  base <- sprintf(
    "%s | %s | FE=%s | bw=%s | bin=%dm",
    outcome_label[[outcome]], sample_tag, fe_spec, bw_label, as.integer(bin_width_m)
  )
  paste0(base, " | ", gm_jump_label(estimate, std_error, p_value))
}

build_panel <- function(bin_dt, outcome, sample_tag, fe_spec, bw_m, bw_label, bin_width_m, estimate, std_error, p_value) {
  ggplot(bin_dt, aes(x = bin_center_m, y = mean_y)) +
    geom_point(size = 1.8, alpha = 0.95, color = "#1f4e79") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.6) +
    coord_cartesian(xlim = c(-bw_m, bw_m)) +
    labs(
      title = panel_title(outcome, sample_tag, fe_spec, bw_label, bin_width_m, estimate, std_error, p_value),
      x = "Distance to border (meters)",
      y = panel_y_label(outcome)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 10.25)
    )
}

build_bins <- function(dt, outcome, bw_m, bin_width_m, fe_formula) {
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

message("Loading reference inference rows...")
ref_plot <- fread(ref_plot_path)
ref_plot <- ref_plot[
  sample_tag %in% sample_tags &
    transform == fixed_transform &
    fe_spec %in% names(fe_map) &
    outcome %in% outcomes &
    bandwidth_label %in% bandwidths$bandwidth_label,
  .(sample_tag, fe_spec, outcome, bandwidth_label, estimate, std_error, p_value, t_value, kernel_selected, n_obs, n_pairs)
]

expected_ref <- length(sample_tags) * length(fe_map) * length(outcomes) * nrow(bandwidths)
if (nrow(ref_plot) != expected_ref) {
  stop(sprintf("Reference rows incomplete: got %d expected %d.", nrow(ref_plot), expected_ref), call. = FALSE)
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

data_by_sample <- list(
  all = copy(dat),
  pruned = copy(dat[drop_confound == FALSE])
)

message("Sweeping bin widths across samples and FE...")
detail_rows <- list()
row_idx <- 1L

for (sample_tag_val in sample_tags) {
  d_sample <- data_by_sample[[sample_tag_val]]
  for (fe_name in names(fe_map)) {
    fe_formula <- fe_map[[fe_name]]
    for (bw_i in seq_len(nrow(bandwidths))) {
      bw_label <- bandwidths$bandwidth_label[bw_i]
      bw_m <- bandwidths$bandwidth_m[bw_i]

      for (bin_w in bin_widths_m) {
        for (outcome_val in outcomes) {
          b <- build_bins(d_sample, outcome_val, bw_m, bin_w, fe_formula)

          bins_out <- file.path(
            bins_dir,
            sprintf(
              "gm_bayer_kulka_binsweep_%s_%s_%s_bw%s_bin%dm.csv",
              sample_tag_val, fe_name, outcome_val, bw_label, as.integer(bin_w)
            )
          )

          bins_write <- copy(b$bins)
          bins_write[, `:=`(
            sample_tag = sample_tag_val,
            transform = fixed_transform,
            fe_spec = fe_name,
            outcome = outcome_val,
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

          ref_row <- ref_plot[
            sample_tag == sample_tag_val &
              fe_spec == fe_name &
              outcome == outcome_val &
              bandwidth_label == bw_label
          ][1]

          detail_rows[[row_idx]] <- data.table(
            sample_tag = sample_tag_val,
            transform = fixed_transform,
            fe_spec = fe_name,
            fe_formula = fe_formula,
            outcome = outcome_val,
            bandwidth_label = bw_label,
            bandwidth_m = as.numeric(bw_m),
            bin_width_m = as.integer(bin_w),
            estimate = as.numeric(ref_row$estimate),
            std_error = as.numeric(ref_row$std_error),
            p_value = as.numeric(ref_row$p_value),
            t_value = as.numeric(ref_row$t_value),
            kernel_selected = as.character(ref_row$kernel_selected),
            n_obs_ref_model = as.integer(ref_row$n_obs),
            n_pairs_ref_model = as.integer(ref_row$n_pairs),
            n_obs_plot = b$n_obs_plot,
            n_pairs_plot = b$n_pairs_plot,
            n_bins_total = b$n_bins_total,
            n_bins_left = b$n_bins_left,
            n_bins_right = b$n_bins_right,
            bins_csv = bins_out,
            stacked_plot_pdf = NA_character_,
            comparison_plot_pdf = NA_character_
          )
          row_idx <- row_idx + 1L
        }
      }
    }
  }
}

detail_dt <- rbindlist(detail_rows, fill = TRUE)
setorder(detail_dt, sample_tag, fe_spec, bandwidth_m, bin_width_m, outcome)

message("Rendering stacked bin plots...")
for (sample_tag_val in sample_tags) {
  for (fe_name in names(fe_map)) {
    for (bw_i in seq_len(nrow(bandwidths))) {
      bw_label <- bandwidths$bandwidth_label[bw_i]
      bw_m <- bandwidths$bandwidth_m[bw_i]

      for (bin_w in bin_widths_m) {
        d_sub <- detail_dt[
          sample_tag == sample_tag_val &
            fe_spec == fe_name &
            bandwidth_label == bw_label &
            bin_width_m == as.integer(bin_w)
        ]
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

        p_far <- build_panel(far_bins, "density_far", sample_tag_val, fe_name, bw_m, bw_label, bin_w, far_row$estimate, far_row$std_error, far_row$p_value)
        p_dupac <- build_panel(dupac_bins, "density_dupac", sample_tag_val, fe_name, bw_m, bw_label, bin_w, dupac_row$estimate, dupac_row$std_error, dupac_row$p_value)

        stacked <- p_far / p_dupac + plot_annotation(
          title = sprintf("Bin width sensitivity | %s | FE=%s | bw=%s", sample_tag_val, fe_name, bw_label)
        )

        out_pdf <- file.path(
          plots_dir,
          sprintf(
            "gm_bayer_kulka_binsweep_%s_%s_bw%s_bin%dm_stacked_far_dupac.pdf",
            sanitize(sample_tag_val), sanitize(fe_name), bw_label, as.integer(bin_w)
          )
        )
        ggsave(out_pdf, stacked, width = 8.8, height = 10.4, dpi = 300)

        detail_dt[
          sample_tag == sample_tag_val &
            fe_spec == fe_name &
            bandwidth_label == bw_label &
            bin_width_m == as.integer(bin_w),
          stacked_plot_pdf := out_pdf
        ]
      }

      far_panels <- vector("list", length(bin_widths_m))
      dupac_panels <- vector("list", length(bin_widths_m))

      for (j in seq_along(bin_widths_m)) {
        bin_w <- bin_widths_m[j]
        far_row <- detail_dt[
          sample_tag == sample_tag_val &
            fe_spec == fe_name &
            bandwidth_label == bw_label &
            bin_width_m == as.integer(bin_w) &
            outcome == "density_far"
        ][1]
        dupac_row <- detail_dt[
          sample_tag == sample_tag_val &
            fe_spec == fe_name &
            bandwidth_label == bw_label &
            bin_width_m == as.integer(bin_w) &
            outcome == "density_dupac"
        ][1]

        if (nrow(far_row) == 1L && file.exists(far_row$bins_csv)) {
          far_bins <- fread(far_row$bins_csv)
          far_panels[[j]] <- build_panel(far_bins, "density_far", sample_tag_val, fe_name, bw_m, bw_label, bin_w, far_row$estimate, far_row$std_error, far_row$p_value)
        } else {
          far_panels[[j]] <- ggplot() + theme_void()
        }

        if (nrow(dupac_row) == 1L && file.exists(dupac_row$bins_csv)) {
          dupac_bins <- fread(dupac_row$bins_csv)
          dupac_panels[[j]] <- build_panel(dupac_bins, "density_dupac", sample_tag_val, fe_name, bw_m, bw_label, bin_w, dupac_row$estimate, dupac_row$std_error, dupac_row$p_value)
        } else {
          dupac_panels[[j]] <- ggplot() + theme_void()
        }
      }

      comp_plot <- wrap_plots(far_panels, nrow = 1) / wrap_plots(dupac_panels, nrow = 1) +
        plot_annotation(
          title = sprintf("Bin width comparison | %s | FE=%s | bw=%s", sample_tag_val, fe_name, bw_label)
        )

      comp_out <- file.path(
        plots_dir,
        sprintf(
          "gm_bayer_kulka_binsweep_comparison_%s_%s_bw%s.pdf",
          sanitize(sample_tag_val), sanitize(fe_name), bw_label
        )
      )
      ggsave(comp_out, comp_plot, width = 20, height = 8.8, dpi = 300)

      detail_dt[
        sample_tag == sample_tag_val &
          fe_spec == fe_name &
          bandwidth_label == bw_label,
        comparison_plot_pdf := comp_out
      ]
    }
  }
}

score_dt <- detail_dt[, .(
  sample_tag,
  fe_spec,
  bandwidth_label,
  bin_width_m,
  mean_bins_side = mean((n_bins_left + n_bins_right) / 2),
  min_bins_side = min(pmin(n_bins_left, n_bins_right)),
  n_obs_plot = max(n_obs_plot),
  p_value_far = p_value[outcome == "density_far"][1],
  p_value_dupac = p_value[outcome == "density_dupac"][1],
  stacked_plot_pdf = stacked_plot_pdf[outcome == "density_far"][1],
  comparison_plot_pdf = comparison_plot_pdf[outcome == "density_far"][1]
), by = .(sample_tag, fe_spec, bandwidth_label, bin_width_m)]

score_dt[, aesthetic_target := 12]
score_dt[, aesthetic_distance := abs(mean_bins_side - aesthetic_target)]
setorder(score_dt, sample_tag, fe_spec, bandwidth_label, aesthetic_distance, -min_bins_side, -bin_width_m)
score_dt[, rank_within_spec := seq_len(.N), by = .(sample_tag, fe_spec, bandwidth_label)]

fwrite(detail_dt, detail_out)
fwrite(score_dt, scorecard_out)

summary_lines <- c(
  "# Bin Width Sweep Multispec Summary",
  "",
  "- Fixed model dimensions:",
  paste0("  - transform: `", fixed_transform, "`"),
  paste0("  - outcomes: `", paste(outcomes, collapse = "`, `"), "`"),
  paste0("  - bandwidths: `", paste(bandwidths$bandwidth_label, collapse = "`, `"), "`"),
  paste0("  - samples: `", paste(sample_tags, collapse = "`, `"), "`"),
  paste0("  - FE specs: `", paste(names(fe_map), collapse = "`, `"), "`"),
  paste0("  - bin widths (meters): ", paste(bin_widths_m, collapse = ", ")),
  paste0("  - min bin mass: n >= ", min_bin_n),
  "",
  "## Jump Label Source",
  "- Jump-label components (estimate, SE, stars via p-value) are copied from `gm_bayer_kulka_construction_plot_detail_ft500_1000.csv` for the exact `(sample_tag, fe_spec, outcome, bandwidth)` row.",
  "- Inference equation behind those p-values:",
  "  - `feols(outcome_val ~ side + signed_distance_m + side:signed_distance_m | FE, cluster = ~ward_pair, weights = kernel_weight)`",
  "  - outcome transform here fixed to `log`.",
  "",
  "## Outputs",
  paste0("- detail: `", detail_out, "`"),
  paste0("- scorecard: `", scorecard_out, "`"),
  paste0("- plots dir: `", plots_dir, "`"),
  ""
)

for (sample_tag_val in sample_tags) {
  summary_lines <- c(summary_lines, paste0("## Sample: ", sample_tag_val))
  for (fe_name in names(fe_map)) {
    summary_lines <- c(summary_lines, paste0("### FE: ", fe_name))
    for (bw_label in bandwidths$bandwidth_label) {
      top <- score_dt[
        sample_tag == sample_tag_val &
          fe_spec == fe_name &
          bandwidth_label == bw_label &
          rank_within_spec == 1
      ][1]
      summary_lines <- c(
        summary_lines,
        sprintf(
          "- %s best-by-target: bin=%dm, mean bins/side=%.1f, min bins/side=%d, p(FAR)=%.4f, p(DUPAC)=%.4f",
          bw_label, as.integer(top$bin_width_m), top$mean_bins_side, as.integer(top$min_bins_side), top$p_value_far, top$p_value_dupac
        ),
        sprintf("  - stacked: `%s`", top$stacked_plot_pdf),
        sprintf("  - comparison: `%s`", top$comparison_plot_pdf)
      )
    }
  }
  summary_lines <- c(summary_lines, "")
}

writeLines(summary_lines, summary_out)

message("Saved:")
message(sprintf("  - %s", detail_out))
message(sprintf("  - %s", scorecard_out))
message(sprintf("  - %s", summary_out))
message(sprintf("  - %s", plots_dir))
