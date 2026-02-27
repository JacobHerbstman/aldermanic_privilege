library(data.table)
library(ggplot2)
library(patchwork)
source("rd_label_utils.R")

out_dir <- "../output/bayer_kulka_prices_rents"
spec_inference_dir <- file.path(out_dir, "spec_inference")
spec_plot_dir <- file.path(out_dir, "spec_plot")
plots_dir <- file.path(out_dir, "plots")

inference_out <- file.path(out_dir, "gm_prices_rents_inference_detail.csv")
plot_out <- file.path(out_dir, "gm_prices_rents_plot_detail.csv")
scorecard_out <- file.path(out_dir, "gm_prices_rents_scorecard.csv")
qc_out <- file.path(out_dir, "gm_prices_rents_input_qc.csv")
summary_out <- file.path(out_dir, "gm_prices_rents_summary.md")

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

inference_files <- list.files(spec_inference_dir, pattern = "\\.csv$", full.names = TRUE)
plot_files <- list.files(spec_plot_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(inference_files) == 0L) {
  stop(sprintf("No spec inference files found in %s", spec_inference_dir), call. = FALSE)
}
if (length(plot_files) == 0L) {
  stop(sprintf("No spec plot files found in %s", spec_plot_dir), call. = FALSE)
}

sanitize_token <- function(x) {
  x <- trimws(as.character(x))
  if (!nzchar(x)) {
    return("")
  }
  gsub("[^A-Za-z0-9_-]", "_", x)
}

make_spec_id <- function(dataset, outcome, transform, sample_tag, bandwidth_ft, fe_structure, controls_tag) {
  sprintf(
    "dataset_%s__outcome_%s__transform_%s__sample_%s__bw_%dft__fe_%s__ctrl_%s",
    sanitize_token(dataset),
    sanitize_token(outcome),
    sanitize_token(transform),
    sanitize_token(sample_tag),
    as.integer(bandwidth_ft),
    sanitize_token(fe_structure),
    sanitize_token(controls_tag)
  )
}

plot_y_label <- function(outcome_name, transform_name) {
  if (outcome_name == "home_price") {
    return(if (transform_name == "log") "Log(Home price)" else "Home price")
  }
  if (transform_name == "log") {
    "Log(Rent)"
  } else {
    "Rent"
  }
}

build_panel <- function(bin_dt, spec_row, y_col = "y_bin_allbins") {
  d <- copy(bin_dt)
  d <- d[is.finite(get(y_col))]

  title_stub <- sprintf(
    "%s | %s | %s FE | bw=%dft | %s",
    if (spec_row$outcome == "home_price") "Home price" else "Rent",
    spec_row$dataset,
    spec_row$fe_structure,
    as.integer(spec_row$bandwidth_ft),
    gm_jump_label(spec_row$estimate, spec_row$std_error, spec_row$p_value)
  )

  ggplot(d, aes(x = bin_mid_m, y = get(y_col))) +
    geom_point(size = 1.8, alpha = 0.95, color = "#1f4e79") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.6) +
    coord_cartesian(xlim = c(-spec_row$bandwidth_m, spec_row$bandwidth_m)) +
    labs(
      title = title_stub,
      x = "Distance to border (meters)",
      y = plot_y_label(spec_row$outcome, spec_row$transform)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 11)
    )
}

inference_dt <- rbindlist(lapply(inference_files, fread), fill = TRUE)
plot_dt <- rbindlist(lapply(plot_files, fread), fill = TRUE)

setorder(
  inference_dt,
  dataset, outcome, transform, sample_tag, bandwidth_ft, fe_structure, controls_tag
)
setorder(
  plot_dt,
  dataset, outcome, transform, sample_tag, bandwidth_ft, fe_structure, controls_tag, bin_id
)

if (uniqueN(inference_dt$spec_id) != nrow(inference_dt)) {
  stop("Spec inference files have duplicated spec_id values.", call. = FALSE)
}

expected_grid <- CJ(
  dataset = c("sales", "rent"),
  transform = c("level", "log"),
  sample_tag = c("all", "pruned"),
  bandwidth_ft = c(500L, 1000L),
  fe_structure = c("additive", "interacted"),
  controls_tag = c("no_hedonic", "hedonic")
)
expected_grid[, outcome := fifelse(dataset == "sales", "home_price", "rent_price")]
expected_grid[, spec_id := make_spec_id(
  dataset, outcome, transform, sample_tag, bandwidth_ft, fe_structure, controls_tag
), by = .I]
setorder(expected_grid, spec_id)

missing_specs <- setdiff(expected_grid$spec_id, inference_dt$spec_id)
extra_specs <- setdiff(inference_dt$spec_id, expected_grid$spec_id)

if (length(missing_specs) > 0L || length(extra_specs) > 0L) {
  stop(
    sprintf(
      "Spec completeness check failed. Missing: %d, extra: %d.",
      length(missing_specs), length(extra_specs)
    ),
    call. = FALSE
  )
}

if (nrow(inference_dt) != nrow(expected_grid)) {
  stop(
    sprintf("Expected %d inference rows, got %d.", nrow(expected_grid), nrow(inference_dt)),
    call. = FALSE
  )
}

inference_export <- inference_dt[, .(
  spec_id,
  dataset,
  outcome,
  transform,
  sample_tag,
  bandwidth_ft,
  bandwidth_m,
  fe_structure,
  controls_tag,
  kernel,
  estimate,
  std_error,
  t_value,
  p_value,
  n_obs,
  n_pairs,
  n_left,
  n_right,
  first_bin_left_n,
  first_bin_right_n,
  first_bin_left_mean,
  first_bin_right_mean,
  first_bin_gap,
  obs_lock_signature,
  bins_csv,
  plot_allbins_pdf,
  plot_display_n5_pdf,
  n_rows_raw,
  n_rows_base,
  join_match_rate,
  n_unmapped_rows,
  n_unmapped_pair_era,
  drop_confound_share,
  pair_parse_missing,
  era_missing,
  unmatched_pair_era_csv
)]
setorder(
  inference_export,
  dataset, outcome, transform, sample_tag, bandwidth_ft, fe_structure, controls_tag
)
fwrite(inference_export, inference_out)

plot_export <- plot_dt[, .(
  spec_id,
  dataset,
  outcome,
  transform,
  sample_tag,
  bandwidth_ft,
  fe_structure,
  controls_tag,
  bin_width_m,
  bin_id,
  bin_mid_m,
  n_bin,
  y_bin_allbins,
  y_bin_display_n5
)]
setorder(
  plot_export,
  dataset, outcome, transform, sample_tag, bandwidth_ft, fe_structure, controls_tag, bin_id
)
fwrite(plot_export, plot_out)

scorecard <- copy(inference_export)
scorecard[, abs_t := abs(t_value)]
scorecard[, sample_priority := fifelse(sample_tag == "pruned", 0L, 1L)]
scorecard[, p_sort := fifelse(is.finite(p_value), p_value, Inf)]
scorecard[, t_sort := fifelse(is.finite(abs_t), abs_t, -Inf)]
scorecard[, n_sort := fifelse(is.finite(n_obs), n_obs, -Inf)]
setorder(
  scorecard,
  outcome,
  sample_priority,
  p_sort,
  -t_sort,
  -n_sort,
  bandwidth_ft,
  fe_structure,
  controls_tag,
  transform
)
scorecard[, rank_outcome := seq_len(.N), by = outcome]
fwrite(scorecard, scorecard_out)

qc_summary <- inference_export[, .(
  n_rows_raw = max(n_rows_raw, na.rm = TRUE),
  n_rows_base = max(n_rows_base, na.rm = TRUE),
  join_match_rate = max(join_match_rate, na.rm = TRUE),
  n_unmapped_rows = max(n_unmapped_rows, na.rm = TRUE),
  n_unmapped_pair_era = max(n_unmapped_pair_era, na.rm = TRUE),
  drop_confound_share = max(drop_confound_share, na.rm = TRUE),
  pair_parse_missing = max(pair_parse_missing, na.rm = TRUE),
  era_missing = max(era_missing, na.rm = TRUE)
), by = .(dataset)]
qc_summary[, has_unmapped_pair_era := as.logical(n_unmapped_pair_era > 0)]
qc_summary[, `:=`(
  row_type = "dataset_summary",
  ward_pair = NA_character_,
  era = NA_character_,
  n_rows_unmapped_pair_era = NA_integer_
)]

unmatched_files <- list.files(out_dir, pattern = "^qc_unmapped_pair_era_(sales|rent)\\.csv$", full.names = TRUE)
qc_unmatched_rows <- list()
if (length(unmatched_files) > 0L) {
  idx <- 1L
  for (f in unmatched_files) {
    dataset_name <- sub("^qc_unmapped_pair_era_", "", basename(f))
    dataset_name <- sub("\\.csv$", "", dataset_name)
    d <- fread(f)
    if (nrow(d) == 0L) {
      next
    }
    setorder(d, -N)
    d <- d[1:min(50L, .N)]
    d[, `:=`(
      row_type = "unmapped_pair_era",
      dataset = dataset_name,
      ward_pair = as.character(ward_pair),
      era = as.character(era),
      n_rows_unmapped_pair_era = as.integer(N),
      n_rows_raw = NA_integer_,
      n_rows_base = NA_integer_,
      join_match_rate = NA_real_,
      n_unmapped_rows = NA_integer_,
      n_unmapped_pair_era = NA_integer_,
      has_unmapped_pair_era = NA,
      drop_confound_share = NA_real_,
      pair_parse_missing = NA_integer_,
      era_missing = NA_integer_
    )]
    qc_unmatched_rows[[idx]] <- d[, .(
      row_type, dataset, ward_pair, era, n_rows_unmapped_pair_era,
      n_rows_raw, n_rows_base, join_match_rate, n_unmapped_rows,
      n_unmapped_pair_era, has_unmapped_pair_era, drop_confound_share,
      pair_parse_missing, era_missing
    )]
    idx <- idx + 1L
  }
}

qc_out_dt <- rbindlist(list(
  qc_summary[, .(
    row_type,
    dataset,
    ward_pair,
    era,
    n_rows_unmapped_pair_era,
    n_rows_raw,
    n_rows_base,
    join_match_rate,
    n_unmapped_rows,
    n_unmapped_pair_era,
    has_unmapped_pair_era,
    drop_confound_share,
    pair_parse_missing,
    era_missing
  )],
  rbindlist(qc_unmatched_rows, fill = TRUE)
), fill = TRUE)
setorder(qc_out_dt, dataset, row_type, -n_rows_unmapped_pair_era)
fwrite(qc_out_dt, qc_out)

mon_all <- inference_export[sample_tag == "all", .(
  dataset, outcome, transform, bandwidth_ft, fe_structure, controls_tag, n_obs_all = n_obs
)]
mon_pruned <- inference_export[sample_tag == "pruned", .(
  dataset, outcome, transform, bandwidth_ft, fe_structure, controls_tag, n_obs_pruned = n_obs
)]
mon <- merge(
  mon_all, mon_pruned,
  by = c("dataset", "outcome", "transform", "bandwidth_ft", "fe_structure", "controls_tag"),
  all = FALSE
)
mon_viol <- mon[n_obs_pruned > n_obs_all]

lock_check <- inference_export[, .(
  n_controls = uniqueN(controls_tag),
  n_signatures = uniqueN(obs_lock_signature)
), by = .(
  dataset, outcome, transform, sample_tag, bandwidth_ft, fe_structure
)]
lock_viol <- lock_check[n_controls != 2L | n_signatures != 1L]

display_consistency <- list(
  n_bins_total = nrow(plot_export),
  n_bins_lt5 = plot_export[n_bin < 5, .N],
  n_bins_ge5 = plot_export[n_bin >= 5, .N],
  n_violate_lt5 = plot_export[n_bin < 5 & !is.na(y_bin_display_n5), .N],
  n_violate_ge5 = plot_export[
    n_bin >= 5 &
      (
        (is.finite(y_bin_allbins) & is.finite(y_bin_display_n5) & abs(y_bin_allbins - y_bin_display_n5) > 1e-10) |
          (is.finite(y_bin_allbins) & is.na(y_bin_display_n5)) |
          (!is.finite(y_bin_allbins) & is.finite(y_bin_display_n5))
      ),
    .N
  ]
)

main_plot_paths <- character()
for (bw_ft in c(500L, 1000L)) {
  anchor <- inference_export[
    sample_tag == "pruned" &
      transform == "log" &
      fe_structure == "additive" &
      controls_tag == "hedonic" &
      bandwidth_ft == bw_ft
  ]
  if (nrow(anchor) == 0L) {
    next
  }
  anchor <- anchor[order(dataset)]
  if (!all(c("sales", "rent") %in% anchor$dataset)) {
    next
  }

  sales_row <- anchor[dataset == "sales"][1]
  rent_row <- anchor[dataset == "rent"][1]
  if (!file.exists(sales_row$bins_csv) || !file.exists(rent_row$bins_csv)) {
    next
  }

  sales_bins <- fread(sales_row$bins_csv)
  rent_bins <- fread(rent_row$bins_csv)
  if (nrow(sales_bins) == 0L || nrow(rent_bins) == 0L) {
    next
  }

  p_sales <- build_panel(sales_bins, sales_row, "y_bin_allbins")
  p_rent <- build_panel(rent_bins, rent_row, "y_bin_allbins")
  stacked <- p_sales / p_rent + plot_annotation(
    title = sprintf(
      "Prices/Rents RD (pruned, log, additive FE, hedonic) | bw=%dft | all bins",
      as.integer(bw_ft)
    )
  )
  out_pdf <- file.path(
    plots_dir,
    sprintf("gm_prices_rents_main_stacked_pruned_log_additive_hedonic_bw%dft_allbins.pdf", as.integer(bw_ft))
  )
  ggsave(out_pdf, stacked, width = 8.8, height = 10.4, dpi = 300)
  main_plot_paths <- c(main_plot_paths, out_pdf)
}

summary_lines <- c(
  "# GM Prices/Rents RD Summary",
  "",
  "## Completeness",
  sprintf("- Inference rows expected: %d", nrow(expected_grid)),
  sprintf("- Inference rows found: %d", nrow(inference_export)),
  sprintf("- Missing spec rows: %d", length(missing_specs)),
  sprintf("- Extra spec rows: %d", length(extra_specs)),
  "",
  "## Core Checks",
  sprintf("- Pruned monotonicity violations (n_obs_pruned > n_obs_all): %d", nrow(mon_viol)),
  sprintf("- FE/control lock violations: %d", nrow(lock_viol)),
  sprintf("- Bins with n < 5: %d", display_consistency$n_bins_lt5),
  sprintf("- Display-n5 consistency violations for n < 5 bins: %d", display_consistency$n_violate_lt5),
  sprintf("- Display-n5 consistency violations for n >= 5 bins: %d", display_consistency$n_violate_ge5),
  "",
  "## Anchor Plots",
  if (length(main_plot_paths) > 0) paste0("- `", main_plot_paths, "`") else "- None generated (missing anchor rows).",
  "",
  "## Outputs",
  sprintf("- `%s`", inference_out),
  sprintf("- `%s`", plot_out),
  sprintf("- `%s`", scorecard_out),
  sprintf("- `%s`", qc_out),
  sprintf("- `%s`", plots_dir)
)

writeLines(summary_lines, summary_out)

message("Saved:")
message(sprintf("  - %s", inference_out))
message(sprintf("  - %s", plot_out))
message(sprintf("  - %s", scorecard_out))
message(sprintf("  - %s", qc_out))
message(sprintf("  - %s", summary_out))
if (length(main_plot_paths) > 0) {
  for (pp in main_plot_paths) {
    message(sprintf("  - %s", pp))
  }
}
