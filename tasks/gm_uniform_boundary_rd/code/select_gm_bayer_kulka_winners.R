source("../../setup_environment/code/packages.R")
source("rd_label_utils.R")

library(data.table)
library(ggplot2)
library(patchwork)

fmt_bw <- function(spec_row) {
  if ("bandwidth_label" %in% names(spec_row)) {
    bw_lab <- as.character(spec_row$bandwidth_label)
    if (!is.na(bw_lab) && nzchar(bw_lab)) {
      return(bw_lab)
    }
  }
  if ("bandwidth_ft" %in% names(spec_row) && is.finite(spec_row$bandwidth_ft)) {
    return(sprintf("%dft", as.integer(round(spec_row$bandwidth_ft))))
  }
  sprintf("%dm", as.integer(round(spec_row$bandwidth_m)))
}

out_dir <- "../output/bayer_kulka_construction"
run_tag <- trimws(Sys.getenv("BK_RUN_TAG", ""))
run_tag <- gsub("[^A-Za-z0-9_-]", "_", run_tag)
run_suffix <- if (nzchar(run_tag)) paste0("_", run_tag) else ""

plots_dir <- file.path(out_dir, paste0("plots", run_suffix))
inference_path <- file.path(out_dir, paste0("gm_bayer_kulka_construction_inference_detail", run_suffix, ".csv"))
plot_path <- file.path(out_dir, paste0("gm_bayer_kulka_construction_plot_detail", run_suffix, ".csv"))

scorecard_out <- file.path(out_dir, paste0("gm_bayer_kulka_construction_scorecard", run_suffix, ".csv"))
winners_out <- file.path(out_dir, paste0("gm_bayer_kulka_construction_winners", run_suffix, ".csv"))
summary_out <- file.path(out_dir, paste0("gm_bayer_kulka_construction_summary", run_suffix, ".md"))
main_plot_out <- file.path(
  plots_dir,
  paste0("gm_bayer_kulka_main_stacked_pruned_far_dupac", run_suffix, ".pdf")
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(inference_path), file.exists(plot_path))

inference_dt <- fread(inference_path)
plot_dt <- fread(plot_path)

if (nrow(inference_dt) == 0 || nrow(plot_dt) == 0) {
  stop("Missing inference or plot detail rows.", call. = FALSE)
}
if (!("bandwidth_label" %in% names(plot_dt))) {
  plot_dt[, bandwidth_label := sprintf("%dm", as.integer(round(bandwidth_m)))]
}
if (!("bandwidth_label" %in% names(inference_dt))) {
  inference_dt[, bandwidth_label := sprintf("%dm", as.integer(round(bandwidth_m)))]
}

outcome_labels <- c(
  density_far = "FAR",
  density_dupac = "DUPAC"
)

y_label <- function(outcome, transform) {
  base <- outcome_labels[[outcome]]
  if (is.null(base)) {
    base <- outcome
  }
  if (transform == "log") {
    paste0("Log(", base, ")")
  } else {
    base
  }
}

sanitize <- function(x) {
  x <- as.character(x)
  gsub("[^A-Za-z0-9_-]", "", x)
}

build_bins_plot <- function(spec_row, title_text) {
  bins <- fread(spec_row$bins_csv)
  if (nrow(bins) == 0) {
    stop(sprintf("Bins file is empty: %s", spec_row$bins_csv), call. = FALSE)
  }

  ggplot(bins, aes(x = bin_center_m, y = mean_y)) +
    geom_point(size = 1.8, alpha = 0.95, color = "#1f4e79") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray35", linewidth = 0.6) +
    coord_cartesian(xlim = c(-spec_row$bandwidth_m, spec_row$bandwidth_m)) +
    labs(
      title = title_text,
      x = "Distance to border (meters)",
      y = y_label(spec_row$outcome, spec_row$transform)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "gray85", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.title = element_text(color = "black"),
      plot.title = element_text(face = "bold", size = 11)
    )
}

score <- copy(plot_dt)
score[, usable_visual := as.logical(usable_visual)]
score[, sample_priority := fifelse(sample_tag == "pruned", 0L, 1L)]
score[, usable_priority := fifelse(usable_visual, 0L, 1L)]
score[, jump_sort := fifelse(is.na(jump_over_rmse), -Inf, jump_over_rmse)]
score[, p_sort := fifelse(is.na(p_value), Inf, p_value)]
score[, t_sort := fifelse(is.na(t_value), -Inf, abs(t_value))]
score[, n_sort := fifelse(is.na(n_obs), -Inf, n_obs)]

setorder(
  score,
  sample_priority,
  usable_priority,
  -jump_sort,
  p_sort,
  -t_sort,
  -n_sort,
  outcome,
  transform,
  bandwidth_m,
  fe_spec
)

score[, rank_overall := .I]
score[, `:=`(
  is_candidate = FALSE,
  candidate_rank = NA_integer_,
  candidate_pdf = NA_character_,
  is_winner = FALSE,
  winner_pdf = NA_character_
)]

usable_dt <- score[usable_visual == TRUE]
if (nrow(usable_dt) == 0) {
  stop("No usable_visual == TRUE rows. Cannot select winners.", call. = FALSE)
}

candidate_n <- min(12L, nrow(usable_dt))
candidates <- copy(usable_dt[1:candidate_n])
candidates[, candidate_rank := .I]

for (i in seq_len(nrow(candidates))) {
  rr <- candidates[i]
  candidate_pdf_path <- file.path(
    plots_dir,
    sprintf(
      "gm_bayer_kulka_candidate_%02d_%s_%s_%s_bw%s_%s_%s.pdf",
      i,
      sanitize(rr$sample_tag),
      sanitize(rr$outcome),
      sanitize(rr$transform),
      sanitize(fmt_bw(rr)),
      sanitize(rr$fe_spec),
      sanitize(rr$kernel_selected)
    )
  )
  candidate_title <- sprintf(
    "%s | %s sample | %s | bw=%s | FE=%s | kernel=%s | %s",
    y_label(rr$outcome, rr$transform),
    rr$sample_tag,
    rr$transform,
    fmt_bw(rr),
    rr$fe_spec,
    rr$kernel_selected,
    gm_jump_label(rr$estimate, rr$std_error, rr$p_value)
  )
  p <- build_bins_plot(rr, candidate_title)
  ggsave(candidate_pdf_path, p, width = 8.4, height = 4.8, dpi = 300)
  candidates[i, candidate_pdf := candidate_pdf_path]
}

wanted_outcomes <- c("density_far", "density_dupac")
winner_rows <- list()
for (outcome_name in wanted_outcomes) {
  out_pool <- usable_dt[outcome == outcome_name]
  if (nrow(out_pool) == 0) {
    out_pool <- score[outcome == outcome_name]
  }
  if (nrow(out_pool) == 0) {
    stop(sprintf("No candidates found for outcome %s.", outcome_name), call. = FALSE)
  }
  winner_rows[[outcome_name]] <- out_pool[1]
}
winners <- rbindlist(winner_rows, fill = TRUE)
setorder(winners, outcome)

if (nrow(winners) != 2L) {
  stop(sprintf("Expected exactly 2 winners; got %d.", nrow(winners)), call. = FALSE)
}

for (i in seq_len(nrow(winners))) {
  rr <- winners[i]
  winner_pdf_path <- file.path(
    plots_dir,
    sprintf("gm_bayer_kulka_winner_%s.pdf", sanitize(rr$outcome))
  )
  winner_title <- sprintf(
    "Winner: %s | %s sample | %s | bw=%s | FE=%s | kernel=%s | %s",
    y_label(rr$outcome, rr$transform),
    rr$sample_tag,
    rr$transform,
    fmt_bw(rr),
    rr$fe_spec,
    rr$kernel_selected,
    gm_jump_label(rr$estimate, rr$std_error, rr$p_value)
  )
  p <- build_bins_plot(rr, winner_title)
  ggsave(winner_pdf_path, p, width = 8.4, height = 4.8, dpi = 300)
  winners[i, winner_pdf := winner_pdf_path]
}

for (i in seq_len(nrow(candidates))) {
  score[spec_id == candidates$spec_id[i], `:=`(
    is_candidate = TRUE,
    candidate_rank = as.integer(candidates$candidate_rank[i]),
    candidate_pdf = candidates$candidate_pdf[i]
  )]
}

for (i in seq_len(nrow(winners))) {
  score[spec_id == winners$spec_id[i], `:=`(
    is_winner = TRUE,
    winner_pdf = winners$winner_pdf[i]
  )]
}

far_winner <- winners[outcome == "density_far"][1]
dupac_winner <- winners[outcome == "density_dupac"][1]
if (nrow(far_winner) != 1 || nrow(dupac_winner) != 1) {
  stop("Missing FAR or DUPAC winner.", call. = FALSE)
}

p_far <- build_bins_plot(
  far_winner,
  sprintf(
    "FAR winner | %s sample | %s | bw=%s | FE=%s | kernel=%s | %s",
    far_winner$sample_tag,
    far_winner$transform,
    fmt_bw(far_winner),
    far_winner$fe_spec,
    far_winner$kernel_selected,
    gm_jump_label(far_winner$estimate, far_winner$std_error, far_winner$p_value)
  )
)
p_dupac <- build_bins_plot(
  dupac_winner,
  sprintf(
    "DUPAC winner | %s sample | %s | bw=%s | FE=%s | kernel=%s | %s",
    dupac_winner$sample_tag,
    dupac_winner$transform,
    fmt_bw(dupac_winner),
    dupac_winner$fe_spec,
    dupac_winner$kernel_selected,
    gm_jump_label(dupac_winner$estimate, dupac_winner$std_error, dupac_winner$p_value)
  )
)

stacked <- p_far / p_dupac + plot_annotation(
  title = "Bayer/Kulka-style binned RD (construction outcomes, pruned-priority ranking)"
)
ggsave(main_plot_out, stacked, width = 8.6, height = 10.4, dpi = 300)

winners[, main_stacked_pdf := main_plot_out]

setorder(score, rank_overall)
fwrite(score, scorecard_out)

winners_export <- winners[, .(
  outcome,
  spec_id,
  sample_tag,
  transform,
  bandwidth_m,
  bandwidth_ft,
  bandwidth_label,
  fe_spec,
  kernel_selected,
  estimate,
  std_error,
  p_value,
  t_value,
  n_obs,
  n_pairs,
  n_bins_total,
  n_bins_left,
  n_bins_right,
  cutoff_bin_present,
  rmse_piecewise_linear_bins,
  cutoff_gap_bin,
  jump_over_rmse,
  usable_visual,
  bins_csv,
  winner_pdf,
  main_stacked_pdf
)]
fwrite(winners_export, winners_out)

mon_all <- plot_dt[sample_tag == "all", .(
  outcome, transform, bandwidth_m, bandwidth_label, fe_spec, n_obs_all = n_obs
)]
mon_pruned <- plot_dt[sample_tag == "pruned", .(
  outcome, transform, bandwidth_m, bandwidth_label, fe_spec, n_obs_pruned = n_obs
)]
mon <- merge(
  mon_all, mon_pruned,
  by = c("outcome", "transform", "bandwidth_m", "bandwidth_label", "fe_spec"),
  all = FALSE
)
mon_viol <- mon[n_obs_pruned > n_obs_all]

summary_lines <- c(
  "# GM Bayer/Kulka Construction RD Summary",
  "",
  sprintf("- Inference rows: %d", nrow(inference_dt)),
  sprintf("- Plot-spec rows: %d", nrow(plot_dt)),
  sprintf("- Usable visual rows: %d", nrow(usable_dt)),
  sprintf("- Candidate plots generated: %d", nrow(candidates)),
  sprintf("- Pruned monotonicity violations (n_obs_pruned > n_obs_all): %d", nrow(mon_viol)),
  "",
  "## Winners",
  ""
)

for (i in seq_len(nrow(winners_export))) {
  rr <- winners_export[i]
  summary_lines <- c(
    summary_lines,
    sprintf(
      "- %s: sample=%s, transform=%s, bw=%s, FE=%s, kernel=%s, est=%.4f, p %.4f, jump_over_rmse=%s",
      rr$outcome,
      rr$sample_tag,
      rr$transform,
      fmt_bw(rr),
      rr$fe_spec,
      rr$kernel_selected,
      rr$estimate,
      rr$p_value,
      ifelse(is.finite(rr$jump_over_rmse), sprintf("%.4f", rr$jump_over_rmse), "NA")
    ),
    sprintf("  - bins: `%s`", rr$bins_csv),
    sprintf("  - winner plot: `%s`", rr$winner_pdf)
  )
}

summary_lines <- c(
  summary_lines,
  "",
  "## Main Figure",
  sprintf("- `%s`", main_plot_out),
  "",
  "## Top Candidate Plots",
  ""
)

for (i in seq_len(nrow(candidates))) {
  rr <- candidates[i]
  summary_lines <- c(
    summary_lines,
    sprintf(
      "- rank %d: %s | sample=%s | %s | bw=%s | FE=%s | kernel=%s | p %.4f | `%s`",
      as.integer(rr$candidate_rank),
      rr$outcome,
      rr$sample_tag,
      rr$transform,
      fmt_bw(rr),
      rr$fe_spec,
      rr$kernel_selected,
      rr$p_value,
      rr$candidate_pdf
    )
  )
}

writeLines(summary_lines, summary_out)

message("Saved:")
message(sprintf("  - %s", scorecard_out))
message(sprintf("  - %s", winners_out))
message(sprintf("  - %s", summary_out))
message(sprintf("  - %s", main_plot_out))
