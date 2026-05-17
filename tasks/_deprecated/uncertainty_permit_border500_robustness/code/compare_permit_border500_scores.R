## Compare baseline and permit-exclusion alderman stringency scores

source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/uncertainty_permit_border500_robustness/code")
# baseline_input <- "../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH.csv"
# variant_input <- "../output/alderman_uncertainty_index_permit_border500.csv"
# joined_output <- "../output/score_comparison_permit_border500_vs_baseline.csv"
# summary_output <- "../output/score_comparison_permit_border500_vs_baseline_summary.csv"
# summary_tex_output <- "../output/score_comparison_permit_border500_vs_baseline_summary.tex"
# plot_output <- "../output/score_comparison_permit_border500_vs_baseline_scatter.pdf"
# variant_id <- "permit_border500"
# variant_label <- "Drop permits within 500 ft of ward borders"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(baseline_input, variant_input, joined_output, summary_output, summary_tex_output, plot_output, variant_id, variant_label)
}

if (length(args) >= 6) {
  baseline_input <- args[1]
  variant_input <- args[2]
  joined_output <- args[3]
  summary_output <- args[4]
  summary_tex_output <- args[5]
  plot_output <- args[6]
} else {
  stop(
    "FATAL: Script requires 6 args: <baseline_input> <variant_input> <joined_output> <summary_output> <summary_tex_output> <plot_output>",
    call. = FALSE
  )
}

variant_id <- if (length(args) >= 7) args[7] else "permit_border500"
variant_label <- if (length(args) >= 8) args[8] else "Drop permits within 500 ft of ward borders"
score_col_name <- paste0(variant_id, "_score")
rank_col_name <- paste0(variant_id, "_rank")

fmt_num <- function(x, digits = 3) {
  ifelse(
    is.finite(x),
    formatC(x, format = "f", digits = digits, big.mark = ","),
    ""
  )
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

write_tabular_tex <- function(df, file) {
  lines <- c(
    "\\begin{tabular}{ll}",
    "\\toprule",
    paste(paste(latex_escape(names(df)), collapse = " & "), "\\\\"),
    "\\midrule"
  )

  for (i in seq_len(nrow(df))) {
    row_text <- vapply(df[i, ], function(x) latex_escape(as.character(x)), character(1))
    lines <- c(lines, paste(paste(row_text, collapse = " & "), "\\\\"))
  }

  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, file)
}

baseline_scores <- read_csv(baseline_input, show_col_types = FALSE) %>%
  transmute(
    alderman,
    baseline_score = as.numeric(uncertainty_index)
  )

variant_scores <- read_csv(variant_input, show_col_types = FALSE) %>%
  transmute(
    alderman,
    !!score_col_name := as.numeric(uncertainty_index)
  )

joined <- baseline_scores %>%
  inner_join(variant_scores, by = "alderman") %>%
  mutate(
    baseline_rank = rank(-baseline_score, ties.method = "average"),
    !!rank_col_name := rank(-.data[[score_col_name]], ties.method = "average"),
    rank_change = .data[[rank_col_name]] - baseline_rank,
    abs_rank_change = abs(rank_change)
  ) %>%
  arrange(.data[[rank_col_name]], alderman)

if (nrow(joined) == 0) {
  stop("No aldermen matched across baseline and permit-border500 score files.", call. = FALSE)
}

summary_row <- tibble(
  pearson_correlation = cor(joined$baseline_score, joined[[score_col_name]], use = "complete.obs"),
  spearman_correlation = cor(
    joined$baseline_score,
    joined[[score_col_name]],
    use = "complete.obs",
    method = "spearman"
  ),
  mean_absolute_rank_change = mean(joined$abs_rank_change, na.rm = TRUE),
  max_absolute_rank_change = max(joined$abs_rank_change, na.rm = TRUE),
  matched_alderman_count = nrow(joined)
)

summary_tex <- tibble(
  Metric = c(
    "Pearson correlation",
    "Spearman correlation",
    "Mean absolute rank change",
    "Max absolute rank change",
    "Matched alderman count"
  ),
  Value = c(
    fmt_num(summary_row$pearson_correlation),
    fmt_num(summary_row$spearman_correlation),
    fmt_num(summary_row$mean_absolute_rank_change),
    fmt_num(summary_row$max_absolute_rank_change),
    fmt_num(summary_row$matched_alderman_count, 0)
  )
)

write_csv(joined, joined_output)
write_csv(summary_row, summary_output)
write_tabular_tex(summary_tex, summary_tex_output)

plot_limits <- range(c(joined$baseline_score, joined[[score_col_name]]), na.rm = TRUE)

p <- ggplot(joined, aes(x = baseline_score, y = .data[[score_col_name]])) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
  geom_point(color = "#2c7fb8", alpha = 0.85, size = 2.1) +
  coord_cartesian(xlim = plot_limits, ylim = plot_limits) +
  labs(
    title = paste0("Baseline vs. ", variant_label, " score"),
    subtitle = sprintf(
      "Pearson = %s | Spearman = %s | N = %d",
      fmt_num(summary_row$pearson_correlation),
      fmt_num(summary_row$spearman_correlation),
      summary_row$matched_alderman_count
    ),
    x = "Baseline score",
    y = paste0(variant_label, " score")
  ) +
  theme_minimal(base_size = 11)

ggsave(plot_output, p, width = 6.5, height = 6.2, device = "pdf", bg = "white")

message("Saved score comparison outputs:")
message("  Joined CSV: ", joined_output)
message("  Summary CSV: ", summary_output)
message("  Summary TEX: ", summary_tex_output)
message("  Scatter plot: ", plot_output)
