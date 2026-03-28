## Summarize permit-subset processing-time heterogeneity

source("../../setup_environment/code/packages.R")
source("../../_lib/alderman_uncertainty_helpers.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/new_construction_score_density_robustness/code")
# permits_input <- "../input/permits_for_uncertainty_index.csv"
# summary_output <- "../output/new_construction_processing_time_summary.csv"
# summary_tex_output <- "../output/new_construction_processing_time_summary.tex"
# alderman_output <- "../output/new_construction_alderman_processing_times.csv"
# plot_output <- "../output/new_construction_alderman_processing_time_density.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(permits_input, summary_output, summary_tex_output, alderman_output, plot_output)
}

if (length(args) >= 5) {
  permits_input <- args[1]
  summary_output <- args[2]
  summary_tex_output <- args[3]
  alderman_output <- args[4]
  plot_output <- args[5]
} else {
  stop(
    "FATAL: Script requires 5 args: <permits_input> <summary_output> <summary_tex_output> <alderman_output> <plot_output>",
    call. = FALSE
  )
}

variant_id <- if (length(args) >= 6) args[6] else "new_construction"
variant_label <- if (length(args) >= 7) args[7] else "New construction only"
permit_types_csv <- if (length(args) >= 8) args[8] else "new_construction"
permit_types <- strsplit(permit_types_csv, ",", fixed = TRUE)[[1]] |> trimws()

fmt_num <- function(x, digits = 2) {
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

config <- default_uncertainty_config()

prepared <- prepare_uncertainty_sample(
  permits = load_uncertainty_permits(permits_input) %>%
    filter(permit_type_clean %in% permit_types),
  include_porch = config$include_porch,
  volume_ctrl = config$volume_ctrl,
  volume_stage = config$volume_stage
)

analysis_sample <- prepared$permits %>%
  filter(is.finite(processing_time), processing_time > 0, !is.na(alderman))

if (nrow(analysis_sample) == 0) {
  stop("No usable new-construction permits with positive processing times.", call. = FALSE)
}

alderman_stats <- analysis_sample %>%
  group_by(alderman) %>%
  summarise(
    n_permits = n(),
    mean_processing_time = mean(processing_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_processing_time), alderman)

summary_row <- tibble(
  permit_count = nrow(analysis_sample),
  alderman_count = n_distinct(analysis_sample$alderman),
  mean_processing_time = mean(analysis_sample$processing_time, na.rm = TRUE),
  median_processing_time = median(analysis_sample$processing_time, na.rm = TRUE),
  p10_processing_time = as.numeric(quantile(analysis_sample$processing_time, 0.10, na.rm = TRUE)),
  p25_processing_time = as.numeric(quantile(analysis_sample$processing_time, 0.25, na.rm = TRUE)),
  p75_processing_time = as.numeric(quantile(analysis_sample$processing_time, 0.75, na.rm = TRUE)),
  p90_processing_time = as.numeric(quantile(analysis_sample$processing_time, 0.90, na.rm = TRUE)),
  alderman_mean_processing_time_mean = mean(alderman_stats$mean_processing_time, na.rm = TRUE),
  alderman_mean_processing_time_sd = sd(alderman_stats$mean_processing_time, na.rm = TRUE),
  alderman_mean_processing_time_iqr = IQR(alderman_stats$mean_processing_time, na.rm = TRUE),
  alderman_mean_processing_time_min = min(alderman_stats$mean_processing_time, na.rm = TRUE),
  alderman_mean_processing_time_max = max(alderman_stats$mean_processing_time, na.rm = TRUE)
)

summary_tex <- tibble(
  Metric = c(
    "Permit count",
    "Alderman count",
    "Mean processing time (days)",
    "Median processing time (days)",
    "P10 processing time (days)",
    "P25 processing time (days)",
    "P75 processing time (days)",
    "P90 processing time (days)",
    "Mean of alderman mean times",
    "SD of alderman mean times",
    "IQR of alderman mean times",
    "Min alderman mean time",
    "Max alderman mean time"
  ),
  Value = c(
    fmt_num(summary_row$permit_count, 0),
    fmt_num(summary_row$alderman_count, 0),
    fmt_num(summary_row$mean_processing_time),
    fmt_num(summary_row$median_processing_time),
    fmt_num(summary_row$p10_processing_time),
    fmt_num(summary_row$p25_processing_time),
    fmt_num(summary_row$p75_processing_time),
    fmt_num(summary_row$p90_processing_time),
    fmt_num(summary_row$alderman_mean_processing_time_mean),
    fmt_num(summary_row$alderman_mean_processing_time_sd),
    fmt_num(summary_row$alderman_mean_processing_time_iqr),
    fmt_num(summary_row$alderman_mean_processing_time_min),
    fmt_num(summary_row$alderman_mean_processing_time_max)
  )
)

write_csv(summary_row, summary_output)
write_csv(alderman_stats, alderman_output)
write_tabular_tex(summary_tex, summary_tex_output)

p <- ggplot(alderman_stats, aes(x = mean_processing_time)) +
  geom_histogram(bins = 18, fill = "#2c7fb8", color = "white", linewidth = 0.2) +
  geom_vline(
    xintercept = mean(alderman_stats$mean_processing_time, na.rm = TRUE),
    linetype = "dashed",
    color = "#d95f0e",
    linewidth = 0.8
  ) +
  labs(
    title = paste0(variant_label, " permit processing times by alderman"),
    subtitle = sprintf("Distribution of alderman-level mean processing times (N = %d aldermen)", nrow(alderman_stats)),
    x = "Mean processing time (days)",
    y = "Number of aldermen"
  ) +
  theme_minimal(base_size = 11)

ggsave(plot_output, p, width = 8, height = 5.2, device = "pdf", bg = "white")

message("Saved new-construction processing-time summaries:")
message("  Variant: ", variant_id, " (", variant_label, ")")
message("  Summary CSV: ", summary_output)
message("  Summary TEX: ", summary_tex_output)
message("  Alderman CSV: ", alderman_output)
message("  Plot: ", plot_output)
