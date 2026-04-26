library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/border_pair_FE_bandwidth_path_ambiguity_compare/code")
# input_dir <- "../temp"
# combined_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare.csv"
# far_multifamily_main_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_far_multifamily_main.pdf"
# far_multifamily_pruned_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_far_multifamily_pruned.pdf"
# far_all_main_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_far_all_main.pdf"
# far_all_pruned_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_far_all_pruned.pdf"
# dupac_multifamily_main_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_dupac_multifamily_main.pdf"
# dupac_multifamily_pruned_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_dupac_multifamily_pruned.pdf"
# dupac_all_main_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_dupac_all_main.pdf"
# dupac_all_pruned_output <- "../output/border_pair_fe_bandwidth_path_ambiguity_compare_log_density_dupac_all_pruned.pdf"
# bandwidths <- "100 150 200 250 300 350 400 450 500 550 600 650 700 750 800 850 900 950 1000"
# samples <- "multifamily all"
# prune_specs <- "main pruned"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    input_dir,
    combined_output,
    far_multifamily_main_output,
    far_multifamily_pruned_output,
    far_all_main_output,
    far_all_pruned_output,
    dupac_multifamily_main_output,
    dupac_multifamily_pruned_output,
    dupac_all_main_output,
    dupac_all_pruned_output,
    bandwidths,
    samples,
    prune_specs
  )
}

if (length(args) != 13) {
  stop(
    "FATAL: Script requires 13 args: <input_dir> <combined_output> <far_multifamily_main_output> <far_multifamily_pruned_output> <far_all_main_output> <far_all_pruned_output> <dupac_multifamily_main_output> <dupac_multifamily_pruned_output> <dupac_all_main_output> <dupac_all_pruned_output> <bandwidths> <samples> <prune_specs>",
    call. = FALSE
  )
}

input_dir <- args[1]
combined_output <- args[2]
far_multifamily_main_output <- args[3]
far_multifamily_pruned_output <- args[4]
far_all_main_output <- args[5]
far_all_pruned_output <- args[6]
dupac_multifamily_main_output <- args[7]
dupac_multifamily_pruned_output <- args[8]
dupac_all_main_output <- args[9]
dupac_all_pruned_output <- args[10]
bandwidths <- as.integer(strsplit(trimws(args[11]), "\\s+")[[1]])
samples <- strsplit(trimws(args[12]), "\\s+")[[1]]
prune_specs <- strsplit(trimws(args[13]), "\\s+")[[1]]

if (any(!is.finite(bandwidths))) {
  stop("bandwidths must parse to integers.", call. = FALSE)
}
if (!all(samples %in% c("multifamily", "all"))) {
  stop("samples must be drawn from: multifamily, all", call. = FALSE)
}
if (!all(prune_specs %in% c("main", "pruned"))) {
  stop("prune_specs must be drawn from: main, pruned", call. = FALSE)
}

summary_files <- unlist(lapply(bandwidths, function(bandwidth) {
  unlist(lapply(samples, function(sample_filter) {
    unlist(lapply(prune_specs, function(prune_spec) {
      c(
        file.path(
          input_dir,
          sprintf(
            "border_pair_fe_bandwidth_path_bw%d_%s_%s_baseline.csv",
            as.integer(bandwidth),
            sample_filter,
            prune_spec
          )
        ),
        file.path(
          input_dir,
          sprintf(
            "border_pair_fe_bandwidth_path_bw%d_%s_%s_corner_clean.csv",
            as.integer(bandwidth),
            sample_filter,
            prune_spec
          )
        )
      )
    }))
  }))
}))

if (!all(file.exists(summary_files))) {
  stop(sprintf("Missing summary files: %s", paste(summary_files[!file.exists(summary_files)], collapse = ", ")), call. = FALSE)
}

combined <- bind_rows(lapply(summary_files, read_csv, show_col_types = FALSE)) %>%
  mutate(
    bw_ft = as.integer(bw_ft),
    sample_filter = as.character(sample_filter),
    prune_sample = as.character(prune_sample),
    yvar = as.character(yvar),
    drop_ambiguous_within_bw = as.logical(drop_ambiguous_within_bw),
    prune_spec = case_when(
      prune_sample == "pruned" ~ "pruned",
      TRUE ~ "main"
    ),
    comparison_spec = case_when(
      drop_ambiguous_within_bw ~ "corner_clean",
      TRUE ~ "baseline"
    )
  ) %>%
  filter(yvar %in% c("log(density_far)", "log(density_dupac)")) %>%
  arrange(yvar, sample_filter, prune_spec, comparison_spec, bw_ft)

expected_grid <- expand_grid(
  bw_ft = bandwidths,
  sample_filter = samples,
  prune_spec = prune_specs,
  comparison_spec = c("baseline", "corner_clean"),
  yvar = c("log(density_far)", "log(density_dupac)")
)

combined_keys <- combined %>%
  distinct(bw_ft, sample_filter, prune_spec, comparison_spec, yvar)

missing_grid <- anti_join(
  expected_grid,
  combined_keys,
  by = c("bw_ft", "sample_filter", "prune_spec", "comparison_spec", "yvar")
)

duplicate_grid <- combined %>%
  count(bw_ft, sample_filter, prune_spec, comparison_spec, yvar) %>%
  filter(n > 1)

if (nrow(missing_grid) > 0) {
  stop("Missing ambiguity-compare bandwidth-path combinations.", call. = FALSE)
}
if (nrow(duplicate_grid) > 0) {
  stop("Duplicate ambiguity-compare bandwidth-path combinations.", call. = FALSE)
}

write_csv(combined, combined_output)

plot_df <- combined %>%
  mutate(
    outcome_label = case_when(
      yvar == "log(density_far)" ~ "ln(FAR)",
      yvar == "log(density_dupac)" ~ "ln(DUPAC)"
    ),
    sample_label = case_when(
      sample_filter == "multifamily" ~ "Multifamily",
      sample_filter == "all" ~ "All Construction"
    ),
    comparison_label = case_when(
      comparison_spec == "baseline" ~ "Baseline",
      comparison_spec == "corner_clean" ~ "Corner-Clean"
    )
  )

plot_specs <- tribble(
  ~yvar, ~sample_filter, ~prune_spec, ~output_path,
  "log(density_far)", "multifamily", "main", far_multifamily_main_output,
  "log(density_far)", "multifamily", "pruned", far_multifamily_pruned_output,
  "log(density_far)", "all", "main", far_all_main_output,
  "log(density_far)", "all", "pruned", far_all_pruned_output,
  "log(density_dupac)", "multifamily", "main", dupac_multifamily_main_output,
  "log(density_dupac)", "multifamily", "pruned", dupac_multifamily_pruned_output,
  "log(density_dupac)", "all", "main", dupac_all_main_output,
  "log(density_dupac)", "all", "pruned", dupac_all_pruned_output
)

for (i in seq_len(nrow(plot_specs))) {
  yvar_i <- plot_specs$yvar[i]
  sample_i <- plot_specs$sample_filter[i]
  prune_i <- plot_specs$prune_spec[i]
  output_path <- plot_specs$output_path[i]

  plot_data <- plot_df %>%
    filter(yvar == yvar_i, sample_filter == sample_i, prune_spec == prune_i)

  prune_label <- if_else(prune_i == "main", "Main", "Pruned")
  baseline_color <- if_else(prune_i == "main", "#1f77b4", "#ff7f0e")

  p <- ggplot() +
    geom_ribbon(
      data = plot_data %>% filter(comparison_spec == "baseline"),
      aes(x = bw_ft, ymin = ci_low, ymax = ci_high),
      alpha = 0.16,
      fill = baseline_color,
      color = NA
    ) +
    geom_line(
      data = plot_data %>% filter(comparison_spec == "baseline"),
      aes(x = bw_ft, y = estimate),
      linewidth = 1.0,
      color = baseline_color
    ) +
    geom_point(
      data = plot_data %>% filter(comparison_spec == "baseline"),
      aes(x = bw_ft, y = estimate),
      size = 1.7,
      color = baseline_color
    ) +
    geom_ribbon(
      data = plot_data %>% filter(comparison_spec == "corner_clean"),
      aes(x = bw_ft, ymin = ci_low, ymax = ci_high),
      alpha = 0.20,
      fill = "#bdbdbd",
      color = NA
    ) +
    geom_line(
      data = plot_data %>% filter(comparison_spec == "corner_clean"),
      aes(x = bw_ft, y = estimate),
      linewidth = 1.0,
      color = "black",
      linetype = "dashed"
    ) +
    geom_point(
      data = plot_data %>% filter(comparison_spec == "corner_clean"),
      aes(x = bw_ft, y = estimate),
      size = 1.7,
      color = "black"
    ) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
    geom_vline(xintercept = 250, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_vline(xintercept = 500, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    scale_x_continuous(breaks = seq(min(bandwidths), max(bandwidths), by = 100)) +
    labs(
      title = sprintf("Bandwidth Path: %s, %s, %s", plot_data$outcome_label[[1]], plot_data$sample_label[[1]], prune_label),
      subtitle = "Zoning-group FE, segment FE, year FE, side-specific distance slopes, ward-pair clustered SEs",
      x = "Bandwidth (ft)",
      y = "Coefficient on Stringency Index"
    ) +
    annotate("text", x = max(bandwidths), y = max(plot_data$ci_high, na.rm = TRUE), label = "Baseline", hjust = 1, vjust = -0.4, color = baseline_color, size = 3.3) +
    annotate("text", x = max(bandwidths), y = max(plot_data$ci_high, na.rm = TRUE) - 0.08 * diff(range(c(plot_data$ci_low, plot_data$ci_high), na.rm = TRUE)), label = "Corner-Clean", hjust = 1, vjust = -0.4, color = "black", size = 3.3) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold")
    )

  ggsave(output_path, plot = p, width = 8.4, height = 5.6, dpi = 300)
}

message(sprintf("Built %s", combined_output))
message(sprintf("Built %s", far_multifamily_main_output))
message(sprintf("Built %s", far_multifamily_pruned_output))
message(sprintf("Built %s", far_all_main_output))
message(sprintf("Built %s", far_all_pruned_output))
message(sprintf("Built %s", dupac_multifamily_main_output))
message(sprintf("Built %s", dupac_multifamily_pruned_output))
message(sprintf("Built %s", dupac_all_main_output))
message(sprintf("Built %s", dupac_all_pruned_output))
