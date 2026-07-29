# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/spatial_rd_multifamily_threshold/code")
# min_units_values <- "2 3 5"
# prune_specs <- "main pruned"
# pd_screens <- "allzoning nopd"

source("../../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(min_units_values, prune_specs, pd_screens)
}

if (length(cli_args) != 3) {
  stop("FATAL: Script requires args: <min_units_values> <prune_specs> <pd_screens>", call. = FALSE)
}

min_units_values <- as.integer(strsplit(trimws(cli_args[1]), "\\s+")[[1]])
prune_specs <- strsplit(trimws(cli_args[2]), "\\s+")[[1]]
pd_screens <- strsplit(trimws(cli_args[3]), "\\s+")[[1]]

if (any(!is.finite(min_units_values)) || any(min_units_values < 2)) {
  stop("min_units_values must parse to integers >= 2.", call. = FALSE)
}
if (!all(prune_specs %in% c("main", "pruned"))) {
  stop("prune_specs must be drawn from: main, pruned.", call. = FALSE)
}
if (!all(pd_screens %in% c("allzoning", "nopd"))) {
  stop("pd_screens must be drawn from: allzoning, nopd.", call. = FALSE)
}

summary_parts <- list()
for (pd_screen in pd_screens) {
  pd_suffix <- if_else(pd_screen == "nopd", "_nopd", "")

  for (min_units in min_units_values) {
    for (prune_spec in prune_specs) {
      summary_path <- sprintf(
        "../temp/fe_summary_500ft_minunits%d%s_zonegroup_segment_year_additive_clust_ward_pair_%s.csv",
        min_units,
        pd_suffix,
        prune_spec
      )
      if (!file.exists(summary_path)) {
        stop(sprintf("Missing summary file: %s", summary_path), call. = FALSE)
      }

      summary_parts[[length(summary_parts) + 1L]] <- read_csv(summary_path, show_col_types = FALSE) %>%
        mutate(
          min_units = min_units,
          pd_screen = pd_screen,
          prune_spec = prune_spec,
          multifamily_definition = sprintf("%d+ units", min_units),
          pd_label = if_else(pd_screen == "nopd", "Excluding PD", "All zoning")
        )
    }
  }
}

combined_raw <- bind_rows(summary_parts) %>%
  filter(yvar %in% c("log(density_far)", "log(density_dupac)")) %>%
  mutate(
    outcome_short = case_when(
      yvar == "log(density_far)" ~ "ln(FAR)",
      yvar == "log(density_dupac)" ~ "ln(DUPAC)",
      TRUE ~ yvar
    ),
    prune_label = if_else(prune_spec == "main", "Main", "Pruned"),
    effect_pct = 100 * (exp(estimate) - 1),
    ci_low_pct = 100 * (exp(ci_low) - 1),
    ci_high_pct = 100 * (exp(ci_high) - 1)
  )

expected_grid <- tidyr::expand_grid(
  pd_screen = pd_screens,
  min_units = min_units_values,
  prune_spec = prune_specs,
  yvar = c("log(density_far)", "log(density_dupac)")
)

combined_keys <- combined_raw %>%
  distinct(pd_screen, min_units, prune_spec, yvar)

missing_grid <- anti_join(expected_grid, combined_keys, by = c("pd_screen", "min_units", "prune_spec", "yvar"))
duplicate_grid <- combined_raw %>%
  count(pd_screen, min_units, prune_spec, yvar) %>%
  filter(n > 1)

if (nrow(missing_grid) > 0) {
  stop("Missing threshold specification rows in combined summaries.", call. = FALSE)
}
if (nrow(duplicate_grid) > 0) {
  stop("Duplicate threshold specification rows in combined summaries.", call. = FALSE)
}

baseline <- combined_raw %>%
  filter(min_units == min(min_units_values)) %>%
  select(
    pd_screen,
    prune_spec,
    yvar,
    baseline_estimate = estimate,
    baseline_n_obs = n_obs,
    baseline_n_ward_pairs = n_ward_pairs
  )

if (anyDuplicated(baseline[c("pd_screen", "prune_spec", "yvar")]) > 0) {
  stop("Baseline threshold rows are not unique.", call. = FALSE)
}

combined <- combined_raw %>%
  left_join(baseline, by = c("pd_screen", "prune_spec", "yvar"), relationship = "many-to-one") %>%
  mutate(
    delta_vs_2plus = estimate - baseline_estimate,
    n_share_vs_2plus = n_obs / baseline_n_obs,
    ward_pair_share_vs_2plus = n_ward_pairs / baseline_n_ward_pairs
  ) %>%
  arrange(pd_screen, prune_spec, yvar, min_units)

write_csv(combined, "../output/spatial_rd_multifamily_threshold_summary.csv")

table_data <- combined %>%
  mutate(
    star_text = case_when(
      !is.finite(p_value) ~ "",
      p_value <= 0.01 ~ "***",
      p_value <= 0.05 ~ "**",
      p_value <= 0.1 ~ "*",
      TRUE ~ ""
    ),
    estimate_text = if_else(
      star_text == "",
      sprintf("%.3f", estimate),
      sprintf("%.3f$^{%s}$", estimate, star_text)
    ),
    se_text = sprintf("(%.3f)", se),
    n_text = trimws(format(n_obs, big.mark = ",")),
    pd_order = match(pd_screen, c("allzoning", "nopd")),
    prune_order = match(prune_spec, c("main", "pruned")),
    outcome_order = match(yvar, c("log(density_far)", "log(density_dupac)"))
  ) %>%
  arrange(pd_order, prune_order, outcome_order, min_units)

table_lines <- c(
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lllccc}",
  "   \\toprule",
  "   Zoning screen & Sample screen & Outcome & 2+ units & 3+ units & 5+ units \\\\",
  "   \\midrule"
)

for (pd_screen in c("allzoning", "nopd")) {
  for (prune_spec in c("main", "pruned")) {
    for (yvar in c("log(density_far)", "log(density_dupac)")) {
      row_data <- table_data %>%
        filter(
          .data$pd_screen == .env$pd_screen,
          .data$prune_spec == .env$prune_spec,
          .data$yvar == .env$yvar
        ) %>%
        arrange(min_units)

      if (nrow(row_data) != length(min_units_values)) {
        stop("Unexpected row count while building threshold table.", call. = FALSE)
      }

      table_lines <- c(
        table_lines,
        paste0(
          "   ",
          row_data$pd_label[[1]],
          " & ",
          row_data$prune_label[[1]],
          " & ",
          row_data$outcome_short[[1]],
          " & ",
          paste(row_data$estimate_text, collapse = " & "),
          " \\\\"
        ),
        paste0("            &      &          & ", paste(row_data$se_text, collapse = " & "), " \\\\"),
        paste0("            &      & N        & ", paste(row_data$n_text, collapse = " & "), " \\\\")
      )
    }
  }
  if (pd_screen == "allzoning") {
    table_lines <- c(table_lines, "   \\midrule")
  }
}

table_lines <- c(
  table_lines,
  "   \\bottomrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

writeLines(table_lines, "../output/spatial_rd_multifamily_threshold_table.tex")

plot_df <- combined %>%
  mutate(
    min_units_label = factor(sprintf("%d+", min_units), levels = sprintf("%d+", min_units_values)),
    outcome_short = factor(outcome_short, levels = c("ln(FAR)", "ln(DUPAC)")),
    pd_label = factor(pd_label, levels = c("All zoning", "Excluding PD")),
    prune_label = factor(prune_label, levels = c("Main", "Pruned"))
  )

threshold_plot <- ggplot(plot_df, aes(x = min_units_label, y = estimate, color = outcome_short, group = outcome_short)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray45") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, linewidth = 0.45) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_grid(pd_label ~ prune_label) +
  scale_color_manual(values = c("ln(FAR)" = "#1f77b4", "ln(DUPAC)" = "#d62728"), name = NULL) +
  labs(
    title = "Main Spatial RD Estimates by Multifamily Unit Threshold",
    subtitle = "500ft bandwidth, zoning-group FE, segment FE, year FE, ward-pair clustered SEs; all zoning vs excluding PD",
    x = "Multifamily definition",
    y = "Coefficient on Stringency Index"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  "../output/spatial_rd_multifamily_threshold_estimates.pdf",
  plot = threshold_plot,
  width = 8.4,
  height = 6.4,
  dpi = 300
)
