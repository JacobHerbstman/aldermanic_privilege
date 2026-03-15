source("../../setup_environment/code/packages.R")

# =======================================================================================
# --- Interactive Test Block --- (uncomment to run in RStudio)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_summary_stats/code")
# Rscript permit_processing_time_high_vs_low_summary.R "../input/building_permits_clean.gpkg" "../input/ward_panel.gpkg" "../input/chicago_alderman_panel.csv" "../output/permit_processing_time_high_vs_low_summary.csv" "../output/permit_processing_time_high_vs_low_summary.tex" "../output/ward_mean_processing_time_high_vs_low.csv" "../output/ward_mean_processing_time_high_vs_low_density.pdf" "../output/permit_processing_time_high_vs_low_alderman_summary.csv" "../output/permit_processing_time_high_vs_low_alderman_summary.tex" "../output/alderman_mean_processing_time_high_vs_low.csv" "../output/alderman_mean_processing_time_high_vs_low_density.pdf" "../output/permit_processing_time_volume_correlation.csv" "../output/permit_processing_time_volume_correlation.tex"
# =======================================================================================

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) >= 13) {
  clean_permits_input <- cli_args[1]
  ward_panel_input <- cli_args[2]
  alderman_panel_input <- cli_args[3]
  ward_summary_csv_output <- cli_args[4]
  ward_summary_tex_output <- cli_args[5]
  ward_means_output <- cli_args[6]
  ward_figure_output <- cli_args[7]
  alderman_summary_csv_output <- cli_args[8]
  alderman_summary_tex_output <- cli_args[9]
  alderman_means_output <- cli_args[10]
  alderman_figure_output <- cli_args[11]
  correlation_csv_output <- cli_args[12]
  correlation_tex_output <- cli_args[13]
} else {
  if (!exists("clean_permits_input") ||
      !exists("ward_panel_input") ||
      !exists("alderman_panel_input") ||
      !exists("ward_summary_csv_output") ||
      !exists("ward_summary_tex_output") ||
      !exists("ward_means_output") ||
      !exists("ward_figure_output") ||
      !exists("alderman_summary_csv_output") ||
      !exists("alderman_summary_tex_output") ||
      !exists("alderman_means_output") ||
      !exists("alderman_figure_output") ||
      !exists("correlation_csv_output") ||
      !exists("correlation_tex_output")) {
    stop(
      "FATAL: Script requires 13 args: <clean_permits_input> <ward_panel_input> <alderman_panel_input> <ward_summary_csv_output> <ward_summary_tex_output> <ward_means_output> <ward_figure_output> <alderman_summary_csv_output> <alderman_summary_tex_output> <alderman_means_output> <alderman_figure_output> <correlation_csv_output> <correlation_tex_output>",
      call. = FALSE
    )
  }
}

group_levels <- c("High-Discretion", "Low-Discretion")
group_labels <- c(`1` = "High-Discretion", `0` = "Low-Discretion")
signs_permit_type <- "PERMIT - SIGNS"
base_row_levels <- c(
  "Total permits in sample",
  "Mean processing time (days)",
  "Median processing time (days)",
  "Number of unique wards",
  "Number of unique aldermen"
)

fmt_num <- function(x, digits = 1) {
  ifelse(is.finite(x), formatC(x, format = "f", digits = digits, big.mark = ","), "")
}

fmt_int <- function(x) {
  ifelse(is.finite(x), format(round(x), big.mark = ","), "")
}

fmt_corr <- function(x) {
  ifelse(is.finite(x), formatC(x, format = "f", digits = 3), "")
}

safe_iqr <- function(x) {
  as.numeric(IQR(x, na.rm = TRUE))
}

trim_alderman <- function(x) {
  gsub("\\s+", " ", trimws(as.character(x)))
}

write_summary_tex <- function(summary_table, output_path) {
  summary_tex <- file(output_path, open = "wt")
  writeLines(
    c(
      "\\begin{tabular}{lrr}",
      "\\toprule",
      "Statistic & High-Discretion & Low-Discretion \\\\",
      "\\midrule"
    ),
    con = summary_tex
  )

  integer_rows <- c(
    "Total permits in sample",
    "Number of unique wards",
    "Number of unique aldermen"
  )

  for (i in seq_len(nrow(summary_table))) {
    row_i <- summary_table[i, ]
    formatter <- if (row_i$row %in% integer_rows) fmt_int else fmt_num
    writeLines(
      sprintf(
        "%s & %s & %s \\\\",
        row_i$row,
        formatter(row_i$`High-Discretion`),
        formatter(row_i$`Low-Discretion`)
      ),
      con = summary_tex
    )
  }

  writeLines(c("\\bottomrule", "\\end{tabular}"), con = summary_tex)
  close(summary_tex)
}

write_correlation_tex <- function(correlation_table, output_path) {
  correlation_tex <- file(output_path, open = "wt")
  writeLines(
    c(
      "\\begin{tabular}{lr}",
      "\\toprule",
      "Permit group & Corr. of processing time and ward-month permit volume \\\\",
      "\\midrule"
    ),
    con = correlation_tex
  )

  for (i in seq_len(nrow(correlation_table))) {
    row_i <- correlation_table[i, ]
    writeLines(
      sprintf(
        "%s & %s \\\\",
        row_i$group,
        fmt_corr(row_i$`Corr. of processing time and ward-month permit volume`)
      ),
      con = correlation_tex
    )
  }

  writeLines(c("\\bottomrule", "\\end{tabular}"), con = correlation_tex)
  close(correlation_tex)
}

build_summary_table <- function(summary_stats, entity_distribution, entity_label) {
  row_levels <- c(
    base_row_levels[1:3],
    paste0(entity_label, "-level mean of processing time"),
    paste0(entity_label, "-level IQR of mean processing time"),
    base_row_levels[4:5]
  )

  bind_rows(
    tibble(row = "Total permits in sample", group = summary_stats$group, value = summary_stats$total_permits),
    tibble(row = "Mean processing time (days)", group = summary_stats$group, value = summary_stats$mean_processing_time),
    tibble(row = "Median processing time (days)", group = summary_stats$group, value = summary_stats$median_processing_time),
    tibble(
      row = paste0(entity_label, "-level mean of processing time"),
      group = entity_distribution$group,
      value = entity_distribution$entity_level_mean_processing_time
    ),
    tibble(
      row = paste0(entity_label, "-level IQR of mean processing time"),
      group = entity_distribution$group,
      value = entity_distribution$entity_level_iqr_mean_processing_time
    ),
    tibble(row = "Number of unique wards", group = summary_stats$group, value = summary_stats$n_unique_wards),
    tibble(row = "Number of unique aldermen", group = summary_stats$group, value = summary_stats$n_unique_aldermen)
  ) %>%
    mutate(
      row = factor(row, levels = row_levels),
      group = factor(group, levels = group_levels)
    ) %>%
    arrange(row, group) %>%
    pivot_wider(names_from = group, values_from = value) %>%
    mutate(row = as.character(row))
}

save_density_plot <- function(entity_means, entity_label, mean_col, figure_output) {
  mean_refs <- entity_means %>%
    group_by(group) %>%
    summarise(mean_processing_time = mean(.data[[mean_col]], na.rm = TRUE), .groups = "drop")

  p_density <- ggplot(
    entity_means,
    aes(x = .data[[mean_col]], color = group, fill = group)
  ) +
    geom_density(alpha = 0.28, linewidth = 0.95, adjust = 1.1) +
    scale_color_manual(values = c("High-Discretion" = "#b74d2c", "Low-Discretion" = "#1f3c4a")) +
    scale_fill_manual(values = c("High-Discretion" = "#b74d2c", "Low-Discretion" = "#1f3c4a")) +
    theme_bw(base_size = 12) +
    labs(
      title = paste0(entity_label, "-Level Mean Permit Processing Times"),
      subtitle = NULL,
      x = paste0(entity_label, "-level mean processing time (days)"),
      y = "Density",
      color = NULL,
      fill = NULL
    ) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "white")
    )

  ggsave(
    filename = figure_output,
    plot = p_density,
    width = 8.5,
    height = 5.5,
    dpi = 300
  )
}

build_volume_correlation <- function(permits_data, row_label) {
  permits_with_volume <- permits_data %>%
    group_by(ward, application_start_date_ym) %>%
    mutate(ward_month_permit_volume = n()) %>%
    ungroup()

  tibble(
    group = row_label,
    `Corr. of processing time and ward-month permit volume` = cor(
      permits_with_volume$processing_time,
      permits_with_volume$ward_month_permit_volume,
      use = "complete.obs",
      method = "pearson"
    )
  )
}

ward_panel <- st_read(ward_panel_input, quiet = TRUE)

alderman_panel <- read_csv(alderman_panel_input, show_col_types = FALSE) %>%
  mutate(
    month = as.yearmon(month),
    alderman = trim_alderman(alderman)
  )

permits <- st_read(clean_permits_input, quiet = TRUE) %>%
  select(id, application_start_date_ym, processing_time, high_discretion, permit_type) %>%
  mutate(
    application_start_date_ym = as.yearmon(application_start_date_ym),
    high_discretion = as.integer(high_discretion)
  ) %>%
  filter(
    !is.na(application_start_date_ym),
    !is.na(high_discretion),
    processing_time > 0
  )

if (st_crs(permits) != st_crs(ward_panel)) {
  permits <- st_transform(permits, st_crs(ward_panel))
}

ward_geoms_map1 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

ward_geoms_map2 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

ward_geoms_map3 <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

assign_ward_by_month <- function(permits_data) {
  permits_p1 <- permits_data %>%
    filter(application_start_date_ym < as.yearmon("2015-05"))

  permits_p2 <- permits_data %>%
    filter(
      application_start_date_ym >= as.yearmon("2015-05"),
      application_start_date_ym < as.yearmon("2023-05")
    )

  permits_p3 <- permits_data %>%
    filter(application_start_date_ym >= as.yearmon("2023-05"))

  do_join <- function(pts, polys) {
    if (nrow(pts) == 0) {
      return(pts %>% st_drop_geometry())
    }

    st_join(pts, polys, join = st_within) %>%
      filter(!is.na(ward)) %>%
      st_drop_geometry()
  }

  bind_rows(
    do_join(permits_p1, ward_geoms_map1),
    do_join(permits_p2, ward_geoms_map2),
    do_join(permits_p3, ward_geoms_map3)
  )
}

permits_with_ward <- assign_ward_by_month(permits)

permits_analysis_all <- permits_with_ward %>%
  mutate(
    ward = as.integer(ward)
  ) %>%
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month")) %>%
  mutate(alderman = trim_alderman(alderman)) %>%
  filter(!is.na(alderman), alderman != "")

permits_analysis <- permits_analysis_all %>%
  filter(high_discretion == 1 | permit_type != signs_permit_type) %>%
  mutate(
    group = recode(as.character(high_discretion), !!!group_labels)
  ) %>%
  mutate(group = factor(group, levels = group_levels))

if (!all(group_levels %in% unique(as.character(permits_analysis$group)))) {
  stop("Expected both High-Discretion and Low-Discretion permits in the analysis sample.", call. = FALSE)
}

summary_stats <- permits_analysis %>%
  group_by(group) %>%
  summarise(
    total_permits = n(),
    mean_processing_time = mean(processing_time, na.rm = TRUE),
    median_processing_time = median(processing_time, na.rm = TRUE),
    n_unique_wards = n_distinct(ward),
    n_unique_aldermen = n_distinct(alderman),
    .groups = "drop"
  )

ward_means <- permits_analysis %>%
  group_by(group, ward) %>%
  summarise(
    n_permits = n(),
    ward_mean_processing_time = mean(processing_time, na.rm = TRUE),
    n_unique_aldermen = n_distinct(alderman),
    .groups = "drop"
  ) %>%
  mutate(group = factor(group, levels = group_levels)) %>%
  arrange(group, ward)

write_csv(ward_means, ward_means_output)

ward_distribution <- ward_means %>%
  group_by(group) %>%
  summarise(
    entity_level_mean_processing_time = mean(ward_mean_processing_time, na.rm = TRUE),
    entity_level_iqr_mean_processing_time = safe_iqr(ward_mean_processing_time),
    .groups = "drop"
  )

ward_summary_table <- build_summary_table(
  summary_stats = summary_stats,
  entity_distribution = ward_distribution,
  entity_label = "Ward"
)

write_csv(ward_summary_table, ward_summary_csv_output)
write_summary_tex(ward_summary_table, ward_summary_tex_output)

save_density_plot(
  entity_means = ward_means,
  entity_label = "Ward",
  mean_col = "ward_mean_processing_time",
  figure_output = ward_figure_output
)

alderman_means <- permits_analysis %>%
  group_by(group, alderman) %>%
  summarise(
    n_permits = n(),
    alderman_mean_processing_time = mean(processing_time, na.rm = TRUE),
    n_unique_wards = n_distinct(ward),
    .groups = "drop"
  ) %>%
  mutate(group = factor(group, levels = group_levels)) %>%
  arrange(group, alderman)

write_csv(alderman_means, alderman_means_output)

alderman_distribution <- alderman_means %>%
  group_by(group) %>%
  summarise(
    entity_level_mean_processing_time = mean(alderman_mean_processing_time, na.rm = TRUE),
    entity_level_iqr_mean_processing_time = safe_iqr(alderman_mean_processing_time),
    .groups = "drop"
  )

alderman_summary_table <- build_summary_table(
  summary_stats = summary_stats,
  entity_distribution = alderman_distribution,
  entity_label = "Alderman"
)

write_csv(alderman_summary_table, alderman_summary_csv_output)
write_summary_tex(alderman_summary_table, alderman_summary_tex_output)

save_density_plot(
  entity_means = alderman_means,
  entity_label = "Alderman",
  mean_col = "alderman_mean_processing_time",
  figure_output = alderman_figure_output
)

correlation_table <- bind_rows(
  build_volume_correlation(
    permits_analysis_all %>% filter(high_discretion == 1),
    "High-Discretion"
  ),
  build_volume_correlation(
    permits_analysis_all %>% filter(high_discretion == 0, permit_type != signs_permit_type),
    "Low-Discretion"
  ),
  build_volume_correlation(
    permits_analysis_all,
    "All"
  )
)

write_csv(correlation_table, correlation_csv_output)
write_correlation_tex(correlation_table, correlation_tex_output)
