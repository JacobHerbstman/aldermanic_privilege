source("../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_summary_stats/code")
# clean_permits_input <- "../input/building_permits_clean.gpkg"
# ward_panel_input <- "../input/ward_panel.gpkg"
# alderman_panel_input <- "../input/chicago_alderman_panel.csv"
# ward_summary_csv_output <- "../output/permit_processing_time_high_vs_low_summary.csv"
# ward_summary_tex_output <- "../output/permit_processing_time_high_vs_low_summary.tex"
# ward_means_output <- "../output/ward_mean_processing_time_high_vs_low.csv"
# ward_figure_output <- "../output/ward_mean_processing_time_high_vs_low_density.pdf"
# alderman_summary_csv_output <- "../output/permit_processing_time_high_vs_low_alderman_summary.csv"
# alderman_summary_tex_output <- "../output/permit_processing_time_high_vs_low_alderman_summary.tex"
# alderman_means_output <- "../output/alderman_mean_processing_time_high_vs_low.csv"
# alderman_figure_output <- "../output/alderman_mean_processing_time_high_vs_low_density.pdf"
# correlation_csv_output <- "../output/permit_processing_time_volume_correlation.csv"
# correlation_tex_output <- "../output/permit_processing_time_volume_correlation.tex"
# max_application_ym <- "2022-12"

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(clean_permits_input, ward_panel_input, alderman_panel_input, ward_summary_csv_output, ward_summary_tex_output, ward_means_output, ward_figure_output, alderman_summary_csv_output, alderman_summary_tex_output, alderman_means_output, alderman_figure_output, correlation_csv_output, correlation_tex_output, max_application_ym)
}

if (length(cli_args) != 14) {
  stop(
    "FATAL: Script requires 14 args: <clean_permits_input> <ward_panel_input> <alderman_panel_input> <ward_summary_csv_output> <ward_summary_tex_output> <ward_means_output> <ward_figure_output> <alderman_summary_csv_output> <alderman_summary_tex_output> <alderman_means_output> <alderman_figure_output> <correlation_csv_output> <correlation_tex_output> <max_application_ym>",
    call. = FALSE
  )
}
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
max_application_ym <- cli_args[14]

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
integer_rows <- c(
  "Total permits in sample",
  "Number of unique wards",
  "Number of unique aldermen"
)

ward_panel <- st_read(ward_panel_input, quiet = TRUE)

alderman_panel <- read_csv(alderman_panel_input, show_col_types = FALSE) %>%
  mutate(
    month = as.yearmon(month),
    alderman = gsub("\\s+", " ", trimws(as.character(alderman)))
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
    processing_time > 0,
    application_start_date_ym <= as.yearmon(max_application_ym)
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

permits_p1 <- permits %>%
  filter(application_start_date_ym < as.yearmon("2015-05"))
permits_p2 <- permits %>%
  filter(application_start_date_ym >= as.yearmon("2015-05"))

if (nrow(permits_p1) == 0) {
  permits_with_ward_p1 <- permits_p1 %>% st_drop_geometry()
} else {
  permits_with_ward_p1 <- st_join(permits_p1, ward_geoms_map1, join = st_within) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}

if (nrow(permits_p2) == 0) {
  permits_with_ward_p2 <- permits_p2 %>% st_drop_geometry()
} else {
  permits_with_ward_p2 <- st_join(permits_p2, ward_geoms_map2, join = st_within) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}

permits_with_ward <- bind_rows(
  permits_with_ward_p1,
  permits_with_ward_p2
)

permits_analysis_all <- permits_with_ward %>%
  mutate(ward = as.integer(ward)) %>%
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month")) %>%
  mutate(alderman = gsub("\\s+", " ", trimws(as.character(alderman)))) %>%
  filter(!is.na(alderman), alderman != "")

permits_analysis <- permits_analysis_all %>%
  filter(high_discretion == 1 | permit_type != signs_permit_type) %>%
  mutate(group = recode(as.character(high_discretion), !!!group_labels)) %>%
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
    entity_level_iqr_mean_processing_time = as.numeric(IQR(ward_mean_processing_time, na.rm = TRUE)),
    .groups = "drop"
  )

ward_row_levels <- c(
  base_row_levels[1:3],
  "Ward-level mean of processing time",
  "Ward-level IQR of mean processing time",
  base_row_levels[4:5]
)

ward_summary_table <- bind_rows(
  tibble(row = "Total permits in sample", group = summary_stats$group, value = summary_stats$total_permits),
  tibble(row = "Mean processing time (days)", group = summary_stats$group, value = summary_stats$mean_processing_time),
  tibble(row = "Median processing time (days)", group = summary_stats$group, value = summary_stats$median_processing_time),
  tibble(
    row = "Ward-level mean of processing time",
    group = ward_distribution$group,
    value = ward_distribution$entity_level_mean_processing_time
  ),
  tibble(
    row = "Ward-level IQR of mean processing time",
    group = ward_distribution$group,
    value = ward_distribution$entity_level_iqr_mean_processing_time
  ),
  tibble(row = "Number of unique wards", group = summary_stats$group, value = summary_stats$n_unique_wards),
  tibble(row = "Number of unique aldermen", group = summary_stats$group, value = summary_stats$n_unique_aldermen)
) %>%
  mutate(
    row = factor(row, levels = ward_row_levels),
    group = factor(group, levels = group_levels)
  ) %>%
  arrange(row, group) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(row = as.character(row))

write_csv(ward_summary_table, ward_summary_csv_output)

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
    entity_level_iqr_mean_processing_time = as.numeric(IQR(alderman_mean_processing_time, na.rm = TRUE)),
    .groups = "drop"
  )

alderman_row_levels <- c(
  base_row_levels[1:3],
  "Alderman-level mean of processing time",
  "Alderman-level IQR of mean processing time",
  base_row_levels[4:5]
)

alderman_summary_table <- bind_rows(
  tibble(row = "Total permits in sample", group = summary_stats$group, value = summary_stats$total_permits),
  tibble(row = "Mean processing time (days)", group = summary_stats$group, value = summary_stats$mean_processing_time),
  tibble(row = "Median processing time (days)", group = summary_stats$group, value = summary_stats$median_processing_time),
  tibble(
    row = "Alderman-level mean of processing time",
    group = alderman_distribution$group,
    value = alderman_distribution$entity_level_mean_processing_time
  ),
  tibble(
    row = "Alderman-level IQR of mean processing time",
    group = alderman_distribution$group,
    value = alderman_distribution$entity_level_iqr_mean_processing_time
  ),
  tibble(row = "Number of unique wards", group = summary_stats$group, value = summary_stats$n_unique_wards),
  tibble(row = "Number of unique aldermen", group = summary_stats$group, value = summary_stats$n_unique_aldermen)
) %>%
  mutate(
    row = factor(row, levels = alderman_row_levels),
    group = factor(group, levels = group_levels)
  ) %>%
  arrange(row, group) %>%
  pivot_wider(names_from = group, values_from = value) %>%
  mutate(row = as.character(row))

write_csv(alderman_summary_table, alderman_summary_csv_output)

summary_tex_specs <- list(
  list(summary_table = ward_summary_table, output_path = ward_summary_tex_output),
  list(summary_table = alderman_summary_table, output_path = alderman_summary_tex_output)
)

for (i in seq_along(summary_tex_specs)) {
  summary_table_i <- summary_tex_specs[[i]]$summary_table
  tex_lines_i <- c(
    "\\begin{tabular}{lrr}",
    "\\toprule",
    "Statistic & High-Discretion & Low-Discretion \\\\",
    "\\midrule"
  )

  for (j in seq_len(nrow(summary_table_i))) {
    row_i <- summary_table_i[j, ]
    high_value <- row_i$`High-Discretion`
    low_value <- row_i$`Low-Discretion`

    if (row_i$row %in% integer_rows) {
      high_text <- ifelse(is.finite(high_value), format(round(high_value), big.mark = ","), "")
      low_text <- ifelse(is.finite(low_value), format(round(low_value), big.mark = ","), "")
    } else {
      high_text <- ifelse(is.finite(high_value), formatC(high_value, format = "f", digits = 1, big.mark = ","), "")
      low_text <- ifelse(is.finite(low_value), formatC(low_value, format = "f", digits = 1, big.mark = ","), "")
    }

    tex_lines_i <- c(
      tex_lines_i,
      sprintf("%s & %s & %s \\\\", row_i$row, high_text, low_text)
    )
  }

  tex_lines_i <- c(tex_lines_i, "\\bottomrule", "\\end{tabular}")
  writeLines(tex_lines_i, con = summary_tex_specs[[i]]$output_path)
}

plot_specs <- list(
  list(
    entity_means = ward_means,
    entity_label = "Ward",
    mean_col = "ward_mean_processing_time",
    figure_output = ward_figure_output
  ),
  list(
    entity_means = alderman_means,
    entity_label = "Alderman",
    mean_col = "alderman_mean_processing_time",
    figure_output = alderman_figure_output
  )
)

for (i in seq_along(plot_specs)) {
  entity_means_i <- plot_specs[[i]]$entity_means
  entity_label_i <- plot_specs[[i]]$entity_label
  mean_col_i <- plot_specs[[i]]$mean_col

  p_density_i <- ggplot(
    entity_means_i,
    aes(x = .data[[mean_col_i]], color = group, fill = group)
  ) +
    geom_density(alpha = 0.28, linewidth = 0.95, adjust = 1.1) +
    scale_color_manual(values = c("High-Discretion" = "#b74d2c", "Low-Discretion" = "#1f3c4a")) +
    scale_fill_manual(values = c("High-Discretion" = "#b74d2c", "Low-Discretion" = "#1f3c4a")) +
    theme_bw(base_size = 12) +
    labs(
      title = paste0(entity_label_i, "-Level Mean Permit Processing Times"),
      subtitle = NULL,
      x = paste0(entity_label_i, "-level mean processing time (days)"),
      y = "Density",
      color = NULL,
      fill = NULL
    ) +
    theme(
      legend.position = "bottom",
      strip.background = element_rect(fill = "white"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = plot_specs[[i]]$figure_output,
    plot = p_density_i,
    width = 8.5,
    height = 5.5,
    dpi = 300
  )
}

permits_high_with_volume <- permits_analysis_all %>%
  filter(high_discretion == 1) %>%
  group_by(ward, application_start_date_ym) %>%
  mutate(ward_month_permit_volume = n()) %>%
  ungroup()

permits_low_with_volume <- permits_analysis_all %>%
  filter(high_discretion == 0, permit_type != signs_permit_type) %>%
  group_by(ward, application_start_date_ym) %>%
  mutate(ward_month_permit_volume = n()) %>%
  ungroup()

permits_all_with_volume <- permits_analysis_all %>%
  group_by(ward, application_start_date_ym) %>%
  mutate(ward_month_permit_volume = n()) %>%
  ungroup()

correlation_table <- bind_rows(
  tibble(
    group = "High-Discretion",
    `Corr. of processing time and ward-month permit volume` = cor(
      permits_high_with_volume$processing_time,
      permits_high_with_volume$ward_month_permit_volume,
      use = "complete.obs",
      method = "pearson"
    )
  ),
  tibble(
    group = "Low-Discretion",
    `Corr. of processing time and ward-month permit volume` = cor(
      permits_low_with_volume$processing_time,
      permits_low_with_volume$ward_month_permit_volume,
      use = "complete.obs",
      method = "pearson"
    )
  ),
  tibble(
    group = "All",
    `Corr. of processing time and ward-month permit volume` = cor(
      permits_all_with_volume$processing_time,
      permits_all_with_volume$ward_month_permit_volume,
      use = "complete.obs",
      method = "pearson"
    )
  )
)

write_csv(correlation_table, correlation_csv_output)

correlation_tex_lines <- c(
  "\\begin{tabular}{lr}",
  "\\toprule",
  "Permit group & Corr. of processing time and ward-month permit volume \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(correlation_table))) {
  correlation_value_i <- correlation_table$`Corr. of processing time and ward-month permit volume`[i]
  correlation_text_i <- ifelse(
    is.finite(correlation_value_i),
    formatC(correlation_value_i, format = "f", digits = 3),
    ""
  )
  correlation_tex_lines <- c(
    correlation_tex_lines,
    sprintf("%s & %s \\\\", correlation_table$group[i], correlation_text_i)
  )
}

correlation_tex_lines <- c(correlation_tex_lines, "\\bottomrule", "\\end{tabular}")
writeLines(correlation_tex_lines, correlation_tex_output)
