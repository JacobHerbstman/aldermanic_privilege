# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/permit_summary_stats/code")
# max_application_ym <- "2022-12"

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(max_application_ym)
}

if (length(cli_args) != 1) {
  stop("FATAL: Script requires 1 arg: <max_application_ym>.", call. = FALSE)
}
max_application_ym <- cli_args[1]

group_levels <- c("High-Discretion", "Low-Discretion")
signs_permit_type <- "PERMIT - SIGNS"
alderman_row_levels <- c(
  "Total permits in sample",
  "Mean processing time (days)",
  "Median processing time (days)",
  "Alderman-level mean of processing time",
  "Alderman-level IQR of mean processing time",
  "Number of unique wards",
  "Number of unique aldermen"
)
integer_rows <- c(
  "Total permits in sample",
  "Number of unique wards",
  "Number of unique aldermen"
)

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)

alderman_panel <- read_csv("../input/chicago_alderman_panel.csv", show_col_types = FALSE) %>%
  mutate(
    month = as.yearmon(month),
    alderman = gsub("\\s+", " ", trimws(as.character(alderman)))
  )
if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

permits <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
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

ward_geoms_2014 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

ward_geoms_2016 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

permits_pre_2015_ward <- permits %>%
  filter(application_start_date_ym < as.yearmon("2015-05")) %>%
  st_join(ward_geoms_2014, join = st_within) %>%
  filter(!is.na(ward)) %>%
  st_drop_geometry()

permits_post_2015_ward <- permits %>%
  filter(application_start_date_ym >= as.yearmon("2015-05")) %>%
  st_join(ward_geoms_2016, join = st_within) %>%
  filter(!is.na(ward)) %>%
  st_drop_geometry()

permits_with_ward <- bind_rows(permits_pre_2015_ward, permits_post_2015_ward)
if (anyDuplicated(permits_with_ward$id) > 0) {
  stop("Ward spatial join assigned at least one permit to multiple wards.", call. = FALSE)
}

permits_analysis_all <- permits_with_ward %>%
  mutate(ward = as.integer(ward)) %>%
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month"), relationship = "many-to-one") %>%
  mutate(alderman = gsub("\\s+", " ", trimws(as.character(alderman)))) %>%
  filter(!is.na(alderman), alderman != "")

permits_analysis <- permits_analysis_all %>%
  filter(high_discretion == 1 | permit_type != signs_permit_type) %>%
  mutate(
    group = if_else(high_discretion == 1L, "High-Discretion", "Low-Discretion"),
    group = factor(group, levels = group_levels)
  )

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

alderman_distribution <- alderman_means %>%
  group_by(group) %>%
  summarise(
    entity_level_mean_processing_time = mean(alderman_mean_processing_time, na.rm = TRUE),
    entity_level_iqr_mean_processing_time = as.numeric(IQR(alderman_mean_processing_time, na.rm = TRUE)),
    .groups = "drop"
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

alderman_summary_tex_lines <- c(
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Statistic & High-Discretion & Low-Discretion \\\\",
  "\\midrule"
)

for (j in seq_len(nrow(alderman_summary_table))) {
  row_i <- alderman_summary_table[j, ]
  high_value <- row_i$`High-Discretion`
  low_value <- row_i$`Low-Discretion`

  if (row_i$row %in% integer_rows) {
    high_text <- ifelse(is.finite(high_value), format(round(high_value), big.mark = ","), "")
    low_text <- ifelse(is.finite(low_value), format(round(low_value), big.mark = ","), "")
  } else {
    high_text <- ifelse(is.finite(high_value), formatC(high_value, format = "f", digits = 1, big.mark = ","), "")
    low_text <- ifelse(is.finite(low_value), formatC(low_value, format = "f", digits = 1, big.mark = ","), "")
  }

  alderman_summary_tex_lines <- c(
    alderman_summary_tex_lines,
    sprintf("%s & %s & %s \\\\", row_i$row, high_text, low_text)
  )
}

alderman_summary_tex_lines <- c(alderman_summary_tex_lines, "\\bottomrule", "\\end{tabular}")
writeLines(alderman_summary_tex_lines, "../output/permit_processing_time_high_vs_low_alderman_summary.tex")

p_density <- ggplot(
  alderman_means,
  aes(x = alderman_mean_processing_time, color = group, fill = group)
) +
  geom_density(alpha = 0.28, linewidth = 0.95, adjust = 1.1) +
  scale_color_manual(values = c("High-Discretion" = "#b74d2c", "Low-Discretion" = "#1f3c4a")) +
  scale_fill_manual(values = c("High-Discretion" = "#b74d2c", "Low-Discretion" = "#1f3c4a")) +
  theme_bw(base_size = 12) +
  labs(
    title = "Alderman-Level Mean Permit Processing Times",
    subtitle = NULL,
    x = "Alderman-level mean processing time (days)",
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
  filename = "../output/alderman_mean_processing_time_high_vs_low_density.pdf",
  plot = p_density,
  width = 8.5,
  height = 5.5,
  dpi = 300
)

correlation_table <- bind_rows(
  permits_analysis_all %>% filter(high_discretion == 1) %>% mutate(group = "High-Discretion"),
  permits_analysis_all %>% filter(high_discretion == 0, permit_type != signs_permit_type) %>% mutate(group = "Low-Discretion"),
  permits_analysis_all %>% mutate(group = "All")
) %>%
  mutate(group = factor(group, levels = c(group_levels, "All"))) %>%
  group_by(group, ward, application_start_date_ym) %>%
  summarise(
    mean_processing_time = mean(processing_time),
    ward_month_permit_volume = n(),
    .groups = "drop"
  ) %>%
  group_by(group) %>%
  summarise(
    `Corr. of mean processing time and ward-month permit volume` = cor(
      mean_processing_time,
      ward_month_permit_volume,
      use = "complete.obs",
      method = "pearson"
    ),
    .groups = "drop"
  ) %>%
  arrange(group)

correlation_tex_lines <- c(
  "\\begin{tabular}{lr}",
  "\\toprule",
  "Permit group & Corr. of mean processing time and ward-month permit volume \\\\",
  "\\midrule"
)

for (i in seq_len(nrow(correlation_table))) {
  correlation_value_i <- correlation_table$`Corr. of mean processing time and ward-month permit volume`[i]
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
writeLines(correlation_tex_lines, "../output/permit_processing_time_volume_correlation.tex")
