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
