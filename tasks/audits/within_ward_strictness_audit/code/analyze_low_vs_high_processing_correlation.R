source("../../../setup_environment/code/packages.R")

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/within_ward_strictness_audit/code")
# spec <- "ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022"
# building_permits_input <- "../input/building_permits_clean.gpkg"
# ward_panel_input <- "../input/ward_panel.gpkg"
# alderman_panel_input <- "../input/chicago_alderman_panel.csv"
# output_wide_csv <- "../output/low_vs_high_processing_alderman_wide_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_summary_csv <- "../output/low_vs_high_processing_correlation_summary_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv"
# output_scatter_pdf <- "../output/low_vs_high_processing_scatter_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf"

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  args <- c(
    spec,
    building_permits_input,
    ward_panel_input,
    alderman_panel_input,
    output_wide_csv,
    output_summary_csv,
    output_scatter_pdf
  )
}

if (length(args) != 7) {
  stop(
    paste(
      "FATAL: Script requires 7 args:",
      "<spec> <building_permits_input> <ward_panel_input> <alderman_panel_input>",
      "<output_wide_csv> <output_summary_csv> <output_scatter_pdf>"
    ),
    call. = FALSE
  )
}

spec <- args[1]
building_permits_input <- args[2]
ward_panel_input <- args[3]
alderman_panel_input <- args[4]
output_wide_csv <- args[5]
output_summary_csv <- args[6]
output_scatter_pdf <- args[7]

spec_max_year <- str_match(spec, "through([0-9]{4})")[, 2]
max_permit_year <- ifelse(is.na(spec_max_year), NA_integer_, as.integer(spec_max_year))
if (!is.finite(max_permit_year)) {
  stop("Could not parse through-year from spec.", call. = FALSE)
}

include_porch <- str_detect(spec, "porchTRUE")
signs_permit_type <- "PERMIT - SIGNS"
plot_threshold <- 25L

weighted_cor <- function(x, y, w) {
  keep <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  if (sum(keep) < 2) {
    return(NA_real_)
  }

  x <- x[keep]
  y <- y[keep]
  w <- w[keep]
  w <- w / sum(w)

  x_centered <- x - sum(w * x)
  y_centered <- y - sum(w * y)
  cov_xy <- sum(w * x_centered * y_centered)
  var_x <- sum(w * x_centered^2)
  var_y <- sum(w * y_centered^2)

  if (!is.finite(var_x) || !is.finite(var_y) || var_x <= 0 || var_y <= 0) {
    return(NA_real_)
  }

  cov_xy / sqrt(var_x * var_y)
}

cor_row <- function(df, sample_label, correlation_type, x_col, y_col, weights = NULL) {
  if (nrow(df) < 2) {
    return(tibble(
      sample = sample_label,
      correlation = correlation_type,
      n_aldermen = nrow(df),
      estimate = NA_real_
    ))
  }

  estimate <- if (is.null(weights)) {
    cor(df[[x_col]], df[[y_col]], method = correlation_type, use = "complete.obs")
  } else {
    weighted_cor(df[[x_col]], df[[y_col]], df[[weights]])
  }

  tibble(
    sample = sample_label,
    correlation = correlation_type,
    n_aldermen = nrow(df),
    estimate = estimate
  )
}

message("=== Low-vs-High Processing-Time Correlation Check ===")
message("Spec: ", spec)
message("Through-year cutoff: ", max_permit_year)
message("Include porch in high-discretion group: ", include_porch)

ward_panel <- st_read(ward_panel_input, quiet = TRUE)

permits <- st_read(building_permits_input, quiet = TRUE) %>%
  mutate(
    application_start_date_ym = as.yearmon(application_start_date_ym),
    application_year = year(as.Date(application_start_date_ym))
  ) %>%
  filter(
    !is.na(application_start_date_ym),
    !is.na(application_year),
    application_year <= max_permit_year,
    processing_time > 0
  )

if (!include_porch) {
  permits <- permits %>% filter(!grepl("PORCH", permit_type))
}

message("Permits after year and processing-time filters: ", nrow(permits))

if (st_crs(permits) != st_crs(ward_panel)) {
  permits <- st_transform(permits, st_crs(ward_panel))
}

ward_geoms_map1 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_map1)$ward) > 0) {
  stop("2014 ward geometries must be unique by ward.", call. = FALSE)
}

ward_geoms_map2 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_map2)$ward) > 0) {
  stop("2016 ward geometries must be unique by ward.", call. = FALSE)
}

ward_geoms_map3 <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
if (anyDuplicated(st_drop_geometry(ward_geoms_map3)$ward) > 0) {
  stop("Latest ward geometries must be unique by ward.", call. = FALSE)
}

permits_pre2015 <- permits %>%
  filter(application_start_date_ym < as.yearmon("2015-05"))

permits_2015_2023 <- permits %>%
  filter(
    application_start_date_ym >= as.yearmon("2015-05"),
    application_start_date_ym < as.yearmon("2023-05")
  )

permits_post2023 <- permits %>%
  filter(application_start_date_ym >= as.yearmon("2023-05"))

permits_ward_pre2015 <- if (nrow(permits_pre2015) == 0) {
  permits_pre2015 %>% st_drop_geometry()
} else {
  st_join(permits_pre2015, ward_geoms_map1, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}
if (anyDuplicated(permits_ward_pre2015$id) > 0) {
  stop("Pre-2015 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_2015_2023 <- if (nrow(permits_2015_2023) == 0) {
  permits_2015_2023 %>% st_drop_geometry()
} else {
  st_join(permits_2015_2023, ward_geoms_map2, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}
if (anyDuplicated(permits_ward_2015_2023$id) > 0) {
  stop("2015-2023 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_post2023 <- if (nrow(permits_post2023) == 0) {
  permits_post2023 %>% st_drop_geometry()
} else {
  st_join(permits_post2023, ward_geoms_map3, join = st_within) %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
}
if (anyDuplicated(permits_ward_post2023$id) > 0) {
  stop("Post-2023 ward spatial join assigned a permit to multiple wards.", call. = FALSE)
}

permits_ward_data <- bind_rows(permits_ward_pre2015, permits_ward_2015_2023, permits_ward_post2023)
if (anyDuplicated(permits_ward_data$id) > 0) {
  stop("Combined ward-assigned permit data must be unique by permit id.", call. = FALSE)
}

alderman_panel <- read_csv(alderman_panel_input, show_col_types = FALSE) %>%
  mutate(
    month = as.yearmon(month),
    alderman = gsub("\\s+", " ", trimws(as.character(alderman)))
  )
if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("Alderman panel must be unique by ward-month.", call. = FALSE)
}

permits_with_alderman <- permits_ward_data %>%
  left_join(alderman_panel, by = c("ward", "application_start_date_ym" = "month"), relationship = "many-to-one") %>%
  mutate(alderman = gsub("\\s+", " ", trimws(as.character(alderman)))) %>%
  filter(!is.na(alderman), alderman != "")

permits_grouped <- permits_with_alderman %>%
  mutate(
    group = case_when(
      high_discretion == 1L ~ "high",
      high_discretion == 0L & permit_type != signs_permit_type ~ "low",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(group)) %>%
  mutate(log_processing_time = log(processing_time))

message("Permits after alderman assignment and group filters: ", nrow(permits_grouped))

alderman_wide <- permits_grouped %>%
  group_by(alderman, group) %>%
  summarise(
    n_permits = n(),
    mean_log_days = mean(log_processing_time, na.rm = TRUE),
    mean_days = mean(processing_time, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = group,
    values_from = c(n_permits, mean_log_days, mean_days),
    names_glue = "{.value}_{group}"
  ) %>%
  transmute(
    alderman,
    n_high = n_permits_high,
    n_low = n_permits_low,
    mean_log_high = mean_log_days_high,
    mean_log_low = mean_log_days_low,
    mean_days_high = mean_days_high,
    mean_days_low = mean_days_low
  ) %>%
  filter(!is.na(n_high), !is.na(n_low)) %>%
  mutate(
    plot_sample = n_high >= plot_threshold & n_low >= plot_threshold,
    harmonic_weight = 2 / ((1 / n_high) + (1 / n_low))
  ) %>%
  arrange(desc(plot_sample), alderman)

write_csv(alderman_wide, output_wide_csv)
message("Saved: ", output_wide_csv)

full_sample <- alderman_wide
plot_sample_df <- alderman_wide %>% filter(plot_sample)

summary_rows <- bind_rows(
  cor_row(full_sample, "full_overlap", "pearson", "mean_log_high", "mean_log_low"),
  cor_row(full_sample, "full_overlap", "spearman", "mean_log_high", "mean_log_low"),
  cor_row(plot_sample_df, "threshold_25_each", "pearson", "mean_log_high", "mean_log_low"),
  cor_row(plot_sample_df, "threshold_25_each", "spearman", "mean_log_high", "mean_log_low"),
  cor_row(full_sample, "full_overlap", "weighted_pearson_harmonic", "mean_log_high", "mean_log_low", "harmonic_weight"),
  cor_row(plot_sample_df, "threshold_25_each", "weighted_pearson_harmonic", "mean_log_high", "mean_log_low", "harmonic_weight")
)

write_csv(summary_rows, output_summary_csv)
message("Saved: ", output_summary_csv)

plot_pearson <- summary_rows %>%
  filter(sample == "threshold_25_each", correlation == "pearson") %>%
  pull(estimate)

annotation_text <- paste0(
  "N = ", nrow(plot_sample_df),
  "\nPearson r = ", formatC(plot_pearson, digits = 3, format = "f")
)

p_scatter <- ggplot(plot_sample_df, aes(x = mean_log_high, y = mean_log_low)) +
  geom_point(color = "#2C7FB8", size = 2.5, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, color = "#D95F0E", linewidth = 0.7) +
  annotate(
    "text",
    x = Inf,
    y = -Inf,
    label = annotation_text,
    hjust = 1.02,
    vjust = -0.1,
    size = 3.6
  ) +
  labs(
    x = "Mean log days, high-discretion permits",
    y = "Mean log days, low-discretion permits",
    title = "Alderman Mean Processing Times: High vs. Low Discretion",
    subtitle = paste0("Aldermen with at least ", plot_threshold, " permits in each group; signs excluded from low-discretion")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

ggsave(output_scatter_pdf, p_scatter, width = 7, height = 5)
message("Saved: ", output_scatter_pdf)
