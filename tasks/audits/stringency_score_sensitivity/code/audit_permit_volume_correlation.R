# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/stringency_score_sensitivity/code")

source("../../../setup_environment/code/packages.R")

signs_permit_type <- "PERMIT - SIGNS"
max_application_ym <- as.yearmon("2022-12")

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
alderman_panel <- read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(
    month = as.yearmon(month),
    alderman = str_squish(as.character(alderman))
  )

if (anyDuplicated(alderman_panel[c("ward", "month")]) > 0) {
  stop("The alderman panel is not unique by ward-month.", call. = FALSE)
}

permits <- st_read(
  "../input/building_permits_clean.gpkg",
  quiet = TRUE
) %>%
  select(
    id,
    application_start_date_ym,
    processing_time,
    high_discretion,
    permit_type
  ) %>%
  mutate(
    application_start_date_ym = as.yearmon(application_start_date_ym),
    high_discretion = as.integer(high_discretion)
  ) %>%
  filter(
    !is.na(application_start_date_ym),
    !is.na(high_discretion),
    processing_time >= 0,
    application_start_date_ym <= max_application_ym
  )

if (st_crs(permits) != st_crs(ward_panel)) {
  permits <- st_transform(permits, st_crs(ward_panel))
}

ward_geometries_2014 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
ward_geometries_2016 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

permits_with_ward <- bind_rows(
  permits %>%
    filter(application_start_date_ym < as.yearmon("2015-05")) %>%
    st_join(ward_geometries_2014, join = st_within) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry(),
  permits %>%
    filter(application_start_date_ym >= as.yearmon("2015-05")) %>%
    st_join(ward_geometries_2016, join = st_within) %>%
    filter(!is.na(ward)) %>%
    st_drop_geometry()
)

if (anyDuplicated(permits_with_ward$id) > 0) {
  stop("The ward spatial join assigned a permit more than once.", call. = FALSE)
}

permits_with_alderman <- permits_with_ward %>%
  mutate(ward = as.integer(ward)) %>%
  left_join(
    alderman_panel,
    by = c("ward", "application_start_date_ym" = "month"),
    relationship = "many-to-one"
  ) %>%
  mutate(alderman = str_squish(as.character(alderman))) %>%
  filter(!is.na(alderman), alderman != "")

permit_groups <- bind_rows(
  permits_with_alderman %>%
    filter(high_discretion == 1L) %>%
    mutate(permit_group = "High-Discretion"),
  permits_with_alderman %>%
    filter(high_discretion == 0L, permit_type != signs_permit_type) %>%
    mutate(permit_group = "Low-Discretion"),
  permits_with_alderman %>%
    filter(high_discretion == 0L) %>%
    mutate(permit_group = "Low-Discretion Including Signs"),
  permits_with_alderman %>%
    mutate(permit_group = "All")
)

ward_months <- permit_groups %>%
  group_by(permit_group, ward, application_start_date_ym) %>%
  summarise(
    mean_positive_processing_time = mean(
      processing_time[processing_time > 0],
      na.rm = TRUE
    ),
    mean_nonnegative_processing_time = mean(processing_time),
    positive_permit_volume = sum(processing_time > 0),
    nonnegative_permit_volume = n(),
    .groups = "drop"
  )

correlation_results <- ward_months %>%
  group_by(permit_group) %>%
  summarise(
    old_positive_only = cor(
      mean_positive_processing_time,
      positive_permit_volume,
      use = "complete.obs"
    ),
    selected_score_definition = cor(
      mean_positive_processing_time,
      nonnegative_permit_volume,
      use = "complete.obs"
    ),
    nonnegative_mean_and_volume = cor(
      mean_nonnegative_processing_time,
      nonnegative_permit_volume,
      use = "complete.obs"
    ),
    ward_months = n(),
    same_day_share = sum(nonnegative_permit_volume - positive_permit_volume) /
      sum(nonnegative_permit_volume),
    .groups = "drop"
  ) %>%
  mutate(
    permit_group = factor(
      permit_group,
      levels = c(
        "High-Discretion",
        "Low-Discretion",
        "Low-Discretion Including Signs",
        "All"
      )
    )
  ) %>%
  arrange(permit_group)

write_csv(
  correlation_results,
  "../output/permit_volume_correlation_definition_audit.csv"
)

summary_groups <- permit_groups %>%
  filter(permit_group %in% c("High-Discretion", "Low-Discretion")) %>%
  mutate(
    permit_group = factor(
      permit_group,
      levels = c("High-Discretion", "Low-Discretion")
    )
  )

summary_statistics <- summary_groups %>%
  group_by(permit_group) %>%
  summarise(
    total_permits = n(),
    mean_processing_time = mean(processing_time),
    median_processing_time = median(processing_time),
    unique_wards = n_distinct(ward),
    unique_aldermen = n_distinct(alderman),
    .groups = "drop"
  )

alderman_statistics <- summary_groups %>%
  group_by(permit_group, alderman) %>%
  summarise(
    alderman_mean_processing_time = mean(processing_time),
    .groups = "drop"
  ) %>%
  group_by(permit_group) %>%
  summarise(
    alderman_level_mean = mean(alderman_mean_processing_time),
    alderman_level_iqr = IQR(alderman_mean_processing_time),
    .groups = "drop"
  )

summary_statistics <- summary_statistics %>%
  left_join(alderman_statistics, by = "permit_group", relationship = "one-to-one") %>%
  arrange(permit_group)

summary_rows <- tribble(
  ~label, ~variable, ~digits,
  "Total permits in sample", "total_permits", 0L,
  "Mean processing time (days)", "mean_processing_time", 1L,
  "Median processing time (days)", "median_processing_time", 1L,
  "Alderman-level mean of processing time", "alderman_level_mean", 1L,
  "Alderman-level IQR of mean processing time", "alderman_level_iqr", 1L,
  "Number of unique wards", "unique_wards", 0L,
  "Number of unique aldermen", "unique_aldermen", 0L
)

summary_tex <- c(
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Statistic & High-Discretion & Low-Discretion \\\\ ",
  "\\midrule"
)
for (row_i in seq_len(nrow(summary_rows))) {
  variable_i <- summary_rows$variable[row_i]
  digits_i <- summary_rows$digits[row_i]
  values_i <- summary_statistics[[variable_i]]
  values_text <- if (digits_i == 0L) {
    format(round(values_i), big.mark = ",", scientific = FALSE, trim = TRUE)
  } else {
    formatC(values_i, format = "f", digits = digits_i, big.mark = ",")
  }
  summary_tex <- c(
    summary_tex,
    sprintf(
      "%s & %s & %s \\\\ ",
      summary_rows$label[row_i],
      values_text[1],
      values_text[2]
    )
  )
}
summary_tex <- c(summary_tex, "\\bottomrule", "\\end{tabular}")
writeLines(
  summary_tex,
  "../output/selected_permit_processing_summary.tex"
)

selected_correlations <- correlation_results %>%
  filter(permit_group %in% c("High-Discretion", "Low-Discretion", "All")) %>%
  arrange(permit_group)
correlation_tex <- c(
  "\\begin{tabular}{lr}",
  "\\toprule",
  "Permit group & Corr. of mean processing time and ward-month volume \\\\ ",
  "\\midrule"
)
for (row_i in seq_len(nrow(selected_correlations))) {
  correlation_tex <- c(
    correlation_tex,
    sprintf(
      "%s & %.3f \\\\ ",
      selected_correlations$permit_group[row_i],
      selected_correlations$selected_score_definition[row_i]
    )
  )
}
correlation_tex <- c(correlation_tex, "\\bottomrule", "\\end{tabular}")
writeLines(
  correlation_tex,
  "../output/selected_permit_volume_correlation.tex"
)
