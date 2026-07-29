# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

permits <- st_read(
  "../../../clean_building_permits/output/building_permits_clean.gpkg",
  query = paste(
    "SELECT id, permit_status, permit_type, review_type, application_start_date_ym,",
    "processing_time, reported_cost, high_discretion, geom",
    "FROM building_permits_clean"
  ),
  quiet = TRUE
) %>%
  mutate(
    id = as.character(id),
    month = as.yearmon(as.Date(application_start_date_ym)),
    year = year(as.Date(month)),
    permit_status = str_to_upper(trimws(permit_status)),
    permit_type = str_to_upper(trimws(permit_type)),
    high_discretion = as.integer(high_discretion),
    processing_time_sample = if_else(processing_time == 0, "zero_day", "positive_day")
  ) %>%
  filter(
    !is.na(month),
    month <= as.yearmon("2022-12"),
    !is.na(high_discretion),
    is.finite(processing_time),
    processing_time >= 0
  )

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE)
if (st_crs(permits) != st_crs(ward_panel)) {
  permits <- st_transform(permits, st_crs(ward_panel))
}

wards_2014 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")
wards_2016 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop")

pre_2015 <- permits %>%
  filter(month < as.yearmon("2015-05")) %>%
  st_join(wards_2014, join = st_within, left = TRUE)
post_2015 <- permits %>%
  filter(month >= as.yearmon("2015-05")) %>%
  st_join(wards_2016, join = st_within, left = TRUE)
permits_with_ward <- bind_rows(pre_2015, post_2015)

if (anyDuplicated(permits_with_ward$id) > 0) {
  stop("Permit-to-ward assignment is not unique by permit ID.", call. = FALSE)
}

aldermen <- read_csv(
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(
    month = as.yearmon(month),
    alderman = str_squish(as.character(alderman))
  )

permits_analysis <- permits_with_ward %>%
  st_drop_geometry() %>%
  filter(!is.na(ward)) %>%
  mutate(ward = as.integer(ward)) %>%
  left_join(aldermen, by = c("ward", "month"), relationship = "many-to-one") %>%
  filter(!is.na(alderman), alderman != "", high_discretion == 1L | permit_type != "PERMIT - SIGNS") %>%
  mutate(
    permit_group = if_else(high_discretion == 1L, "High-Discretion", "Low-Discretion")
  )

descriptive_samples <- bind_rows(
  permits_analysis %>%
    filter(processing_time > 0) %>%
    mutate(sample = "production_positive_days"),
  permits_analysis %>%
    mutate(sample = "paper_stated_nonnegative_days")
)

group_statistics <- descriptive_samples %>%
  group_by(sample, permit_group) %>%
  summarise(
    permits = n(),
    zero_day_permits = sum(processing_time == 0),
    zero_day_share = mean(processing_time == 0),
    mean_processing_time = mean(processing_time),
    median_processing_time = median(processing_time),
    wards = n_distinct(ward),
    aldermen = n_distinct(alderman),
    .groups = "drop"
  )

alderman_statistics <- descriptive_samples %>%
  group_by(sample, permit_group, alderman) %>%
  summarise(alderman_mean_processing_time = mean(processing_time), .groups = "drop") %>%
  group_by(sample, permit_group) %>%
  summarise(
    alderman_level_mean_processing_time = mean(alderman_mean_processing_time),
    alderman_level_iqr_mean_processing_time = IQR(alderman_mean_processing_time),
    .groups = "drop"
  )

write_csv(
  left_join(group_statistics, alderman_statistics,
            by = c("sample", "permit_group"), relationship = "one-to-one"),
  "../output/permit_descriptive_group_statistics.csv"
)

volume_correlations <- bind_rows(
  descriptive_samples,
  descriptive_samples %>% mutate(permit_group = "All")
) %>%
  group_by(sample, permit_group, ward, month) %>%
  summarise(
    mean_processing_time = mean(processing_time),
    ward_month_permit_volume = n(),
    .groups = "drop"
  ) %>%
  group_by(sample, permit_group) %>%
  summarise(
    ward_month_cells = n(),
    pearson_correlation = cor(mean_processing_time, ward_month_permit_volume),
    spearman_correlation = cor(mean_processing_time, ward_month_permit_volume, method = "spearman"),
    .groups = "drop"
  )
write_csv(volume_correlations, "../output/permit_descriptive_volume_correlations.csv")

type_profile <- permits_analysis %>%
  filter(high_discretion == 1L) %>%
  group_by(permit_type) %>%
  summarise(
    permits = n(),
    zero_day_permits = sum(processing_time == 0),
    zero_day_share = mean(processing_time == 0),
    missing_or_nonpositive_cost = sum(!is.finite(reported_cost) | reported_cost <= 0),
    cost_at_most_10000 = sum(is.finite(reported_cost) & reported_cost <= 10000),
    cost_at_most_50000 = sum(is.finite(reported_cost) & reported_cost <= 50000),
    cost_at_least_100000 = sum(is.finite(reported_cost) & reported_cost >= 100000),
    median_reported_cost_positive = median(reported_cost[is.finite(reported_cost) & reported_cost > 0]),
    .groups = "drop"
  ) %>%
  arrange(desc(permits))
write_csv(type_profile, "../output/permit_high_discretion_type_profile.csv")

status_profile <- permits_analysis %>%
  count(permit_group, processing_time_sample, permit_status, name = "permits") %>%
  arrange(permit_group, processing_time_sample, desc(permits))
write_csv(status_profile, "../output/permit_status_profile.csv")

scores <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)

alderman_sequence <- aldermen %>%
  filter(month >= as.yearmon("2006-01")) %>%
  group_by(ward, alderman) %>%
  summarise(first_month = min(month), .groups = "drop") %>%
  arrange(ward, first_month)

turnover_pairs <- alderman_sequence %>%
  group_by(ward) %>%
  arrange(first_month, .by_group = TRUE) %>%
  transmute(
    ward,
    predecessor = alderman,
    successor = lead(alderman),
    predecessor_first_month = first_month,
    successor_first_month = lead(first_month)
  ) %>%
  ungroup() %>%
  filter(!is.na(successor)) %>%
  inner_join(scores %>% rename(predecessor = alderman, predecessor_score = score),
             by = "predecessor", relationship = "many-to-one") %>%
  inner_join(scores %>% rename(successor = alderman, successor_score = score),
             by = "successor", relationship = "many-to-one")

predecessor_successor_check <- tibble(
  method = c("production_pair_construction", "unique_alderman_pair"),
  pairs = c(nrow(turnover_pairs), nrow(distinct(turnover_pairs, predecessor, successor, .keep_all = TRUE))),
  pearson_correlation = c(
    cor(turnover_pairs$predecessor_score, turnover_pairs$successor_score),
    turnover_pairs %>%
      distinct(predecessor, successor, .keep_all = TRUE) %>%
      summarise(value = cor(predecessor_score, successor_score)) %>%
      pull(value)
  ),
  spearman_correlation = c(
    cor(turnover_pairs$predecessor_score, turnover_pairs$successor_score, method = "spearman"),
    turnover_pairs %>%
      distinct(predecessor, successor, .keep_all = TRUE) %>%
      summarise(value = cor(predecessor_score, successor_score, method = "spearman")) %>%
      pull(value)
  )
)
write_csv(predecessor_successor_check, "../output/permit_predecessor_successor_check.csv")
