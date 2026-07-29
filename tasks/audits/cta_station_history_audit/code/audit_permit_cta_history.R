# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/cta_station_history_audit/code")

source("../../../_lib/alderman_uncertainty_helpers.R")

score_permits <- read_csv(
  "../input/permits_for_uncertainty_index.csv",
  show_col_types = FALSE,
  col_types = cols(id = col_character(), .default = col_guess())
)

if (anyDuplicated(score_permits$id) > 0) {
  stop("Score input must be unique by permit ID.", call. = FALSE)
}

permit_points <- st_read("../input/building_permits_clean.gpkg", quiet = TRUE) %>%
  mutate(
    id = as.character(id),
    application_date = as.Date(application_start_date)
  ) %>%
  select(id, application_date) %>%
  inner_join(
    score_permits %>% select(id, production_history_count = n_rail_stations_800m),
    by = "id",
    relationship = "one-to-one"
  ) %>%
  st_transform(26916)

if (nrow(permit_points) != nrow(score_permits)) {
  stop("Not every score permit has a unique geometry.", call. = FALSE)
}
if (any(is.na(permit_points$application_date))) {
  stop("Score permits must have an application date.", call. = FALSE)
}

cta_stops <- st_read("../input/cta_stops.gpkg", quiet = TRUE) %>%
  mutate(
    active_from_date = as.Date(active_from_date),
    active_to_date = as.Date(active_to_date)
  ) %>%
  st_transform(26916)

if (anyDuplicated(cta_stops$station_id) > 0) {
  stop("CTA history must be unique by station ID.", call. = FALSE)
}

network_change_dates <- sort(unique(c(
  cta_stops$active_from_date,
  cta_stops$active_to_date + 1
)))
network_change_dates <- network_change_dates[!is.na(network_change_dates)]
network_group <- findInterval(
  as.numeric(permit_points$application_date),
  as.numeric(network_change_dates)
)
permit_rows <- split(seq_len(nrow(permit_points)), network_group)
historical_station_count <- integer(nrow(permit_points))
current_file_cta <- cta_stops %>%
  filter(source == "cta")
current_file_station_count <- lengths(st_is_within_distance(
  permit_points,
  current_file_cta,
  dist = 800
))

for (network_i in names(permit_rows)) {
  row_i <- permit_rows[[network_i]]
  application_date_i <- permit_points$application_date[row_i[1]]
  active_cta <- cta_stops %>%
    filter(
      active_from_date <= application_date_i,
      is.na(active_to_date) | active_to_date >= application_date_i
    )
  if (nrow(active_cta) == 0) {
    stop(sprintf("No active CTA stations on %s.", application_date_i), call. = FALSE)
  }
  historical_station_count[row_i] <- lengths(st_is_within_distance(
    permit_points[row_i, ],
    active_cta,
    dist = 800
  ))
}

comparison <- st_drop_geometry(permit_points) %>%
  mutate(
    historical_station_count = historical_station_count,
    current_file_station_count = current_file_station_count,
    station_count_change = historical_station_count - current_file_station_count,
    production_history_difference = production_history_count - historical_station_count,
    year = year(application_date)
  )

comparison %>%
  summarise(
    permits = n(),
    permits_with_changed_count = sum(station_count_change != 0),
    share_with_changed_count = mean(station_count_change != 0),
    permits_with_more_historical_stations = sum(station_count_change > 0),
    permits_with_fewer_historical_stations = sum(station_count_change < 0),
    production_history_mismatches = sum(production_history_difference != 0),
    mean_count_change = mean(station_count_change),
    maximum_absolute_change = max(abs(station_count_change))
  ) %>%
  write_csv("../output/cta_history_permit_count_summary.csv")

comparison %>%
  group_by(year) %>%
  summarise(
    permits = n(),
    permits_with_changed_count = sum(station_count_change != 0),
    share_with_changed_count = mean(station_count_change != 0),
    mean_count_change = mean(station_count_change),
    minimum_count_change = min(station_count_change),
    maximum_count_change = max(station_count_change),
    .groups = "drop"
  ) %>%
  write_csv("../output/cta_history_permit_count_by_year.csv")

corrected_counts <- comparison %>%
  select(id, historical_station_count, current_file_station_count)

current_file_permits <- score_permits %>%
  left_join(corrected_counts, by = "id", relationship = "one-to-one") %>%
  mutate(
    month = as.yearmon(month),
    n_rail_stations_800m = current_file_station_count
  ) %>%
  select(-historical_station_count, -current_file_station_count)

historical_permits <- score_permits %>%
  left_join(corrected_counts, by = "id", relationship = "one-to-one") %>%
  mutate(
    month = as.yearmon(month),
    n_rail_stations_800m = historical_station_count
  ) %>%
  select(-historical_station_count, -current_file_station_count)

config <- default_uncertainty_config()
score_summary <- list()
alderman_changes <- list()

for (cutoff_year in c(2014L, 2022L)) {
  cutoff_month <- as.yearmon(sprintf("Dec %s", cutoff_year))
  current_file_result <- build_residualized_uncertainty_index(
    permits = current_file_permits %>% filter(month <= cutoff_month),
    config = config,
    variant_id = "current_file_cta_control",
    stage1_outcome = "log_processing_time",
    drop_covariates = c("share_bach_plus", "median_hh_income_10k"),
    construction_rule = "Score with static current-file CTA count"
  )
  historical_result <- build_residualized_uncertainty_index(
    permits = historical_permits %>% filter(month <= cutoff_month),
    config = config,
    variant_id = "historical_cta_control",
    stage1_outcome = "log_processing_time",
    drop_covariates = c("share_bach_plus", "median_hh_income_10k"),
    construction_rule = "Score with application-date CTA count"
  )

  changes <- current_file_result$alderman_index %>%
    select(alderman, current_file_score = uncertainty_index) %>%
    inner_join(
      historical_result$alderman_index %>%
        select(alderman, historical_score = uncertainty_index),
      by = "alderman",
      relationship = "one-to-one"
    ) %>%
    mutate(
      cutoff_year = cutoff_year,
      score_change = historical_score - current_file_score,
      absolute_score_change = abs(score_change),
      current_file_rank = min_rank(current_file_score),
      historical_rank = min_rank(historical_score),
      rank_change = historical_rank - current_file_rank
    ) %>%
    relocate(cutoff_year)

  pair_indices <- combn(seq_len(nrow(changes)), 2)
  current_file_gaps <- changes$current_file_score[pair_indices[1, ]] -
    changes$current_file_score[pair_indices[2, ]]
  historical_gaps <- changes$historical_score[pair_indices[1, ]] -
    changes$historical_score[pair_indices[2, ]]
  pair_reversals <- sum(current_file_gaps * historical_gaps < 0)

  score_summary[[as.character(cutoff_year)]] <- changes %>%
    summarise(
      cutoff_year = .env$cutoff_year,
      aldermen = n(),
      pearson_correlation = cor(current_file_score, historical_score),
      spearman_correlation = cor(current_file_score, historical_score, method = "spearman"),
      mean_absolute_score_change = mean(absolute_score_change),
      maximum_absolute_score_change = max(absolute_score_change),
      maximum_absolute_rank_change = max(abs(rank_change)),
      pairwise_rank_reversals = pair_reversals,
      pairwise_comparisons = ncol(pair_indices),
      pairwise_reversal_share = pair_reversals / ncol(pair_indices)
    )
  alderman_changes[[as.character(cutoff_year)]] <- changes
}

bind_rows(score_summary) %>%
  write_csv("../output/cta_history_score_comparison.csv")

bind_rows(alderman_changes) %>%
  arrange(cutoff_year, desc(absolute_score_change)) %>%
  write_csv("../output/cta_history_score_alderman_changes.csv")
