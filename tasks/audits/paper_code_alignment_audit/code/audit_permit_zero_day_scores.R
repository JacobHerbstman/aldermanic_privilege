# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/alderman_uncertainty_helpers.R")
source("../../../_lib/border_pair_helpers.R")

assign_wards <- function(permits, wards) {
  if (nrow(permits) == 0) {
    return(permits %>% st_drop_geometry())
  }

  assigned <- st_join(permits, wards, join = st_within, left = TRUE) %>%
    st_drop_geometry()

  if (anyDuplicated(assigned$id) > 0) {
    stop("A zero-day permit matched multiple wards.", call. = FALSE)
  }

  assigned %>%
    mutate(ward = ward.y) %>%
    select(-any_of(c("ward.x", "ward.y"))) %>%
    filter(!is.na(ward))
}

estimate_score <- function(
    permits,
    cutoff,
    variant,
    outcome,
    zero_day_value = NA_real_,
    include_porch = TRUE) {
  sample <- permits %>%
    filter(month <= as.yearmon(as.Date(sprintf("%d-12-01", cutoff))))

  if (is.finite(zero_day_value)) {
    sample <- sample %>%
      mutate(log_processing_time = log(if_else(processing_time == 0, zero_day_value, processing_time)))
  }

  score_config <- default_uncertainty_config()
  score_config$include_porch <- include_porch

  result <- build_residualized_uncertainty_index(
    permits = sample,
    config = score_config,
    variant_id = variant,
    stage1_outcome = outcome,
    drop_covariates = c("share_bach_plus"),
    construction_rule = variant
  )

  result$alderman_index %>%
    transmute(
      alderman,
      cutoff,
      variant,
      n_permits,
      score = uncertainty_index,
      raw_score = standardize_uncertainty(alderman_fe_raw),
      alderman_effect_raw = alderman_fe_raw,
      alderman_effect_se = alderman_se,
      shrinkage = shrinkage_B
    )
}

extract_model <- function(model, term, analysis, cutoff, variant, sample, outcome, treatment) {
  tibble(
    analysis,
    cutoff,
    variant,
    sample,
    outcome,
    treatment,
    coefficient = coef(model)[[term]],
    standard_error = se(model)[[term]],
    p_value = pvalue(model)[[term]],
    percent_effect = 100 * expm1(coef(model)[[term]]),
    observations = nobs(model)
  )
}

positive_permits <- read_csv(
  "../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv",
  show_col_types = FALSE,
  col_types = cols(id = col_character(), pin = col_character(), .default = col_guess())
) %>%
  filter(processing_time > 0) %>%
  mutate(month = as.yearmon(month))

zero_permits <- st_read(
  "../../../clean_building_permits/output/building_permits_clean.gpkg",
  query = paste(
    "SELECT id, pin, ward, application_start_date_ym, processing_time, reported_cost,",
    "permit_type, review_type, high_discretion, geom",
    "FROM building_permits_clean",
    "WHERE high_discretion = 1 AND processing_time = 0"
  ),
  quiet = TRUE
) %>%
  mutate(
    id = as.character(id),
    application_start_date_ym = as.yearmon(application_start_date_ym),
    application_year = year(as.Date(application_start_date_ym))
  )

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE)
if (st_crs(zero_permits) != st_crs(ward_panel)) {
  zero_permits <- st_transform(zero_permits, st_crs(ward_panel))
}

wards_2014 <- ward_panel %>%
  filter(year == 2014) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 1L)
wards_2016 <- ward_panel %>%
  filter(year == 2016) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 2L)
wards_latest <- ward_panel %>%
  filter(year == max(year)) %>%
  select(ward) %>%
  group_by(ward) %>%
  summarise(.groups = "drop") %>%
  mutate(map_version = 3L)

zero_ward_data <- bind_rows(
  assign_wards(
    zero_permits %>% filter(application_start_date_ym < as.yearmon("2015-05")),
    wards_2014
  ),
  assign_wards(
    zero_permits %>% filter(
      application_start_date_ym >= as.yearmon("2015-05"),
      application_start_date_ym < as.yearmon("2023-05")
    ),
    wards_2016
  ),
  assign_wards(
    zero_permits %>% filter(application_start_date_ym >= as.yearmon("2023-05")),
    wards_latest
  )
)

if (anyDuplicated(zero_ward_data$id) > 0) {
  stop("Zero-day ward assignment is not unique by permit ID.", call. = FALSE)
}

community_areas <- st_read(
  "../../../download_chicago_spatial_data/output/community_areas.geojson",
  quiet = TRUE
) %>%
  select(area_numbe, community) %>%
  transmute(ca_id = as.numeric(area_numbe), ca_name = community)

zero_community_data <- zero_permits %>%
  select(id) %>%
  st_transform(st_crs(community_areas)) %>%
  st_join(community_areas, join = st_within, left = TRUE) %>%
  st_drop_geometry()

if (anyDuplicated(zero_community_data$id) > 0) {
  stop("A zero-day permit matched multiple community areas.", call. = FALSE)
}

aldermen <- read_csv(
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(month = as.yearmon(month))

ward_controls <- read_csv(
  "../../../create_ward_controls/output/ward_controls_2000_2023.csv",
  show_col_types = FALSE
)

max_permit_year <- max(zero_ward_data$application_year, na.rm = TRUE)
max_control_year <- max(ward_controls$year, na.rm = TRUE)
if (max_permit_year > max_control_year) {
  ward_controls <- bind_rows(
    ward_controls,
    ward_controls %>%
      filter(year == max_control_year) %>%
      select(-year) %>%
      tidyr::crossing(year = (max_control_year + 1L):max_permit_year)
  )
}

zero_controls <- zero_ward_data %>%
  left_join(zero_community_data, by = "id", relationship = "one-to-one") %>%
  left_join(
    aldermen,
    by = c("ward", "application_start_date_ym" = "month"),
    relationship = "many-to-one"
  ) %>%
  filter(!is.na(alderman), !is.na(ca_id)) %>%
  left_join(
    ward_controls,
    by = c("ward", "application_year" = "year"),
    relationship = "many-to-one"
  ) %>%
  filter(!is.na(homeownership_rate))

zero_points <- zero_permits %>%
  select(id) %>%
  semi_join(zero_controls %>% select(id), by = "id") %>%
  st_transform(26916)

cta_stations <- st_read(
  "../../../download_chicago_spatial_data/output/cta_stations.geojson",
  quiet = TRUE
) %>%
  st_transform(26916)
water <- st_read(
  "../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp",
  quiet = TRUE
) %>%
  st_transform(26916)

cbd <- st_sfc(st_point(c(-87.6313, 41.8837)), crs = 4326) %>%
  st_transform(26916)
lake_michigan <- water %>%
  filter(!is.na(name), tolower(name) == "lake michigan") %>%
  st_make_valid() %>%
  st_union()

zero_place_controls <- tibble(
  id = zero_points$id,
  dist_cbd_km = as.numeric(units::set_units(st_distance(zero_points, cbd), "m")) / 1000,
  dist_lake_km = as.numeric(units::set_units(st_distance(zero_points, lake_michigan), "m")) / 1000,
  n_rail_stations_800m = lengths(st_is_within_distance(zero_points, cta_stations, dist = 800))
)

zero_analysis <- zero_controls %>%
  left_join(zero_place_controls, by = "id", relationship = "one-to-one") %>%
  mutate(
    month = application_start_date_ym,
    year = application_year,
    log_processing_time = NA_real_,
    log_reported_cost = log(if_else(reported_cost > 0, reported_cost, NA_real_)),
    permit_type_clean = case_when(
      grepl("NEW CONSTRUCTION", permit_type) ~ "new_construction",
      grepl("RENOVATION|ALTERATION", permit_type) ~ "renovation",
      grepl("WRECKING|DEMOLITION", permit_type) ~ "demolition",
      grepl("PORCH", permit_type) ~ "porch",
      grepl("REINSTATE", permit_type) ~ "reinstate",
      TRUE ~ "other"
    ),
    is_porch = grepl("PORCH", permit_type),
    review_type_clean = if_else(is.na(review_type), "unknown", review_type)
  ) %>%
  select(all_of(names(positive_permits)))

nonnegative_permits <- bind_rows(positive_permits, zero_analysis) %>%
  mutate(
    same_day = as.integer(processing_time == 0),
    positive_processing_time = if_else(
      processing_time > 0,
      processing_time,
      NA_real_
    )
  )
if (anyDuplicated(nonnegative_permits$id) > 0) {
  stop("Combined positive and zero-day score sample has duplicate permit IDs.", call. = FALSE)
}

audit_cutoffs <- c(2014L, 2022L, 2025L)

score_rows <- list()
for (cutoff in audit_cutoffs) {
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    positive_permits, cutoff, "positive_log", "log_processing_time"
  )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    nonnegative_permits,
    cutoff,
    "positive_log_all_permit_volume",
    "log_processing_time"
  )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    nonnegative_permits,
    cutoff,
    "positive_log_all_permit_volume_no_porch",
    "log_processing_time",
    include_porch = FALSE
  )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    nonnegative_permits,
    cutoff,
    "same_day_stringency",
    "same_day"
  ) %>%
    mutate(
      score = -score,
      raw_score = -raw_score,
      alderman_effect_raw = -alderman_effect_raw
    )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    nonnegative_permits, cutoff, "zero_as_one_day_log", "log_processing_time", 1
  )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    nonnegative_permits, cutoff, "zero_as_half_day_log", "log_processing_time", 0.5
  )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    positive_permits, cutoff, "positive_unlogged_days", "processing_time"
  )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    nonnegative_permits,
    cutoff,
    "positive_unlogged_days_all_permit_volume",
    "positive_processing_time"
  )
  score_rows[[length(score_rows) + 1L]] <- estimate_score(
    nonnegative_permits, cutoff, "nonnegative_unlogged_days", "processing_time"
  )
}
scores_long <- bind_rows(score_rows)

official_scores <- bind_rows(
  read_csv(
    "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv",
    show_col_types = FALSE
  ) %>% transmute(
    alderman,
    cutoff = 2014L,
    variant = "official",
    n_permits,
    score = uncertainty_index,
    raw_score = standardize_uncertainty(alderman_fe_raw),
    alderman_effect_raw = alderman_fe_raw,
    alderman_effect_se = alderman_se,
    shrinkage = shrinkage_B
  ),
  read_csv(
    "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
    show_col_types = FALSE
  ) %>% transmute(
    alderman,
    cutoff = 2022L,
    variant = "official",
    n_permits,
    score = uncertainty_index,
    raw_score = standardize_uncertainty(alderman_fe_raw),
    alderman_effect_raw = alderman_fe_raw,
    alderman_effect_se = alderman_se,
    shrinkage = shrinkage_B
  )
)
scores_long <- bind_rows(official_scores, scores_long)

scores_wide <- scores_long %>%
  select(alderman, cutoff, variant, score) %>%
  pivot_wider(names_from = variant, values_from = score)

score_correlations <- list()
for (cutoff in intersect(audit_cutoffs, unique(official_scores$cutoff))) {
  cutoff_scores <- scores_wide %>% filter(cutoff == !!cutoff)
  for (variant in setdiff(names(cutoff_scores), c("alderman", "cutoff", "official"))) {
    pair <- cutoff_scores %>%
      select(official, comparison = all_of(variant)) %>%
      filter(complete.cases(.))
    score_correlations[[length(score_correlations) + 1L]] <- tibble(
      cutoff,
      variant,
      aldermen = nrow(pair),
      pearson = cor(pair$official, pair$comparison),
      spearman = cor(pair$official, pair$comparison, method = "spearman"),
      maximum_absolute_score_difference = max(abs(pair$official - pair$comparison))
    )
  }
}
write_csv(bind_rows(score_correlations), "../output/permit_zero_day_score_correlations.csv")

score_aldermen <- scores_long %>%
  group_by(cutoff, variant) %>%
  mutate(
    rank = min_rank(score),
    strictness_rank = min_rank(desc(score))
  ) %>%
  ungroup() %>%
  select(
    cutoff, variant, alderman, n_permits, score, raw_score,
    alderman_effect_raw, alderman_effect_se, shrinkage, rank, strictness_rank
  )
write_csv(score_aldermen, "../output/permit_zero_day_score_aldermen.csv")

porch_exclusion_changes <- score_aldermen %>%
  filter(variant %in% c(
    "positive_log_all_permit_volume",
    "positive_log_all_permit_volume_no_porch"
  )) %>%
  select(cutoff, alderman, variant, n_permits, score, strictness_rank) %>%
  pivot_wider(
    names_from = variant,
    values_from = c(n_permits, score, strictness_rank)
  ) %>%
  mutate(
    permits_excluded =
      n_permits_positive_log_all_permit_volume -
      n_permits_positive_log_all_permit_volume_no_porch,
    score_change =
      score_positive_log_all_permit_volume_no_porch -
      score_positive_log_all_permit_volume,
    absolute_score_change = abs(score_change),
    rank_change =
      strictness_rank_positive_log_all_permit_volume_no_porch -
      strictness_rank_positive_log_all_permit_volume
  ) %>%
  arrange(cutoff, desc(absolute_score_change), alderman)

unlogged_alderman_comparison <- score_aldermen %>%
  filter(variant %in% c(
    "positive_log",
    "positive_unlogged_days",
    "positive_unlogged_days_all_permit_volume",
    "nonnegative_unlogged_days"
  )) %>%
  select(cutoff, alderman, variant, n_permits, score, strictness_rank) %>%
  pivot_wider(
    names_from = variant,
    values_from = c(n_permits, score, strictness_rank)
  ) %>%
  mutate(
    unlogged_rank_difference =
      strictness_rank_nonnegative_unlogged_days -
      strictness_rank_positive_unlogged_days_all_permit_volume,
    absolute_unlogged_rank_difference = abs(unlogged_rank_difference)
  ) %>%
  arrange(cutoff, desc(absolute_unlogged_rank_difference), alderman)

write_csv(
  unlogged_alderman_comparison,
  "../output/permit_zero_day_unlogged_alderman_comparison.csv"
)

unlogged_alderman_comparison %>%
  filter(cutoff == 2022L) %>%
  transmute(
    alderman,
    logged_score = score_positive_log,
    nonnegative_unlogged_score = score_nonnegative_unlogged_days,
    score_change = nonnegative_unlogged_score - logged_score,
    absolute_score_change = abs(score_change),
    logged_strictness_rank = strictness_rank_positive_log,
    nonnegative_unlogged_strictness_rank =
      strictness_rank_nonnegative_unlogged_days,
    rank_change =
      nonnegative_unlogged_strictness_rank - logged_strictness_rank,
    crosses_score_mean = sign(logged_score) != sign(nonnegative_unlogged_score)
  ) %>%
  arrange(desc(absolute_score_change), alderman) %>%
  write_csv("../output/permit_zero_day_logged_unlogged_2022_changes.csv")

unlogged_correlations <- list()
unlogged_comparison_pairs <- list(
  c("score_positive_log", "score_positive_unlogged_days"),
  c("score_positive_log", "score_nonnegative_unlogged_days"),
  c("score_positive_unlogged_days", "score_nonnegative_unlogged_days"),
  c(
    "score_positive_unlogged_days_all_permit_volume",
    "score_nonnegative_unlogged_days"
  )
)

for (cutoff in audit_cutoffs) {
  cutoff_unlogged <- unlogged_alderman_comparison %>%
    filter(cutoff == !!cutoff)

  for (comparison_pair in unlogged_comparison_pairs) {
    comparison_data <- cutoff_unlogged %>%
      select(
        first_score = all_of(comparison_pair[1]),
        second_score = all_of(comparison_pair[2])
      ) %>%
      filter(complete.cases(.))

    unlogged_correlations[[length(unlogged_correlations) + 1L]] <- tibble(
      cutoff,
      first_score = comparison_pair[1],
      second_score = comparison_pair[2],
      aldermen = nrow(comparison_data),
      pearson = cor(comparison_data$first_score, comparison_data$second_score),
      spearman = cor(
        comparison_data$first_score,
        comparison_data$second_score,
        method = "spearman"
      )
    )
  }
}

write_csv(
  bind_rows(unlogged_correlations),
  "../output/permit_zero_day_unlogged_correlations.csv"
)

logged_stage1 <- build_residualized_uncertainty_index(
  permits = positive_permits %>%
    filter(month <= as.yearmon("2022-12")),
  config = default_uncertainty_config(),
  variant_id = "logged_positive_days",
  stage1_outcome = "log_processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = "Production logged positive-duration score"
)

unlogged_stage1 <- build_residualized_uncertainty_index(
  permits = nonnegative_permits %>%
    filter(month <= as.yearmon("2022-12")),
  config = default_uncertainty_config(),
  variant_id = "unlogged_nonnegative_days",
  stage1_outcome = "processing_time",
  drop_covariates = c("share_bach_plus"),
  construction_rule = "Unlogged score including zero-day permits"
)

bind_rows(
  logged_stage1$stage1_terms %>%
    transmute(
      model = "logged_positive_days",
      outcome = "log_processing_time",
      term,
      estimate,
      standard_error = std_error,
      p_value,
      observations = logged_stage1$metadata$stage1_nobs,
      adjusted_r_squared = logged_stage1$metadata$stage1_r2,
      volume_control_universe = "positive_duration_permits"
    ),
  unlogged_stage1$stage1_terms %>%
    transmute(
      model = "unlogged_nonnegative_days",
      outcome = "processing_time_days",
      term,
      estimate,
      standard_error = std_error,
      p_value,
      observations = unlogged_stage1$metadata$stage1_nobs,
      adjusted_r_squared = unlogged_stage1$metadata$stage1_r2,
      volume_control_universe = "all_nonnegative_duration_permits"
    )
) %>%
  arrange(term, model) %>%
  write_csv("../output/permit_zero_day_stage1_comparison.csv")

raw_alderman_rows <- list()
for (cutoff in audit_cutoffs) {
  raw_alderman_rows[[length(raw_alderman_rows) + 1L]] <- nonnegative_permits %>%
    filter(month <= as.yearmon(as.Date(sprintf("%d-12-01", cutoff)))) %>%
    group_by(alderman) %>%
    summarise(
      cutoff = cutoff,
      all_nonnegative_permits = n(),
      zero_day_permits = sum(same_day),
      positive_day_permits = sum(1L - same_day),
      raw_zero_day_share = mean(same_day),
      raw_mean_log_positive_days = mean(log_processing_time, na.rm = TRUE),
      .groups = "drop"
    )
}

alderman_comparison <- score_aldermen %>%
  filter(variant %in% c(
    "positive_log",
    "positive_log_all_permit_volume",
    "same_day_stringency"
  )) %>%
  select(
    cutoff, alderman, variant, n_permits, score, raw_score,
    alderman_effect_raw, alderman_effect_se, shrinkage, strictness_rank
  ) %>%
  pivot_wider(
    names_from = variant,
    values_from = c(
      n_permits, score, raw_score, alderman_effect_raw, alderman_effect_se,
      shrinkage, strictness_rank
    )
  ) %>%
  left_join(
    bind_rows(raw_alderman_rows),
    by = c("cutoff", "alderman"),
    relationship = "one-to-one"
  ) %>%
  mutate(
    adjusted_rank_difference =
      strictness_rank_same_day_stringency - strictness_rank_positive_log,
    absolute_adjusted_rank_difference = abs(adjusted_rank_difference)
  ) %>%
  arrange(cutoff, desc(absolute_adjusted_rank_difference), alderman)

write_csv(
  alderman_comparison,
  "../output/permit_zero_day_alderman_comparison.csv"
)

alderman_correlations <- list()
for (cutoff in audit_cutoffs) {
  cutoff_comparison <- alderman_comparison %>%
    filter(cutoff == !!cutoff)

  comparison_pairs <- list(
    c("score_positive_log", "score_positive_log_all_permit_volume"),
    c("score_positive_log", "score_same_day_stringency"),
    c("score_positive_log_all_permit_volume", "score_same_day_stringency"),
    c("raw_mean_log_positive_days", "raw_zero_day_share"),
    c("raw_mean_log_positive_days", "score_positive_log"),
    c("raw_zero_day_share", "score_same_day_stringency"),
    c("raw_score_same_day_stringency", "score_same_day_stringency")
  )

  for (comparison_pair in comparison_pairs) {
    comparison_data <- cutoff_comparison %>%
      select(
        first_measure = all_of(comparison_pair[1]),
        second_measure = all_of(comparison_pair[2])
      ) %>%
      filter(complete.cases(.))

    alderman_correlations[[length(alderman_correlations) + 1L]] <- tibble(
      cutoff,
      first_measure = comparison_pair[1],
      second_measure = comparison_pair[2],
      aldermen = nrow(comparison_data),
      pearson = cor(comparison_data$first_measure, comparison_data$second_measure),
      spearman = cor(
        comparison_data$first_measure,
        comparison_data$second_measure,
        method = "spearman"
      )
    )
  }
}

alderman_correlations <- bind_rows(alderman_correlations)
write_csv(
  alderman_correlations,
  "../output/permit_zero_day_alderman_correlations.csv"
)

plot_labels <- alderman_comparison %>%
  group_by(cutoff) %>%
  slice_max(absolute_adjusted_rank_difference, n = 8, with_ties = FALSE) %>%
  ungroup()

correlation_labels <- alderman_correlations %>%
  filter(
    first_measure == "score_positive_log",
    second_measure == "score_same_day_stringency"
  ) %>%
  transmute(
    cutoff,
    label = sprintf("Pearson correlation = %.2f", pearson)
  )

comparison_plot <- ggplot(
  alderman_comparison,
  aes(x = score_positive_log, y = score_same_day_stringency)
) +
  geom_hline(yintercept = 0, linewidth = 0.3, color = "grey70") +
  geom_vline(xintercept = 0, linewidth = 0.3, color = "grey70") +
  geom_point(aes(size = all_nonnegative_permits), alpha = 0.65, color = "#236192") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.7, color = "#9E2A2B") +
  ggrepel::geom_text_repel(
    data = plot_labels,
    aes(label = alderman),
    size = 3,
    min.segment.length = 0,
    box.padding = 0.35,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  geom_text(
    data = correlation_labels,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.05,
    vjust = 1.5,
    size = 3.4,
    inherit.aes = FALSE
  ) +
  facet_wrap(~cutoff, scales = "free") +
  scale_size_continuous(range = c(1.5, 5), guide = "none") +
  labs(
    title = "Conditional processing delay and same-day issuance capture different alderman dimensions",
    subtitle = "Both scores use the paper's controls and fixed effects; higher values indicate greater stringency",
    x = "Positive-duration processing-time score",
    y = "Lower adjusted same-day issuance probability"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey95")
  )

ggsave(
  "../output/permit_zero_day_alderman_comparison.png",
  comparison_plot,
  width = 12,
  height = 6.5,
  dpi = 220
)

score_summary <- bind_rows(
  tibble(
    metric = c(
      "clean_high_discretion_zero_day_permits",
      "zero_day_permits_with_ward_assignment",
      "zero_day_permits_with_alderman_and_controls",
      "zero_day_permits_in_final_nonnegative_sample",
      "positive_day_permits_in_production_sample"
    ),
    value = c(
      nrow(zero_permits),
      nrow(zero_ward_data),
      nrow(zero_controls),
      nrow(zero_analysis),
      nrow(positive_permits)
    )
  ),
  scores_long %>%
    group_by(cutoff, variant) %>%
    summarise(value = n(), .groups = "drop") %>%
    transmute(metric = paste0("aldermen_", cutoff, "_", variant), value)
)
write_csv(score_summary, "../output/permit_zero_day_score_summary.csv")

density_data <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  mutate(zone_group = zone_group_from_code(zone_code))

density_results <- list()
density_scores <- scores_long %>% filter(cutoff == 2022L)
for (variant_i in unique(density_scores$variant)) {
  score_lookup <- density_scores %>%
    filter(variant == variant_i) %>%
    select(alderman, score)

  variant_data <- density_data %>%
    select(-strictness_own, -strictness_neighbor, -sign, -signed_distance, -signed_distance_m) %>%
    left_join(score_lookup, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_own = score) %>%
    left_join(score_lookup, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_neighbor = score) %>%
    mutate(
      sign = case_when(
        strictness_own > strictness_neighbor ~ 1,
        strictness_own < strictness_neighbor ~ -1,
        TRUE ~ NA_real_
      ),
      signed_distance_m = dist_to_boundary_m * sign,
      side = as.integer(signed_distance_m > 0),
      lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
      strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
    )

  for (sample_i in c("All Construction", "Multifamily")) {
    for (outcome_i in c("density_far", "density_dupac")) {
      model_data <- variant_data %>%
        filter(
          construction_year >= 2006,
          construction_year <= 2022,
          arealotsf > 1,
          areabuilding > 1,
          if (sample_i == "All Construction") unitscount > 0 else unitscount > 1,
          dist_to_boundary_m <= 152.4,
          !is.na(ward_pair),
          !is.na(zone_code),
          !is.na(segment_id),
          is.finite(.data[[outcome_i]]),
          .data[[outcome_i]] > 0
        ) %>%
        mutate(outcome_value = log(.data[[outcome_i]]))

      continuous_model <- feols(
        outcome_value ~ strictness_own + lenient_dist + strict_dist +
          share_white_own + share_black_own + median_hh_income_own +
          share_bach_plus_own + homeownership_rate_own |
          zone_group + segment_id + construction_year,
        data = model_data,
        cluster = ~ward_pair
      )
      binary_model <- feols(
        outcome_value ~ side + lenient_dist + strict_dist +
          share_white_own + share_black_own + median_hh_income_own +
          share_bach_plus_own + homeownership_rate_own |
          zone_group + segment_id + construction_year,
        data = model_data,
        cluster = ~ward_pair
      )

      density_results[[length(density_results) + 1L]] <- extract_model(
        continuous_model, "strictness_own", "density", 2022L, variant_i,
        sample_i, outcome_i, "continuous"
      )
      density_results[[length(density_results) + 1L]] <- extract_model(
        binary_model, "side", "density", 2022L, variant_i,
        sample_i, outcome_i, "binary"
      )
    }
  }
}

event_panel <- read_parquet(
  "../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"
)
event_scores <- scores_long %>% filter(cutoff == 2014L)
event_results <- list()
for (variant_i in unique(event_scores$variant)) {
  score_lookup <- event_scores %>%
    filter(variant == variant_i) %>%
    select(alderman, score)

  variant_panel <- event_panel %>%
    select(-strictness_origin_frozen, -strictness_dest_frozen, -strictness_change_frozen) %>%
    left_join(score_lookup, by = c("alderman_origin_2014" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_origin_frozen = score) %>%
    left_join(score_lookup, by = c("alderman_dest_2014" = "alderman"), relationship = "many-to-one") %>%
    rename(strictness_dest_frozen = score) %>%
    mutate(strictness_change_frozen = strictness_dest_frozen - strictness_origin_frozen) %>%
    filter(
      dist_m <= 152.4,
      relative_year >= -5L,
      relative_year <= 5L,
      !is.na(strictness_change_frozen),
      !is.na(ward_pair_id),
      ward_pair_id != ""
    )

  for (outcome_i in c("n_high_discretion_application", "n_low_discretion_nosigns_application")) {
    model_data <- variant_panel %>%
      mutate(outcome = .data[[outcome_i]])
    pre_controls <- model_data %>%
      filter(relative_year < 0L) %>%
      group_by(block_id) %>%
      summarise(pre_period_permit_volume = sum(outcome), .groups = "drop") %>%
      mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0L))
    model_data <- model_data %>%
      left_join(pre_controls, by = "block_id", relationship = "many-to-one") %>%
      mutate(post_treat = as.integer(relative_year >= 0L) * strictness_change_frozen)

    model <- fepois(
      outcome ~ post_treat +
        pre_period_permit_volume:factor(year) +
        no_pre_period_permits:factor(year) |
        block_id + ward_pair_id^year,
      data = model_data,
      cluster = ~ward_pair_id,
      notes = FALSE
    )
    event_results[[length(event_results) + 1L]] <- extract_model(
      model, "post_treat", "permit_event_study", 2014L, variant_i,
      "ITT within 500 feet", outcome_i, "continuous assigned change"
    )
  }
}

write_csv(
  bind_rows(density_results, event_results),
  "../output/permit_zero_day_score_downstream_models.csv"
)
write_csv(
  porch_exclusion_changes,
  "../output/permit_score_porch_exclusion_changes.csv"
)
nonnegative_permits %>%
  mutate(month = format(month, "%Y-%m")) %>%
  write_csv("../output/permit_nonnegative_score_sample.csv")
