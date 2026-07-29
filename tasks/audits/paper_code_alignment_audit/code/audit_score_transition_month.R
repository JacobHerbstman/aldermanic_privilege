# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../_lib/alderman_uncertainty_helpers.R")
source("../../../_lib/border_pair_helpers.R")

config <- default_uncertainty_config()

permits <- read_csv(
  "../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv",
  show_col_types = FALSE
) %>%
  mutate(id = as.character(id), month = as.yearmon(month))

permit_points <- st_read(
  "../../../clean_building_permits/output/building_permits_clean.gpkg",
  query = paste(
    "SELECT id, application_start_date, geom",
    "FROM building_permits_clean",
    "WHERE application_start_date >= '2015-05-01'",
    "AND application_start_date < '2015-05-18'"
  ),
  quiet = TRUE
) %>%
  mutate(
    id = as.character(id),
    application_start_date = as.Date(application_start_date)
  ) %>%
  semi_join(permits %>% select(id), by = "id")

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE)
wards_2014 <- ward_panel %>%
  filter(year == 2014) %>%
  select(corrected_ward = ward) %>%
  group_by(corrected_ward) %>%
  summarise(.groups = "drop")

if (st_crs(permit_points) != st_crs(wards_2014)) {
  permit_points <- st_transform(permit_points, st_crs(wards_2014))
}

corrected_assignments <- permit_points %>%
  st_join(wards_2014, join = st_within, left = TRUE) %>%
  st_drop_geometry()

if (anyDuplicated(corrected_assignments$id) > 0 || anyNA(corrected_assignments$corrected_ward)) {
  stop("Exact-date May 2015 ward assignment is duplicated or incomplete.", call. = FALSE)
}

aldermen_may2015 <- read_csv(
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  mutate(month = as.yearmon(month)) %>%
  filter(month == as.yearmon("2015-05")) %>%
  transmute(corrected_ward = ward, corrected_alderman = alderman)

controls_2014 <- read_csv(
  "../../../create_ward_controls/output/ward_controls_2000_2023.csv",
  show_col_types = FALSE
) %>%
  filter(year == 2014) %>%
  transmute(
    corrected_ward = ward,
    corrected_pop_total = pop_total,
    corrected_median_hh_income = median_hh_income,
    corrected_share_black = share_black,
    corrected_share_hisp = share_hisp,
    corrected_share_white = share_white,
    corrected_homeownership_rate = homeownership_rate,
    corrected_share_bach_plus = share_bach_plus
  )

corrected_assignments <- corrected_assignments %>%
  left_join(aldermen_may2015, by = "corrected_ward", relationship = "many-to-one") %>%
  left_join(controls_2014, by = "corrected_ward", relationship = "many-to-one")

if (anyNA(corrected_assignments$corrected_homeownership_rate)) {
  stop(
    sprintf(
      "Corrected May 2015 assignments are missing %d ward controls; wards: %s.",
      sum(is.na(corrected_assignments$corrected_homeownership_rate)),
      paste(sort(unique(corrected_assignments$corrected_ward[
        is.na(corrected_assignments$corrected_homeownership_rate)
      ])), collapse = ",")
    ),
    call. = FALSE
  )
}

assignment_audit <- permits %>%
  filter(id %in% corrected_assignments$id) %>%
  select(
    id,
    application_month = month,
    production_ward = ward,
    production_alderman = alderman,
    production_map_version = map_version
  ) %>%
  left_join(corrected_assignments, by = "id", relationship = "one-to-one") %>%
  mutate(
    ward_changed = production_ward != corrected_ward,
    alderman_changed = is.na(corrected_alderman) | production_alderman != corrected_alderman
  ) %>%
  arrange(application_start_date, id)

write_csv(assignment_audit, "../output/score_transition_month_reassignment.csv")

corrected_permits <- permits %>%
  left_join(
    corrected_assignments %>%
      select(
        id,
        corrected_ward,
        corrected_alderman,
        starts_with("corrected_")
      ),
    by = "id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    ward = coalesce(corrected_ward, ward),
    alderman = if_else(!is.na(corrected_ward), corrected_alderman, alderman),
    pop_total = coalesce(corrected_pop_total, pop_total),
    median_hh_income = coalesce(corrected_median_hh_income, median_hh_income),
    share_black = coalesce(corrected_share_black, share_black),
    share_hisp = coalesce(corrected_share_hisp, share_hisp),
    share_white = coalesce(corrected_share_white, share_white),
    homeownership_rate = coalesce(corrected_homeownership_rate, homeownership_rate),
    share_bach_plus = coalesce(corrected_share_bach_plus, share_bach_plus),
    map_version = if_else(!is.na(corrected_ward), 1L, as.integer(map_version))
  ) %>%
  select(-starts_with("corrected_")) %>%
  filter(month <= as.yearmon("2022-12"))

corrected_result <- build_residualized_uncertainty_index(
  permits = corrected_permits,
  config = config,
  variant_id = "baseline",
  stage1_outcome = "log_processing_time",
  drop_covariates = "share_bach_plus",
  construction_rule = "Exact May 18, 2015 ward-map transition"
)

production_scores <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, production_score = uncertainty_index)

score_comparison <- production_scores %>%
  full_join(
    corrected_result$alderman_index %>%
      select(alderman, corrected_score = uncertainty_index, corrected_n_permits = n_permits),
    by = "alderman",
    relationship = "one-to-one"
  ) %>%
  mutate(
    score_difference = corrected_score - production_score,
    absolute_difference = abs(score_difference),
    production_rank = rank(production_score),
    corrected_rank = rank(corrected_score),
    rank_difference = corrected_rank - production_rank
  ) %>%
  arrange(desc(absolute_difference))

write_csv(score_comparison, "../output/score_transition_month_score_sensitivity.csv")

score_summary <- tibble(
  affected_score_input_rows = nrow(assignment_audit),
  rows_changing_ward = sum(assignment_audit$ward_changed),
  rows_changing_alderman = sum(assignment_audit$alderman_changed),
  aldermen = nrow(score_comparison),
  pearson_correlation = cor(score_comparison$production_score, score_comparison$corrected_score),
  spearman_correlation = cor(
    score_comparison$production_score,
    score_comparison$corrected_score,
    method = "spearman"
  ),
  mean_absolute_score_change = mean(score_comparison$absolute_difference),
  max_absolute_score_change = max(score_comparison$absolute_difference),
  aldermen_changing_rank = sum(score_comparison$rank_difference != 0)
)
write_csv(score_summary, "../output/score_transition_month_summary.csv")

corrected_lookup <- corrected_result$alderman_index %>%
  select(alderman, corrected_score = uncertainty_index)

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

parcels <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  left_join(
    corrected_lookup %>% rename(alderman_own = alderman, corrected_score_own = corrected_score),
    by = "alderman_own",
    relationship = "many-to-one"
  ) %>%
  left_join(
    corrected_lookup %>% rename(alderman_neighbor = alderman, corrected_score_neighbor = corrected_score),
    by = "alderman_neighbor",
    relationship = "many-to-one"
  ) %>%
  mutate(
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    production_side = as.integer(signed_distance_m > 0),
    production_lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    production_strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    corrected_side = as.integer(corrected_score_own > corrected_score_neighbor),
    corrected_signed_distance_m = if_else(corrected_side == 1L, dist_to_boundary_m, -dist_to_boundary_m),
    corrected_lenient_dist = abs(corrected_signed_distance_m) * as.integer(corrected_signed_distance_m <= 0),
    corrected_strict_dist = abs(corrected_signed_distance_m) * as.integer(corrected_signed_distance_m > 0)
  )

model_specs <- tribble(
  ~sample, ~outcome, ~outcome_var,
  "All Construction", "FAR", "density_far",
  "All Construction", "DUPAC", "density_dupac",
  "Multifamily", "FAR", "density_far",
  "Multifamily", "DUPAC", "density_dupac"
)

density_rows <- list()
for (i in seq_len(nrow(model_specs))) {
  sample_label <- model_specs$sample[i]
  outcome_label <- model_specs$outcome[i]
  outcome_var <- model_specs$outcome_var[i]

  data_i <- parcels %>%
    filter(
      construction_year >= 2006,
      construction_year <= 2022,
      arealotsf > 1,
      areabuilding > 1,
      if (sample_label == "All Construction") unitscount > 0 else unitscount > 1,
      dist_to_boundary_m <= 152.4,
      !is.na(ward_pair),
      !is.na(zone_code),
      !is.na(segment_id),
      segment_id != "",
      is.finite(.data[[outcome_var]]),
      .data[[outcome_var]] > 0
    ) %>%
    mutate(outcome_value = log(.data[[outcome_var]]))

  treatment_specs <- tribble(
    ~score_rule, ~treatment_var, ~lenient_var, ~strict_var,
    "production_continuous", "strictness_own", "production_lenient_dist", "production_strict_dist",
    "exact_transition_continuous", "corrected_score_own", "production_lenient_dist", "production_strict_dist",
    "production_binary", "production_side", "production_lenient_dist", "production_strict_dist",
    "exact_transition_binary", "corrected_side", "corrected_lenient_dist", "corrected_strict_dist"
  )

  for (j in seq_len(nrow(treatment_specs))) {
    formula_j <- as.formula(paste0(
      "outcome_value ~ ", treatment_specs$treatment_var[j], " + ",
      treatment_specs$lenient_var[j], " + ", treatment_specs$strict_var[j], " + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    ))
    model_j <- feols(formula_j, data = data_i, cluster = ~ward_pair)
    table_j <- coeftable(model_j)
    treatment_j <- treatment_specs$treatment_var[j]
    density_rows[[length(density_rows) + 1L]] <- tibble(
      sample = sample_label,
      outcome = outcome_label,
      score_rule = treatment_specs$score_rule[j],
      estimate = unname(table_j[treatment_j, "Estimate"]),
      standard_error = unname(table_j[treatment_j, "Std. Error"]),
      p_value = unname(table_j[treatment_j, "Pr(>|t|)"]),
      observations = nobs(model_j),
      ward_pairs = n_distinct(data_i$ward_pair[obs(model_j)])
    )
  }
}

write_csv(bind_rows(density_rows), "../output/score_transition_month_density_sensitivity.csv")
