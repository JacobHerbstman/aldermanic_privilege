# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

panel_paths <- read_csv("../output/permit_event_rebuild_status.csv", show_col_types = FALSE)

estimate_panel <- function(panel_label, panel_path, outcome_var, sample_label) {
  data <- read_parquet(panel_path) %>%
    filter(
      dist_m <= 152.4,
      relative_year >= -5L,
      relative_year <= 5L,
      !is.na(strictness_change_frozen),
      !is.na(ward_pair_id),
      ward_pair_id != ""
    ) %>%
    mutate(
      outcome = .data[[outcome_var]],
      strictness_change = strictness_change_frozen
    )
  if (sample_label == "stable") {
    data <- data %>% filter(stable_both)
  }

  pre_period_controls <- data %>%
    filter(relative_year < 0L) %>%
    group_by(block_id) %>%
    summarise(pre_period_permit_volume = sum(outcome), .groups = "drop") %>%
    mutate(no_pre_period_permits = as.integer(pre_period_permit_volume == 0L))

  data <- data %>%
    left_join(pre_period_controls, by = "block_id", relationship = "many-to-one") %>%
    mutate(post_treat = as.integer(relative_year >= 0L) * strictness_change)

  event_model <- fepois(
    outcome ~ i(relative_year, strictness_change, ref = -1) +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  pooled_model <- fepois(
    outcome ~ post_treat +
      pre_period_permit_volume:factor(year) +
      no_pre_period_permits:factor(year) |
      block_id + ward_pair_id^year,
    data = data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  lead_terms <- names(coef(event_model))[grepl("^relative_year::-[2-5]:strictness_change$", names(coef(event_model)))]
  pretrend <- wald(event_model, lead_terms, print = FALSE)
  used_rows <- obs(pooled_model)

  tibble(
    panel = panel_label,
    outcome = outcome_var,
    sample = sample_label,
    coefficient = coef(pooled_model)[["post_treat"]],
    standard_error = se(pooled_model)[["post_treat"]],
    p_value_fixest = pvalue(pooled_model)[["post_treat"]],
    p_value_normal = 2 * pnorm(-abs(coef(pooled_model)[["post_treat"]] / se(pooled_model)[["post_treat"]])),
    percent_effect = 100 * expm1(coef(pooled_model)[["post_treat"]]),
    pretrend_p_value = pretrend$p,
    pretrend_terms = length(lead_terms),
    input_observations = nrow(data),
    estimation_observations = nobs(pooled_model),
    input_blocks = n_distinct(data$block_id),
    estimation_blocks = n_distinct(data$block_id[used_rows]),
    input_ward_pairs = n_distinct(data$ward_pair_id),
    estimation_ward_pairs = n_distinct(data$ward_pair_id[used_rows])
  )
}

model_results <- list()
for (panel_i in seq_len(nrow(panel_paths))) {
  for (outcome_i in c("n_high_discretion_application", "n_low_discretion_nosigns_application")) {
    model_results[[length(model_results) + 1L]] <- estimate_panel(
      panel_paths$panel[panel_i],
      panel_paths$path[panel_i],
      outcome_i,
      "itt"
    )
  }
  model_results[[length(model_results) + 1L]] <- estimate_panel(
    panel_paths$panel[panel_i],
    panel_paths$path[panel_i],
    "n_high_discretion_application",
    "stable"
  )
}
write_csv(bind_rows(model_results), "../output/permit_event_model_reproduction.csv")

panels <- lapply(seq_len(nrow(panel_paths)), function(i) {
  read_parquet(panel_paths$path[i]) %>%
    filter(relative_year == -1L, dist_m <= 152.4) %>%
    mutate(panel = panel_paths$panel[i])
})
names(panels) <- panel_paths$panel

sample_comparison <- bind_rows(panels) %>%
  group_by(panel) %>%
  summarise(
    blocks = n_distinct(block_id),
    switched_blocks = n_distinct(block_id[switched]),
    unchanged_blocks = n_distinct(block_id[!switched]),
    stable_blocks = n_distinct(block_id[stable_both]),
    ward_pairs = n_distinct(ward_pair_id),
    identifying_pairs = n_distinct(ward_pair_id[ward_pair_id %in% {
      pair_status <- tibble(ward_pair_id = ward_pair_id, switched = switched) %>%
        group_by(ward_pair_id) %>%
        summarise(n_groups = n_distinct(switched), .groups = "drop") %>%
        filter(n_groups == 2L) %>%
        pull(ward_pair_id)
      pair_status
    }]),
    .groups = "drop"
  )
write_csv(sample_comparison, "../output/permit_event_sample_comparison.csv")

production <- panels[["production"]] %>%
  select(block_id, year, ward_origin, ward_dest, switched, ward_pair_id, dist_m,
         strictness_change_frozen, n_high_discretion_application,
         n_low_discretion_nosigns_application)
fresh_current <- panels[["fresh_current_rule"]] %>%
  select(block_id, year, ward_origin, ward_dest, switched, ward_pair_id, dist_m,
         strictness_change_frozen, n_high_discretion_application,
         n_low_discretion_nosigns_application)

panel_compare <- full_join(
  production %>% rename_with(~ paste0("production_", .x), -c(block_id, year)),
  fresh_current %>% rename_with(~ paste0("fresh_", .x), -c(block_id, year)),
  by = c("block_id", "year"),
  relationship = "one-to-one"
) %>%
  summarise(
    production_rows = sum(!is.na(production_ward_origin)),
    fresh_rows = sum(!is.na(fresh_ward_origin)),
    common_rows = sum(!is.na(production_ward_origin) & !is.na(fresh_ward_origin)),
    production_only_rows = sum(!is.na(production_ward_origin) & is.na(fresh_ward_origin)),
    fresh_only_rows = sum(is.na(production_ward_origin) & !is.na(fresh_ward_origin)),
    changed_ward_rows = sum(
      !is.na(production_ward_origin) & !is.na(fresh_ward_origin) &
        (production_ward_origin != fresh_ward_origin | production_ward_dest != fresh_ward_dest)
    ),
    changed_pair_rows = sum(
      !is.na(production_ward_pair_id) & !is.na(fresh_ward_pair_id) &
        production_ward_pair_id != fresh_ward_pair_id
    ),
    max_distance_difference_m = max(abs(production_dist_m - fresh_dist_m), na.rm = TRUE),
    max_score_difference = max(abs(production_strictness_change_frozen - fresh_strictness_change_frozen), na.rm = TRUE),
    changed_high_outcome_rows = sum(
      production_n_high_discretion_application != fresh_n_high_discretion_application,
      na.rm = TRUE
    ),
    changed_low_outcome_rows = sum(
      production_n_low_discretion_nosigns_application != fresh_n_low_discretion_nosigns_application,
      na.rm = TRUE
    )
  )
write_csv(panel_compare, "../output/permit_event_panel_rebuild_comparison.csv")

treatment_pre <- read_csv(
  "../../../create_block_treatment_panel/output/block_treatment_pre_scores.csv",
  show_col_types = FALSE
) %>%
  filter(cohort == 2015 | cohort == "2015")

treatment_rules <- treatment_pre %>%
  count(
    switched,
    ward_had_turnover,
    has_complete_ward_assignment,
    valid,
    name = "blocks"
  ) %>%
  arrange(valid, switched, ward_had_turnover)
write_csv(treatment_rules, "../output/permit_event_treatment_sample_rules.csv")

high_discretion_types <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION",
  "PERMIT - WRECKING/DEMOLITION",
  "PERMIT - PORCH CONSTRUCTION",
  "PERMIT - REINSTATE REVOKED PMT"
)

raw_permits <- fread(
  "../../../download_building_permits/output/building_permits.csv",
  select = c(
    "id", "permit_status", "permit_type", "application_start_date",
    "issue_date", "processing_time", "latitude", "longitude",
    "xcoordinate", "ycoordinate"
  ),
  na.strings = c("", "NA")
) %>%
  as_tibble() %>%
  mutate(
    id = as.character(id),
    application_date = as.Date(application_start_date, format = "%m/%d/%Y"),
    issue_date_parsed = as.Date(issue_date, format = "%m/%d/%Y"),
    application_year = year(application_date),
    application_month = as.yearmon(application_date),
    permit_status = str_to_upper(trimws(permit_status)),
    permit_type = str_to_upper(trimws(permit_type)),
    processing_time = suppressWarnings(as.numeric(processing_time)),
    high_discretion = permit_type %in% high_discretion_types,
    status_group = case_when(
      permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING") ~ "issued_status",
      permit_status == "EXPIRED" ~ "expired",
      permit_status %in% c("CANCELLED", "REVOKED", "SUSPENDED") ~ "production_excluded_status",
      is.na(permit_status) | permit_status == "" ~ "missing_status",
      TRUE ~ "other_status"
    ),
    processing_time_group = case_when(
      is.na(processing_time) ~ "missing",
      processing_time < 0 ~ "negative",
      processing_time == 0 ~ "zero",
      TRUE ~ "positive"
    ),
    nonnegative_processing_time = processing_time_group %in% c("zero", "positive"),
    has_issue_date = !is.na(issue_date_parsed),
    issue_precedes_application = has_issue_date & issue_date_parsed < application_date,
    computed_processing_time = as.numeric(issue_date_parsed - application_date),
    processing_time_gap = processing_time - computed_processing_time,
    has_coordinate_source =
      (is.finite(latitude) & is.finite(longitude)) |
      (is.finite(xcoordinate) & is.finite(ycoordinate))
  )

clean_ids <- st_read(
  "../../../clean_building_permits/output/building_permits_clean.gpkg",
  query = "SELECT id, permit_status FROM building_permits_clean",
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  transmute(id = as.character(id), survives_cleaning = TRUE)

raw_with_clean_flag <- raw_permits %>%
  left_join(clean_ids, by = "id", relationship = "one-to-one") %>%
  mutate(survives_cleaning = replace_na(survives_cleaning, FALSE))

status_summary <- raw_with_clean_flag %>%
  filter(application_year >= 2010L, application_year <= 2020L) %>%
  count(
    high_discretion,
    status_group,
    permit_status,
    nonnegative_processing_time,
    has_issue_date,
    issue_precedes_application,
    has_coordinate_source,
    survives_cleaning,
    name = "permits"
  ) %>%
  arrange(desc(high_discretion), status_group, permit_status,
          desc(nonnegative_processing_time), desc(has_coordinate_source))
write_csv(status_summary, "../output/permit_status_eligibility.csv")

processing_validation <- raw_with_clean_flag %>%
  filter(
    application_year >= 2006L,
    application_month <= as.yearmon("2022-12"),
    high_discretion | permit_type != "PERMIT - SIGNS"
  ) %>%
  mutate(
    permit_group = if_else(high_discretion, "High-Discretion", "Low-Discretion"),
    exact_date_match = is.finite(processing_time_gap) & processing_time_gap == 0
  ) %>%
  group_by(permit_group) %>%
  summarise(
    raw_applications = n(),
    survives_cleaning = sum(survives_cleaning),
    missing_processing_time = sum(processing_time_group == "missing"),
    negative_processing_time = sum(processing_time_group == "negative"),
    zero_processing_time = sum(processing_time_group == "zero"),
    positive_processing_time = sum(processing_time_group == "positive"),
    missing_issue_date = sum(!has_issue_date),
    issue_precedes_application = sum(issue_precedes_application, na.rm = TRUE),
    finite_portal_and_computed_days = sum(is.finite(processing_time_gap)),
    exact_portal_date_difference_matches = sum(exact_date_match),
    nonzero_portal_date_difference_gap = sum(is.finite(processing_time_gap) & processing_time_gap != 0),
    mean_absolute_gap_days = mean(abs(processing_time_gap), na.rm = TRUE),
    max_absolute_gap_days = max(abs(processing_time_gap), na.rm = TRUE),
    .groups = "drop"
  )
write_csv(processing_validation, "../output/permit_processing_time_validation.csv")

permit_type_counts <- raw_with_clean_flag %>%
  filter(
    application_year >= 2006L,
    application_month <= as.yearmon("2022-12"),
    survives_cleaning,
    processing_time > 0,
    high_discretion | permit_type != "PERMIT - SIGNS"
  ) %>%
  mutate(permit_group = if_else(high_discretion, "High-Discretion", "Low-Discretion")) %>%
  count(permit_group, permit_type, sort = TRUE, name = "permits")
write_csv(permit_type_counts, "../output/permit_type_sample_counts.csv")
