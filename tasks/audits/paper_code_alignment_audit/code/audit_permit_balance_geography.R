# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")

panel <- read_parquet(
  "../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet"
)
if (anyDuplicated(panel[c("block_id", "year")]) > 0) {
  stop("Permit panel is not unique by block-year.", call. = FALSE)
}

balance_sample <- panel %>%
  filter(
    cohort == "2015",
    relative_year == -1,
    dist_m <= 152.4,
    !is.na(strictness_change_frozen),
    !is.na(ward_pair_id),
    ward_pair_id != ""
  ) %>%
  transmute(
    block_id = as.character(block_id),
    block_group_id = substr(as.character(block_id), 1, 12),
    ward_pair_id = as.character(ward_pair_id),
    treated = as.integer(switched)
  )

identifying_pairs <- balance_sample %>%
  group_by(ward_pair_id) %>%
  summarise(treatment_groups = n_distinct(treated), .groups = "drop") %>%
  filter(treatment_groups == 2) %>%
  select(ward_pair_id)

balance_sample <- balance_sample %>%
  semi_join(identifying_pairs, by = "ward_pair_id")

pre_period <- panel %>%
  transmute(
    block_id = as.character(block_id),
    relative_year,
    n_high_discretion_application,
    n_low_discretion_nosigns_application,
    n_new_construction_application
  ) %>%
  semi_join(balance_sample %>% select(block_id), by = "block_id") %>%
  filter(between(relative_year, -5, -1)) %>%
  group_by(block_id) %>%
  summarise(
    pre_high_discretion_applications = sum(n_high_discretion_application),
    no_pre_high_discretion_applications = as.integer(pre_high_discretion_applications == 0),
    pre_low_discretion_applications = sum(n_low_discretion_nosigns_application),
    no_pre_low_discretion_applications = as.integer(pre_low_discretion_applications == 0),
    pre_new_construction_applications = sum(n_new_construction_application),
    no_pre_new_construction_applications = as.integer(pre_new_construction_applications == 0),
    pre_period_years = n(),
    .groups = "drop"
  )
if (any(pre_period$pre_period_years != 5)) {
  stop("A balance-sample block does not have five pre-period years.", call. = FALSE)
}

block_group_controls <- read_csv(
  "../../../create_block_group_controls/output/block_group_controls.csv",
  col_types = cols(GEOID = col_character()),
  show_col_types = FALSE
) %>%
  filter(year == 2014) %>%
  transmute(
    block_group_id = GEOID,
    median_household_income = median_income,
    homeownership_rate,
    bachelors_share = share_bach_plus,
    black_share = percent_black,
    hispanic_share = percent_hispanic,
    median_gross_rent = median_rent,
    median_home_value,
    average_household_size = avg_household_size,
    median_age,
    population_density
  )

balance_sample <- balance_sample %>%
  left_join(
    pre_period %>% select(-pre_period_years),
    by = "block_id",
    relationship = "one-to-one"
  ) %>%
  left_join(
    block_group_controls,
    by = "block_group_id",
    relationship = "many-to-one"
  )

block_group_pair_counts <- balance_sample %>%
  distinct(block_group_id, ward_pair_id) %>%
  count(block_group_id, name = "ward_pairs")
block_group_treatment_counts <- balance_sample %>%
  distinct(block_group_id, treated) %>%
  count(block_group_id, name = "treatment_groups")
block_group_cell_counts <- balance_sample %>%
  distinct(block_group_id, ward_pair_id, treated) %>%
  count(block_group_id, name = "pair_treatment_cells")

geography_summary <- bind_rows(
  tibble(
    metric = c(
      "blocks",
      "block_groups",
      "ward_pairs",
      "block_group_pair_cells",
      "block_group_pair_treatment_cells",
      "block_groups_spanning_multiple_ward_pairs",
      "block_groups_containing_treated_and_control_blocks",
      "maximum_blocks_in_one_block_group_pair_treatment_cell",
      "median_blocks_per_block_group_pair_treatment_cell"
    ),
    value = c(
      nrow(balance_sample),
      n_distinct(balance_sample$block_group_id),
      n_distinct(balance_sample$ward_pair_id),
      nrow(balance_sample %>% distinct(block_group_id, ward_pair_id)),
      nrow(balance_sample %>% distinct(block_group_id, ward_pair_id, treated)),
      sum(block_group_pair_counts$ward_pairs > 1),
      sum(block_group_treatment_counts$treatment_groups > 1),
      max(balance_sample %>% count(block_group_id, ward_pair_id, treated) %>% pull(n)),
      median(balance_sample %>% count(block_group_id, ward_pair_id, treated) %>% pull(n))
    )
  ),
  block_group_pair_counts %>%
    count(ward_pairs, name = "value") %>%
    transmute(metric = paste0("block_groups_with_", ward_pairs, "_ward_pairs"), value),
  block_group_cell_counts %>%
    count(pair_treatment_cells, name = "value") %>%
    transmute(
      metric = paste0("block_groups_with_", pair_treatment_cells, "_pair_treatment_cells"),
      value
    )
)
write_csv(geography_summary, "../output/permit_balance_geography_summary.csv")

covariates <- c(
  "pre_high_discretion_applications",
  "no_pre_high_discretion_applications",
  "pre_low_discretion_applications",
  "no_pre_low_discretion_applications",
  "pre_new_construction_applications",
  "no_pre_new_construction_applications",
  "median_household_income",
  "homeownership_rate",
  "bachelors_share",
  "black_share",
  "hispanic_share",
  "median_gross_rent",
  "median_home_value",
  "average_household_size",
  "median_age",
  "population_density"
)

extract_result <- function(model, inference) {
  table <- coeftable(model)
  p_value_column <- grep("^Pr\\(", colnames(table), value = TRUE)[1]
  tibble(
    inference,
    estimate = unname(table["treated", "Estimate"]),
    se = unname(table["treated", "Std. Error"]),
    p_value = unname(table["treated", p_value_column]),
    observations = nobs(model)
  )
}

sensitivity_rows <- list()
for (variable_i in covariates) {
  block_data <- balance_sample %>%
    transmute(
      ward_pair_id,
      block_group_id,
      treated,
      outcome = .data[[variable_i]]
    ) %>%
    filter(is.finite(outcome))

  published_model <- feols(
    outcome ~ treated | ward_pair_id,
    data = block_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  block_group_model <- feols(
    outcome ~ treated | ward_pair_id,
    data = block_data,
    cluster = ~block_group_id,
    notes = FALSE
  )
  two_way_model <- feols(
    outcome ~ treated | ward_pair_id,
    data = block_data,
    cluster = ~ward_pair_id + block_group_id,
    notes = FALSE
  )

  collapsed_data <- block_data %>%
    group_by(ward_pair_id, block_group_id, treated) %>%
    summarise(
      outcome = mean(outcome),
      blocks = n(),
      .groups = "drop"
    )
  collapsed_equal_model <- feols(
    outcome ~ treated | ward_pair_id,
    data = collapsed_data,
    cluster = ~ward_pair_id,
    notes = FALSE
  )
  collapsed_weighted_model <- feols(
    outcome ~ treated | ward_pair_id,
    data = collapsed_data,
    weights = ~blocks,
    cluster = ~ward_pair_id,
    notes = FALSE
  )

  sensitivity_rows[[length(sensitivity_rows) + 1L]] <- bind_rows(
    extract_result(published_model, "published_block_rows_pair_cluster"),
    extract_result(block_group_model, "block_rows_block_group_cluster"),
    extract_result(two_way_model, "block_rows_pair_and_block_group_cluster"),
    extract_result(collapsed_equal_model, "equal_weight_block_group_pair_treatment_cells"),
    extract_result(collapsed_weighted_model, "block_weighted_block_group_pair_treatment_cells")
  ) %>%
    mutate(
      variable = variable_i,
      block_groups = n_distinct(block_data$block_group_id),
      ward_pairs = n_distinct(block_data$ward_pair_id)
    ) %>%
    relocate(variable)
}

write_csv(
  bind_rows(sensitivity_rows),
  "../output/permit_balance_inference_sensitivity.csv"
)
