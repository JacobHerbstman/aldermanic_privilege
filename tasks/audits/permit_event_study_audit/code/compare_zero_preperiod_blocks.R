# setwd("tasks/audits/permit_event_study_audit/code")

source("../../../setup_environment/code/packages.R")

panel <- arrow::read_parquet(
  "../input/permit_block_year_panel_2015.parquet"
)

if (anyDuplicated(panel[c("block_id", "year")])) {
  stop("The permit panel must be unique by block and year.")
}

blocks <- panel |>
  dplyr::filter(
    relative_year == -1L,
    dist_m <= 152.4,
    !is.na(ward_pair_id),
    ward_pair_id != "",
    stable_both
  ) |>
  dplyr::transmute(
    block_id = as.character(block_id),
    block_group_id = substr(as.character(block_id), 1, 12),
    ward_pair_id = as.character(ward_pair_id),
    dist_boundary_feet = dist_m * 3.28084,
    assigned_more_stringent = as.integer(
      strictness_change_frozen > 0
    ),
    assigned_more_lenient = as.integer(
      strictness_change_frozen < 0
    )
  )

if (anyDuplicated(blocks$block_id)) {
  stop("The stable balance sample must be unique by block.")
}

pre_period <- panel |>
  dplyr::filter(
    relative_year >= -5L,
    relative_year <= -1L
  ) |>
  dplyr::semi_join(
    blocks |>
      dplyr::select(block_id),
    by = "block_id"
  ) |>
  dplyr::group_by(block_id) |>
  dplyr::summarise(
    pre_high_discretion = sum(n_high_discretion_application),
    pre_low_discretion = sum(
      n_low_discretion_nosigns_application
    ),
    pre_new_construction = sum(n_new_construction_application),
    pre_period_years = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    no_pre_high_discretion = as.integer(pre_high_discretion == 0),
    no_pre_low_discretion = as.integer(pre_low_discretion == 0),
    no_pre_new_construction = as.integer(
      pre_new_construction == 0
    )
  )

if (
  anyDuplicated(pre_period$block_id) ||
    any(pre_period$pre_period_years != 5L) ||
    nrow(pre_period) != nrow(blocks)
) {
  stop("Every stable-sample block must have five pre-period years.")
}

block_group_controls <- readr::read_csv(
  "../input/block_group_controls.csv",
  col_types = readr::cols(GEOID = readr::col_character()),
  show_col_types = FALSE
) |>
  dplyr::filter(year == 2014L) |>
  dplyr::transmute(
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

if (
  nrow(block_group_controls) == 0L ||
    anyDuplicated(block_group_controls$block_group_id)
) {
  stop("The 2014 block-group controls failed validation.")
}

balance_data <- blocks |>
  dplyr::left_join(
    pre_period,
    by = "block_id",
    relationship = "one-to-one"
  ) |>
  dplyr::left_join(
    block_group_controls,
    by = "block_group_id",
    relationship = "many-to-one"
  )

covariates <- tibble::tribble(
  ~section, ~variable, ~label,
  "Design", "dist_boundary_feet", "Distance to boundary (feet)",
  "Design", "assigned_more_stringent", "Assigned toward greater stringency",
  "Design", "assigned_more_lenient", "Assigned toward greater leniency",
  "Permit history", "pre_low_discretion", "Low-discretion permits, 2010--2014",
  "Permit history", "no_pre_low_discretion", "No low-discretion permit, 2010--2014",
  "Permit history", "pre_new_construction", "New-construction permits, 2010--2014",
  "Permit history", "no_pre_new_construction", "No new-construction permit, 2010--2014",
  "Neighborhood characteristics", "median_household_income", "Median household income",
  "Neighborhood characteristics", "homeownership_rate", "Homeownership rate",
  "Neighborhood characteristics", "bachelors_share", "Bachelor's degree or higher share",
  "Neighborhood characteristics", "black_share", "Black population share",
  "Neighborhood characteristics", "hispanic_share", "Hispanic population share",
  "Neighborhood characteristics", "median_gross_rent", "Median gross rent",
  "Neighborhood characteristics", "median_home_value", "Median home value",
  "Neighborhood characteristics", "average_household_size", "Average household size",
  "Neighborhood characteristics", "median_age", "Median age",
  "Neighborhood characteristics", "population_density", "Population per square kilometer"
)

results <- vector("list", nrow(covariates))

for (i in seq_len(nrow(covariates))) {
  variable <- covariates$variable[i]
  regression_data <- balance_data |>
    dplyr::transmute(
      ward_pair_id,
      block_group_id,
      no_pre_high_discretion,
      outcome = .data[[variable]]
    ) |>
    dplyr::filter(is.finite(outcome))

  model <- fixest::feols(
    outcome ~ no_pre_high_discretion | ward_pair_id,
    data = regression_data,
    cluster = ~ward_pair_id + block_group_id,
    notes = FALSE
  )

  positive_mean <- mean(
    regression_data$outcome[
      regression_data$no_pre_high_discretion == 0L
    ]
  )
  zero_mean <- mean(
    regression_data$outcome[
      regression_data$no_pre_high_discretion == 1L
    ]
  )
  outcome_sd <- stats::sd(regression_data$outcome)

  results[[i]] <- tibble::tibble(
    section = covariates$section[i],
    variable,
    label = covariates$label[i],
    positive_history_mean = positive_mean,
    zero_history_mean = zero_mean,
    pair_adjusted_difference = stats::coef(model)[
      "no_pre_high_discretion"
    ],
    standard_error = fixest::se(model)[
      "no_pre_high_discretion"
    ],
    p_value = fixest::pvalue(model)[
      "no_pre_high_discretion"
    ],
    standardized_difference = pair_adjusted_difference / outcome_sd,
    observations = stats::nobs(model)
  )
}

results <- dplyr::bind_rows(results)

joint_sets <- list(
  design = covariates$variable[covariates$section == "Design"],
  neighborhood = covariates$variable[
    covariates$section == "Neighborhood characteristics"
  ],
  design_and_neighborhood = covariates$variable[
    covariates$section != "Permit history"
  ]
)

joint_results <- vector("list", length(joint_sets))

for (i in seq_along(joint_sets)) {
  joint_variables <- joint_sets[[i]]
  joint_data <- balance_data |>
    dplyr::select(
      ward_pair_id,
      block_group_id,
      no_pre_high_discretion,
      dplyr::all_of(joint_variables)
    ) |>
    tidyr::drop_na()

  joint_model <- fixest::feols(
    stats::as.formula(
      paste(
        "no_pre_high_discretion ~",
        paste(
          sprintf("scale(%s)", joint_variables),
          collapse = " + "
        ),
        "| ward_pair_id"
      )
    ),
    data = joint_data,
    cluster = ~ward_pair_id + block_group_id,
    notes = FALSE
  )
  joint_test <- fixest::wald(joint_model, print = FALSE)

  joint_results[[i]] <- tibble::tibble(
    covariate_set = names(joint_sets)[i],
    joint_test_p_value = joint_test$p,
    complete_case_observations = stats::nobs(joint_model),
    zero_history_blocks = sum(
      balance_data$no_pre_high_discretion == 1L
    ),
    positive_history_blocks = sum(
      balance_data$no_pre_high_discretion == 0L
    ),
    ward_pairs = dplyr::n_distinct(balance_data$ward_pair_id),
    block_groups = dplyr::n_distinct(balance_data$block_group_id)
  )
}

joint_results <- dplyr::bind_rows(joint_results)

readr::write_csv(
  results,
  "../output/stable_zero_preperiod_observable_balance_500ft.csv"
)
readr::write_csv(
  joint_results,
  "../output/stable_zero_preperiod_observable_joint_test_500ft.csv"
)
