# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_covariate_balance/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

covariates <- tribble(
  ~covariate_group, ~covariate, ~label, ~display_scale,
  "Density model controls", "share_white_own", "Share White", 1,
  "Density model controls", "share_black_own", "Share Black", 1,
  "Density model controls", "median_hh_income_own", "Median household income (thousands)", 1000,
  "Density model controls", "share_bach_plus_own", "Bachelor's degree or higher share", 1,
  "Density model controls", "homeownership_rate_own", "Homeownership rate", 1,
  "Block-group characteristics", "percent_white_bg", "Share White", 1,
  "Block-group characteristics", "percent_black_bg", "Share Black", 1,
  "Block-group characteristics", "percent_hispanic_bg", "Share Hispanic", 1,
  "Block-group characteristics", "homeownership_rate_bg", "Homeownership rate", 1,
  "Block-group characteristics", "median_income_bg", "Median household income (thousands)", 1000,
  "Block-group characteristics", "share_bach_plus_bg", "Bachelor's degree or higher share", 1,
  "Block-group characteristics", "median_rent_bg", "Median gross rent (thousands)", 1000,
  "Block-group characteristics", "median_home_value_bg", "Median home value (thousands)", 1000,
  "Block-group characteristics", "avg_household_size_bg", "Average household size", 1,
  "Block-group characteristics", "median_age_bg", "Median age", 1,
  "Block-group characteristics", "population_density_bg", "Population per square kilometer (thousands)", 1000
)

parcels <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    construction_year = as.integer(construction_year),
    ward_pair = as.character(ward_pair),
    segment_id = as.character(segment_id),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2,
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    between(construction_year, 2006, 2022),
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(construction_zone_group),
    !is.na(segment_id),
    segment_id != "",
    is.finite(pair_average_score),
    is.finite(continuous_score_difference)
  )

balance_rows <- list()
joint_rows <- list()

for (sample_name in c("All construction", "Multifamily")) {
  sample_data <- if (sample_name == "All construction") {
    parcels %>% filter(unitscount > 0)
  } else {
    parcels %>% filter(unitscount > 1)
  }

  for (group_name in unique(covariates$covariate_group)) {
    group_covariates <- covariates %>% filter(covariate_group == group_name)
    covariate_names <- group_covariates$covariate

    common_data <- sample_data %>%
      filter(if_all(all_of(covariate_names), ~ is.finite(.x)))
    if (nrow(common_data) == 0) {
      stop(sprintf("No complete observations remain for %s, %s.", sample_name, group_name), call. = FALSE)
    }

    for (i in seq_len(nrow(group_covariates))) {
      covariate_name <- group_covariates$covariate[i]
      covariate_sd <- sd(common_data[[covariate_name]])
      if (!is.finite(covariate_sd) || covariate_sd <= 0) {
        stop(sprintf("%s has no variation in the %s sample.", covariate_name, sample_name), call. = FALSE)
      }

      model_data <- common_data %>%
        mutate(balance_outcome = (.data[[covariate_name]] - mean(.data[[covariate_name]])) / covariate_sd)

      binary_model <- feols(
        balance_outcome ~ side + pair_average_score + lenient_dist + strict_dist |
          segment_id + construction_year,
        data = model_data,
        cluster = ~ward_pair,
        warn = FALSE
      )
      continuous_model <- feols(
        balance_outcome ~ continuous_score_difference + pair_average_score + lenient_dist + strict_dist |
          segment_id + construction_year,
        data = model_data,
        cluster = ~ward_pair,
        warn = FALSE
      )

      binary_table <- coeftable(binary_model)
      continuous_table <- coeftable(continuous_model)
      display_scale <- group_covariates$display_scale[i]

      balance_rows[[length(balance_rows) + 1L]] <- tibble(
        sample = sample_name,
        covariate_group = group_name,
        covariate = covariate_name,
        label = group_covariates$label[i],
        lenient_mean = mean(model_data[[covariate_name]][model_data$side == 0]) / display_scale,
        stringent_mean = mean(model_data[[covariate_name]][model_data$side == 1]) / display_scale,
        binary_estimate_sd = unname(binary_table["side", "Estimate"]),
        binary_se = unname(binary_table["side", "Std. Error"]),
        binary_p_value = unname(binary_table["side", "Pr(>|t|)"]),
        continuous_estimate_sd = unname(continuous_table["continuous_score_difference", "Estimate"]),
        continuous_se = unname(continuous_table["continuous_score_difference", "Std. Error"]),
        continuous_p_value = unname(continuous_table["continuous_score_difference", "Pr(>|t|)"]),
        observations = nrow(model_data),
        ward_pairs = n_distinct(model_data$ward_pair),
        segments = n_distinct(model_data$segment_id)
      )
    }

    standardized_names <- paste0("z_", covariate_names)
    joint_data <- common_data
    for (i in seq_along(covariate_names)) {
      joint_data[[standardized_names[i]]] <- as.numeric(scale(joint_data[[covariate_names[i]]]))
    }
    joint_controls <- paste(standardized_names, collapse = " + ")

    for (treatment_name in c("side", "continuous_score_difference")) {
      joint_model <- feols(
        as.formula(paste0(
          treatment_name,
          " ~ ", joint_controls,
          " + pair_average_score + lenient_dist + strict_dist | segment_id + construction_year"
        )),
        data = joint_data,
        cluster = ~ward_pair,
        warn = FALSE
      )
      joint_test <- wald(joint_model, keep = "^z_", print = FALSE)

      joint_rows[[length(joint_rows) + 1L]] <- tibble(
        sample = sample_name,
        covariate_group = group_name,
        treatment = treatment_name,
        statistic = joint_test$stat,
        p_value = joint_test$p,
        numerator_df = joint_test$df1,
        denominator_df = joint_test$df2,
        observations = nobs(joint_model),
        ward_pairs = n_distinct(joint_data$ward_pair),
        segments = n_distinct(joint_data$segment_id)
      )
    }
  }
}

balance <- bind_rows(balance_rows)
joint_tests <- bind_rows(joint_rows)

write_csv(balance, "../output/density_covariate_balance.csv")
write_csv(joint_tests, "../output/density_covariate_joint_tests.csv")

table_data <- balance %>%
  filter(covariate_group == "Block-group characteristics") %>%
  mutate(
    table_label = case_when(
      covariate == "median_income_bg" ~ "Median household income",
      covariate == "median_rent_bg" ~ "Median gross rent",
      covariate == "median_home_value_bg" ~ "Median home value",
      covariate == "population_density_bg" ~ "Population density",
      TRUE ~ label
    )
  ) %>%
  select(sample, table_label, binary_estimate_sd, binary_se) %>%
  pivot_wider(
    names_from = sample,
    values_from = c(binary_estimate_sd, binary_se)
  )

table_lines <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Adjusted difference (SD)} \\\\",
  "\\cmidrule(lr){2-3}",
  " & All construction & Multifamily \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(table_data))) {
  table_lines <- c(
    table_lines,
    sprintf(
      "%s & %.3f & %.3f \\\\",
      table_data$table_label[i],
      table_data$`binary_estimate_sd_All construction`[i],
      table_data$binary_estimate_sd_Multifamily[i]
    ),
    sprintf(
      " & (%.3f) & (%.3f) \\\\",
      table_data$`binary_se_All construction`[i],
      table_data$binary_se_Multifamily[i]
    )
  )
}

binary_joint <- joint_tests %>%
  filter(covariate_group == "Block-group characteristics", treatment == "side")
table_lines <- c(
  table_lines,
  "\\midrule",
  sprintf(
    "Joint-test $p$-value & %.3f & %.3f \\\\",
    binary_joint$p_value[binary_joint$sample == "All construction"],
    binary_joint$p_value[binary_joint$sample == "Multifamily"]
  ),
  sprintf(
    "Observations & %s & %s \\\\",
    format(binary_joint$observations[binary_joint$sample == "All construction"], big.mark = ",", scientific = FALSE),
    format(binary_joint$observations[binary_joint$sample == "Multifamily"], big.mark = ",", scientific = FALSE)
  ),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(table_lines, "../output/density_covariate_balance.tex")
