# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

ward_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

block_group_controls <- c(
  "percent_white_bg",
  "percent_black_bg",
  "median_income_bg",
  "share_bach_plus_bg",
  "homeownership_rate_bg"
)

parcels <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    zone_group = zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    arealotsf > 1,
    areabuilding > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != ""
  )

specifications <- list(
  production_own_ward_year = ward_controls,
  no_demographic_controls = character(),
  block_group_period_controls = block_group_controls
)

model_rows <- list()

for (sample_name in c("all", "multifamily")) {
  sample_data <- if (sample_name == "all") {
    parcels %>% filter(unitscount > 0)
  } else {
    parcels %>% filter(unitscount > 1)
  }

  for (outcome_name in c("density_far", "density_dupac")) {
    outcome_data <- sample_data %>%
      filter(is.finite(.data[[outcome_name]]), .data[[outcome_name]] > 0) %>%
      mutate(outcome = log(.data[[outcome_name]]))

    for (treatment_name in c("continuous", "binary")) {
      treatment_variable <- if (treatment_name == "continuous") "strictness_own" else "side"

      for (specification_name in names(specifications)) {
        controls <- specifications[[specification_name]]
        right_hand_side <- c(treatment_variable, "lenient_dist", "strict_dist", controls)
        model <- feols(
          as.formula(paste0(
            "outcome ~ ",
            paste(right_hand_side, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = outcome_data,
          cluster = ~ward_pair
        )

        coefficient_table <- coeftable(model)
        model_rows[[length(model_rows) + 1L]] <- tibble(
          sample = sample_name,
          outcome = outcome_name,
          treatment = treatment_name,
          specification = specification_name,
          estimate = unname(coefficient_table[treatment_variable, "Estimate"]),
          se = unname(coefficient_table[treatment_variable, "Std. Error"]),
          p_value = unname(coefficient_table[treatment_variable, "Pr(>|t|)"]),
          n = nobs(model),
          ward_pairs = n_distinct(outcome_data$ward_pair[obs(model)])
        )
      }
    }
  }
}

write_csv(
  bind_rows(model_rows),
  "../output/density_demographic_control_sensitivity.csv"
)

ward_control_vintages <- read_csv(
  "../../../create_ward_controls/output/ward_controls_2000_2023.csv",
  show_col_types = FALSE
) %>%
  mutate(
    source_vintage = case_when(
      year <= 2009 ~ "2000 Decennial SF3",
      year <= 2012 ~ "2010 Decennial counts plus 2013 ACS economics",
      TRUE ~ paste0(year, " ACS 5-year")
    )
  ) %>%
  count(year, source_vintage, name = "ward_rows")

write_csv(
  ward_control_vintages,
  "../output/density_ward_control_vintages.csv"
)
