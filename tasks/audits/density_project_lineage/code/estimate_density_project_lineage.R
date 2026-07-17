# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_project_lineage/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

lineage <- read_csv(
  "../output/density_project_lineage.csv",
  show_col_types = FALSE,
  col_types = cols(member_pins = col_character(), .default = col_guess())
)

if (anyDuplicated(lineage$project_key) > 0) {
  stop("Project lineage is not unique by project key.", call. = FALSE)
}

lineage_pins <- lineage %>%
  select(
    project_key,
    member_pins,
    recovered_500ft,
    lineage_status,
    recommended_action
  ) %>%
  separate_longer_delim(member_pins, delim = ";") %>%
  rename(pin = member_pins)

if (anyDuplicated(lineage_pins$pin) > 0) {
  stop("A historical PIN belongs to more than one lineage group.", call. = FALSE)
}

recovered <- read_csv(
  "../input/density_historical_recovered_exact_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    .default = col_guess()
  )
) %>%
  left_join(lineage_pins, by = "pin", relationship = "one-to-one")

if (anyDuplicated(recovered$pin) > 0) {
  stop("Recovered exact-year rows are not unique by PIN.", call. = FALSE)
}
if (any(is.na(recovered$project_key))) {
  stop("A recovered exact-year row is missing from the lineage.", call. = FALSE)
}

address_recovered <- read_csv(
  "../output/density_parcel_address_recovered_model_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    source_class = col_character(),
    .default = col_guess()
  )
)
if (anyDuplicated(address_recovered$pin) > 0) {
  stop("Address-recovered rows are not unique by PIN.", call. = FALSE)
}

production <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    .default = col_guess()
  )
) %>%
  mutate(
    sample_source = "production_current_2025_coordinate",
    project_key = NA_character_
  )

analysis_samples <- list(
  production = production,
  naive_322_pin_recovery = bind_rows(production, recovered),
  lineage_182_pin_recovery = bind_rows(
    production,
    recovered %>% filter(recommended_action == "candidate_for_recovery")
  ),
  lineage_184_candidate_recovery = bind_rows(
    production,
    recovered %>% filter(recommended_action == "candidate_for_recovery"),
    address_recovered
  )
)

model_rows <- list()
for (analysis_sample in names(analysis_samples)) {
  parcels <- analysis_samples[[analysis_sample]] %>%
    ensure_meter_distance_columns() %>%
    mutate(
      pin = as.character(pin),
      construction_year = as.integer(construction_year),
      segment_id = as.character(segment_id),
      zone_group = zone_group_from_code(zone_code),
      lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
      strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
      side = as.integer(signed_distance_m > 0)
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

  for (construction_sample in c("all", "multifamily")) {
    model_sample <- if (construction_sample == "all") {
      parcels %>% filter(unitscount > 0)
    } else {
      parcels %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      outcome_sample <- model_sample %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      for (treatment in c("continuous", "binary")) {
        treatment_variable <- if (treatment == "continuous") "strictness_own" else "side"

        model <- feols(
          as.formula(paste0(
            "outcome_value ~ ", treatment_variable,
            " + lenient_dist + strict_dist + ",
            paste(demographic_controls, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = outcome_sample,
          cluster = ~ward_pair
        )

        coefficient_table <- coeftable(model)
        used_rows <- obs(model)
        recovered_used_rows <- used_rows[
          outcome_sample$sample_source[used_rows] !=
            "production_current_2025_coordinate"
        ]

        model_rows[[length(model_rows) + 1L]] <- tibble(
          analysis_sample,
          construction_sample,
          outcome,
          treatment,
          estimate = unname(coefficient_table[treatment_variable, "Estimate"]),
          se = unname(coefficient_table[treatment_variable, "Std. Error"]),
          p_value = unname(coefficient_table[treatment_variable, "Pr(>|t|)"]),
          n = nobs(model),
          recovered_pin_rows = length(recovered_used_rows),
          recovered_projects = n_distinct(
            outcome_sample$project_key[recovered_used_rows],
            na.rm = TRUE
          ),
          ward_pairs = n_distinct(outcome_sample$ward_pair[used_rows])
        )
      }
    }
  }
}

model_results <- bind_rows(model_rows)
production_results <- model_results %>%
  filter(analysis_sample == "production") %>%
  select(
    construction_sample,
    outcome,
    treatment,
    production_estimate = estimate,
    production_se = se,
    production_n = n
  )

model_results <- model_results %>%
  left_join(
    production_results,
    by = c("construction_sample", "outcome", "treatment"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    estimate_change_from_production = estimate - production_estimate,
    n_change_from_production = n - production_n
  ) %>%
  arrange(construction_sample, outcome, treatment, analysis_sample)

expected <- expand_grid(
  analysis_sample = names(analysis_samples),
  construction_sample = c("all", "multifamily"),
  outcome = c("density_far", "density_dupac"),
  treatment = c("continuous", "binary")
)

if (nrow(anti_join(
  expected,
  model_results,
  by = c("analysis_sample", "construction_sample", "outcome", "treatment")
)) > 0 || nrow(model_results) != nrow(expected)) {
  stop("Density lineage audit did not produce all expected models.", call. = FALSE)
}

write_csv(model_results, "../output/density_project_lineage_models.csv")
