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
) %>%
  left_join(
    read_csv(
      "../output/density_parcel_address_lineage_evidence.csv",
      show_col_types = FALSE,
      col_types = cols(member_pins = col_character(), .default = col_guess())
    ) %>%
      select(project_key, address_audit_recommendation),
    by = "project_key",
    relationship = "one-to-one"
  ) %>%
  mutate(
    final_recovery =
      recommended_action == "candidate_for_recovery" &
      address_audit_recommendation != "exclude_address_confirmed_duplicate"
  )

lineage_pins <- lineage %>%
  select(project_key, member_pins, final_recovery) %>%
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
  left_join(lineage_pins, by = "pin", relationship = "one-to-one") %>%
  filter(final_recovery)

if (any(is.na(recovered$project_key))) {
  stop("A recovered exact-year row is missing from final project lineage.", call. = FALSE)
}

address_recovered <- read_csv(
  "../output/density_parcel_address_recovered_model_rows.csv",
  show_col_types = FALSE,
  col_types = cols(
    pin = col_character(), segment_id = col_character(),
    source_class = col_character(), .default = col_guess()
  )
)

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

parcels <- bind_rows(production, recovered, address_recovered) %>%
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
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != ""
  )

bandwidths <- tribble(
  ~bandwidth, ~bandwidth_m,
  "250ft", 76.2,
  "100m", 100,
  "500ft", 152.4,
  "1000ft", 304.8
)

fixed_effects <- tribble(
  ~fixed_effect_spec, ~fixed_effect_formula,
  "main", "zone_group + segment_id + construction_year",
  "segment_by_year", "zone_group + segment_id^construction_year",
  "ward_pair_by_year", "zone_group + ward_pair^construction_year",
  "ward_by_year_with_segment", "zone_group + segment_id + ward^construction_year",
  "no_zoning_same_sample", "segment_id + construction_year"
)

model_rows <- list()
for (bandwidth_i in seq_len(nrow(bandwidths))) {
  bandwidth_sample <- parcels %>%
    filter(dist_to_boundary_m <= bandwidths$bandwidth_m[bandwidth_i])

  for (construction_sample in c("all", "multifamily")) {
    construction_rows <- if (construction_sample == "all") {
      bandwidth_sample %>% filter(unitscount > 0)
    } else {
      bandwidth_sample %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      outcome_rows <- construction_rows %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      for (treatment in c("continuous", "binary")) {
        treatment_variable <- if (treatment == "continuous") "strictness_own" else "side"

        for (fixed_effect_i in seq_len(nrow(fixed_effects))) {
          if (
            treatment == "continuous" &&
            fixed_effects$fixed_effect_spec[fixed_effect_i] ==
              "ward_by_year_with_segment"
          ) {
            model_rows[[length(model_rows) + 1L]] <- tibble(
              bandwidth = bandwidths$bandwidth[bandwidth_i],
              bandwidth_m = bandwidths$bandwidth_m[bandwidth_i],
              construction_sample,
              outcome,
              treatment,
              fixed_effect_spec = fixed_effects$fixed_effect_spec[fixed_effect_i],
              estimate = NA_real_,
              se = NA_real_,
              p_value = NA_real_,
              n = NA_integer_,
              dep_var_mean = NA_real_,
              recovered_pin_rows = NA_integer_,
              ward_pairs = NA_integer_,
              model_status = "treatment_defined_at_ward_year_level"
            )
            next
          }

          model <- tryCatch(
            feols(
              as.formula(paste0(
                "outcome_value ~ ", treatment_variable,
                " + lenient_dist + strict_dist + ",
                paste(demographic_controls, collapse = " + "),
                " | ", fixed_effects$fixed_effect_formula[fixed_effect_i]
              )),
              data = outcome_rows,
              cluster = ~ward_pair,
              notes = FALSE,
              warn = FALSE
            ),
            error = function(error) error
          )

          if (inherits(model, "error")) {
            model_rows[[length(model_rows) + 1L]] <- tibble(
              bandwidth = bandwidths$bandwidth[bandwidth_i],
              bandwidth_m = bandwidths$bandwidth_m[bandwidth_i],
              construction_sample,
              outcome,
              treatment,
              fixed_effect_spec = fixed_effects$fixed_effect_spec[fixed_effect_i],
              estimate = NA_real_,
              se = NA_real_,
              p_value = NA_real_,
              n = NA_integer_,
              dep_var_mean = NA_real_,
              recovered_pin_rows = NA_integer_,
              ward_pairs = NA_integer_,
              model_status = paste0("error: ", conditionMessage(model))
            )
            next
          }

          coefficient_table <- coeftable(model)
          treatment_present <- treatment_variable %in% rownames(coefficient_table)
          used_rows <- obs(model)

          model_rows[[length(model_rows) + 1L]] <- tibble(
            bandwidth = bandwidths$bandwidth[bandwidth_i],
            bandwidth_m = bandwidths$bandwidth_m[bandwidth_i],
            construction_sample,
            outcome,
            treatment,
            fixed_effect_spec = fixed_effects$fixed_effect_spec[fixed_effect_i],
            estimate = if (treatment_present) {
              unname(coefficient_table[treatment_variable, "Estimate"])
            } else {
              NA_real_
            },
            se = if (treatment_present) {
              unname(coefficient_table[treatment_variable, "Std. Error"])
            } else {
              NA_real_
            },
            p_value = if (treatment_present) {
              unname(coefficient_table[treatment_variable, "Pr(>|t|)"])
            } else {
              NA_real_
            },
            n = nobs(model),
            dep_var_mean = mean(outcome_rows[[outcome]][used_rows]),
            recovered_pin_rows = sum(
              outcome_rows$sample_source[used_rows] !=
                "production_current_2025_coordinate"
            ),
            ward_pairs = n_distinct(outcome_rows$ward_pair[used_rows]),
            model_status = if_else(
              treatment_present,
              "estimated",
              "treatment_absorbed_by_fixed_effects"
            )
          )
        }
      }
    }
  }
}

bind_rows(model_rows) %>%
  arrange(
    construction_sample,
    outcome,
    treatment,
    fixed_effect_spec,
    bandwidth_m
  ) %>%
  write_csv("../output/density_bandwidth_fe_robustness.csv")
