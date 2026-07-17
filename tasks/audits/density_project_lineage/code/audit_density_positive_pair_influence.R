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
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    unitscount > 0,
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

fit_continuous_model <- function(data) {
  feols(
    as.formula(paste0(
      "log_outcome ~ strictness_own + lenient_dist + strict_dist + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    data = data,
    cluster = ~ward_pair,
    notes = FALSE,
    warn = FALSE
  )
}

influence_rows <- list()
for (bandwidth in c(152.4, 304.8)) {
  for (outcome in c("density_far", "density_dupac")) {
    model_rows <- parcels %>%
      filter(
        dist_to_boundary_m <= bandwidth,
        is.finite(.data[[outcome]]),
        .data[[outcome]] > 0
      ) %>%
      mutate(log_outcome = log(.data[[outcome]]))

    full_model <- fit_continuous_model(model_rows)
    full_table <- coeftable(full_model)
    full_estimate <- unname(full_table["strictness_own", "Estimate"])
    full_se <- unname(full_table["strictness_own", "Std. Error"])

    for (ward_pair_i in sort(unique(model_rows$ward_pair))) {
      pair_rows <- model_rows %>% filter(ward_pair == ward_pair_i)
      leave_out_model <- fit_continuous_model(
        model_rows %>% filter(ward_pair != ward_pair_i)
      )
      leave_out_table <- coeftable(leave_out_model)
      leave_out_estimate <- unname(
        leave_out_table["strictness_own", "Estimate"]
      )

      recovered_pair_rows <- pair_rows %>%
        filter(sample_source != "production_current_2025_coordinate")
      if (nrow(recovered_pair_rows) > 0) {
        without_recovered_model <- fit_continuous_model(
          model_rows %>%
            filter(!(
              ward_pair == ward_pair_i &
              sample_source != "production_current_2025_coordinate"
            ))
        )
        without_recovered_table <- coeftable(without_recovered_model)
        estimate_without_recovered_rows <- unname(
          without_recovered_table["strictness_own", "Estimate"]
        )
      } else {
        estimate_without_recovered_rows <- full_estimate
      }

      influence_rows[[length(influence_rows) + 1L]] <- tibble(
        bandwidth = if_else(bandwidth == 152.4, "500ft", "1000ft"),
        bandwidth_m = bandwidth,
        outcome,
        ward_pair = ward_pair_i,
        full_estimate,
        full_se,
        leave_out_estimate,
        positive_contribution = full_estimate - leave_out_estimate,
        estimate_without_recovered_rows,
        recovered_positive_contribution =
          full_estimate - estimate_without_recovered_rows,
        rows = nrow(pair_rows),
        recovered_rows = nrow(recovered_pair_rows),
        less_stringent_rows = sum(pair_rows$side == 0),
        more_stringent_rows = sum(pair_rows$side == 1),
        mean_log_outcome_less = mean(
          pair_rows$log_outcome[pair_rows$side == 0],
          na.rm = TRUE
        ),
        mean_log_outcome_more = mean(
          pair_rows$log_outcome[pair_rows$side == 1],
          na.rm = TRUE
        )
      )
    }
  }
}

bind_rows(influence_rows) %>%
  group_by(bandwidth, outcome) %>%
  arrange(desc(positive_contribution), .by_group = TRUE) %>%
  mutate(positive_influence_rank = row_number()) %>%
  ungroup() %>%
  write_csv("../output/density_all_continuous_ward_pair_influence.csv")
