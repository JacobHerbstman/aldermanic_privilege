# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_m <- 152.4
demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

residential <- read_csv(
  "../../../residential_improvements_data_cleaning/output/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  mutate(
    residential_single_family =
      (!is.na(single_v_multi_family) &
        str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    residential_units = if_else(
      residential_single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  ) %>%
  select(pin, residential_units)

commercial <- read_csv(
  "../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    commercial_units = as.numeric(tot_units),
    source_tot_units = as.numeric(source_tot_units),
    apartment_unit_sum = as.numeric(apartment_unit_sum),
    source_landsf = as.numeric(source_landsf),
    cleaned_landsf = as.numeric(landsf),
    pin_group_count = as.integer(pin_group_count),
    modelgroup,
    unit_source,
    unit_override_applied = as.logical(unit_override_applied),
    apply_land_correction = as.logical(apply_land_correction),
    tot_units_apartment_sum_gap = as.logical(tot_units_apartment_sum_gap),
    tot_units_apartment_sum_large_gap = as.logical(tot_units_apartment_sum_large_gap)
  ) %>%
  mutate(
    units_without_manual_override = case_when(
      apartment_unit_sum > 0 ~ apartment_unit_sum,
      source_tot_units > 0 ~ source_tot_units,
      TRUE ~ NA_real_
    )
  )

parcels <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  left_join(residential, by = "pin", relationship = "many-to-one") %>%
  left_join(commercial, by = "pin", relationship = "many-to-one") %>%
  mutate(
    selected_source = case_when(
      !is.na(residential_units) & is.na(commercial_units) ~ "residential",
      is.na(residential_units) & !is.na(commercial_units) ~ "commercial",
      !is.na(residential_units) & !is.na(commercial_units) &
        commercial_units > residential_units ~ "commercial",
      !is.na(residential_units) ~ "residential",
      TRUE ~ "unrecovered"
    ),
    zone_group = zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  ) %>%
  filter(
    construction_year >= 2006,
    construction_year <= 2022,
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(zone_code),
    !is.na(segment_id),
    segment_id != ""
  )

commercial_summary <- parcels %>%
  filter(selected_source == "commercial") %>%
  summarise(
    commercial_rows = n(),
    manual_unit_overrides = sum(unit_override_applied %in% TRUE),
    land_area_corrections = sum(apply_land_correction %in% TRUE),
    multi_pin_buildings = sum(pin_group_count > 1, na.rm = TRUE),
    unit_count_disagreements = sum(tot_units_apartment_sum_gap %in% TRUE),
    large_unit_count_disagreements = sum(tot_units_apartment_sum_large_gap %in% TRUE),
    commercial_rows_six_units_or_fewer = sum(commercial_units <= 6, na.rm = TRUE)
  )
write_csv(commercial_summary, "../output/density_commercial_source_summary.csv")

commercial_interventions <- parcels %>%
  filter(
    selected_source == "commercial",
    unit_override_applied %in% TRUE | apply_land_correction %in% TRUE
  ) %>%
  select(
    pin, construction_year, ward_pair, segment_id, dist_to_boundary_m,
    commercial_units, units_without_manual_override,
    cleaned_landsf, source_landsf, areabuilding,
    density_far, density_dupac,
    unit_override_applied, apply_land_correction
  )
write_csv(
  commercial_interventions,
  "../output/density_commercial_intervention_rows.csv"
)

model_specs <- list(
  production = parcels,
  interventions_reverted = parcels %>%
    mutate(
      model_units = if_else(
        selected_source == "commercial" & unit_override_applied %in% TRUE,
        units_without_manual_override,
        unitscount
      ),
      model_land = if_else(
        selected_source == "commercial" & apply_land_correction %in% TRUE,
        source_landsf,
        arealotsf
      ),
      density_far = areabuilding / model_land,
      density_dupac = 43560 * model_units / model_land,
      unitscount = model_units
    )
)

model_rows <- list()
for (model_spec_name in names(model_specs)) {
  model_spec <- model_specs[[model_spec_name]]
  for (construction_sample in c("all", "multifamily")) {
    model_data <- if (construction_sample == "all") {
      model_spec %>% filter(unitscount > 0)
    } else {
      model_spec %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      model_data_outcome <- model_data %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      for (treatment in c("continuous", "binary")) {
        treatment_var <- if (treatment == "continuous") "strictness_own" else "side"
        model <- feols(
          as.formula(paste0(
            "outcome_value ~ ", treatment_var, " + lenient_dist + strict_dist + ",
            paste(demographic_controls, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = model_data_outcome,
          cluster = ~ward_pair
        )
        coefficient_table <- coeftable(model)
        model_rows[[length(model_rows) + 1L]] <- tibble(
          model_spec = model_spec_name,
          construction_sample,
          outcome,
          treatment,
          estimate = unname(coefficient_table[treatment_var, "Estimate"]),
          se = unname(coefficient_table[treatment_var, "Std. Error"]),
          p_value = unname(coefficient_table[treatment_var, "Pr(>|t|)"]),
          n = nobs(model)
        )
      }
    }
  }
}
write_csv(bind_rows(model_rows), "../output/density_commercial_source_models.csv")
