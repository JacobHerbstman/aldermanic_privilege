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
  col_types = cols(
    pin = col_character(), proration_key_pin = col_character(), .default = col_guess()
  )
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
  transmute(
    pin,
    residential_year = as.integer(year_built),
    residential_units,
    residential_building_sqft = as.numeric(building_sqft),
    residential_land_sqft = as.numeric(land_sqft),
    source_class = as.character(class),
    source_card = as.integer(card_num),
    proration_key_pin = na_if(str_trim(proration_key_pin), ""),
    pin_proration_rate = as.numeric(pin_proration_rate),
    card_proration_rate = as.numeric(card_proration_rate),
    pin_is_multicard = as.logical(pin_is_multicard),
    pin_num_cards = as.integer(pin_num_cards),
    pin_is_multiland = as.logical(pin_is_multiland),
    pin_num_landlines = as.integer(pin_num_landlines)
  )

commercial <- read_csv(
  "../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    commercial_year = as.integer(yearbuilt),
    commercial_units = as.numeric(tot_units),
    commercial_building_sqft = as.numeric(bldgsf),
    commercial_land_sqft = as.numeric(landsf),
    commercial_pin_group_count = as.integer(pin_group_count),
    commercial_pins = as.character(pins)
  )

if (anyDuplicated(residential$pin) > 0 || anyDuplicated(commercial$pin) > 0) {
  stop("Density source cross-sections are not unique by PIN.", call. = FALSE)
}

parcels <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  left_join(residential, by = "pin", relationship = "many-to-one") %>%
  left_join(commercial, by = "pin", relationship = "many-to-one") %>%
  mutate(
    in_residential_source = !is.na(residential_year),
    in_commercial_source = !is.na(commercial_year),
    selected_source = case_when(
      in_residential_source & !in_commercial_source ~ "residential",
      !in_residential_source & in_commercial_source ~ "commercial",
      in_residential_source & in_commercial_source &
        (is.na(residential_units) | commercial_units > residential_units) ~ "commercial",
      in_residential_source & in_commercial_source ~ "residential",
      TRUE ~ "unrecovered"
    ),
    expected_units = case_when(
      selected_source == "residential" ~ residential_units,
      selected_source == "commercial" ~ commercial_units,
      TRUE ~ NA_real_
    ),
    selected_values_match = is.na(expected_units) | abs(unitscount - expected_units) < 1e-8,
    residential_building_key = if_else(
      selected_source == "residential",
      paste0(coalesce(proration_key_pin, pin), "_card", source_card),
      NA_character_
    ),
    building_key = if_else(
      selected_source == "residential",
      residential_building_key,
      paste0("commercial_", pin)
    ),
    is_prorated_residential = selected_source == "residential" &
      !is.na(proration_key_pin),
    is_key_pin = selected_source == "residential" &
      !is.na(proration_key_pin) & pin == proration_key_pin,
    zone_group = zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

if (any(!parcels$selected_values_match, na.rm = TRUE)) {
  stop("Density source reconstruction does not match production units.", call. = FALSE)
}

production_window <- parcels %>%
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

duplicate_groups <- production_window %>%
  filter(selected_source == "residential") %>%
  group_by(building_key) %>%
  mutate(rows_in_building_key = n()) %>%
  ungroup() %>%
  filter(rows_in_building_key > 1) %>%
  arrange(building_key, desc(is_key_pin), desc(pin_proration_rate), pin) %>%
  select(
    building_key, rows_in_building_key, pin, construction_year,
    source_class, source_card, proration_key_pin, pin_proration_rate,
    is_key_pin, unitscount, arealotsf, areabuilding,
    density_far, density_dupac, ward_pair, segment_id, dist_to_boundary_m
  )
write_csv(duplicate_groups, "../output/density_tieback_duplicate_rows.csv")

identity_summary <- bind_rows(
  production_window %>%
    summarise(
      universe = "production_500ft_input",
      rows = n(),
      distinct_building_keys = n_distinct(building_key),
      residential_rows = sum(selected_source == "residential"),
      prorated_residential_rows = sum(is_prorated_residential),
      duplicate_building_key_rows = sum(building_key %in% duplicate_groups$building_key),
      duplicate_building_key_groups = n_distinct(duplicate_groups$building_key),
      multicard_residential_rows = sum(
        selected_source == "residential" & pin_is_multicard %in% TRUE,
        na.rm = TRUE
      )
    ),
  parcels %>%
    filter(
      construction_year >= 2006,
      construction_year <= 2022,
      arealotsf > 1,
      areabuilding > 1,
      unitscount > 0
    ) %>%
    summarise(
      universe = "all_current_geocoded_eligible",
      rows = n(),
      distinct_building_keys = n_distinct(building_key),
      residential_rows = sum(selected_source == "residential"),
      prorated_residential_rows = sum(is_prorated_residential),
      duplicate_building_key_rows = sum(duplicated(building_key) | duplicated(building_key, fromLast = TRUE)),
      duplicate_building_key_groups = n_distinct(
        building_key[duplicated(building_key) | duplicated(building_key, fromLast = TRUE)]
      ),
      multicard_residential_rows = sum(
        selected_source == "residential" & pin_is_multicard %in% TRUE,
        na.rm = TRUE
      )
    )
)
write_csv(identity_summary, "../output/density_building_identity_summary.csv")

deduplicated_window <- production_window %>%
  arrange(
    building_key,
    desc(is_key_pin),
    desc(coalesce(pin_proration_rate, -Inf)),
    desc(areabuilding),
    pin
  ) %>%
  distinct(building_key, .keep_all = TRUE)

model_samples <- list(
  production_pin_rows = production_window,
  tieback_building_deduplicated = deduplicated_window
)
model_rows <- list()
for (model_sample_name in names(model_samples)) {
  model_sample <- model_samples[[model_sample_name]]
  for (construction_sample in c("all", "multifamily")) {
    model_data <- if (construction_sample == "all") {
      model_sample
    } else {
      model_sample %>% filter(unitscount > 1)
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
          model_sample = model_sample_name,
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
write_csv(bind_rows(model_rows), "../output/density_tieback_deduplication_models.csv")
