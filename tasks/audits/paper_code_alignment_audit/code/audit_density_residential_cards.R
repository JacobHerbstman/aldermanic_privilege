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

confirmed_sum_pins <- c(
  "11303200330000",
  "13141270250000",
  "19083090620000",
  "20211200290000",
  "20231160220000",
  "25194060030000"
)

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

card_cross_section <- dbGetQuery(con, "
WITH parsed AS (
  SELECT
    trim(pin) AS pin,
    try_cast(numeric_text(year) AS INTEGER) AS tax_year,
    try_cast(numeric_text(card) AS INTEGER) AS card_num,
    trim(class) AS class,
    trim(tieback_key_pin) AS proration_key_pin,
    try_cast(numeric_text(tieback_proration_rate) AS DOUBLE) AS pin_proration_rate,
    try_cast(numeric_text(card_proration_rate) AS DOUBLE) AS card_proration_rate,
    lower(trim(pin_is_multicard)) = 'true' AS pin_is_multicard,
    try_cast(numeric_text(pin_num_cards) AS INTEGER) AS pin_num_cards,
    try_cast(numeric_text(char_yrblt) AS INTEGER) AS year_built,
    try_cast(numeric_text(char_bldg_sf) AS DOUBLE) AS building_sqft,
    try_cast(numeric_text(char_land_sf) AS DOUBLE) AS land_sqft,
    trim(char_use) AS single_v_multi_family,
    trim(char_type_resd) AS type_of_residence,
    CASE
      WHEN lower(trim(char_apts)) IN ('none', 'zero') THEN 0
      WHEN lower(trim(char_apts)) = 'one' THEN 1
      WHEN lower(trim(char_apts)) = 'two' THEN 2
      WHEN lower(trim(char_apts)) = 'three' THEN 3
      WHEN lower(trim(char_apts)) = 'four' THEN 4
      WHEN lower(trim(char_apts)) = 'five' THEN 5
      WHEN lower(trim(char_apts)) = 'six' THEN 6
      ELSE try_cast(numeric_text(char_apts) AS INTEGER)
    END AS num_apartments
  FROM read_csv(
    '../../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv',
    all_varchar = true,
    header = true,
    ignore_errors = true,
    max_line_size = 10000000
  )
  WHERE try_cast(numeric_text(township_code) AS INTEGER) IN (70, 71, 72, 73, 74, 75, 76, 77)
    AND try_cast(numeric_text(char_yrblt) AS INTEGER) >= 1999
),
ranked AS (
  SELECT
    *,
    row_number() OVER (
      PARTITION BY pin, card_num
      ORDER BY year_built, tax_year, building_sqft DESC NULLS LAST
    ) AS selected_row
  FROM parsed
  WHERE pin IS NOT NULL
    AND pin != ''
    AND card_num IS NOT NULL
    AND year_built IS NOT NULL
)
SELECT * EXCLUDE (selected_row)
FROM ranked
WHERE selected_row = 1
ORDER BY pin, card_num
") %>%
  as_tibble() %>%
  mutate(
    pin = as.character(pin),
    proration_key_pin = na_if(as.character(proration_key_pin), ""),
    single_family =
      (!is.na(single_v_multi_family) &
        str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    unitscount = if_else(
      single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  )

write_csv(card_cross_section, "../output/density_residential_card_cross_section.csv")

current_pins <- read_csv(
  "../../../download_parcel_universe_data/output/parcel_universe_2025_city.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  select(pin)

production_residential <- read_csv(
  "../../../residential_improvements_data_cleaning/output/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), proration_key_pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    selected_card = as.integer(card_num),
    selected_year = as.integer(year_built),
    selected_proration_key_pin = na_if(str_trim(as.character(proration_key_pin)), ""),
    selected_pin_proration_rate = as.numeric(pin_proration_rate)
  )

eligible_cards <- card_cross_section %>%
  filter(
    year_built >= 2006,
    year_built <= 2022,
    building_sqft > 1,
    land_sqft > 1,
    unitscount > 0
  )
current_eligible_cards <- eligible_cards %>%
  semi_join(current_pins, by = "pin")

card_counts <- current_eligible_cards %>%
  group_by(pin) %>%
  summarise(
    eligible_cards = n(),
    eligible_construction_years = n_distinct(year_built),
    .groups = "drop"
  )

parcels <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  left_join(production_residential, by = "pin", relationship = "many-to-one") %>%
  mutate(
    zone_group = zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

commercial_pins <- read_csv(
  "../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(pin, commercial_units = as.numeric(tot_units))

production_units <- read_csv(
  "../../../residential_improvements_data_cleaning/output/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  mutate(
    single_family =
      (!is.na(single_v_multi_family) &
        str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    residential_units = if_else(
      single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    )
  ) %>%
  select(pin, residential_units)

parcels <- parcels %>%
  left_join(production_units, by = "pin", relationship = "many-to-one") %>%
  left_join(commercial_pins, by = "pin", relationship = "many-to-one") %>%
  mutate(
    selected_source = case_when(
      !is.na(residential_units) & is.na(commercial_units) ~ "residential",
      is.na(residential_units) & !is.na(commercial_units) ~ "commercial",
      !is.na(residential_units) & !is.na(commercial_units) &
        commercial_units > residential_units ~ "commercial",
      !is.na(residential_units) ~ "residential",
      TRUE ~ "unrecovered"
    )
  )

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

model_residential_pins <- production_window %>%
  filter(selected_source == "residential") %>%
  distinct(pin)

card_summary <- bind_rows(
  eligible_cards %>%
    summarise(
      universe = "all_eligible_historical_pin_cards",
      pins = n_distinct(pin),
      cards = n(),
      pins_with_multiple_eligible_cards = n_distinct(pin[pin %in% card_counts$pin[card_counts$eligible_cards > 1]]),
      cards_on_multiple_card_pins = sum(pin %in% card_counts$pin[card_counts$eligible_cards > 1])
    ),
  current_eligible_cards %>%
    summarise(
      universe = "current_2025_geocoded_eligible_pin_cards",
      pins = n_distinct(pin),
      cards = n(),
      pins_with_multiple_eligible_cards = n_distinct(pin[pin %in% card_counts$pin[card_counts$eligible_cards > 1]]),
      cards_on_multiple_card_pins = sum(pin %in% card_counts$pin[card_counts$eligible_cards > 1])
    ),
  current_eligible_cards %>%
    semi_join(model_residential_pins, by = "pin") %>%
    summarise(
      universe = "production_500ft_residential_pins_all_eligible_cards",
      pins = n_distinct(pin),
      cards = n(),
      pins_with_multiple_eligible_cards = n_distinct(pin[pin %in% card_counts$pin[card_counts$eligible_cards > 1]]),
      cards_on_multiple_card_pins = sum(pin %in% card_counts$pin[card_counts$eligible_cards > 1])
    )
)
write_csv(card_summary, "../output/density_residential_card_summary.csv")

selected_year_aggregates <- current_eligible_cards %>%
  group_by(pin, year_built) %>%
  summarise(
    cards_in_selected_year = n(),
    total_building_sqft_cards = sum(building_sqft),
    total_units_cards = sum(unitscount),
    .groups = "drop"
  )

card_sensitivity_data <- production_window %>%
  left_join(
    selected_year_aggregates,
    by = c("pin", "construction_year" = "year_built"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    card_aggregate_applied = selected_source == "residential" & cards_in_selected_year > 1,
    confirmed_sum_applied = card_aggregate_applied & pin %in% confirmed_sum_pins,
    density_far_card_aggregated = if_else(
      card_aggregate_applied,
      total_building_sqft_cards / arealotsf,
      density_far
    ),
    density_dupac_card_aggregated = if_else(
      card_aggregate_applied,
      43560 * total_units_cards / arealotsf,
      density_dupac
    ),
    unitscount_card_aggregated = if_else(
      card_aggregate_applied,
      total_units_cards,
      unitscount
    ),
    density_far_confirmed_sum = if_else(
      confirmed_sum_applied,
      total_building_sqft_cards / arealotsf,
      density_far
    ),
    density_dupac_confirmed_sum = if_else(
      confirmed_sum_applied,
      43560 * total_units_cards / arealotsf,
      density_dupac
    ),
    unitscount_confirmed_sum = if_else(
      confirmed_sum_applied,
      total_units_cards,
      unitscount
    )
  )

card_affected_rows <- card_sensitivity_data %>%
  filter(card_aggregate_applied) %>%
  select(
    pin, construction_year, selected_card, cards_in_selected_year,
    arealotsf, areabuilding, total_building_sqft_cards,
    unitscount, total_units_cards,
    density_far, density_far_card_aggregated,
    density_dupac, density_dupac_card_aggregated,
    ward_pair, segment_id, dist_to_boundary_m
  )
if (nrow(card_affected_rows) == 0) {
  card_affected_rows <- tibble(
    pin = character(), construction_year = integer(), selected_card = integer(),
    cards_in_selected_year = integer(), arealotsf = double(), areabuilding = double(),
    total_building_sqft_cards = double(), unitscount = double(), total_units_cards = double(),
    density_far = double(), density_far_card_aggregated = double(),
    density_dupac = double(), density_dupac_card_aggregated = double(),
    ward_pair = character(), segment_id = character(), dist_to_boundary_m = double()
  )
}
write_csv(card_affected_rows, "../output/density_residential_card_affected_rows.csv")

write_csv(
  card_sensitivity_data %>%
    filter(confirmed_sum_applied) %>%
    select(
      pin, construction_year, cards_in_selected_year,
      areabuilding, total_building_sqft_cards,
      unitscount, total_units_cards,
      density_far, density_far_confirmed_sum,
      density_dupac, density_dupac_confirmed_sum,
      ward_pair, segment_id, dist_to_boundary_m
    ),
  "../output/density_residential_confirmed_sum_rows.csv"
)

model_rows <- list()
for (outcome_spec in c(
  "production",
  "exclude_multicard_pin_years",
  "confirmed_sums_only",
  "same_year_cards_aggregated"
)) {
  model_data_base <- card_sensitivity_data %>%
    filter(
      outcome_spec != "exclude_multicard_pin_years" | !card_aggregate_applied
    ) %>%
    mutate(
      model_far = case_when(
        outcome_spec %in% c("production", "exclude_multicard_pin_years") ~ density_far,
        outcome_spec == "confirmed_sums_only" ~ density_far_confirmed_sum,
        TRUE ~ density_far_card_aggregated
      ),
      model_dupac = case_when(
        outcome_spec %in% c("production", "exclude_multicard_pin_years") ~ density_dupac,
        outcome_spec == "confirmed_sums_only" ~ density_dupac_confirmed_sum,
        TRUE ~ density_dupac_card_aggregated
      ),
      model_units = case_when(
        outcome_spec %in% c("production", "exclude_multicard_pin_years") ~ unitscount,
        outcome_spec == "confirmed_sums_only" ~ unitscount_confirmed_sum,
        TRUE ~ unitscount_card_aggregated
      )
    )

  for (construction_sample in c(
    "all",
    "multifamily",
    "multifamily_fixed_production_sample"
  )) {
    if (construction_sample == "all") {
      model_data <- model_data_base
    } else if (construction_sample == "multifamily") {
      model_data <- model_data_base %>% filter(model_units > 1)
    } else {
      model_data <- model_data_base %>% filter(unitscount > 1)
    }

    for (outcome in c("model_far", "model_dupac")) {
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
          outcome_spec,
          construction_sample,
          outcome = if_else(outcome == "model_far", "density_far", "density_dupac"),
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
write_csv(bind_rows(model_rows), "../output/density_residential_card_models.csv")

residential_card_components <- current_eligible_cards %>%
  mutate(property_key = coalesce(proration_key_pin, pin)) %>%
  arrange(
    property_key, year_built, card_num,
    desc(pin == property_key), desc(coalesce(pin_proration_rate, -Inf)), pin
  ) %>%
  distinct(property_key, year_built, card_num, .keep_all = TRUE) %>%
  group_by(property_key, year_built) %>%
  summarise(
    property_card_count = n(),
    property_building_sqft = sum(building_sqft),
    property_units = sum(unitscount),
    .groups = "drop"
  )

residential_land_components <- current_eligible_cards %>%
  mutate(property_key = coalesce(proration_key_pin, pin)) %>%
  group_by(property_key, year_built, pin) %>%
  summarise(pin_land_sqft = max(land_sqft), .groups = "drop") %>%
  group_by(property_key, year_built) %>%
  summarise(
    property_pin_count = n(),
    property_land_sqft = sum(pin_land_sqft),
    .groups = "drop"
  )

residential_property_aggregates <- residential_card_components %>%
  left_join(
    residential_land_components,
    by = c("property_key", "year_built"),
    relationship = "one-to-one"
  )

joint_correction_data <- production_window %>%
  mutate(
    property_key = if_else(
      selected_source == "residential",
      coalesce(selected_proration_key_pin, pin),
      paste0("commercial_", pin)
    )
  ) %>%
  left_join(
    residential_property_aggregates,
    by = c("property_key", "construction_year" = "year_built"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    residential_property_aggregate_available =
      selected_source == "residential" &
      is.finite(property_building_sqft) & property_building_sqft > 0 &
      is.finite(property_land_sqft) & property_land_sqft > 0 &
      is.finite(property_units) & property_units > 0,
    density_far_joint = if_else(
      residential_property_aggregate_available,
      property_building_sqft / property_land_sqft,
      density_far
    ),
    density_dupac_joint = if_else(
      residential_property_aggregate_available,
      43560 * property_units / property_land_sqft,
      density_dupac
    ),
    unitscount_joint = if_else(
      residential_property_aggregate_available,
      property_units,
      unitscount
    )
  )

property_assignment_consistency <- joint_correction_data %>%
  group_by(property_key) %>%
  summarise(
    rows = n(),
    ward_pairs = n_distinct(ward_pair),
    segments = n_distinct(segment_id),
    treatment_sides = n_distinct(side),
    maximum_pairwise_distance_m = max(dist_to_boundary_m) - min(dist_to_boundary_m),
    .groups = "drop"
  ) %>%
  filter(rows > 1)
write_csv(
  property_assignment_consistency,
  "../output/density_residential_property_assignment_consistency.csv"
)

joint_correction_data <- joint_correction_data %>%
  arrange(
    property_key,
    desc(pin == property_key),
    desc(coalesce(selected_pin_proration_rate, -Inf)),
    pin
  ) %>%
  distinct(property_key, .keep_all = TRUE)

joint_affected_rows <- joint_correction_data %>%
  filter(
    selected_source == "residential",
    property_card_count > 1 | property_pin_count > 1
  ) %>%
  select(
    property_key, pin, construction_year,
    property_pin_count, property_card_count,
    arealotsf, property_land_sqft,
    areabuilding, property_building_sqft,
    unitscount, property_units,
    density_far, density_far_joint,
    density_dupac, density_dupac_joint,
    ward_pair, segment_id, dist_to_boundary_m
  )
write_csv(
  joint_affected_rows,
  "../output/density_residential_joint_correction_rows.csv"
)

joint_model_rows <- list()
for (construction_sample in c("all", "multifamily")) {
  model_data <- if (construction_sample == "all") {
    joint_correction_data
  } else {
    joint_correction_data %>% filter(unitscount_joint > 1)
  }

  for (outcome in c("density_far_joint", "density_dupac_joint")) {
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
      joint_model_rows[[length(joint_model_rows) + 1L]] <- tibble(
        outcome_spec = "cards_and_tieback_jointly_aggregated",
        construction_sample,
        outcome = if_else(
          outcome == "density_far_joint", "density_far", "density_dupac"
        ),
        treatment,
        estimate = unname(coefficient_table[treatment_var, "Estimate"]),
        se = unname(coefficient_table[treatment_var, "Std. Error"]),
        p_value = unname(coefficient_table[treatment_var, "Pr(>|t|)"]),
        n = nobs(model)
      )
    }
  }
}

write_csv(
  bind_rows(read_csv(
    "../output/density_residential_card_models.csv",
    show_col_types = FALSE
  ), bind_rows(joint_model_rows)),
  "../output/density_residential_card_models.csv"
)
