# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/density_construction_year_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_m <- 152.4
paper_years <- 2006:2022
rules <- c(
  "minimum_post_1999",
  "latest_report_at_sample_end",
  "latest_report_through_2022",
  "latest_report_through_2025",
  "latest_report_all",
  "modal_post_1999",
  "latest_repeated_post_1999"
)
demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

selections <- read_csv(
  "../output/construction_year_rule_selections.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(cards_in_history == 1, selection_rule %in% rules) %>%
  mutate(
    single_family =
      (!is.na(building_use) & str_detect(building_use, regex("^single", ignore_case = TRUE))) |
      (!is.na(residence_type) & residence_type %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    alternate_units = if_else(
      single_family & (is.na(apartments) | apartments == 0),
      1,
      as.numeric(apartments)
    )
  ) %>%
  transmute(
    selection_rule,
    pin,
    alternate_year = as.integer(construction_year),
    alternate_tax_year = as.integer(selected_tax_year),
    alternate_lot = as.numeric(land_sqft),
    alternate_building = as.numeric(building_sqft),
    alternate_units
  )

if (anyDuplicated(selections[c("selection_rule", "pin")]) > 0) {
  stop("Single-card construction-year selections are not unique by rule and PIN.", call. = FALSE)
}

minus_one_permit_matches <- read_csv(
  "../output/construction_year_revision_cases.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  filter(
    cards_in_history == 1,
    revised_post_1999,
    unique_pin10_permit_match,
    latest_report_at_sample_end_construction_year == permit_application_year - 1
  ) %>%
  transmute(pin, permit_year = as.integer(permit_application_year))

if (anyDuplicated(minus_one_permit_matches$pin) > 0) {
  stop("Minus-one permit matches are not unique by PIN.", call. = FALSE)
}

production_residential <- read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), class = col_character(), .default = col_guess())
) %>%
  mutate(
    pin = as.character(pin),
    production_year = as.integer(year_built),
    single_family =
      (!is.na(single_v_multi_family) &
        str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    production_units = if_else(
      single_family & (is.na(num_apartments) | num_apartments == 0),
      1,
      as.numeric(num_apartments)
    ),
    production_lot = as.numeric(land_sqft),
    production_building = as.numeric(building_sqft)
  ) %>%
  select(
    pin, production_year, production_lot, production_building, production_units
  )

if (anyDuplicated(production_residential$pin) > 0) {
  stop("Production residential cross section is not unique by PIN.", call. = FALSE)
}

commercial <- read_csv(
  "../input/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(pin = as.character(pin), commercial_units = as.numeric(tot_units))

if (anyDuplicated(commercial$pin) > 0) {
  stop("Commercial multifamily input is not unique by PIN.", call. = FALSE)
}

source_choice <- full_join(
  production_residential %>% select(pin, residential_units = production_units),
  commercial,
  by = "pin",
  relationship = "one-to-one"
) %>%
  mutate(
    selected_source = case_when(
      !is.na(residential_units) & is.na(commercial_units) ~ "residential",
      is.na(residential_units) & !is.na(commercial_units) ~ "commercial",
      !is.na(residential_units) & !is.na(commercial_units) &
        commercial_units > residential_units ~ "commercial",
      !is.na(residential_units) ~ "residential",
      TRUE ~ NA_character_
    )
  )

selected_residential_pins <- source_choice %>%
  filter(selected_source == "residential") %>%
  select(pin)
selected_commercial_pins <- source_choice %>%
  filter(selected_source == "commercial") %>%
  select(pin)

parcel_points <- st_read("../input/parcels_with_geometry.gpkg", quiet = TRUE) %>%
  select(pin, GEOID, zone_code) %>%
  mutate(pin = as.character(pin))

if (anyDuplicated(parcel_points$pin) > 0) {
  stop("Production parcel geometry is not unique by PIN.", call. = FALSE)
}

production_residential_observations <- production_residential %>%
  semi_join(selected_residential_pins, by = "pin") %>%
  semi_join(st_drop_geometry(parcel_points) %>% select(pin), by = "pin")

alternative_rows <- list()
change_rows <- list()
for (rule_i in rules) {
  replacement <- selections %>%
    filter(selection_rule == rule_i) %>%
    select(-selection_rule)

  joined <- production_residential_observations %>%
    left_join(replacement, by = "pin", relationship = "one-to-one")

  alternative_rows[[length(alternative_rows) + 1L]] <- joined %>%
    transmute(
      variant = paste0("year_only_", rule_i),
      pin,
      construction_year = coalesce(alternate_year, production_year),
      arealotsf = production_lot,
      areabuilding = production_building,
      unitscount = production_units,
      source = "residential"
    )

  alternative_rows[[length(alternative_rows) + 1L]] <- joined %>%
    transmute(
      variant = paste0("full_record_", rule_i),
      pin,
      construction_year = coalesce(alternate_year, production_year),
      arealotsf = coalesce(alternate_lot, production_lot),
      areabuilding = coalesce(alternate_building, production_building),
      unitscount = coalesce(alternate_units, production_units),
      source = "residential"
    )

  change_rows[[length(change_rows) + 1L]] <- joined %>%
    filter(
      !is.na(alternate_year),
      production_year != alternate_year |
        production_lot != alternate_lot |
        production_building != alternate_building |
        production_units != alternate_units
    ) %>%
    transmute(
      selection_rule = rule_i,
      pin,
      production_year,
      alternate_year,
      alternate_tax_year,
      year_change = alternate_year - production_year,
      production_lot,
      alternate_lot,
      production_building,
      alternate_building,
      production_units,
      alternate_units,
      production_in_paper_window = production_year %in% paper_years,
      alternate_in_paper_window = alternate_year %in% paper_years
  )
}

endpoint_minus_one <- production_residential_observations %>%
  left_join(
    selections %>%
      filter(selection_rule == "latest_report_at_sample_end") %>%
      select(-selection_rule),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  left_join(minus_one_permit_matches, by = "pin", relationship = "one-to-one")

alternative_rows[[length(alternative_rows) + 1L]] <- endpoint_minus_one %>%
  transmute(
    variant = "year_only_endpoint_minus_one_to_permit_year",
    pin,
    construction_year = coalesce(permit_year, alternate_year, production_year),
    arealotsf = production_lot,
    areabuilding = production_building,
    unitscount = production_units,
    source = "residential"
  )

alternative_rows[[length(alternative_rows) + 1L]] <- endpoint_minus_one %>%
  transmute(
    variant = "full_record_endpoint_minus_one_to_permit_year",
    pin,
    construction_year = coalesce(permit_year, alternate_year, production_year),
    arealotsf = coalesce(alternate_lot, production_lot),
    areabuilding = coalesce(alternate_building, production_building),
    unitscount = coalesce(alternate_units, production_units),
    source = "residential"
  )

latest_2025_joined <- production_residential_observations %>%
  left_join(
    selections %>%
      filter(selection_rule == "latest_report_through_2025") %>%
      select(pin, alternate_year),
    by = "pin",
    relationship = "one-to-one"
  )

alternative_rows[[length(alternative_rows) + 1L]] <- latest_2025_joined %>%
  transmute(
    variant = "year_only_2014_to_2015_2016_corrections",
    pin,
    construction_year = if_else(
      production_year == 2014 & alternate_year %in% c(2015L, 2016L),
      alternate_year,
      production_year
    ),
    arealotsf = production_lot,
    areabuilding = production_building,
    unitscount = production_units,
    source = "residential"
  )

alternative_rows[[length(alternative_rows) + 1L]] <- latest_2025_joined %>%
  filter(is.na(alternate_year) | production_year == alternate_year) %>%
  transmute(
    variant = "production_stable_reported_year_only",
    pin,
    construction_year = production_year,
    arealotsf = production_lot,
    areabuilding = production_building,
    unitscount = production_units,
    source = "residential"
  )

alternative_rows[[length(alternative_rows) + 1L]] <- latest_2025_joined %>%
  transmute(
    variant = "year_only_pre_2006_moved_into_sample",
    pin,
    construction_year = if_else(
      production_year < 2006 & alternate_year %in% paper_years,
      alternate_year,
      production_year
    ),
    arealotsf = production_lot,
    areabuilding = production_building,
    unitscount = production_units,
    source = "residential"
  )

alternative_residential <- bind_rows(alternative_rows) %>%
  filter(construction_year %in% paper_years)

latest_2025_common_pins <- production_residential_observations %>%
  left_join(
    selections %>%
      filter(selection_rule == "latest_report_through_2025") %>%
      select(pin, alternate_year),
    by = "pin",
    relationship = "one-to-one"
  ) %>%
  mutate(alternate_year = coalesce(alternate_year, production_year)) %>%
  filter(production_year %in% paper_years, alternate_year %in% paper_years) %>%
  select(pin)

alternative_residential <- bind_rows(
  alternative_residential,
  alternative_residential %>%
    filter(variant == "year_only_latest_report_through_2025") %>%
    semi_join(latest_2025_common_pins, by = "pin") %>%
    mutate(variant = "year_only_latest_report_through_2025_common_sample")
)

if (anyDuplicated(alternative_residential[c("variant", "pin")]) > 0) {
  stop("Alternative residential model data are not unique by variant and PIN.", call. = FALSE)
}

assignment_keys <- alternative_residential %>%
  distinct(pin, construction_year) %>%
  mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year),
    yearmon_key = as.character(as.yearmon(construction_date))
  )

assignment_sf <- parcel_points %>%
  inner_join(assignment_keys, by = "pin", relationship = "one-to-many")

ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")
segment_layers <- load_segment_line_layers("../input/boundary_segments_1320ft.gpkg")

boundary_assignment <- assign_points_to_boundaries(
  points_sf = assignment_sf,
  era_values = assignment_sf$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

assignment_sf <- bind_cols(assignment_sf, boundary_assignment) %>%
  rename(
    assigned_ward = ward,
    other_ward = neighbor_ward,
    ward_pair = ward_pair_id,
    dist_to_boundary_m = dist_m,
    dist_to_boundary = dist_ft
  )

assignment_sf$segment_id <- assign_points_to_nearest_segments(
  points_sf = assignment_sf,
  era_values = assignment_sf$era,
  pair_values = assignment_sf$ward_pair,
  segment_layers = segment_layers,
  max_distance = units::set_units(250, "m"),
  chunk_n = 50000L
)

alderman_lookup <- read_csv(
  "../input/chicago_alderman_panel.csv",
  show_col_types = FALSE
) %>%
  transmute(
    ward = as.integer(ward),
    yearmon_key = as.character(as.yearmon(month, format = "%b %Y")),
    alderman
  )

if (anyDuplicated(alderman_lookup[c("ward", "yearmon_key")]) > 0) {
  stop("Alderman lookup is not unique by ward-month.", call. = FALSE)
}

ward_controls <- read_csv("../input/ward_controls.csv", show_col_types = FALSE) %>%
  select(
    ward, year, share_white, share_black, median_hh_income,
    share_bach_plus, homeownership_rate
  )

if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Ward controls are not unique by ward-year.", call. = FALSE)
}

scores <- read_csv(
  "../input/alderman_uncertainty_index_through2022.csv",
  show_col_types = FALSE
) %>%
  select(alderman, score = uncertainty_index)

if (anyDuplicated(scores$alderman) > 0) {
  stop("Score lookup is not unique by alderman.", call. = FALSE)
}

assignment_data <- assignment_sf %>%
  st_drop_geometry() %>%
  left_join(
    alderman_lookup,
    by = c("assigned_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_own = alderman) %>%
  left_join(
    alderman_lookup %>% rename(alderman_neighbor = alderman),
    by = c("other_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    ward_controls %>% rename_with(~ paste0(.x, "_own"), -c(ward, year)),
    by = c("assigned_ward" = "ward", "construction_year" = "year"),
    relationship = "many-to-one"
  ) %>%
  left_join(scores, by = c("alderman_own" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_own = score) %>%
  left_join(scores, by = c("alderman_neighbor" = "alderman"), relationship = "many-to-one") %>%
  rename(strictness_neighbor = score) %>%
  mutate(
    sign = case_when(
      strictness_own > strictness_neighbor ~ 1,
      strictness_own < strictness_neighbor ~ -1,
      TRUE ~ NA_real_
    ),
    signed_distance_m = dist_to_boundary_m * sign,
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2,
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

production_panel <- read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns()

production_zoning <- production_panel %>%
  filter(between(construction_year, min(paper_years), max(paper_years))) %>%
  distinct(pin, construction_zone_group)
if (anyDuplicated(production_zoning$pin) > 0) {
  stop("Production construction-year zoning is not unique by PIN.", call. = FALSE)
}

alternative_model_data <- alternative_residential %>%
  inner_join(
    assignment_data,
    by = c("pin", "construction_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(production_zoning, by = "pin", relationship = "many-to-one") %>%
  mutate(
    zone_group = construction_zone_group,
    density_far = areabuilding / arealotsf,
    density_dupac = 43560 * unitscount / arealotsf
  )

production_model_data <- production_panel %>%
  mutate(
    variant = "production",
    source = source_choice$selected_source[match(pin, source_choice$pin)],
    zone_group = construction_zone_group,
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2,
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

fixed_commercial <- production_model_data %>%
  semi_join(selected_commercial_pins, by = "pin")

production_common_sample <- production_model_data %>%
  filter(source == "commercial" | pin %in% latest_2025_common_pins$pin) %>%
  mutate(variant = "production_common_sample_latest_report_through_2025")

commercial_by_variant <- alternative_residential %>%
  distinct(variant) %>%
  tidyr::crossing(fixed_commercial %>% select(-variant))

model_columns <- c(
  "variant", "pin", "construction_year", "arealotsf", "areabuilding",
  "unitscount", "density_far", "density_dupac", "source",
  "dist_to_boundary_m", "ward_pair", "signed_distance_m", "zone_group",
  "segment_id", "strictness_own", "strictness_neighbor", "side",
  "continuous_score_difference", "pair_average_score", "lenient_dist",
  "strict_dist", demographic_controls
)

all_model_data <- bind_rows(
  production_model_data %>% select(all_of(model_columns)),
  production_common_sample %>% select(all_of(model_columns)),
  alternative_model_data %>% select(all_of(model_columns)),
  commercial_by_variant %>% select(all_of(model_columns))
) %>%
  filter(
    construction_year %in% paper_years,
    arealotsf > 1,
    areabuilding > 1,
    unitscount > 0,
    dist_to_boundary_m <= bandwidth_m,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(zone_group),
    !is.na(segment_id),
    segment_id != ""
  )

sample_summary <- all_model_data %>%
  group_by(variant) %>%
  summarise(
    rows_500ft = n(),
    residential_rows_500ft = sum(source == "residential", na.rm = TRUE),
    commercial_rows_500ft = sum(source == "commercial", na.rm = TRUE),
    multifamily_rows_500ft = sum(unitscount > 1, na.rm = TRUE),
    pins_500ft = n_distinct(pin),
    ward_pairs_500ft = n_distinct(ward_pair),
    .groups = "drop"
  )

model_rows <- list()
for (variant_i in sort(unique(all_model_data$variant))) {
  variant_data <- all_model_data %>% filter(variant == variant_i)

  for (construction_sample in c("all", "multifamily")) {
    sample_data <- if (construction_sample == "all") {
      variant_data
    } else {
      variant_data %>% filter(unitscount > 1)
    }

    for (outcome in c("density_far", "density_dupac")) {
      outcome_data <- sample_data %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0) %>%
        mutate(outcome_value = log(.data[[outcome]]))

      for (treatment in c("continuous", "binary")) {
        treatment_var <- if (treatment == "continuous") "continuous_score_difference" else "side"
        model <- feols(
          as.formula(paste0(
            "outcome_value ~ ", treatment_var,
            " + pair_average_score + lenient_dist + strict_dist + ",
            paste(demographic_controls, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = outcome_data,
          cluster = ~ward_pair,
          warn = FALSE
        )
        coefficient_table <- coeftable(model)

        if (!treatment_var %in% rownames(coefficient_table)) {
          stop(
            paste("Model failed to estimate", treatment_var, "for", variant_i, construction_sample, outcome),
            call. = FALSE
          )
        }

        model_rows[[length(model_rows) + 1L]] <- tibble(
          variant = variant_i,
          construction_sample,
          outcome,
          treatment,
          estimate = unname(coefficient_table[treatment_var, "Estimate"]),
          se = unname(coefficient_table[treatment_var, "Std. Error"]),
          p_value = unname(coefficient_table[treatment_var, "Pr(>|t|)"]),
          n = nobs(model),
          ward_pairs = n_distinct(outcome_data$ward_pair),
          pins = n_distinct(outcome_data$pin)
        )
      }
    }
  }
}

production_assignment <- production_model_data %>%
  select(
    pin,
    production_ward = ward,
    production_ward_pair = ward_pair,
    production_segment_id = segment_id,
    production_alderman = alderman_own,
    production_strictness = strictness_own,
    production_neighbor_strictness = strictness_neighbor,
    production_score_difference = continuous_score_difference,
    production_distance_m = dist_to_boundary_m
  )

changed_rows <- bind_rows(change_rows) %>%
  left_join(
    assignment_data %>%
      select(
        pin, alternate_year = construction_year,
        alternate_ward = assigned_ward,
        alternate_ward_pair = ward_pair,
        alternate_segment_id = segment_id,
        alternate_alderman = alderman_own,
        alternate_strictness = strictness_own,
        alternate_neighbor_strictness = strictness_neighbor,
        alternate_score_difference = continuous_score_difference,
        alternate_distance_m = dist_to_boundary_m
      ),
    by = c("pin", "alternate_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(production_assignment, by = "pin", relationship = "many-to-one") %>%
  mutate(
    boundary_assignment_changed = production_ward_pair != alternate_ward_pair,
    alderman_changed = production_alderman != alternate_alderman,
    treatment_value_changed = production_score_difference != alternate_score_difference,
    production_in_500ft = production_in_paper_window & production_distance_m <= bandwidth_m,
    alternate_in_500ft = alternate_in_paper_window & alternate_distance_m <= bandwidth_m
  ) %>%
  arrange(selection_rule, pin)

write_csv(bind_rows(model_rows), "../output/construction_year_model_results.csv")
write_csv(sample_summary, "../output/construction_year_model_sample_summary.csv")
write_csv(changed_rows, "../output/construction_year_model_changed_rows.csv")
