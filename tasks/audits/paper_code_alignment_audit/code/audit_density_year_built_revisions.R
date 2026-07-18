# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/paper_code_alignment_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")
source("../../../_lib/border_pair_helpers.R")

bandwidth_m <- 152.4
demographic_controls <- c(
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

invisible(dbExecute(con, "
CREATE TEMP TABLE parsed_cards AS
SELECT
  trim(pin) AS pin,
  try_cast(numeric_text(year) AS INTEGER) AS tax_year,
  try_cast(numeric_text(card) AS INTEGER) AS card_num,
  trim(class) AS class,
  trim(tieback_key_pin) AS proration_key_pin,
  try_cast(numeric_text(tieback_proration_rate) AS DOUBLE) AS pin_proration_rate,
  try_cast(numeric_text(card_proration_rate) AS DOUBLE) AS card_proration_rate,
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
  END AS num_apartments,
  trim(row_id) AS row_id
FROM read_csv(
  '../../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv',
  all_varchar = true,
  header = true,
  ignore_errors = true,
  max_line_size = 10000000
)
WHERE try_cast(numeric_text(township_code) AS INTEGER) IN (70, 71, 72, 73, 74, 75, 76, 77)
  AND try_cast(numeric_text(char_yrblt) AS INTEGER) >= 1999
  AND trim(pin) IS NOT NULL
  AND trim(pin) != ''
  AND try_cast(numeric_text(card) AS INTEGER) IS NOT NULL
  AND try_cast(numeric_text(char_yrblt) AS INTEGER) IS NOT NULL;
"))

read_selected_cards <- function(rule) {
  query <- switch(
    rule,
    minimum_report = "
      WITH ranked AS (
        SELECT *, row_number() OVER (
          PARTITION BY pin, card_num
          ORDER BY year_built, tax_year, building_sqft DESC NULLS LAST, row_id
        ) AS selected_row
        FROM parsed_cards
      )
      SELECT * EXCLUDE (selected_row) FROM ranked WHERE selected_row = 1",
    latest_report = "
      WITH ranked AS (
        SELECT *, row_number() OVER (
          PARTITION BY pin, card_num
          ORDER BY tax_year DESC NULLS LAST, building_sqft DESC NULLS LAST,
                   year_built DESC, row_id DESC
        ) AS selected_row
        FROM parsed_cards
      )
      SELECT * EXCLUDE (selected_row) FROM ranked WHERE selected_row = 1",
    latest_report_through_2022 = "
      WITH ranked AS (
        SELECT *, row_number() OVER (
          PARTITION BY pin, card_num
          ORDER BY tax_year DESC NULLS LAST, building_sqft DESC NULLS LAST,
                   year_built DESC, row_id DESC
        ) AS selected_row
        FROM parsed_cards
        WHERE tax_year <= 2022
      )
      SELECT * EXCLUDE (selected_row) FROM ranked WHERE selected_row = 1",
    modal_report = "
      WITH year_counts AS (
        SELECT
          pin, card_num, year_built,
          count(*) AS reports,
          max(tax_year) AS latest_tax_year
        FROM parsed_cards
        GROUP BY pin, card_num, year_built
      ),
      chosen_year AS (
        SELECT *, row_number() OVER (
          PARTITION BY pin, card_num
          ORDER BY reports DESC, latest_tax_year DESC, year_built DESC
        ) AS chosen_year_row
        FROM year_counts
      ),
      ranked AS (
        SELECT p.*, row_number() OVER (
          PARTITION BY p.pin, p.card_num
          ORDER BY p.tax_year DESC NULLS LAST, p.building_sqft DESC NULLS LAST,
                   p.row_id DESC
        ) AS selected_row
        FROM parsed_cards p
        INNER JOIN chosen_year y
          ON p.pin = y.pin
         AND p.card_num = y.card_num
         AND p.year_built = y.year_built
         AND y.chosen_year_row = 1
      )
      SELECT * EXCLUDE (selected_row) FROM ranked WHERE selected_row = 1"
  )

  dbGetQuery(con, query) %>%
    as_tibble() %>%
    mutate(selection_rule = rule)
}

standardize_cards <- function(df) {
  df %>%
    mutate(
      pin = as.character(pin),
      card_num = as.integer(card_num),
      construction_year = as.integer(year_built),
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
      ),
      arealotsf = as.numeric(land_sqft),
      areabuilding = as.numeric(building_sqft)
    )
}

selected_cards <- bind_rows(lapply(
  c("minimum_report", "latest_report", "latest_report_through_2022", "modal_report"),
  read_selected_cards
)) %>%
  standardize_cards()

card_history <- dbGetQuery(con, "
SELECT
  pin,
  card_num,
  count(*) AS reports,
  count(DISTINCT year_built) AS distinct_reported_years,
  min(tax_year) AS earliest_tax_year,
  max(tax_year) AS latest_tax_year,
  min(year_built) AS earliest_reported_year,
  max(year_built) AS latest_reported_year
FROM parsed_cards
GROUP BY pin, card_num
ORDER BY pin, card_num
") %>%
  as_tibble() %>%
  mutate(pin = as.character(pin), card_num = as.integer(card_num))

year_comparison <- selected_cards %>%
  select(pin, card_num, selection_rule, construction_year, selected_tax_year = tax_year) %>%
  pivot_wider(
    names_from = selection_rule,
    values_from = c(construction_year, selected_tax_year),
    names_glue = "{selection_rule}_{.value}"
  ) %>%
  left_join(card_history, by = c("pin", "card_num"), relationship = "one-to-one") %>%
  mutate(
    latest_minus_minimum = latest_report_construction_year - minimum_report_construction_year,
    modal_minus_minimum = modal_report_construction_year - minimum_report_construction_year,
    minimum_in_paper_sample = between(minimum_report_construction_year, 2006, 2022),
    latest_in_paper_sample = between(latest_report_construction_year, 2006, 2022),
    modal_in_paper_sample = between(modal_report_construction_year, 2006, 2022),
    latest_2022_in_paper_sample = between(
      latest_report_through_2022_construction_year, 2006, 2022
    )
  )

production_residential <- read_csv(
  "../../../residential_improvements_data_cleaning/output/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), proration_key_pin = col_character(), .default = col_guess())
) %>%
  mutate(
    pin = as.character(pin),
    card_num = as.integer(card_num),
    construction_year = as.integer(year_built),
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
    ),
    arealotsf = as.numeric(land_sqft),
    areabuilding = as.numeric(building_sqft)
  )

write_csv(
  bind_rows(
    selected_cards %>%
      count(selection_rule, construction_year, name = "observations") %>%
      mutate(observation_unit = "pin-card"),
    production_residential %>%
      count(construction_year, name = "observations") %>%
      mutate(
        selection_rule = "production_minimum_report",
        observation_unit = "selected PIN row"
      )
  ) %>%
    select(selection_rule, observation_unit, construction_year, observations) %>%
    arrange(selection_rule, construction_year),
  "../output/density_year_built_counts.csv"
)

commercial <- read_csv(
  "../../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(pin = as.character(pin), commercial_units = as.numeric(tot_units))

source_choice <- full_join(
  production_residential %>% select(pin, residential_units = unitscount),
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

current_geometry <- st_read(
  "../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(pin = as.character(pin))
if (anyDuplicated(current_geometry$pin) > 0) {
  stop("Current density geometry is not unique by PIN.", call. = FALSE)
}

geocoded_pins <- st_drop_geometry(current_geometry) %>% select(pin)
year_comparison <- year_comparison %>%
  mutate(
    selected_residential_source = pin %in% selected_residential_pins$pin,
    current_2025_coordinate = pin %in% geocoded_pins$pin
  )

write_csv(
  year_comparison %>%
    filter(
      distinct_reported_years > 1 |
        minimum_report_construction_year != latest_report_construction_year |
        minimum_report_construction_year != modal_report_construction_year
    ),
  "../output/density_year_built_revision_cases.csv"
)

write_csv(
  year_comparison %>%
    count(
      minimum_report_construction_year,
      latest_report_construction_year,
      modal_report_construction_year,
      name = "pin_cards"
    ) %>%
    arrange(desc(pin_cards), minimum_report_construction_year, latest_report_construction_year),
  "../output/density_year_built_revision_transitions.csv"
)

card_rule_lookup <- selected_cards %>%
  filter(selection_rule %in% c("latest_report", "modal_report")) %>%
  select(
    pin, card_num, selection_rule,
    alternate_year = construction_year,
    alternate_lot = arealotsf,
    alternate_building = areabuilding,
    alternate_units = unitscount
  )

single_card_pins <- card_history %>%
  count(pin, name = "cards") %>%
  filter(cards == 1) %>%
  select(pin)

production_residential_observations <- production_residential %>%
  semi_join(selected_residential_pins, by = "pin") %>%
  transmute(
    pin,
    observation_id = pin,
    construction_year,
    arealotsf,
    areabuilding,
    unitscount
  )

isolated_variants <- lapply(c("latest_report", "modal_report"), function(rule_i) {
  replacement <- card_rule_lookup %>%
    filter(selection_rule == rule_i) %>%
    semi_join(single_card_pins, by = "pin") %>%
    select(-selection_rule)

  joined <- production_residential_observations %>%
    left_join(replacement, by = "pin", relationship = "one-to-one")

  bind_rows(
    joined %>%
      transmute(
        variant = paste0("single_card_year_only_", rule_i),
        pin, observation_id,
        construction_year = coalesce(alternate_year, construction_year),
        arealotsf, areabuilding, unitscount
      ),
    joined %>%
      transmute(
        variant = paste0("single_card_full_record_", rule_i),
        pin, observation_id,
        construction_year = coalesce(alternate_year, construction_year),
        arealotsf = coalesce(alternate_lot, arealotsf),
        areabuilding = coalesce(alternate_building, areabuilding),
        unitscount = coalesce(alternate_units, unitscount)
      )
  )
}) %>% bind_rows()

card_variants <- selected_cards %>%
  filter(selection_rule %in% c("minimum_report", "latest_report", "modal_report")) %>%
  semi_join(selected_residential_pins, by = "pin") %>%
  transmute(
    variant = paste0("card_level_", selection_rule),
    pin,
    observation_id = paste(pin, card_num, sep = "_card_"),
    construction_year,
    arealotsf,
    areabuilding,
    unitscount
  )

pin_year_variants <- card_variants %>%
  mutate(variant = str_replace(variant, "^card_level_", "pin_year_")) %>%
  group_by(variant, pin, construction_year) %>%
  summarise(
    arealotsf = if (all(is.na(arealotsf))) NA_real_ else max(arealotsf, na.rm = TRUE),
    areabuilding = if (all(is.na(areabuilding))) NA_real_ else sum(areabuilding, na.rm = TRUE),
    unitscount = if (all(is.na(unitscount))) NA_real_ else sum(unitscount, na.rm = TRUE),
    cards_in_pin_year = n(),
    .groups = "drop"
  ) %>%
  mutate(
    observation_id = paste(pin, construction_year, sep = "_year_"),
    areabuilding = if_else(areabuilding == 0, NA_real_, areabuilding),
    unitscount = if_else(unitscount == 0, NA_real_, unitscount)
  )

alternative_residential <- bind_rows(isolated_variants, card_variants, pin_year_variants) %>%
  filter(between(construction_year, 2006, 2022))

assignment_keys <- alternative_residential %>%
  distinct(pin, construction_year) %>%
  mutate(
    construction_date = as.Date(paste0(construction_year, "-06-15")),
    boundary_year = canonical_boundary_year_from_date(construction_date),
    era = canonical_era_from_boundary_year(boundary_year),
    yearmon_key = as.character(as.yearmon(construction_date))
  )

assignment_sf <- current_geometry %>%
  select(pin, GEOID, zone_code) %>%
  inner_join(assignment_keys, by = "pin", relationship = "one-to-many")

ward_panel <- st_read("../../../ward_panel_create/output/ward_panel.gpkg", quiet = TRUE)
ward_maps <- load_canonical_ward_maps(ward_panel)
boundary_lines <- load_boundary_layers(
  "../../../border_segment_creation/output/ward_pair_boundaries.gpkg"
)
segment_layers <- load_segment_line_layers(
  "../../../border_segment_creation/output/boundary_segments_1320ft.gpkg"
)

boundary_assignment <- assign_points_to_boundaries(
  points_sf = assignment_sf,
  era_values = assignment_sf$era,
  ward_maps = ward_maps,
  boundary_lines = boundary_lines,
  chunk_n = 2000L
)

assignment_sf <- bind_cols(assignment_sf, boundary_assignment) %>%
  rename(
    ward = ward,
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
  "../../../create_alderman_data/output/chicago_alderman_panel.csv",
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

ward_controls <- read_csv(
  "../../../create_ward_controls/output/ward_controls_2000_2023.csv",
  show_col_types = FALSE
) %>%
  select(ward, year, share_white, share_black, median_hh_income, share_bach_plus, homeownership_rate)
if (anyDuplicated(ward_controls[c("ward", "year")]) > 0) {
  stop("Ward controls are not unique by ward-year.", call. = FALSE)
}

scores <- read_csv(
  "../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv",
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
    by = c("ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  rename(alderman_own = alderman) %>%
  left_join(
    alderman_lookup %>% rename(alderman_neighbor = alderman),
    by = c("other_ward" = "ward", "yearmon_key"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    ward_controls %>%
      rename_with(~ paste0(.x, "_own"), -c(ward, year)),
    by = c("ward", "construction_year" = "year"),
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
    zone_group = zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

alternative_model_data <- alternative_residential %>%
  inner_join(
    assignment_data,
    by = c("pin", "construction_year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    density_far = areabuilding / arealotsf,
    density_dupac = 43560 * unitscount / arealotsf,
    source = "residential"
  )

production_model_data <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    variant = "production",
    observation_id = pin,
    source = source_choice$selected_source[match(pin, source_choice$pin)],
    zone_group = zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

fixed_commercial <- production_model_data %>%
  semi_join(selected_commercial_pins, by = "pin")

commercial_by_variant <- alternative_residential %>%
  distinct(variant) %>%
  tidyr::crossing(fixed_commercial %>% select(-variant))

model_columns <- c(
  "variant", "pin", "observation_id", "construction_year",
  "arealotsf", "areabuilding", "unitscount", "density_far", "density_dupac",
  "source", "dist_to_boundary_m", "ward_pair", "signed_distance_m",
  "zone_code", "zone_group", "segment_id", "strictness_own", "side",
  "lenient_dist", "strict_dist", demographic_controls
)

all_model_data <- bind_rows(
  production_model_data %>% select(all_of(model_columns)),
  alternative_model_data %>% select(all_of(model_columns)),
  commercial_by_variant %>% select(all_of(model_columns))
) %>%
  filter(
    between(construction_year, 2006, 2022),
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

variant_sample_summary <- all_model_data %>%
  group_by(variant) %>%
  summarise(
    rows_500ft = n(),
    residential_rows_500ft = sum(source == "residential", na.rm = TRUE),
    commercial_rows_500ft = sum(source == "commercial", na.rm = TRUE),
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
        treatment_var <- if (treatment == "continuous") "strictness_own" else "side"
        model <- feols(
          as.formula(paste0(
            "outcome_value ~ ", treatment_var, " + lenient_dist + strict_dist + ",
            paste(demographic_controls, collapse = " + "),
            " | zone_group + segment_id + construction_year"
          )),
          data = outcome_data,
          cluster = ~ward_pair,
          warn = FALSE
        )
        coefficient_table <- coeftable(model)

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

write_csv(bind_rows(model_rows), "../output/density_year_built_revision_models.csv")

revision_summary <- bind_rows(
  tibble(
    section = "raw_card_history",
    metric = c(
      "pin_cards",
      "pin_cards_with_revised_reported_year",
      "pin_cards_with_minimum_in_2006_2022",
      "pin_cards_with_latest_in_2006_2022",
      "pin_cards_moved_into_sample_by_latest",
      "pin_cards_moved_out_of_sample_by_latest",
      "pin_cards_moved_into_sample_by_modal",
      "pin_cards_moved_out_of_sample_by_modal",
      "pin_cards_latest_rule_differs_from_minimum",
      "pin_cards_modal_rule_differs_from_minimum"
    ),
    value = c(
      nrow(year_comparison),
      sum(year_comparison$distinct_reported_years > 1, na.rm = TRUE),
      sum(year_comparison$minimum_in_paper_sample, na.rm = TRUE),
      sum(year_comparison$latest_in_paper_sample, na.rm = TRUE),
      sum(!year_comparison$minimum_in_paper_sample & year_comparison$latest_in_paper_sample, na.rm = TRUE),
      sum(year_comparison$minimum_in_paper_sample & !year_comparison$latest_in_paper_sample, na.rm = TRUE),
      sum(!year_comparison$minimum_in_paper_sample & year_comparison$modal_in_paper_sample, na.rm = TRUE),
      sum(year_comparison$minimum_in_paper_sample & !year_comparison$modal_in_paper_sample, na.rm = TRUE),
      sum(year_comparison$minimum_report_construction_year != year_comparison$latest_report_construction_year, na.rm = TRUE),
      sum(year_comparison$minimum_report_construction_year != year_comparison$modal_report_construction_year, na.rm = TRUE)
    )
  ),
  variant_sample_summary %>%
    pivot_longer(-variant, names_to = "metric", values_to = "value") %>%
    transmute(section = variant, metric, value)
)

write_csv(revision_summary, "../output/density_year_built_revision_summary.csv")
