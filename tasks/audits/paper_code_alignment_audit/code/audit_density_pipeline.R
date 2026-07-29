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

corrected_zone_group_from_code <- function(z) {
  z <- str_to_upper(as.character(z))
  case_when(
    str_starts(z, "RS-") ~ "Single-Family Residential",
    str_starts(z, "RT-") | str_starts(z, "RM-") ~ "Multi-Family Residential",
    str_detect(z, "^B-?[1-7]-") ~ "Neighborhood Mixed-Use",
    str_detect(z, "^C-?[1-7]-") ~ "Commercial",
    str_detect(z, "^M-?[1-7]-") ~ "Industrial",
    str_starts(z, "DX-") | str_starts(z, "DR-") | str_starts(z, "DS-") | str_starts(z, "DC-") ~ "Downtown",
    str_starts(z, "PD") ~ "Planned Development",
    TRUE ~ "Other"
  )
}

parcels <- read_csv(
  "../../../merge_in_scores/output/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), segment_id = col_character(), .default = col_guess())
) %>%
  ensure_meter_distance_columns() %>%
  mutate(
    pin = as.character(pin),
    construction_year = as.integer(construction_year),
    segment_id = as.character(segment_id),
    zone_group = zone_group_from_code(zone_code),
    zone_group_corrected = corrected_zone_group_from_code(zone_code),
    side = as.integer(signed_distance_m > 0),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0)
  )

if (anyDuplicated(parcels$pin) > 0) {
  stop("Merged density input contains duplicate PINs.", call. = FALSE)
}

geometry_parcels <- st_read(
  "../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg",
  quiet = TRUE
) %>%
  mutate(pin = as.character(pin))

fresh_clean_zoning <- st_read(
  "../../../zoning_data_cleaning/output/zoning_data_clean.gpkg",
  quiet = TRUE
) %>%
  select(fresh_zone_code = zone_code)

if (st_crs(fresh_clean_zoning) != st_crs(geometry_parcels)) {
  fresh_clean_zoning <- st_transform(fresh_clean_zoning, st_crs(geometry_parcels))
}

fresh_zoning_lookup <- geometry_parcels %>%
  select(pin) %>%
  st_join(fresh_clean_zoning, left = TRUE, largest = TRUE) %>%
  st_drop_geometry()

if (nrow(fresh_zoning_lookup) != nrow(geometry_parcels) || anyDuplicated(fresh_zoning_lookup$pin) > 0) {
  stop("Fresh zoning audit join changed the parcel row count or duplicated PINs.", call. = FALSE)
}

parcels <- parcels %>%
  left_join(fresh_zoning_lookup, by = "pin", relationship = "many-to-one") %>%
  mutate(
    fresh_zone_group = zone_group_from_code(fresh_zone_code),
    fresh_zone_group_corrected = corrected_zone_group_from_code(fresh_zone_code)
  )

sample_specs <- tribble(
  ~sample, ~outcome, ~outcome_expression,
  "All Construction", "FAR", "density_far",
  "All Construction", "DUPAC", "density_dupac",
  "Multifamily", "FAR", "density_far",
  "Multifamily", "DUPAC", "density_dupac"
)

attrition_rows <- list()
model_rows <- list()
display_rows <- list()
model_membership <- list()

for (i in seq_len(nrow(sample_specs))) {
  sample_label <- sample_specs$sample[i]
  outcome_label <- sample_specs$outcome[i]
  outcome_var <- sample_specs$outcome_expression[i]

  dat <- parcels
  stages <- list(
    "Scored and boundary-assigned output" = rep(TRUE, nrow(dat)),
    "Construction years 2006-2022; positive lot and building area" =
      dat$construction_year >= 2006 & dat$construction_year <= 2022 &
      dat$arealotsf > 1 & dat$areabuilding > 1,
    "Positive units for analysis sample" = if (sample_label == "All Construction") {
      dat$unitscount > 0
    } else {
      dat$unitscount > 1
    },
    "Within 500 feet" = is.finite(dat$dist_to_boundary_m) & dat$dist_to_boundary_m <= bandwidth_m,
    "Valid ward pair and signed distance" = !is.na(dat$ward_pair) & is.finite(dat$signed_distance_m),
    "Nonmissing current zoning code" = !is.na(dat$zone_code),
    "Nonmissing boundary segment" = !is.na(dat$segment_id) & dat$segment_id != "",
    "Positive finite outcome" = is.finite(dat[[outcome_var]]) & dat[[outcome_var]] > 0,
    "Complete continuous model covariates" = complete.cases(dat[, c(
      "strictness_own", "lenient_dist", "strict_dist", demographic_controls,
      "zone_group", "segment_id", "construction_year", "ward_pair"
    )]),
    "Complete binary model covariates" = complete.cases(dat[, c(
      "side", "lenient_dist", "strict_dist", demographic_controls,
      "zone_group", "segment_id", "construction_year", "ward_pair"
    )])
  )

  keep <- rep(TRUE, nrow(dat))
  previous_n <- nrow(dat)
  for (stage_name in names(stages)) {
    keep <- keep & !is.na(stages[[stage_name]]) & stages[[stage_name]]
    current_n <- sum(keep)
    attrition_rows[[length(attrition_rows) + 1L]] <- tibble(
      sample = sample_label,
      outcome = outcome_label,
      stage = stage_name,
      n = current_n,
      dropped_at_stage = previous_n - current_n
    )
    previous_n <- current_n
  }

  analysis_dat <- parcels %>%
    filter(
      construction_year >= 2006,
      construction_year <= 2022,
      arealotsf > 1,
      areabuilding > 1,
      if (sample_label == "All Construction") unitscount > 0 else unitscount > 1,
      dist_to_boundary_m <= bandwidth_m,
      !is.na(ward_pair),
      is.finite(signed_distance_m),
      !is.na(zone_code),
      !is.na(segment_id),
      segment_id != "",
      is.finite(.data[[outcome_var]]),
      .data[[outcome_var]] > 0
    ) %>%
    mutate(outcome_value = log(.data[[outcome_var]]))

  exact_formulas <- list(
    continuous = as.formula(paste0(
      "outcome_value ~ strictness_own + lenient_dist + strict_dist + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    )),
    binary = as.formula(paste0(
      "outcome_value ~ side + lenient_dist + strict_dist + ",
      paste(demographic_controls, collapse = " + "),
      " | zone_group + segment_id + construction_year"
    ))
  )

  for (treatment_spec in names(exact_formulas)) {
    treatment_var <- if (treatment_spec == "continuous") "strictness_own" else "side"
    no_zoning_formula <- as.formula(paste0(
      "outcome_value ~ ", treatment_var, " + lenient_dist + strict_dist + ",
      paste(demographic_controls, collapse = " + "),
      " | segment_id + construction_year"
    ))
    model <- feols(exact_formulas[[treatment_spec]], data = analysis_dat, cluster = ~ward_pair)
    coefficient_table <- coeftable(model)

    same_sample_no_zoning <- feols(
      no_zoning_formula,
      data = analysis_dat,
      cluster = ~ward_pair
    )

    expanded_dat <- parcels %>%
      filter(
        construction_year >= 2006,
        construction_year <= 2022,
        arealotsf > 1,
        areabuilding > 1,
        if (sample_label == "All Construction") unitscount > 0 else unitscount > 1,
        dist_to_boundary_m <= bandwidth_m,
        !is.na(ward_pair),
        is.finite(signed_distance_m),
        !is.na(segment_id),
        segment_id != "",
        is.finite(.data[[outcome_var]]),
        .data[[outcome_var]] > 0
      ) %>%
      mutate(outcome_value = log(.data[[outcome_var]]))

    expanded_no_zoning <- feols(
      no_zoning_formula,
      data = expanded_dat,
      cluster = ~ward_pair
    )

    fresh_zoning_dat <- expanded_dat %>%
      filter(!is.na(fresh_zone_code))
    fresh_zoning_formula <- as.formula(paste0(
      "outcome_value ~ ", treatment_var, " + lenient_dist + strict_dist + ",
      paste(demographic_controls, collapse = " + "),
      " | fresh_zone_group + segment_id + construction_year"
    ))
    fresh_zoning_model <- feols(
      fresh_zoning_formula,
      data = fresh_zoning_dat,
      cluster = ~ward_pair
    )

    production_without_pd_dat <- analysis_dat %>%
      filter(zone_group != "Planned Development")
    production_without_pd_model <- feols(
      exact_formulas[[treatment_spec]],
      data = production_without_pd_dat,
      cluster = ~ward_pair
    )

    fresh_corrected_formula <- as.formula(paste0(
      "outcome_value ~ ", treatment_var, " + lenient_dist + strict_dist + ",
      paste(demographic_controls, collapse = " + "),
      " | fresh_zone_group_corrected + segment_id + construction_year"
    ))
    fresh_corrected_model <- feols(
      fresh_corrected_formula,
      data = fresh_zoning_dat,
      cluster = ~ward_pair
    )

    for (spec_name in c(
      "production",
      "same_sample_without_zoning_fe",
      "expanded_sample_without_zoning_fe",
      "production_without_planned_development",
      "fresh_rebuild_with_clean_zoning",
      "fresh_rebuild_with_corrected_zone_groups"
    )) {
      comparison_model <- switch(
        spec_name,
        production = model,
        same_sample_without_zoning_fe = same_sample_no_zoning,
        expanded_sample_without_zoning_fe = expanded_no_zoning,
        production_without_planned_development = production_without_pd_model,
        fresh_rebuild_with_clean_zoning = fresh_zoning_model,
        fresh_rebuild_with_corrected_zone_groups = fresh_corrected_model
      )
      comparison_data <- switch(
        spec_name,
        production = analysis_dat,
        same_sample_without_zoning_fe = analysis_dat,
        expanded_sample_without_zoning_fe = expanded_dat,
        production_without_planned_development = production_without_pd_dat,
        fresh_rebuild_with_clean_zoning = fresh_zoning_dat,
        fresh_rebuild_with_corrected_zone_groups = fresh_zoning_dat
      )
      comparison_table <- coeftable(comparison_model)
      model_rows[[length(model_rows) + 1L]] <- tibble(
        sample = sample_label,
        outcome = outcome_label,
        treatment = treatment_spec,
        specification = spec_name,
        estimate = unname(comparison_table[treatment_var, "Estimate"]),
        se = unname(comparison_table[treatment_var, "Std. Error"]),
        p_value = unname(comparison_table[treatment_var, "Pr(>|t|)"]),
        n = nobs(comparison_model),
        ward_pairs = n_distinct(comparison_data$ward_pair[obs(comparison_model)])
      )
    }

    if (treatment_spec == "binary") {
      residual_model <- feols(
        as.formula(paste0(
          "outcome_value ~ ", paste(demographic_controls, collapse = " + "),
          " | zone_group + segment_id + construction_year"
        )),
        data = analysis_dat
      )
      removed <- residual_model$obs_selection$obsRemoved
      keep_index <- if (is.null(removed)) {
        seq_len(nrow(analysis_dat))
      } else {
        setdiff(seq_len(nrow(analysis_dat)), abs(as.integer(removed)))
      }
      residualized_dat <- analysis_dat[keep_index, , drop = FALSE] %>%
        mutate(residualized_outcome = as.numeric(resid(residual_model)))
      display_model <- feols(
        residualized_outcome ~ side * signed_distance_m,
        data = residualized_dat,
        cluster = ~ward_pair
      )
      display_table <- coeftable(display_model)
      display_rows[[length(display_rows) + 1L]] <- tibble(
        sample = sample_label,
        outcome = outcome_label,
        production_jump = unname(coefficient_table["side", "Estimate"]),
        production_se = unname(coefficient_table["side", "Std. Error"]),
        plotted_line_jump = unname(display_table["side", "Estimate"]),
        plotted_line_se = unname(display_table["side", "Std. Error"]),
        difference = plotted_line_jump - production_jump,
        n_production = nobs(model),
        n_display = nobs(display_model)
      )
    }
  }

  model_membership[[paste(sample_label, outcome_label, sep = "_")]] <- analysis_dat %>%
    transmute(pin, sample = sample_label, outcome = outcome_label)
}

write_csv(bind_rows(attrition_rows), "../output/density_sample_attrition.csv")
write_csv(bind_rows(model_rows), "../output/density_model_reproduction.csv")
write_csv(bind_rows(display_rows), "../output/density_display_spec_comparison.csv")

write_csv(
  bind_rows(
    parcels %>% count(zone_code, zone_group, zone_group_corrected, name = "n") %>% mutate(source = "stale_production_zoning"),
    parcels %>% count(
      zone_code = fresh_zone_code,
      zone_group = fresh_zone_group,
      zone_group_corrected = fresh_zone_group_corrected,
      name = "n"
    ) %>% mutate(source = "fresh_committed_zoning")
  ) %>%
    select(source, zone_code, zone_group, zone_group_corrected, n) %>%
    arrange(source, zone_code),
  "../output/density_zone_group_mapping.csv"
)

residential <- read_csv(
  "../../../residential_improvements_data_cleaning/output/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = cols(pin = col_character(), .default = col_guess())
) %>%
  transmute(
    pin,
    residential_year = as.integer(year_built),
    residential_units_raw = as.numeric(num_apartments),
    residential_is_single_family =
      (!is.na(single_v_multi_family) & str_detect(single_v_multi_family, regex("^single", ignore_case = TRUE))) |
      (!is.na(type_of_residence) & type_of_residence %in% c(
        "1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"
      )),
    residential_units = if_else(
      residential_is_single_family & (is.na(residential_units_raw) | residential_units_raw == 0),
      1,
      residential_units_raw
    ),
    residential_building_sqft = as.numeric(building_sqft),
    residential_land_sqft = as.numeric(land_sqft),
    residential_card = as.integer(card_num),
    residential_multicard = as.logical(pin_is_multicard),
    residential_num_cards = as.integer(pin_num_cards),
    residential_multiland = as.logical(pin_is_multiland),
    residential_num_landlines = as.integer(pin_num_landlines)
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
  stop("Assessor source cross-sections are not unique by PIN.", call. = FALSE)
}

geocoded <- st_read(
  "../../../geocode_ccao_data/output/geocoded_residential_data.gpkg",
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  mutate(pin = as.character(pin)) %>%
  left_join(residential, by = "pin", relationship = "many-to-one") %>%
  left_join(commercial, by = "pin", relationship = "many-to-one") %>%
  mutate(
    in_residential_source = !is.na(residential_year),
    in_commercial_source = !is.na(commercial_year),
    selected_source = case_when(
      in_residential_source & !in_commercial_source ~ "residential_only",
      !in_residential_source & in_commercial_source ~ "commercial_only",
      in_residential_source & in_commercial_source &
        (is.na(residential_units) | commercial_units > residential_units) ~ "commercial_selected_from_overlap",
      in_residential_source & in_commercial_source ~ "residential_selected_from_overlap",
      TRUE ~ "source_not_recovered"
    ),
    expected_units = case_when(
      str_starts(selected_source, "commercial") ~ commercial_units,
      str_starts(selected_source, "residential") ~ residential_units,
      TRUE ~ NA_real_
    ),
    selected_values_match = is.na(expected_units) | abs(unitscount - expected_units) < 1e-8
  )

if (any(!geocoded$selected_values_match, na.rm = TRUE)) {
  stop("Recovered assessor source does not match geocoded output units.", call. = FALSE)
}

model_pins <- bind_rows(model_membership) %>%
  distinct(pin, sample, outcome)

source_universes <- bind_rows(
  geocoded %>% mutate(universe = "All geocoded construction records", sample = "All", outcome = "Any"),
  geocoded %>%
    inner_join(model_pins, by = "pin", relationship = "one-to-many") %>%
    mutate(universe = "Production 500-foot model input")
)

source_summary <- source_universes %>%
  group_by(universe, sample, outcome, selected_source) %>%
  summarise(
    n = n(),
    n_units_six_or_fewer = sum(unitscount <= 6, na.rm = TRUE),
    n_multicard = sum(residential_multicard %in% TRUE, na.rm = TRUE),
    n_multi_pin_commercial = sum(commercial_pin_group_count > 1, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(source_summary, "../output/density_source_summary.csv")

multicard_summary <- source_universes %>%
  mutate(
    card_group = case_when(
      is.na(residential_num_cards) ~ "Not in residential source",
      residential_num_cards <= 1 ~ "One assessor card",
      residential_num_cards == 2 ~ "Two assessor cards",
      residential_num_cards >= 3 ~ "Three or more assessor cards",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(universe, sample, outcome, selected_source, card_group, name = "n")

write_csv(multicard_summary, "../output/density_multicard_summary.csv")

raw_zoning <- st_read(
  "../../../../data_raw/Boundaries_-_Zoning_Districts_20250910.geojson",
  quiet = TRUE
) %>%
  transmute(raw_zone_code = str_to_upper(str_squish(as.character(zone_class))), geometry) %>%
  st_make_valid()

if (st_crs(raw_zoning) != st_crs(geometry_parcels)) {
  raw_zoning <- st_transform(raw_zoning, st_crs(geometry_parcels))
}

zoning_hits <- st_intersects(geometry_parcels, raw_zoning)
zoning_status <- tibble(
  pin = geometry_parcels$pin,
  stale_zone_code = geometry_parcels$zone_code,
  raw_zone_hit_count = lengths(zoning_hits),
  raw_zone_codes = vapply(zoning_hits, function(index) {
    if (length(index) == 0) return(NA_character_)
    paste(sort(unique(raw_zoning$raw_zone_code[index])), collapse = "|")
  }, character(1)),
  raw_has_excluded_zone = vapply(zoning_hits, function(index) {
    if (length(index) == 0) return(FALSE)
    any(str_detect(raw_zoning$raw_zone_code[index], "^(PD|PMD|POS)"), na.rm = TRUE)
  }, logical(1)),
  raw_has_included_zone = vapply(zoning_hits, function(index) {
    if (length(index) == 0) return(FALSE)
    any(!str_detect(raw_zoning$raw_zone_code[index], "^(PD|PMD|POS)"), na.rm = TRUE)
  }, logical(1))
) %>%
  mutate(
    raw_zoning_status = case_when(
      raw_zone_hit_count == 0 ~ "No raw zoning polygon hit",
      raw_has_excluded_zone & !raw_has_included_zone ~ "Only PD/PMD/POS current zoning",
      raw_has_excluded_zone & raw_has_included_zone ~ "Mixed excluded/included polygon hits",
      raw_zone_hit_count > 1 ~ "Multiple included polygon hits",
      TRUE ~ "One included polygon hit"
    )
  )

zoning_status <- zoning_status %>%
  left_join(fresh_zoning_lookup, by = "pin", relationship = "one-to-one")

zoning_model_universes <- bind_rows(
  parcels %>%
    select(pin, construction_year, dist_to_boundary_m, zone_code, fresh_zone_code) %>%
    mutate(universe = "Scored density output", sample = "All", outcome = "Any"),
  parcels %>%
    select(pin, construction_year, dist_to_boundary_m, zone_code, fresh_zone_code) %>%
    inner_join(model_pins, by = "pin", relationship = "one-to-many") %>%
    mutate(universe = "Production 500-foot model input")
) %>%
  left_join(zoning_status, by = "pin", relationship = "many-to-one")

zoning_summary <- zoning_model_universes %>%
  group_by(universe, sample, outcome, raw_zoning_status) %>%
  summarise(
    n = n(),
    n_stale_zone_missing = sum(is.na(zone_code)),
    n_fresh_zone_missing = sum(is.na(fresh_zone_code.x)),
    earliest_construction_year = min(construction_year, na.rm = TRUE),
    latest_construction_year = max(construction_year, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(zoning_summary, "../output/density_zoning_summary.csv")

zoning_excluded_rows <- zoning_model_universes %>%
  filter(raw_has_excluded_zone | is.na(zone_code) | is.na(fresh_zone_code.x)) %>%
  distinct(
    pin, construction_year, dist_to_boundary_m, universe, sample, outcome,
    zone_code, stale_zone_code, fresh_zone_code = fresh_zone_code.x, raw_zone_codes, raw_zoning_status
  ) %>%
  arrange(universe, sample, outcome, construction_year, pin)

if (nrow(zoning_excluded_rows) == 0) {
  zoning_excluded_rows <- tibble(
    pin = character(), construction_year = integer(), dist_to_boundary_m = double(),
    universe = character(), sample = character(), outcome = character(),
    zone_code = character(), stale_zone_code = character(), fresh_zone_code = character(), raw_zone_codes = character(),
    raw_zoning_status = character()
  )
}

write_csv(zoning_excluded_rows, "../output/density_zoning_excluded_rows.csv")
