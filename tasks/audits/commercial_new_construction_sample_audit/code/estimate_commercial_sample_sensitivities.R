# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

commercial <- readr::read_csv(
  "../output/commercial_entity_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), segment_id = readr::col_character(), .default = readr::col_guess())
) %>%
  transmute(
    pin,
    commercial_source = TRUE,
    production_source_landsf,
    production_landsf,
    production_land_correction,
    production_bldgsf,
    commercial_units = production_units,
    source_bldgsf_2021,
    source_bldgsf_2024,
    source_units_2021,
    source_units_2024,
    source_landsf_2021,
    source_landsf_2024,
    component_count_parsed,
    keypin_to_group_centroid_ft,
    max_component_radius_ft,
    shared_components,
    nonkey_components_also_in_500ft_sample,
    selected_year_permit_supported,
    review_stratum,
    selected_valuation_year,
    production_yearbuilt,
    source_yearbuilt_2024,
    selected_yearbuilt_equals_valuation_year = production_yearbuilt == selected_valuation_year,
    unresolved_year_conflict =
      yearbuilt_conflict &
      !selected_year_permit_supported,
    stable_year_or_single_vintage = review_stratum %in% c(
      "same_yearbuilt_both_vintages",
      "single_valuation_vintage"
    ),
    alternate_bldgsf = case_when(
      is.na(production_bldgsf) & !is.na(source_bldgsf_2024) ~ source_bldgsf_2024,
      is.na(production_bldgsf) & !is.na(source_bldgsf_2021) ~ source_bldgsf_2021,
      TRUE ~ NA_real_
    ),
    stable_cross_vintage_units_land =
      !is.na(source_units_2021) &
      !is.na(source_units_2024) &
      abs(log(source_units_2024 / source_units_2021)) <= log(1.1) &
      !is.na(source_landsf_2021) &
      !is.na(source_landsf_2024) &
      abs(log(source_landsf_2024 / source_landsf_2021)) <= log(1.1)
  )

residential_latest_report <- readr::read_csv(
  "../output/residential_latest_report_cases.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), .default = readr::col_guess())
) %>%
  transmute(
    pin,
    residential_endpoint_pre1999_disagreement = endpoint_pre1999_disagreement,
    residential_later_pre1999_report = later_pre1999_report_after_selected_modern_report
  )

residential_tiebacks <- readr::read_csv(
  "../input/residential_cross_section.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    pin = readr::col_character(),
    proration_key_pin = readr::col_character(),
    pin_proration_rate = readr::col_double(),
    year_built = readr::col_double(),
    building_sqft = readr::col_double(),
    land_sqft = readr::col_double(),
    num_apartments = readr::col_double(),
    class = readr::col_character(),
    single_v_multi_family = readr::col_character(),
    .default = readr::col_skip()
  )
) %>%
  transmute(
    pin,
    tieback_group = if_else(
      !is.na(proration_key_pin) & str_squish(proration_key_pin) != "",
      str_replace_all(proration_key_pin, "[^0-9]", ""),
      NA_character_
    ),
    pin_proration_rate,
    residential_yearbuilt = as.integer(year_built),
    residential_bldgsf = building_sqft,
    residential_landsf = land_sqft,
    residential_units = num_apartments,
    residential_class = class,
    residential_building_use = single_v_multi_family,
    explicit_multifamily_missing_units = coalesce(
      str_to_lower(str_squish(single_v_multi_family)) == "multi-family" &
      is.na(num_apartments),
      FALSE
    )
  )

residential_tieback_groups <- residential_tiebacks %>%
  filter(!is.na(tieback_group), tieback_group != "") %>%
  group_by(tieback_group) %>%
  summarise(
    tieback_source_members = n(),
    tieback_proration_sum = sum(pin_proration_rate, na.rm = TRUE),
    tieback_land_sum = sum(residential_landsf, na.rm = TRUE),
    tieback_year_values = n_distinct(residential_yearbuilt, na.rm = TRUE),
    tieback_building_values = n_distinct(residential_bldgsf, na.rm = TRUE),
    tieback_unit_values = n_distinct(residential_units, na.rm = TRUE),
    .groups = "drop"
  )

parcels <- readr::read_csv(
  "../input/parcels_with_ward_distances.csv",
  show_col_types = FALSE,
  col_types = readr::cols(pin = readr::col_character(), segment_id = readr::col_character(), .default = readr::col_guess())
) %>%
  left_join(commercial, by = "pin", relationship = "many-to-one") %>%
  left_join(residential_latest_report, by = "pin", relationship = "many-to-one") %>%
  left_join(residential_tiebacks, by = "pin", relationship = "many-to-one") %>%
  left_join(residential_tieback_groups, by = "tieback_group", relationship = "many-to-one") %>%
  mutate(
    commercial_source = replace_na(commercial_source, FALSE),
    exact_residential_commercial_overlap =
      commercial_source & !is.na(residential_yearbuilt),
    residential_endpoint_pre1999_disagreement = replace_na(residential_endpoint_pre1999_disagreement, FALSE),
    residential_later_pre1999_report = replace_na(residential_later_pre1999_report, FALSE),
    lenient_dist = abs(signed_distance_m) * as.integer(signed_distance_m <= 0),
    strict_dist = abs(signed_distance_m) * as.integer(signed_distance_m > 0),
    side = as.integer(signed_distance_m > 0),
    continuous_score_difference = (strictness_own - strictness_neighbor) / 2,
    pair_average_score = (strictness_own + strictness_neighbor) / 2
  ) %>%
  filter(
    arealotsf > 1,
    construction_year >= 2006,
    construction_year <= 2022,
    dist_to_boundary_m <= 152.4,
    !is.na(ward_pair),
    is.finite(signed_distance_m),
    !is.na(construction_zone_group),
    !is.na(segment_id),
    segment_id != "",
    is.finite(strictness_own),
    is.finite(strictness_neighbor),
    if_all(
      c(
        share_white_own, share_black_own, median_hh_income_own,
        share_bach_plus_own, homeownership_rate_own
      ),
      is.finite
    )
  )

clean_tieback_groups <- parcels %>%
  filter(!is.na(tieback_group), tieback_group != "") %>%
  group_by(tieback_group) %>%
  summarise(
    same_segment = n_distinct(segment_id) == 1,
    same_side = n_distinct(signed_distance_m > 0) == 1,
    no_commercial_member = !any(commercial_source),
    source_members = first(tieback_source_members),
    proration_sum = first(tieback_proration_sum),
    land_sum = first(tieback_land_sum),
    year_values = first(tieback_year_values),
    building_values = first(tieback_building_values),
    unit_values = first(tieback_unit_values),
    .groups = "drop"
  ) %>%
  filter(
    same_segment,
    same_side,
    no_commercial_member,
    is.finite(proration_sum),
    abs(proration_sum - 1) < 0.001,
    is.finite(land_sum),
    land_sum > 1,
    year_values <= 1,
    building_values <= 1,
    unit_values <= 1
  )

clean_tieback_representatives <- parcels %>%
  inner_join(
    clean_tieback_groups %>% select(tieback_group, land_sum),
    by = "tieback_group",
    relationship = "many-to-one"
  ) %>%
  arrange(tieback_group, desc(pin == tieback_group), abs(signed_distance_m), pin) %>%
  group_by(tieback_group) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(
    arealotsf = land_sum,
    density_far = areabuilding / arealotsf,
    density_dupac = 43560 * unitscount / arealotsf
  ) %>%
  select(-land_sum)

collapse_clean_residential_tiebacks <- parcels %>%
  filter(is.na(tieback_group) | !tieback_group %in% clean_tieback_groups$tieback_group) %>%
  bind_rows(clean_tieback_representatives)

residential_tieback_group_review <- parcels %>%
  filter(!is.na(tieback_group), tieback_group != "") %>%
  group_by(tieback_group) %>%
  summarise(
    sample_pins = n(),
    pins = paste(sort(pin), collapse = "/"),
    commercial_members = sum(commercial_source),
    source_members = first(tieback_source_members),
    proration_sum = first(tieback_proration_sum),
    source_land_sum = first(tieback_land_sum),
    year_values = first(tieback_year_values),
    building_values = first(tieback_building_values),
    unit_values = first(tieback_unit_values),
    segment_values = n_distinct(segment_id),
    side_values = n_distinct(signed_distance_m > 0),
    clean_collapse_group = first(tieback_group) %in% clean_tieback_groups$tieback_group,
    .groups = "drop"
  )

readr::write_csv(
  residential_tieback_group_review %>% arrange(desc(clean_collapse_group), tieback_group),
  "../output/residential_tieback_group_review.csv"
)
readr::write_csv(
  tibble::tribble(
    ~metric, ~value,
    "tieback_groups_touching_main_sample", nrow(residential_tieback_group_review),
    "main_sample_pins_with_explicit_tieback", sum(residential_tieback_group_review$sample_pins),
    "clean_same_segment_same_side_groups", sum(residential_tieback_group_review$clean_collapse_group),
    "pins_replaced_by_clean_group_rows", sum(residential_tieback_group_review$sample_pins[residential_tieback_group_review$clean_collapse_group]),
    "clean_group_rows_after_collapse", sum(residential_tieback_group_review$clean_collapse_group)
  ),
  "../output/residential_tieback_summary.csv"
)

conservative_residential_sample <- collapse_clean_residential_tiebacks %>%
  filter(commercial_source | residential_class != "297") %>%
  filter(
    is.na(tieback_group) |
      !tieback_group %in% residential_tieback_group_review$tieback_group[
        residential_tieback_group_review$segment_values > 1 |
          residential_tieback_group_review$side_values > 1
      ]
  )

scenario_data <- list(
  production = parcels,
  drop_tieback_groups_crossing_segments_or_sides = parcels %>%
    filter(
      is.na(tieback_group) |
        !tieback_group %in% residential_tieback_group_review$tieback_group[
          residential_tieback_group_review$segment_values > 1 |
            residential_tieback_group_review$side_values > 1
        ]
    ),
  drop_all_residential_tieback_pins = parcels %>%
    filter(commercial_source | is.na(tieback_group) | tieback_group == ""),
  collapse_clean_residential_tieback_groups = collapse_clean_residential_tiebacks,
  drop_residential_class_297 = parcels %>%
    filter(commercial_source | residential_class != "297"),
  drop_non_class_2_residential_rows = parcels %>%
    filter(commercial_source | str_detect(residential_class, "^2")),
  drop_explicit_multifamily_rows_with_imputed_one_unit = parcels %>%
    filter(commercial_source | !explicit_multifamily_missing_units),
  collapse_clean_tiebacks_and_drop_class_297 = collapse_clean_residential_tiebacks %>%
    filter(commercial_source | residential_class != "297"),
  prefer_commercial_fields_at_exact_source_overlaps = parcels %>%
    mutate(
      unitscount = if_else(exact_residential_commercial_overlap, commercial_units, unitscount),
      areabuilding = if_else(exact_residential_commercial_overlap, production_bldgsf, areabuilding),
      arealotsf = if_else(exact_residential_commercial_overlap, production_landsf, arealotsf),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  prefer_residential_fields_at_exact_source_overlaps = parcels %>%
    mutate(
      unitscount = if_else(exact_residential_commercial_overlap, residential_units, unitscount),
      areabuilding = if_else(exact_residential_commercial_overlap, residential_bldgsf, areabuilding),
      arealotsf = if_else(exact_residential_commercial_overlap, residential_landsf, arealotsf),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  verified_permit_and_public_record_corrections = parcels %>%
    filter(pin != "17153000140000") %>%
    mutate(
      unitscount = case_when(
        pin == "17153000220000" ~ 134,
        pin == "14313080700000" ~ 44,
        pin == "17164010220000" ~ 173,
        pin == "17164010230000" ~ 173,
        pin == "20151060290000" ~ 27,
        TRUE ~ unitscount
      ),
      areabuilding = case_when(
        pin == "17164010220000" ~ 175540,
        pin == "17164010230000" ~ 185094,
        TRUE ~ areabuilding
      ),
      arealotsf = case_when(
        pin == "17164010220000" ~ 21366,
        pin == "17164010230000" ~ 21337,
        TRUE ~ arealotsf
      ),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  verified_corrections_plus_3600_halsted_collapse = parcels %>%
    filter(!pin %in% c("14202300200000", "17153000140000")) %>%
    mutate(
      unitscount = case_when(
        pin == "17153000220000" ~ 134,
        pin == "14313080700000" ~ 44,
        pin == "17164010220000" ~ 173,
        pin == "17164010230000" ~ 173,
        pin == "20151060290000" ~ 27,
        pin == "14202300160000" ~ 79,
        TRUE ~ unitscount
      ),
      areabuilding = case_when(
        pin == "17164010220000" ~ 175540,
        pin == "17164010230000" ~ 185094,
        pin == "14202300160000" ~ 79309,
        TRUE ~ areabuilding
      ),
      arealotsf = case_when(
        pin == "17164010220000" ~ 21366,
        pin == "17164010230000" ~ 21337,
        pin == "14202300160000" ~ 163152,
        TRUE ~ arealotsf
      ),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  disaggregate_warren_entity_fields_at_current_year = parcels %>%
    mutate(
      unitscount = if_else(pin == "17074290130000", 12, unitscount),
      areabuilding = if_else(pin == "17074290130000", 13203, areabuilding),
      arealotsf = if_else(pin == "17074290130000", 11235, arealotsf),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  verified_corrections_plus_entity_repairs = parcels %>%
    filter(!pin %in% c("14202300200000", "17153000140000")) %>%
    mutate(
      unitscount = case_when(
        pin == "17153000220000" ~ 134,
        pin == "14313080700000" ~ 44,
        pin == "17164010220000" ~ 173,
        pin == "17164010230000" ~ 173,
        pin == "20151060290000" ~ 27,
        pin == "14202300160000" ~ 79,
        pin == "17074290130000" ~ 12,
        TRUE ~ unitscount
      ),
      areabuilding = case_when(
        pin == "17164010220000" ~ 175540,
        pin == "17164010230000" ~ 185094,
        pin == "14202300160000" ~ 79309,
        pin == "17074290130000" ~ 13203,
        TRUE ~ areabuilding
      ),
      arealotsf = case_when(
        pin == "17164010220000" ~ 21366,
        pin == "17164010230000" ~ 21337,
        pin == "14202300160000" ~ 163152,
        pin == "17074290130000" ~ 11235,
        TRUE ~ arealotsf
      ),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  combined_conservative_residential_and_verified_commercial = conservative_residential_sample %>%
    filter(pin != "17153000140000") %>%
    mutate(
      unitscount = case_when(
        pin == "17153000220000" ~ 134,
        pin == "14313080700000" ~ 44,
        pin == "17164010220000" ~ 173,
        pin == "17164010230000" ~ 173,
        pin == "20151060290000" ~ 27,
        TRUE ~ unitscount
      ),
      areabuilding = case_when(
        pin == "17164010220000" ~ 175540,
        pin == "17164010230000" ~ 185094,
        TRUE ~ areabuilding
      ),
      arealotsf = case_when(
        pin == "17164010220000" ~ 21366,
        pin == "17164010230000" ~ 21337,
        TRUE ~ arealotsf
      ),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  combined_conservative_plus_commercial_entity_repairs = conservative_residential_sample %>%
    filter(!pin %in% c("14202300200000", "17153000140000")) %>%
    mutate(
      unitscount = case_when(
        pin == "17153000220000" ~ 134,
        pin == "14313080700000" ~ 44,
        pin == "17164010220000" ~ 173,
        pin == "17164010230000" ~ 173,
        pin == "20151060290000" ~ 27,
        pin == "14202300160000" ~ 79,
        pin == "17074290130000" ~ 12,
        TRUE ~ unitscount
      ),
      areabuilding = case_when(
        pin == "17164010220000" ~ 175540,
        pin == "17164010230000" ~ 185094,
        pin == "14202300160000" ~ 79309,
        pin == "17074290130000" ~ 13203,
        TRUE ~ areabuilding
      ),
      arealotsf = case_when(
        pin == "17164010220000" ~ 21366,
        pin == "17164010230000" ~ 21337,
        pin == "14202300160000" ~ 163152,
        pin == "17074290130000" ~ 11235,
        TRUE ~ arealotsf
      ),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  undo_unsupported_land_correction = parcels %>%
    mutate(
      arealotsf = if_else(
        commercial_source & production_land_correction,
        production_source_landsf,
        arealotsf
      ),
      density_far = areabuilding / arealotsf,
      density_dupac = 43560 * unitscount / arealotsf
    ),
  recover_stable_commercial_building_area = parcels %>%
    mutate(
      areabuilding = if_else(
        commercial_source &
          (is.na(areabuilding) | areabuilding <= 1) &
          stable_cross_vintage_units_land &
          alternate_bldgsf > 1,
        alternate_bldgsf,
        areabuilding
      ),
      density_far = areabuilding / arealotsf
    ),
  recover_any_available_commercial_building_area = parcels %>%
    mutate(
      areabuilding = if_else(
        commercial_source &
          (is.na(areabuilding) | areabuilding <= 1) &
          alternate_bldgsf > 1,
        alternate_bldgsf,
        areabuilding
      ),
      density_far = areabuilding / arealotsf
    ),
  dupac_without_building_area_common_sample = parcels,
  drop_all_commercial = parcels %>%
    filter(!commercial_source),
  drop_multi_pin_commercial = parcels %>%
    filter(!commercial_source | component_count_parsed == 1),
  drop_commercial_keypin_displacement_gt100ft = parcels %>%
    filter(!commercial_source | is.na(keypin_to_group_centroid_ft) | keypin_to_group_centroid_ft <= 100),
  drop_commercial_group_radius_gt500ft = parcels %>%
    filter(!commercial_source | is.na(max_component_radius_ft) | max_component_radius_ft <= 500),
  drop_overlapping_commercial_entities = parcels %>%
    filter(!commercial_source | shared_components == 0),
  drop_commercial_entities_with_component_also_sampled = parcels %>%
    filter(!commercial_source | nonkey_components_also_in_500ft_sample == 0),
  drop_unresolved_commercial_year_conflicts = parcels %>%
    filter(!commercial_source | !unresolved_year_conflict),
  drop_clear_commercial_old_building_recodes = parcels %>%
    filter(
      !commercial_source |
        review_stratum != "pre1999_counterpart_stable_fields_no_permit_support"
    ),
  drop_all_commercial_pre1999_counterparts_without_permits = parcels %>%
    filter(
      !commercial_source |
        selected_year_permit_supported |
        !review_stratum %in% c(
          "pre1999_counterpart_stable_fields_no_permit_support",
          "pre1999_counterpart_large_physical_change_no_permit_support",
          "other_yearbuilt_conflict_no_permit_support"
        )
    ),
  drop_commercial_rows_that_disagree_with_2024_year = parcels %>%
    filter(
      !commercial_source |
        is.na(source_yearbuilt_2024) |
        production_yearbuilt == source_yearbuilt_2024
    ),
  require_2024_commercial_year_in_sample_window = parcels %>%
    filter(
      !commercial_source |
        between(source_yearbuilt_2024, 2006, 2022)
    ),
  keep_permit_supported_stable_or_single_vintage_commercial = parcels %>%
    filter(
      !commercial_source |
        selected_year_permit_supported |
        stable_year_or_single_vintage
    ),
  keep_only_permit_supported_or_stable_year_commercial = parcels %>%
    filter(
      !commercial_source |
        selected_year_permit_supported |
        review_stratum == "same_yearbuilt_both_vintages"
    ),
  drop_valuation_year_codes_without_permit_support = parcels %>%
    filter(
      !commercial_source |
        !selected_yearbuilt_equals_valuation_year |
        selected_year_permit_supported
    ),
  drop_residential_pre1999_endpoint_disagreements = parcels %>%
    filter(!residential_endpoint_pre1999_disagreement),
  drop_residential_later_pre1999_reports = parcels %>%
    filter(!residential_later_pre1999_report)
)

controls <- c(
  "pair_average_score",
  "lenient_dist",
  "strict_dist",
  "share_white_own",
  "share_black_own",
  "median_hh_income_own",
  "share_bach_plus_own",
  "homeownership_rate_own"
)

results <- list()
for (scenario in names(scenario_data)) {
  for (sample_name in c("all", "multifamily")) {
    model_data <- scenario_data[[scenario]] %>%
      filter(if (sample_name == "all") unitscount > 0 else unitscount > 1)

    for (outcome in c("density_far", "density_dupac")) {
      model_data_outcome <- model_data %>%
        filter(is.finite(.data[[outcome]]), .data[[outcome]] > 0)
      if (scenario != "dupac_without_building_area_common_sample" || outcome == "density_far") {
        model_data_outcome <- model_data_outcome %>%
          filter(is.finite(areabuilding), areabuilding > 1)
      }

      for (treatment in c("continuous", "binary")) {
        treatment_variable <- if (treatment == "continuous") {
          "continuous_score_difference"
        } else {
          "side"
        }

        model <- fixest::feols(
          as.formula(paste0(
            "log(", outcome, ") ~ ",
            paste(c(treatment_variable, controls), collapse = " + "),
            " | construction_zone_group + segment_id + construction_year"
          )),
          data = model_data_outcome,
          cluster = ~ward_pair
        )

        coefficient <- fixest::coeftable(model)[treatment_variable, ]
        results[[length(results) + 1L]] <- tibble::tibble(
          scenario,
          sample = sample_name,
          outcome,
          treatment,
          estimate = unname(coefficient["Estimate"]),
          std_error = unname(coefficient["Std. Error"]),
          p_value = unname(coefficient["Pr(>|t|)"]),
          observations = stats::nobs(model),
          commercial_observations = sum(model_data_outcome$commercial_source),
          ward_pairs = n_distinct(model_data_outcome$ward_pair)
        )
      }
    }
  }
}

readr::write_csv(
  bind_rows(results),
  "../output/commercial_sample_sensitivity_models.csv"
)
