# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/commercial_new_construction_sample_audit/code")

source("../../../setup_environment/code/packages.R")

parse_units <- function(x, single_family, residence_type) {
  text <- str_to_lower(str_squish(x))
  units <- case_when(
    is.na(text) | text == "" ~ NA_real_,
    text %in% c("none", "zero") ~ 0,
    text == "one" ~ 1,
    text == "two" ~ 2,
    text == "three" ~ 3,
    text == "four" ~ 4,
    text == "five" ~ 5,
    text == "six" ~ 6,
    TRUE ~ suppressWarnings(as.numeric(str_replace_all(text, "[^0-9.-]", "")))
  )

  case_when(
    is.finite(units) & units > 0 ~ units,
    str_detect(single_family, regex("^single", ignore_case = TRUE)) |
      residence_type %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level") ~ 1,
    TRUE ~ NA_real_
  )
}

one_finite_value <- function(x) {
  values <- unique(x[is.finite(x)])
  if (length(values) == 1) values else NA_real_
}

review <- readr::read_csv(
  "../output/residential_manual_review_bundle.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    component_pins = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  filter(project_kind == "tieback_building") %>%
  select(
    project_id,
    candidate_construction_year = construction_year,
    component_pins,
    distance_to_boundary_ft,
    candidate_review_categories,
    temporal_status,
    temporal_reason,
    permit_chain_evidence,
    permit_unit_evidence,
    city_footprint_evidence,
    matched_city_footprints,
    overlap_candidates
  )

history <- readr::read_csv(
  "../output/residential_review_assessor_history.csv",
  show_col_types = FALSE,
  col_types = readr::cols(
    project_id = readr::col_character(),
    pin = readr::col_character(),
    class = readr::col_character(),
    apartments_text = readr::col_character(),
    tieback_group = readr::col_character(),
    row_id = readr::col_character(),
    .default = readr::col_guess()
  )
) %>%
  semi_join(review, by = "project_id") %>%
  mutate(
    dwelling_units = parse_units(
      apartments_text,
      single_v_multi_family,
      type_of_residence
    )
  )

if (anyDuplicated(review$project_id) > 0) {
  stop("Tieback review scope is not unique by project.", call. = FALSE)
}
if (anyDuplicated(history[c("project_id", "pin", "tax_year", "card_num")]) > 0) {
  stop("Assessor review history is not unique by project-PIN-year-card.", call. = FALSE)
}
if (any(!history$project_id %in% review$project_id)) {
  stop("Assessor history contains a project outside the tieback review scope.", call. = FALSE)
}

project_year <- history %>%
  group_by(project_id, tax_year) %>%
  mutate(
    any_positive_card_proration = any(card_proration_rate > 0, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    building_key = paste0(
      "card", card_num,
      "|bldg", coalesce(as.character(building_sqft), "missing"),
      "|units", coalesce(as.character(dwelling_units), "missing"),
      "|type", coalesce(type_of_residence, "missing"),
      "|use", coalesce(single_v_multi_family, "missing")
    )
  )

pin_land <- project_year %>%
  group_by(project_id, tax_year, pin) %>%
  summarise(
    land_values = n_distinct(land_sqft[is.finite(land_sqft) & land_sqft > 0]),
    pin_land_sqft = one_finite_value(land_sqft[land_sqft > 0]),
    .groups = "drop"
  )

site_land <- pin_land %>%
  group_by(project_id, tax_year) %>%
  summarise(
    site_pins = n(),
    pins_with_land = sum(is.finite(pin_land_sqft)),
    conflicting_pin_land = sum(land_values > 1),
    site_land_sqft = sum(pin_land_sqft, na.rm = TRUE),
    site_land_complete = pins_with_land == site_pins & conflicting_pin_land == 0,
    .groups = "drop"
  )

active_cards <- project_year %>%
  group_by(project_id, tax_year, building_key) %>%
  summarise(
    project_has_positive_proration = first(any_positive_card_proration),
    signature_has_positive_proration = any(card_proration_rate > 0, na.rm = TRUE),
    card_num = first(card_num),
    source_pins = paste(sort(unique(pin)), collapse = "/"),
    source_rows = paste(sort(unique(row_id)), collapse = "/"),
    source_pin_count = n_distinct(pin),
    class_values = paste(sort(unique(class)), collapse = "/"),
    construction_year_values = n_distinct(year_built[is.finite(year_built)]),
    construction_year = one_finite_value(year_built),
    reported_years = paste(sort(unique(year_built[is.finite(year_built)])), collapse = "/"),
    building_sqft = first(building_sqft),
    dwelling_units = first(dwelling_units),
    card_proration_sum = sum(card_proration_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!project_has_positive_proration | signature_has_positive_proration) %>%
  group_by(project_id, tax_year, card_num) %>%
  mutate(
    signatures_for_card_number = n(),
    card_signature_rank = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    physical_card_id = paste0("card", card_num, "_signature", card_signature_rank),
    study_period_card = construction_year_values == 1 &
      between(construction_year, 2006L, 2022L),
    card_fields_complete = construction_year_values == 1 &
      is.finite(building_sqft) & building_sqft > 0 &
      is.finite(dwelling_units) & dwelling_units > 0
  )

episodes <- active_cards %>%
  filter(study_period_card) %>%
  group_by(project_id, tax_year, construction_year) %>%
  summarise(
    physical_cards = n(),
    repeated_across_pins = sum(source_pin_count > 1),
    card_number_conflicts = sum(signatures_for_card_number > 1),
    cards_with_complete_fields = sum(card_fields_complete),
    episode_building_sqft = sum(building_sqft, na.rm = TRUE),
    episode_dwelling_units = sum(dwelling_units, na.rm = TRUE),
    episode_component_pins = paste(
      sort(unique(unlist(str_split(source_pins, fixed("/"))))),
      collapse = "/"
    ),
    episode_source_rows = paste(source_rows, collapse = "/"),
    episode_card_evidence = paste0(
      physical_card_id,
      " pins=", source_pins,
      "; building_sqft=", coalesce(as.character(building_sqft), "missing"),
      "; units=", coalesce(as.character(dwelling_units), "missing"),
      "; card_proration_sum=", signif(card_proration_sum, 5),
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  left_join(site_land, by = c("project_id", "tax_year"), relationship = "many-to-one") %>%
  mutate(
    episode_fields_complete = cards_with_complete_fields == physical_cards &
      site_land_complete & site_land_sqft > 0
  )

snapshot_evidence <- active_cards %>%
  group_by(project_id, tax_year) %>%
  summarise(
    active_physical_cards = n(),
    active_card_numbers = n_distinct(card_num),
    card_number_conflicts = sum(signatures_for_card_number > 1),
    cards_repeated_across_pins = sum(source_pin_count > 1),
    cards_with_year_conflicts = sum(construction_year_values != 1),
    study_period_cards = sum(study_period_card),
    study_period_years = paste(
      sort(unique(construction_year[study_period_card])),
      collapse = "/"
    ),
    study_period_episodes = n_distinct(construction_year[study_period_card]),
    study_cards_with_complete_fields = sum(study_period_card & card_fields_complete),
    card_evidence = paste0(
      physical_card_id,
      " pins=", source_pins,
      "; years=", coalesce(reported_years, "missing"),
      "; building_sqft=", coalesce(as.character(building_sqft), "missing"),
      "; units=", coalesce(as.character(dwelling_units), "missing"),
      "; active_proration_sum=", signif(card_proration_sum, 5),
      collapse = " || "
    ),
    .groups = "drop"
  ) %>%
  left_join(site_land, by = c("project_id", "tax_year"), relationship = "one-to-one") %>%
  mutate(
    snapshot_usable = study_period_cards > 0 &
      cards_with_year_conflicts == 0 &
      study_cards_with_complete_fields == study_period_cards &
      site_land_complete & site_land_sqft > 0,
    snapshot_reason = case_when(
      study_period_cards == 0 ~ "no_study_period_card",
      cards_with_year_conflicts > 0 ~ "same_card_signature_has_conflicting_years",
      study_cards_with_complete_fields < study_period_cards ~ "study_card_missing_building_or_units",
      !site_land_complete | site_land_sqft <= 0 ~ "site_land_incomplete",
      card_number_conflicts > 0 ~ "same_card_number_has_distinct_buildings",
      study_period_episodes > 1 ~ "multiple_study_period_construction_episodes",
      TRUE ~ "one_complete_study_period_episode"
    )
  )

preferred_snapshots <- snapshot_evidence %>%
  filter(snapshot_usable) %>%
  mutate(
    selection_tier = case_when(
      tax_year <= 2022 ~ 1L,
      tax_year <= 2025 ~ 2L,
      TRUE ~ 3L
    )
  ) %>%
  group_by(project_id) %>%
  filter(selection_tier == min(selection_tier)) %>%
  arrange(desc(tax_year), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  transmute(
    project_id,
    selected_tax_year = tax_year,
    selection_tier,
    selected_study_period_episodes = study_period_episodes,
    selected_study_period_years = study_period_years,
    selected_active_physical_cards = active_physical_cards,
    selected_study_period_cards = study_period_cards,
    selected_card_number_conflicts = card_number_conflicts,
    selected_cards_repeated_across_pins = cards_repeated_across_pins,
    selected_site_land_sqft = site_land_sqft,
    selected_card_evidence = card_evidence,
    selected_snapshot_reason = snapshot_reason
  )

project_evidence <- review %>%
  left_join(preferred_snapshots, by = "project_id", relationship = "one-to-one") %>%
  left_join(
    episodes %>%
      inner_join(
        preferred_snapshots %>% select(project_id, selected_tax_year),
        by = c("project_id", "tax_year" = "selected_tax_year"),
        relationship = "many-to-one"
      ) %>%
      group_by(project_id) %>%
      summarise(
        selected_episode_evidence = paste0(
          construction_year,
          ": cards=", physical_cards,
          "; building_sqft=", episode_building_sqft,
          "; units=", episode_dwelling_units,
          "; land_sqft=", site_land_sqft,
          "; pins=", episode_component_pins,
          "; ", episode_card_evidence,
          collapse = " || "
        ),
        selected_all_episode_fields_complete = all(episode_fields_complete),
        .groups = "drop"
      ),
    by = "project_id",
    relationship = "one-to-one"
  ) %>%
  mutate(
    evidence_status = case_when(
      overlap_candidates > 0 ~ "commercial_overlap_review",
      is.na(selected_tax_year) ~ "no_usable_card_snapshot",
      selected_study_period_episodes > 1 ~ "multiple_construction_episodes_review",
      selected_card_number_conflicts > 0 ~ "distinct_buildings_share_card_number_review",
      TRUE ~ "one_episode_card_evidence"
    )
  ) %>%
  arrange(evidence_status, project_id)

summary <- bind_rows(
  project_evidence %>%
    count(evidence_status, name = "value") %>%
    transmute(section = "project_status", metric = evidence_status, value),
  tibble::tibble(
    section = "validation",
    metric = c(
      "tieback_projects_in_1500ft_review_scope",
      "projects_with_assessor_history",
      "projects_with_usable_card_snapshot",
      "projects_without_usable_card_snapshot",
      "duplicate_project_ids",
      "duplicate_project_year_card_rows"
    ),
    value = c(
      nrow(review),
      n_distinct(history$project_id),
      sum(!is.na(project_evidence$selected_tax_year)),
      sum(is.na(project_evidence$selected_tax_year)),
      anyDuplicated(project_evidence$project_id),
      anyDuplicated(active_cards[c("project_id", "tax_year", "physical_card_id")])
    )
  )
)

readr::write_csv(
  active_cards,
  "../output/residential_tieback_active_card_evidence.csv"
)
readr::write_csv(
  episodes,
  "../output/residential_tieback_construction_episode_evidence.csv"
)
readr::write_csv(
  snapshot_evidence,
  "../output/residential_tieback_card_snapshot_evidence.csv"
)
readr::write_csv(
  project_evidence,
  "../output/residential_tieback_card_project_evidence.csv"
)
readr::write_csv(
  summary,
  "../output/residential_tieback_card_evidence_summary.csv"
)
