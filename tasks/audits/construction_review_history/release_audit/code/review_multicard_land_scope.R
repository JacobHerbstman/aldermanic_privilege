# setwd("tasks/working_paper_release_audit/code")

source("../../setup_environment/code/packages.R")

projects <- readr::read_csv("../input/projects.csv", show_col_types = FALSE)
matches <- readr::read_csv("../input/multicard_component_successor_matches.csv", show_col_types = FALSE)
reviews <- readr::read_csv("../input/multicard_external_web_reviews.csv", show_col_types = FALSE)
adjudication <- readr::read_csv("../input/multicard_final_adjudication.csv", show_col_types = FALSE)

stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(reviews$project_id),
  !anyDuplicated(adjudication$project_id), !anyDuplicated(matches$card_id),
  !anyDuplicated(matches$successor_id))

# These are the recorded card-to-successor assignments, not every nearby parcel.
successor_totals <- matches %>%
  group_by(project_id) %>%
  summarise(
    matched_cards = n(),
    matched_card_units = sum(card_units),
    matched_card_building_sqft = sum(card_building_sqft),
    matched_successor_units = sum(successor_units),
    matched_successor_building_sqft = sum(successor_building_sqft),
    matched_successor_land_sqft = sum(successor_land_sqft),
    matched_successor_pins = paste(sort(unique(successor_pin)), collapse = "/"),
    .groups = "drop"
  )

comparison <- projects %>%
  filter(project_kind == "same_pin_multiple_cards") %>%
  select(project_id, construction_year, dwelling_units, building_sqft, land_sqft,
    distance_to_boundary_ft, within_500ft, external_multifamily) %>%
  left_join(successor_totals, by = "project_id", relationship = "one-to-one") %>%
  left_join(adjudication %>% select(project_id, target_cards, final_disposition,
    adjudication_reason, adjudication_evidence),
    by = "project_id", relationship = "one-to-one") %>%
  left_join(reviews %>% select(project_id, review_status, supports_final_units, reviewer_notes),
    by = "project_id", relationship = "one-to-one") %>%
  mutate(
    all_target_cards_matched = coalesce(matched_cards == target_cards, FALSE),
    matched_cards_reproduce_final_units = coalesce(matched_card_units == dwelling_units, FALSE),
    matched_cards_reproduce_final_building = coalesce(abs(matched_card_building_sqft - building_sqft) < 0.01, FALSE),
    parent_to_matched_successor_land_ratio = if_else(
      matched_successor_land_sqft > 0, land_sqft / matched_successor_land_sqft, NA_real_)
  ) %>%
  arrange(desc(parent_to_matched_successor_land_ratio), project_id)

# A large ratio is a review lead. Subdivision and common land can explain a gap;
# this comparison does not choose a replacement denominator or change eligibility.
stopifnot(nrow(comparison) == sum(projects$project_kind == "same_pin_multiple_cards"),
  !anyDuplicated(comparison$project_id))
readr::write_csv(comparison, "../output/multicard_land_scope.csv", na = "")
