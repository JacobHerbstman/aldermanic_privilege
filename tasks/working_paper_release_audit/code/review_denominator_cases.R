# setwd("tasks/working_paper_release_audit/code")

source("../../setup_environment/code/packages.R")

cases <- readr::read_csv("../reference/denominator_cases.csv", show_col_types = FALSE) %>%
  mutate(pin = sub("residential_multicard_", "", project_id))
projects <- readr::read_csv("../input/projects.csv", show_col_types = FALSE)
matches <- readr::read_csv("../reference/multicard_component_successor_matches.csv",
  col_types = cols(.default = col_guess(), successor_pin = col_character()))
members <- readr::read_csv("../reference/denominator_current_parcel_evidence.csv",
  col_types = cols(.default = col_guess(), current_pin = col_character()))
stopifnot(!anyDuplicated(cases$project_id), !anyDuplicated(projects$project_id),
  !anyDuplicated(members[c("project_id", "current_pin")]),
  !anyDuplicated(matches$successor_id))

# Confirm the selected historical card rows against the pinned full Assessor source.
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "review_cases", as.data.frame(cases))
cards <- DBI::dbGetQuery(con, "
  SELECT r.* FROM read_csv('../input/residential_improvement_characteristics_full.csv',
    all_varchar=true) r INNER JOIN review_cases c ON r.pin = c.pin
    AND try_cast(r.year AS INTEGER) = c.assessment_year
")
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(cards$row_id), setequal(cards$pin, cases$pin))
card_totals <- cards %>%
  mutate(char_bldg_sf = as.numeric(char_bldg_sf), char_land_sf = as.numeric(char_land_sf)) %>%
  group_by(pin) %>%
  summarise(source_cards = n(), source_building_sqft = sum(char_bldg_sf),
    source_land_values = n_distinct(char_land_sf), source_land_sqft = first(char_land_sf),
    source_built_years = paste(sort(unique(char_yrblt)), collapse = "/"), .groups = "drop")

parcels <- sf::st_read("../input/preferred_predecessor_parcel_source.gpkg", quiet = TRUE) %>%
  inner_join(cases, by = c("target_year" = "assessment_year", "predecessor_pin14" = "pin"),
    relationship = "many-to-one") %>%
  sf::st_transform(3435)
stopifnot(nrow(parcels) == nrow(cases), !anyDuplicated(parcels$project_id),
  all(sf::st_is_valid(parcels)))

members <- members %>%
  left_join(matches %>% select(project_id, current_pin = successor_pin) %>% mutate(card_match = TRUE),
    by = c("project_id", "current_pin"), relationship = "one-to-one") %>%
  mutate(card_match = coalesce(card_match, FALSE),
    individual_project_id = paste0("residential_", current_pin),
    retained_individually = individual_project_id %in% projects$project_id,
    inside_assessment_year_parent = FALSE)
for (id in cases$project_id) {
  rows <- which(members$project_id == id)
  points <- sf::st_as_sf(members[rows, ], coords = c("current_x_3435", "current_y_3435"), crs = 3435)
  members$inside_assessment_year_parent[rows] <-
    lengths(sf::st_within(points, parcels[parcels$project_id == id, ])) > 0
}
member_totals <- members %>%
  filter(inside_assessment_year_parent) %>%
  group_by(project_id) %>%
  summarise(current_parcel_points = n(), matched_successors = sum(card_match),
    matched_successor_land_sqft = sum(latest_assessor_land_sqft[card_match]),
    other_reported_units = sum(latest_assessor_units[!card_match], na.rm = TRUE),
    other_individually_retained_projects = sum(!card_match & retained_individually),
    parcels_with_ca_address = sum(grepl(" CA$", current_address)), .groups = "drop")

review <- cases %>%
  left_join(projects %>% select(project_id, dwelling_units, building_sqft, land_sqft),
    by = "project_id", relationship = "one-to-one") %>%
  left_join(card_totals, by = "pin", relationship = "one-to-one") %>%
  left_join(sf::st_drop_geometry(parcels) %>% select(project_id, object_id) %>%
    mutate(parent_polygon_sqft = as.numeric(sf::st_area(parcels))),
    by = "project_id", relationship = "one-to-one") %>%
  left_join(member_totals, by = "project_id", relationship = "one-to-one")
stopifnot(nrow(review) == 4, all(review$source_cards == review$dwelling_units),
  all(review$source_building_sqft == review$building_sqft),
  all(review$source_land_values == 1), all(review$source_land_sqft == review$land_sqft))
readr::write_csv(review, "../output/denominator_case_review.csv")
readr::write_csv(members, "../output/denominator_case_members.csv")

pdf("../output/denominator_case_maps.pdf", width = 9, height = 7)
for (id in cases$project_id) {
  polygon <- parcels[parcels$project_id == id, ]
  points <- members %>% filter(project_id == id) %>%
    sf::st_as_sf(coords = c("current_x_3435", "current_y_3435"), crs = 3435)
  plot(sf::st_geometry(polygon), col = "#eeeeee", border = "#333333",
    main = paste(polygon$label, "-", polygon$target_year, "parcel"), axes = TRUE)
  plot(sf::st_geometry(points), add = TRUE, pch = 19,
    col = ifelse(points$card_match, "#b43129", "#2876a5"), cex = .8)
  legend("topright", c("Matched home", "Other parcel point"),
    col = c("#b43129", "#2876a5"), pch = 19, bg = "white", cex = .8)
  mtext("Historical parent boundary and recorded 2025 parcel points; not a surveyed land allocation.", side = 1, line = 3, cex = .7)
}
dev.off()
