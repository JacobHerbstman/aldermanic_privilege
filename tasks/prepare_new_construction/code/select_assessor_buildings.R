# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prepare_new_construction/code")
# first_candidate_year <- 1999
# preferred_assessment_year <- 2022
# fallback_assessment_year <- 2025
# first_construction_year <- 2006
# last_construction_year <- 2022
# earlier_commercial_year <- 2021
# permit_application_years_before <- 6
# permit_issue_years_before <- 4
# permit_years_after <- 2
# proration_tolerance <- 0.001
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/assessor_classification.R")
source("../../shared/code/permit_unit_patterns.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(first_candidate_year, preferred_assessment_year, fallback_assessment_year,
  first_construction_year, last_construction_year, earlier_commercial_year, permit_application_years_before,
  permit_issue_years_before, permit_years_after, proration_tolerance)
stopifnot(length(args) == 10L)
first_candidate_year <- as.integer(args[1])
preferred_assessment_year <- as.integer(args[2])
fallback_assessment_year <- as.integer(args[3])
first_construction_year <- as.integer(args[4])
last_construction_year <- as.integer(args[5])
earlier_commercial_year <- as.integer(args[6])
permit_application_years_before <- as.integer(args[7])
permit_issue_years_before <- as.integer(args[8])
permit_years_after <- as.integer(args[9])
proration_tolerance <- as.numeric(args[10])
stopifnot(first_candidate_year <= preferred_assessment_year,
  preferred_assessment_year <= fallback_assessment_year)

# Repeated reports must agree before a measurement describes the whole building.
one_value <- function(x) {
  values <- unique(x[is.finite(x)])
  if (length(values) == 1L) values else NA_real_
}

# Read the candidate properties and every property connected by an Assessor tie.
con <- DBI::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, sprintf("CREATE TEMP TABLE candidate_pins AS
  SELECT DISTINCT pin FROM read_parquet('../input/residential_assessor_history.parquet')
  WHERE year_built >= %d AND (building_sqft IS NOT NULL OR num_apartments IS NOT NULL)",
  first_candidate_year))
DBI::dbExecute(con, "CREATE TEMP TABLE edges AS
  SELECT DISTINCT 'pin:' || pin AS a, 'group:' || proration_key_pin AS b
  FROM read_parquet('../input/residential_assessor_history.parquet')
  WHERE proration_key_pin IS NOT NULL AND proration_key_pin != ''
  UNION SELECT DISTINCT 'pin:' || pin, 'base:' || substr(pin, 1, 10)
  FROM read_parquet('../input/residential_assessor_history.parquet') WHERE right(pin, 4) != '0000'")
DBI::dbExecute(con, "CREATE TEMP TABLE reached AS WITH RECURSIVE reached(node) AS (
  SELECT 'pin:' || pin FROM candidate_pins UNION
  SELECT CASE WHEN edges.a = reached.node THEN edges.b ELSE edges.a END
  FROM reached INNER JOIN edges ON edges.a = reached.node OR edges.b = reached.node)
  SELECT node FROM reached")
history <- DBI::dbGetQuery(con, "SELECT pin, tax_year, card_num, class, proration_key_pin,
  pin_proration_rate, card_proration_rate, pin_is_multicard, pin_num_cards, year_built, building_sqft,
  land_sqft, num_apartments, type_of_residence, single_v_multi_family, row_id, source_row_order
  FROM read_parquet('../input/residential_assessor_history.parquet')
  WHERE 'pin:' || pin IN (SELECT node FROM reached) ORDER BY source_row_order") |> as_tibble()
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(history$row_id))

history <- history |> mutate(
  report_priority = case_when(tax_year <= preferred_assessment_year ~ 1L,
    tax_year <= fallback_assessment_year ~ 2L, TRUE ~ 3L),
  single_family = str_detect(coalesce(single_v_multi_family, ""), regex("^single", TRUE)) |
    type_of_residence %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level"),
  units = case_when(is.finite(num_apartments) & num_apartments > 0 ~ num_apartments,
    single_family ~ 1, TRUE ~ NA_real_))
nonempty <- history |> filter(!is.na(building_sqft) | !is.na(num_apartments))
latest_cards <- nonempty |> arrange(pin, card_num, report_priority, desc(tax_year), desc(row_id)) |>
  distinct(pin, card_num, .keep_all = TRUE)

# Select the ordinary single-card assessment. Retain old years until the final
# construction-year decisions and sample restriction have been applied.
selected <- nonempty |> group_by(pin, card_num) |>
  filter(any(year_built >= first_candidate_year, na.rm = TRUE)) |> ungroup() |>
  arrange(pin, card_num, tax_year, row_id) |> group_by(pin, card_num, tax_year) |>
  slice_tail(n = 1) |> ungroup() |> group_by(pin) |>
  mutate(cards_in_history = n_distinct(card_num)) |> ungroup()
single <- selected |> filter(cards_in_history == 1, year_built >= first_candidate_year) |>
  arrange(pin, report_priority, desc(tax_year), desc(building_sqft), desc(year_built), desc(row_id)) |>
  distinct(pin, .keep_all = TRUE)
multiple <- selected |> filter(cards_in_history > 1, year_built >= first_candidate_year) |>
  group_by(pin) |> slice_min(year_built, with_ties = TRUE) |>
  slice_min(tax_year, with_ties = TRUE) |> slice_max(building_sqft, with_ties = FALSE) |> ungroup()
selected <- bind_rows(single, multiple) |> arrange(pin)
stopifnot(!anyDuplicated(selected$pin))
card_counts <- history |> count(pin, tax_year) |> group_by(pin) |>
  summarise(maximum_cards = max(n), .groups = "drop")
selected <- selected |> left_join(card_counts, by = "pin", relationship = "one-to-one")

# Some fractional PINs omit their common key. Link only complete, single-card
# groups whose tax shares sum to one and whose construction years agree.
fractional <- latest_cards |> filter(str_sub(pin, -4) != "0000") |> group_by(pin) |>
  summarise(base_pin = str_sub(first(pin), 1, 10), cards = n(),
    share = one_value(pin_proration_rate), year = one_value(year_built), .groups = "drop") |>
  group_by(base_pin) |> filter(n() > 1, all(cards == 1), all(is.finite(share) & share > 0 & share < 1),
    abs(sum(share) - 1) < proration_tolerance, n_distinct(year, na.rm = TRUE) == 1, any(is.finite(year))) |>
  mutate(anchor = min(pin)) |> ungroup()
ties <- history |> filter(!is.na(proration_key_pin), proration_key_pin != "")
edges <- bind_rows(ties |> transmute(pin, key = proration_key_pin),
  fractional |> transmute(pin, key = anchor)) |> distinct()
graph <- igraph::graph_from_data_frame(edges, directed = FALSE)
membership <- igraph::components(graph)$membership
lineages <- tibble(pin = names(membership), group = as.integer(membership)) |> group_by(group) |>
  mutate(project_id = paste0("residential_tieback_", min(pin))) |> ungroup() |> select(-group)
ties <- ties |> left_join(lineages, by = "pin", relationship = "many-to-one")
relevant <- ties |> filter(pin %in% selected$pin) |> distinct(project_id)
ties <- ties |> semi_join(relevant, by = "project_id")
lineages <- lineages |> semi_join(relevant, by = "project_id")

# Choose a whole assessment for a shared building: matching repeated building
# measurements, one card per parcel, all parcel shares, and each parcel's lot once.
conflicts <- ties |> group_by(project_id, pin, card_num, tax_year) |>
  summarise(conflict = any(c(n_distinct(proration_key_pin, na.rm = TRUE),
    n_distinct(year_built, na.rm = TRUE), n_distinct(building_sqft, na.rm = TRUE),
    n_distinct(land_sqft, na.rm = TRUE), n_distinct(units, na.rm = TRUE),
    n_distinct(pin_proration_rate, na.rm = TRUE)) > 1L), .groups = "drop")
ties <- ties |> arrange(project_id, pin, card_num, tax_year, desc(row_id)) |>
  distinct(project_id, pin, card_num, tax_year, .keep_all = TRUE) |>
  left_join(conflicts, by = c("project_id", "pin", "card_num", "tax_year"), relationship = "one-to-one")
parcel_reports <- ties |> group_by(project_id, tax_year, pin) |>
  summarise(cards = n_distinct(card_num), share = one_value(pin_proration_rate),
    land = one_value(land_sqft), .groups = "drop")
lot_reports <- parcel_reports |> group_by(project_id, tax_year) |>
  summarise(complete = all(cards == 1 & is.finite(share) & is.finite(land) & land > 0) &
    abs(sum(share) - 1) < proration_tolerance, land_sqft = sum(land), .groups = "drop")
snapshots <- ties |> group_by(project_id, tax_year, report_priority) |>
  summarise(construction_year = one_value(year_built), dwelling_units = one_value(units),
    building_sqft = one_value(building_sqft), component_pins = paste(sort(unique(pin)), collapse = "/"),
    source_row_ids = paste(sort(unique(row_id)), collapse = "/"),
    conflict = any(conflict), .groups = "drop") |>
  left_join(lot_reports, by = c("project_id", "tax_year"), relationship = "one-to-one") |>
  filter(complete, !conflict, is.finite(construction_year), dwelling_units > 0, building_sqft > 0) |>
  arrange(project_id, report_priority, desc(tax_year)) |> distinct(project_id, .keep_all = TRUE)

# A later independent assessment can replace a self-reference when it describes
# the same building. Historical ties that ended before construction do not merge homes.
last_ties <- ties |> group_by(pin) |> summarise(last_tied_year = max(tax_year), .groups = "drop")
lineage_dates <- lineages |> left_join(last_ties, by = "pin", relationship = "one-to-one") |>
  group_by(project_id) |> summarise(last_tied_year = max(last_tied_year, na.rm = TRUE),
    lineage_pins = n(), .groups = "drop")
selected <- selected |> left_join(lineages, by = "pin", relationship = "one-to-one") |>
  left_join(lineage_dates, by = "project_id", relationship = "many-to-one")
independent <- history |> add_count(pin, tax_year, name = "assessment_cards") |>
  filter(assessment_cards == 1, is.na(proration_key_pin), pin_proration_rate == 1,
    is.finite(building_sqft), building_sqft > 1, is.finite(land_sqft), land_sqft > 1)
for (i in which(selected$lineage_pins == 1 & selected$maximum_cards == 1 & selected$class != "297")) {
  matches <- independent |> filter(pin == selected$pin[i], tax_year > selected$tax_year[i],
    class == selected$class[i], year_built == selected$year_built[i], building_sqft == selected$building_sqft[i],
    coalesce(num_apartments, -1) == coalesce(selected$num_apartments[i], -1)) |>
    arrange(desc(tax_year == fallback_assessment_year), tax_year)
  if (nrow(matches) == 0) next
  selected[i, c("tax_year", "land_sqft", "row_id", "pin_proration_rate", "proration_key_pin")] <-
    matches[1, c("tax_year", "land_sqft", "row_id", "pin_proration_rate", "proration_key_pin")]
}
independent_lineages <- selected |> filter(!is.na(project_id)) |> group_by(project_id) |>
  filter(all((year_built > last_tied_year | lineage_pins == 1) & between(year_built, first_construction_year, last_construction_year) &
    is.na(proration_key_pin) & pin_proration_rate == 1 & maximum_cards == 1)) |>
  ungroup() |> distinct(project_id)
selected$project_id[selected$project_id %in% independent_lineages$project_id] <- NA_character_
snapshots <- snapshots |> anti_join(independent_lineages, by = "project_id")

# Fill a missing floor area from a later complete report only for the same
# parcel, construction year, unit count, and land area.
later <- history |> add_count(pin, tax_year, name = "assessment_cards") |>
  filter(assessment_cards == 1) |> mutate(comparable_units = if_else(class %in% single_family_assessor_classes, 1, num_apartments))
for (i in which(is.na(selected$project_id) & selected$maximum_cards == 1 &
  (!is.finite(selected$building_sqft) | selected$building_sqft <= 0))) {
  count <- if (selected$class[i] %in% single_family_assessor_classes) 1 else selected$units[i]
  matches <- later |> filter(pin == selected$pin[i], tax_year > selected$tax_year[i],
    year_built == selected$year_built[i], comparable_units == count, land_sqft == selected$land_sqft[i],
    is.finite(building_sqft), building_sqft > 0) |> arrange(desc(tax_year == fallback_assessment_year), tax_year)
  if (nrow(matches) == 0) next
  selected$building_sqft[i] <- matches$building_sqft[1]
  selected$row_id[i] <- matches$row_id[1]
}

single <- selected |> filter(is.na(project_id), (maximum_cards == 1 & !pin_is_multicard) | class == "297") |>
  transmute(project_id = paste0("residential_", pin), source_family = "residential",
    project_kind = if_else(class == "297", "class_297", "single_pin_single_card"),
    component_pins = pin, construction_year = year_built,
    dwelling_units = case_when(class == "297" ~ num_apartments,
      class %in% single_family_assessor_classes ~ 1, TRUE ~ units),
    building_sqft, land_sqft, class_values = class, source_row_ids = row_id, tax_year)
shared <- snapshots |> transmute(project_id, source_family = "residential", project_kind = "tieback_building",
  component_pins, construction_year, dwelling_units, building_sqft, land_sqft,
  source_row_ids, tax_year) |> left_join(selected |> filter(!is.na(project_id)) |> group_by(project_id) |>
    summarise(class_values = paste(sort(unique(class)), collapse = "/"), .groups = "drop"),
    by = "project_id", relationship = "one-to-one")

# Keep an incomplete shared record available for a reviewed replacement or a
# match to a completed building. Missing measurements never enter density.
incomplete_shared <- lineages |> anti_join(shared, by = "project_id") |>
  anti_join(independent_lineages, by = "project_id") |>
  group_by(project_id) |> summarise(component_pins = paste(sort(unique(pin)), collapse = "/"), .groups = "drop") |>
  left_join(selected |> filter(!is.na(project_id)) |> group_by(project_id) |>
    summarise(construction_year = one_value(year_built),
      dwelling_units = if (all(is.finite(num_apartments) & num_apartments > 0) &&
        n_distinct(year_built) == 1) one_value(num_apartments) else NA_real_,
      class_values = paste(sort(unique(class)), collapse = "/"), .groups = "drop"),
    by = "project_id", relationship = "one-to-one") |>
  mutate(source_family = "residential", project_kind = "tieback_building")
shared <- bind_rows(shared, incomplete_shared)

# Multiple cards contribute only when one assessment contains the whole episode.
multi_pins <- selected |> filter(is.na(project_id), class != "297", maximum_cards > 1 | pin_is_multicard) |> select(pin)
episodes <- latest_cards |> semi_join(multi_pins, by = "pin") |> filter(between(year_built, first_construction_year, last_construction_year)) |>
  select(pin, card_num, selected_year = year_built)
episode_sizes <- episodes |> count(pin, selected_year, name = "episode_cards")
complete <- nonempty |> inner_join(episode_sizes, by = c("pin", "year_built" = "selected_year"), relationship = "many-to-one") |>
  left_join(episodes, by = c("pin", "card_num"), relationship = "many-to-one") |>
  mutate(card_units = if_else(class %in% c("211", "212"), num_apartments, 1)) |>
  group_by(pin, year_built, tax_year, report_priority) |>
  filter(n() == first(episode_cards), all(!is.na(selected_year) & selected_year == year_built),
    all(card_units > 0 & is.finite(card_units) & building_sqft > 0 & is.finite(building_sqft) &
      land_sqft > 0 & is.finite(land_sqft)), n_distinct(land_sqft) == 1) |> ungroup()
complete_years <- complete |> distinct(pin, year_built, tax_year, report_priority) |>
  arrange(pin, year_built, report_priority, desc(tax_year)) |> distinct(pin, year_built, .keep_all = TRUE)
multi <- complete |> semi_join(complete_years, by = c("pin", "year_built", "tax_year")) |>
  group_by(pin) |> summarise(project_id = paste0("residential_multicard_", first(pin)),
    source_family = "residential", project_kind = "same_pin_multiple_cards", component_pins = first(pin),
    construction_year = one_value(year_built), dwelling_units = sum(card_units),
    building_sqft = sum(building_sqft), land_sqft = one_value(land_sqft), tax_year = one_value(tax_year),
    class_values = paste(sort(unique(class)), collapse = "/"),
    source_row_ids = paste(sort(unique(row_id)), collapse = "/"), .groups = "drop") |> select(-pin)
# Commercial reports identify a site with a key PIN and a list of component PINs.
# Parse the recorded lists, including the source's explicit parcel ranges.
parse_pins <- function(keypin, text) {
  text <- coalesce(text, "")
  full <- str_extract_all(text, "(?<![0-9])[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{4}(?![0-9])")[[1]]
  short <- str_extract_all(text, "(?<![0-9])[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{3}(?![0-9])")[[1]]
  pins <- c(str_remove_all(full, "-"), paste0(str_remove_all(short, "-"), "0"))
  repair <- length(short) > 0
  if (str_detect(text, regex("\\bthru\\b", TRUE)) && length(pins) >= 2) {
    a <- pins[1]; b <- tail(pins, 1)
    start <- as.integer(str_sub(a, 8, 10)); end <- as.integer(str_sub(b, 8, 10))
    if (str_sub(a, 1, 7) == str_sub(b, 1, 7) && str_sub(a, 11) == str_sub(b, 11) &&
      end >= start && end - start <= 500) {
      pins <- c(pins, paste0(str_sub(a, 1, 7), str_pad(seq(start, end), 3, pad = "0"), str_sub(a, 11)))
      repair <- TRUE
    }
  }
  residue <- text |> str_remove_all("[0-9]{2}-[0-9]{2}-[0-9]{3}-[0-9]{3}-[0-9]{3,4}") |>
    str_remove_all("[^0-9]")
  tibble(pin = sort(unique(c(keypin, pins[nchar(pins) == 14]))), parser_repair = repair,
    parser_residue = residue != "")
}
commercial_rows <- read_csv("../input/commercial_valuation_data.csv", col_types = cols(.default = "c")) |>
  janitor::clean_names() |> mutate(raw_row = row_number(), keypin = str_remove_all(keypin, "[^0-9]"),
    across(c(year, yearbuilt, studiounits, x1brunits, x2brunits, x3brunits, x4brunits,
      tot_units, bldgsf, landsf), ~ suppressWarnings(as.numeric(str_remove_all(.x, "[^0-9.\\-]"))))) |>
  mutate(apartment_units = rowSums(pick(studiounits, x1brunits, x2brunits, x3brunits, x4brunits), na.rm = TRUE),
    units = case_when(apartment_units > 0 ~ apartment_units, tot_units > 0 ~ tot_units, TRUE ~ NA_real_)) |>
  filter(township %in% c("West Chicago", "South Chicago", "Jefferson", "North Chicago",
    "Lake View", "Rogers Park", "Hyde Park", "Lake"),
    str_detect(sheet, regex("Multifamily|Class3|Class9|Condos", TRUE)), nchar(keypin) == 14, is.finite(units))
commercial_components <- map_dfr(seq_len(nrow(commercial_rows)), function(i) {
  parse_pins(commercial_rows$keypin[i], commercial_rows$pins[i]) |> mutate(raw_row = commercial_rows$raw_row[i])
})
graph <- igraph::graph_from_data_frame(commercial_components |>
  transmute(a = paste0("row:", raw_row), b = paste0("pin:", pin)), directed = FALSE)
membership <- igraph::components(graph)$membership
row_groups <- tibble(node = names(membership), group = as.integer(membership)) |>
  filter(str_starts(node, "row:")) |> transmute(raw_row = as.integer(str_remove(node, "^row:")), group)
commercial_rows <- commercial_rows |> left_join(row_groups, by = "raw_row", relationship = "one-to-one") |>
  group_by(group) |> filter(any(yearbuilt >= first_candidate_year, na.rm = TRUE)) |>
  mutate(project_id = paste0("commercial_", min(keypin))) |> ungroup()
commercial_components <- commercial_components |> inner_join(commercial_rows |> select(raw_row, project_id, year),
  by = "raw_row", relationship = "many-to-one")
commercial_parcels <- commercial_components |> group_by(project_id, year) |>
  summarise(component_pins = paste(sort(unique(pin)), collapse = "/"),
    parser_repair = any(parser_repair), parser_residue = any(parser_residue), .groups = "drop")
commercial_assessments <- commercial_rows |> group_by(project_id, tax_year = year) |>
  summarise(construction_year = one_value(yearbuilt), dwelling_units = one_value(units),
    building_sqft = one_value(bldgsf), land_sqft = one_value(landsf),
    source_row_ids = paste(sort(unique(raw_row)), collapse = "/"),
    source_addresses = paste(sort(unique(address)), collapse = " / "),
    student_housing = any(str_detect(coalesce(property_type_use, ""), regex("student housing", TRUE))),
    conflicting_reports = any(c(n_distinct(keypin), n_distinct(yearbuilt, na.rm = TRUE),
      n_distinct(units, na.rm = TRUE), n_distinct(bldgsf, na.rm = TRUE), n_distinct(landsf, na.rm = TRUE)) > 1L),
    .groups = "drop") |>
  left_join(commercial_parcels, by = c("project_id", "tax_year" = "year"), relationship = "one-to-one")
earlier <- commercial_assessments |> filter(tax_year == earlier_commercial_year) |>
  select(project_id, earlier_pins = component_pins, earlier_building = building_sqft,
    earlier_land = land_sqft, earlier_units = dwelling_units, earlier_rows = source_row_ids)
commercial <- commercial_assessments |> arrange(project_id, desc(tax_year)) |>
  distinct(project_id, .keep_all = TRUE) |>
  left_join(earlier, by = "project_id", relationship = "one-to-one") |>
  mutate(stable_membership = is.na(earlier_pins) | component_pins == earlier_pins,
    building_from_earlier = stable_membership & (!is.finite(building_sqft) | building_sqft <= 0) & earlier_building > 0,
    land_from_earlier = stable_membership & (!is.finite(land_sqft) | land_sqft <= 0) & earlier_land > 0,
    building_sqft = if_else(coalesce(building_from_earlier, FALSE), earlier_building, building_sqft),
    land_sqft = if_else(coalesce(land_from_earlier, FALSE), earlier_land, land_sqft),
    source_family = "commercial", project_kind = "commercial_entity_family", class_values = NA_character_)

# A later site record can omit land while its earlier building records report
# all of the separate lots. Add those reported lots only when their PIN lists
# are disjoint and exactly cover the selected property.
commercial$source_allows_far <- TRUE
for (i in which(!is.finite(commercial$land_sqft) | commercial$land_sqft <= 1)) {
  earlier <- commercial_rows |> filter(project_id == commercial$project_id[i], year == earlier_commercial_year)
  pins <- str_remove_all(unlist(strsplit(earlier$pins, ",")), "[^0-9]")
  complete <- nrow(earlier) > 0 && !anyDuplicated(pins) &&
    setequal(pins, strsplit(commercial$component_pins[i], "/", fixed = TRUE)[[1]]) &&
    all(is.finite(earlier$landsf) & earlier$landsf > 1)
  if (!complete) next
  commercial$land_sqft[i] <- sum(earlier$landsf)
  if (nrow(earlier) > 1) {
    if (all(is.finite(earlier$bldgsf) & earlier$bldgsf > 1) && all(is.finite(earlier$units)) &&
        isTRUE(sum(earlier$units) == commercial$dwelling_units[i])) {
      commercial$building_sqft[i] <- sum(earlier$bldgsf)
    } else if (!isTRUE(sum(earlier$bldgsf) == commercial$building_sqft[i])) {
      commercial$source_allows_far[i] <- FALSE
    }
  }
}

# Permit revisions identify one application history. Keep all issued-date records
# in the recorded source, including older applications and later reinstatements.
permits <- read_csv("../input/building_permits_full.csv", col_types = cols(id = "c", permit_ = "c",
    permit_type = "c", permit_status = "c", application_start_date = "c", issue_date = "c", work_description = "c",
  pin_list = "c", latitude = "d", longitude = "d", xcoordinate = "d", ycoordinate = "d",
  processing_time = "d", .default = col_skip())) |>
  rename(permit_number = permit_, pin_text = pin_list) |>
  mutate(application_date = as.Date(substr(application_start_date, 1, 10)),
    issue_date = as.Date(substr(issue_date, 1, 10)))
missing_coordinates <- which((is.na(permits$longitude) | is.na(permits$latitude)) &
  is.finite(permits$xcoordinate) & is.finite(permits$ycoordinate))
converted <- st_as_sf(permits[missing_coordinates, ], coords = c("xcoordinate", "ycoordinate"), crs = 3435) |>
  st_transform(4326) |> st_coordinates()
permits$longitude[missing_coordinates] <- converted[,1]
permits$latitude[missing_coordinates] <- converted[,2]
permits <- permits |> filter(between(latitude, 41, 43), between(longitude, -89, -87),
  !(abs(latitude - 42.00853640087) < 1e-6 & abs(longitude + 87.91442843927) < 1e-6),
  processing_time >= 0, !is.na(application_date), !is.na(issue_date)) |>
  mutate(references = map2(str_extract_all(coalesce(work_description, ""), "(?<![0-9])10[0-9]{7}(?![0-9])"),
    permit_number, ~ setdiff(unique(.x), .y)))
stopifnot(!anyDuplicated(permits$id), !anyDuplicated(permits$permit_number))
# Preserve the source fields needed for the own-project permit exclusion check.
permit_locations <- permits |> transmute(id, permit = permit_number, pin = pin_text,
  permit_type, permit_status, application_start_date = application_date, issue_date,
  work_description, longitude, latitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |> st_transform(3435)
SaveData(permit_locations, "id", "../output/building_permits_for_verification.gpkg",
  layer = "building_permits_clean", delete_dsn = TRUE, quiet = TRUE)

references <- permits |> select(permit_number, references) |> unnest_longer(references) |>
  filter(references %in% permits$permit_number)
graph <- igraph::graph_from_data_frame(references, vertices = permits$permit_number, directed = FALSE)
membership <- igraph::components(graph)$membership
chains <- tibble(permit_number = names(membership), chain = as.integer(membership)) |>
  left_join(permits |> select(permit_number, permit_type), by = "permit_number", relationship = "one-to-one") |>
  group_by(chain) |> mutate(permit_chain_id = paste0("permit_chain_", min(permit_number[
    permit_type == "PERMIT - NEW CONSTRUCTION" | !any(permit_type == "PERMIT - NEW CONSTRUCTION")]))) |>
  ungroup() |> select(permit_number, permit_chain_id)
permits <- permits |> left_join(chains, by = "permit_number", relationship = "one-to-one")
permit_pin_rows <- permits |> filter(permit_type == "PERMIT - NEW CONSTRUCTION") |>
  separate_longer_delim(pin_text, delim = "|") |> mutate(pin10 = str_remove_all(pin_text, "[^0-9]")) |>
  filter(nchar(pin10) == 10) |> distinct(permit_number, pin10, .keep_all = TRUE)
components <- bind_rows(selected |> filter(!pin %in% ties$pin | class == "297" | maximum_cards > 1) |>
    transmute(project_id = paste0("residential_", pin), pin, first_year = year_built, last_year = year_built),
  selected |> filter(!is.na(project_id), class != "297") |>
    transmute(project_id, pin, first_year = year_built, last_year = year_built),
  commercial_components |> group_by(project_id, pin) |>
    summarise(first_year = min(commercial_rows$yearbuilt[commercial_rows$project_id == first(project_id)], na.rm = TRUE),
      last_year = max(commercial_rows$yearbuilt[commercial_rows$project_id == first(project_id)], na.rm = TRUE), .groups = "drop"))
by_pin <- split(permit_pin_rows, permit_pin_rows$pin10)
exact_links <- map_dfr(seq_len(nrow(components)), function(i) {
  matched <- by_pin[[substr(components$pin[i], 1, 10)]]
  if (is.null(matched)) return(tibble())
  matched |> filter(between(lubridate::year(application_date), components$first_year[i] - permit_application_years_before, components$last_year[i] + permit_years_after),
    between(lubridate::year(issue_date), components$first_year[i] - permit_issue_years_before, components$last_year[i] + permit_years_after)) |>
    transmute(project_id = components$project_id[i], permit_number, permit_chain_id)
}) |> distinct()
# A new-building permit inside the historical property is also direct evidence.
# Match recorded parcels first; a missing old PIN needs a unique old polygon
# containing that exact PIN's nearest recorded coordinate.
initial_parcels <- st_read("../input/historical_project_parcel_source.gpkg", quiet = TRUE) |>
  st_transform(3435) |> group_by(target_year, pin14) |> summarise(.groups = "drop")
initial_predecessors <- st_read("../input/historical_predecessor_parcel_source.gpkg", quiet = TRUE) |> st_transform(3435)
reference_history <- read_csv("../input/predecessor_parcel_history.csv", col_types = cols(
  pin = "c", year = "i", x_3435 = "d", y_3435 = "d", .default = col_skip())) |>
  filter(is.finite(x_3435), is.finite(y_3435))
requests <- components |> filter(is.finite(first_year), is.finite(last_year),
  first_year <= last_construction_year, last_year >= first_construction_year) |>
  mutate(target_year = Map(seq.int, pmax(first_year, first_construction_year), pmin(last_year, last_construction_year))) |>
  unnest_longer(target_year) |> distinct(project_id, pin, target_year)
parcel_keys <- paste(initial_parcels$target_year, initial_parcels$pin14)
matched <- match(paste(requests$target_year, requests$pin), parcel_keys)
base_keys <- paste(initial_parcels$target_year, substr(initial_parcels$pin14, 1, 10))
unique_bases <- !duplicated(base_keys) & !duplicated(base_keys, fromLast = TRUE)
base_match <- match(paste(requests$target_year, substr(requests$pin, 1, 10)), base_keys[unique_bases])
matched[is.na(matched)] <- which(unique_bases)[base_match[is.na(matched)]]
component_geometry <- st_sf(requests[!is.na(matched), ], geometry = st_geometry(initial_parcels)[na.omit(matched)])
missing <- requests[is.na(matched), ]
reference_by_pin <- split(seq_len(nrow(reference_history)), reference_history$pin)
reference_rows <- vapply(seq_len(nrow(missing)), function(i) {
  rows <- reference_by_pin[[missing$pin[i]]]
  if (!length(rows)) return(NA_integer_)
  years <- reference_history$year[rows]
  rows[order(abs(years - missing$target_year[i]), years > missing$target_year[i], -years)[1]]
}, integer(1))
available <- which(!is.na(reference_rows))
points <- st_as_sf(missing[available, ] |> mutate(
  x = reference_history$x_3435[reference_rows[available]],
  y = reference_history$y_3435[reference_rows[available]]), coords = c("x", "y"), crs = 3435)
recovered <- list()
for (year in sort(unique(points$target_year))) {
  year_points <- points |> filter(target_year == year)
  shapes <- initial_predecessors |> filter(target_year == year)
  hits <- st_intersects(year_points, shapes)
  unique <- which(lengths(hits) == 1)
  recovered[[as.character(year)]] <- st_sf(st_drop_geometry(year_points[unique, ]),
    geometry = st_geometry(shapes)[unlist(hits[unique])])
}
component_geometry <- bind_rows(component_geometry, bind_rows(recovered))
complete <- requests |> count(project_id, target_year, name = "requested") |>
  inner_join(st_drop_geometry(component_geometry) |> count(project_id, target_year, name = "found"),
    by = c("project_id", "target_year"), relationship = "one-to-one") |> filter(requested == found)
sites <- component_geometry |> semi_join(complete, by = c("project_id", "target_year")) |>
  group_by(project_id, target_year) |> summarise(.groups = "drop")
new_permit_points <- permits |> filter(permit_type == "PERMIT - NEW CONSTRUCTION") |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
hits <- st_intersects(sites, new_permit_points)
spatial_links <- tibble(site_row = rep(seq_len(nrow(sites)), lengths(hits)), permit_row = unlist(hits)) |>
  transmute(project_id = sites$project_id[site_row], target_year = sites$target_year[site_row],
    permit_number = new_permit_points$permit_number[permit_row],
    permit_chain_id = new_permit_points$permit_chain_id[permit_row],
    application_year = lubridate::year(new_permit_points$application_date[permit_row]),
    issue_year = lubridate::year(new_permit_points$issue_date[permit_row])) |>
  filter(between(target_year - application_year, -permit_years_after, permit_application_years_before),
    between(target_year - issue_year, -permit_years_after, permit_issue_years_before)) |>
  distinct(project_id, permit_number, permit_chain_id)
direct_links <- bind_rows(exact_links, spatial_links) |> distinct()

new_chains <- permits |> filter(permit_type == "PERMIT - NEW CONSTRUCTION") |>
  group_by(permit_chain_id) |> summarise(first_application_year = min(lubridate::year(application_date)), .groups = "drop")
permit_years <- exact_links |> distinct(project_id, permit_chain_id) |>
  left_join(new_chains, by = "permit_chain_id", relationship = "many-to-one") |>
  group_by(project_id) |> filter(n() == 1) |> ungroup() |>
  select(project_id, permit_chain_id, first_application_year)
single <- single |> left_join(permit_years, by = "project_id", relationship = "one-to-one") |>
  mutate(construction_year = if_else(project_kind != "class_297" & coalesce(construction_year == first_application_year - 1, FALSE),
    first_application_year, construction_year))

# Residential and commercial reports can describe the same property. Keep
# that source overlap explicit until the common correction and selection step.
commercial_keys <- commercial_rows |> filter(yearbuilt >= first_candidate_year) |> distinct(keypin)
overlap <- selected |> filter(is.na(project_id), pin %in% commercial_keys$keypin) |>
  transmute(project_id = paste0("residential_", pin), source_family = "residential",
    project_kind = "residential_commercial_overlap", component_pins = pin,
    construction_year = year_built, dwelling_units = units, building_sqft, land_sqft,
    class_values = class, source_row_ids = row_id, tax_year) |>
  left_join(permit_years, by = "project_id", relationship = "one-to-one") |>
  mutate(construction_year = if_else(coalesce(construction_year == first_application_year - 1, FALSE),
    first_application_year, construction_year), project_id = paste0("residential_overlap_", component_pins))
single <- single |> filter(!component_pins %in% commercial_keys$keypin)
multi <- multi |> filter(!component_pins %in% commercial_keys$keypin)

# Development records sharing a permit describe one candidate building. The
# Assessor count takes priority; a single unambiguous permit count can fill it.
unit_pattern <- paste0("\\b[0-9]{1,4}\\)?\\s*(?:TOTAL\\s+)?(?:DWELLING\\s+|RESIDENTI?AL\\s+|APARTMENT\\s+|EFFICIENCY\\s+)*(?:UNITS?|D\\.?U\\.?)\\b|",
  attached_house_count_pattern)
unit_mentions <- permits |> transmute(permit_number, permit_chain_id,
  unit_count = str_extract_all(str_to_upper(coalesce(work_description, "")), unit_pattern)) |>
  unnest_longer(unit_count) |> mutate(unit_count = as.numeric(str_extract(unit_count, "[0-9]{1,4}")))
chain_units <- unit_mentions |> group_by(permit_chain_id) |>
  summarise(permit_units = one_value(unit_count), unit_values = n_distinct(unit_count), .groups = "drop")
development <- single |> filter(project_kind == "class_297")
development_links <- direct_links |> filter(project_id %in% development$project_id) |>
  distinct(project_id, permit_chain_id)
graph <- igraph::graph_from_data_frame(development_links, directed = FALSE,
  vertices = unique(c(development$project_id, development_links$permit_chain_id)))
membership <- igraph::components(graph)$membership
development_groups <- tibble(project_id = names(membership), group = as.integer(membership)) |>
  filter(project_id %in% development$project_id) |> group_by(group) |>
  mutate(final_id = paste0("residential_297_group_", min(str_remove(project_id, "^residential_")))) |> ungroup()
development <- development |> left_join(development_groups, by = "project_id", relationship = "one-to-one") |>
  group_by(final_id) |> summarise(project_id = first(final_id), source_family = "residential", project_kind = "class_297",
    component_pins = paste(sort(component_pins), collapse = "/"),
    construction_year = one_value(construction_year), dwelling_units = one_value(dwelling_units),
    building_sqft = if (n() == 1) first(building_sqft) else NA_real_,
    land_sqft = if (n() == 1) first(land_sqft) else NA_real_,
    source_row_ids = paste(sort(source_row_ids), collapse = "/"), class_values = "297", tax_year = one_value(tax_year),
    component_count = n(), .groups = "drop") |> select(-final_id)
development_counts <- development_links |> left_join(development_groups |> select(project_id, final_id),
  by = "project_id", relationship = "many-to-one") |> distinct(final_id, permit_chain_id) |>
  left_join(chain_units, by = "permit_chain_id", relationship = "many-to-one") |>
  group_by(project_id = final_id) |> summarise(permit_chains = n(), permit_units = one_value(permit_units),
    conflicting_permit_counts = n_distinct(permit_units, na.rm = TRUE) > 1, .groups = "drop")
development <- development |> left_join(development_counts, by = "project_id", relationship = "one-to-one") |>
  mutate(dwelling_units = if_else(component_count == 1 & permit_chains %in% 1,
    coalesce(dwelling_units, permit_units), dwelling_units))

permit_evidence <- direct_links |> group_by(permit_chain_id) |>
  summarise(project_ids = paste(sort(unique(project_id)), collapse = "/"), .groups = "drop") |>
  inner_join(permits |> select(permit_chain_id, permit_number, permit_type, permit_status,
    application_date, issue_date, work_description, longitude, latitude),
    by = "permit_chain_id", relationship = "one-to-many") |>
  left_join(chain_units, by = "permit_chain_id", relationship = "many-to-one") |>
  mutate(direct_project_ids = vapply(permit_number, function(number)
    paste(sort(unique(exact_links$project_id[exact_links$permit_number == number])), collapse = "/"), character(1)))
SaveData(permit_evidence, "permit_number", "../output/building_permit_evidence.csv")

buildings <- bind_rows(single |> filter(project_kind != "class_297"), shared, multi, commercial, development, overlap) |>
  arrange(source_family, project_id)
stopifnot(!anyDuplicated(buildings$project_id))
SaveData(buildings, "project_id", "../output/assessor_buildings.csv")

# Retain the ordinary measurement history for the building-identity checks.
records <- bind_rows(history |> transmute(source_family = "residential", source_row_id = row_id,
    source_project_id = paste0("residential_", pin),
    pin, tax_year, card_num, class, pin_num_cards, proration_key_pin, pin_proration_rate,
    construction_year = year_built, dwelling_units = units,
    building_sqft, land_sqft, component_pins = pin),
  commercial_rows |> transmute(source_family = "commercial", source_row_id = as.character(raw_row), source_project_id = project_id,
    pin = keypin, tax_year = year, card_num = NA_integer_, class = NA_character_,
    construction_year = yearbuilt, dwelling_units = units, building_sqft = bldgsf, land_sqft = landsf,
    component_pins = map2_chr(keypin, pins, ~ paste(parse_pins(.x, .y)$pin, collapse = "/"))))
SaveData(records, c("source_family", "source_row_id"), "../output/assessor_measurement_records.csv")
