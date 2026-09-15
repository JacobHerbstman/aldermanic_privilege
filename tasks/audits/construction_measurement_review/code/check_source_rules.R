# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/construction_measurement_review/code")
library(data.table)
source("../../../shared/code/assessor_classification.R")
source("../../../shared/code/save_data.R")
projects <- fread("../output/project_screen.csv")
buildings <- fread("../input/assessor_buildings.csv")
history <- fread("../input/measurement_history.csv", colClasses = c(source_row_id = "character", pin = "character", class = "character"))
condos <- fread("../input/condominium_history.csv", colClasses = c(row_id = "character", pin = "character", pin10 = "character"))
changes <- fread("../input/recorded_changes.csv")
raw_commercial <- fread("../input/commercial_valuation_data.csv", colClasses = "character")
raw_commercial[, source_row_id := as.character(.I)]
con <- DBI::dbConnect(duckdb::duckdb())
development_counts <- DBI::dbGetQuery(con, "SELECT row_id AS source_row_id, num_apartments
  FROM read_parquet('../input/residential_history.parquet') WHERE class = '297'")
DBI::dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(development_counts$source_row_id))
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(buildings$project_id),
  !anyDuplicated(changes$project_id), !anyDuplicated(history[, .(source_family, source_row_id)]),
  !anyDuplicated(condos$row_id), !anyDuplicated(condos[, .(pin, year)]))

# These helpers compare reported numbers; they do not settle physical identity.
one_value <- function(x) {
  values <- unique(x[is.finite(x)])
  if (length(values) == 1L) values else NA_real_
}
same_value <- function(x, y) (is.na(x) & is.na(y)) |
  (is.finite(x) & is.finite(y) & abs(x - y) < 0.000001)
setkey(history, source_family, source_row_id)
history_by_pin <- split(seq_len(nrow(history)), history$pin)
setkey(condos, row_id)
checks <- vector("list", nrow(projects))

# Start with each final observation, including ordinary and recorded buildings.
# Reconstruct the four numbers from the rows it identifies, using the documented
# aggregation for its source branch. An agreement here is provenance, not proof
# that the source describes the original building correctly.
for (i in seq_len(nrow(projects))) {
  p <- projects[i]
  b <- buildings[match(p$project_id, project_id)]
  c <- changes[match(p$project_id, project_id)]
  ids <- na.omit(unlist(strsplit(p$source_row_ids, "/", fixed = TRUE)))
  pins <- strsplit(p$component_pins, "/", fixed = TRUE)[[1]]
  recorded <- !is.na(c$project_id)
  completed_condo <- startsWith(p$project_id, "residential_condo_")
  route <- if (recorded) "recorded correction" else if (completed_condo) "completed condo building" else p$project_kind
  selected <- history[.(p$source_family, ids), nomatch = 0L]
  selected_condo <- condos[.(ids), nomatch = 0L]
  n_found <- nrow(selected)
  selected_years <- unique(selected$tax_year)
  expected_units <- expected_floor <- expected_land <- expected_year <- NA_real_
  complete_components <- NA
  bedroom_total_conflict <- FALSE
  earlier_area_used <- FALSE
  earlier_area_values_match <- FALSE
  earlier_area_aggregate <- FALSE
  earlier_area_units_agree <- NA
  permit_count_fill <- FALSE
  permit_year_adjustment <- FALSE

  if (completed_condo && !recorded) {
    n_found <- nrow(selected_condo)
    selected_years <- unique(selected_condo$year)
    occupied <- selected_condo[is_parking_space %in% FALSE & is_common_area %in% FALSE]
    expected_units <- nrow(occupied)
    expected_floor <- one_value(occupied$char_building_sf)
    expected_land <- one_value(occupied$char_land_sf)
    expected_year <- one_value(occupied$char_yrblt)
    complete_components <- n_found > 0L && all(!is.na(selected_condo$is_parking_space) & !is.na(selected_condo$is_common_area)) &&
      all(selected_condo$char_building_pins == n_found) &&
      all(selected_condo$char_building_non_units == n_found - nrow(occupied))
  } else if (nrow(selected)) {
    expected_year <- one_value(selected$construction_year)
    if (p$project_kind == "same_pin_multiple_cards") {
      card_units <- ifelse(selected$class %in% c("211", "212"), selected$dwelling_units, 1)
      expected_units <- sum(card_units)
      expected_floor <- sum(selected$building_sqft)
      expected_land <- one_value(selected$land_sqft)
      complete_components <- !anyDuplicated(selected[, .(pin, card_num)]) &&
        length(selected_years) == 1L && uniqueN(selected$construction_year) == 1L &&
        setequal(pins, selected$pin) && all(is.finite(card_units) & card_units > 0)
    } else if (p$project_kind == "tieback_building") {
      expected_units <- one_value(selected$dwelling_units)
      expected_floor <- one_value(selected$building_sqft)
      lots <- selected[, .(land = one_value(land_sqft), share = one_value(pin_proration_rate)), by = pin]
      expected_land <- sum(lots$land)
      complete_components <- !anyDuplicated(selected$pin) && length(selected_years) == 1L &&
        setequal(pins, selected$pin) && all(is.finite(lots$share)) && abs(sum(lots$share) - 1) <= .001
    } else {
      selected_units <- selected$dwelling_units
      if (p$project_kind == "single_pin_single_card")
        selected_units[selected$class %in% single_family_assessor_classes] <- 1
      if (p$project_kind == "class_297")
        selected_units <- development_counts$num_apartments[match(selected$source_row_id, development_counts$source_row_id)]
      expected_units <- one_value(selected_units)
      expected_floor <- one_value(selected$building_sqft)
      expected_land <- one_value(selected$land_sqft)
      complete_components <- length(selected_years) == 1L &&
        (p$source_family == "commercial" || setequal(pins, selected$pin))
    }
    if (p$source_family == "commercial") {
      raw_rows <- raw_commercial[match(selected$source_row_id, source_row_id)]
      bedroom_sum <- rowSums(as.data.frame(lapply(raw_rows[, .(studiounits, `_1brunits`, `_2brunits`, `_3brunits`, `_4brunits`)], as.numeric)), na.rm = TRUE)
      stated_total <- as.numeric(raw_rows$tot_units)
      bedroom_total_conflict <- any(bedroom_sum > 0 & is.finite(stated_total) & stated_total > 0 & bedroom_sum != stated_total)
      earlier_area_used <- (is.finite(p$building_sqft) && !same_value(p$building_sqft, expected_floor) && same_value(p$building_sqft, b$earlier_building)) ||
        (is.finite(p$land_sqft) && !same_value(p$land_sqft, expected_land) && same_value(p$land_sqft, b$earlier_land))
      earlier_area_values_match <- earlier_area_used && isTRUE(b$stable_membership) &&
        (same_value(p$building_sqft, expected_floor) || same_value(p$building_sqft, b$earlier_building)) &&
        (same_value(p$land_sqft, expected_land) || same_value(p$land_sqft, b$earlier_land))
      if (earlier_area_used) earlier_area_units_agree <- same_value(p$dwelling_units, b$earlier_units)
      if (!is.na(b$project_id) && (!is.finite(expected_land) || expected_land <= 1) && !earlier_area_used) {
        older <- history[source_family == "commercial" & source_project_id == b$project_id & tax_year < p$tax_year]
        if (nrow(older)) older <- older[tax_year == max(tax_year)]
        older_pins <- unlist(strsplit(older$component_pins, "/", fixed = TRUE))
        earlier_area_aggregate <- nrow(older) > 1 && !anyDuplicated(older_pins) && setequal(pins, older_pins) &&
          all(is.finite(older$land_sqft) & older$land_sqft > 1) && same_value(p$land_sqft, sum(older$land_sqft)) &&
          (same_value(p$building_sqft, expected_floor) || same_value(p$building_sqft, sum(older$building_sqft)))
        if (earlier_area_aggregate) earlier_area_units_agree <- same_value(p$dwelling_units, sum(older$dwelling_units))
      }
    }
    permit_count_fill <- p$project_kind == "class_297" && !is.finite(expected_units) &&
      is.finite(b$permit_units) && same_value(p$dwelling_units, b$permit_units)
    permit_year_adjustment <- isTRUE(expected_year == b$first_application_year - 1) &&
      isTRUE(p$construction_year == b$first_application_year)
  }

  # A recorded correction is a separate stated source. Check that its explicit
  # values were applied; do not claim to have re-reviewed its supporting evidence.
  recorded_fields_match <- NA
  if (recorded) {
    recorded_fields_match <- TRUE
    for (field in c("dwelling_units", "building_sqft", "land_sqft", "construction_year")) {
      if (is.finite(c[[field]])) recorded_fields_match <- recorded_fields_match && same_value(p[[field]], c[[field]])
    }
    for (field in c("units", "building_area", "land_area")) {
      variable <- c(units = "dwelling_units", building_area = "building_sqft", land_area = "land_sqft")[[field]]
      if (c[[paste0(field, "_unusable")]] %in% TRUE)
        recorded_fields_match <- recorded_fields_match && is.na(p[[variable]])
    }
  }

  # Ask how the same recorded cards change over time without requiring unchanged
  # areas or an arbitrary construction-year window. Differences are not errors:
  # replacements and later alterations must be distinguished in subsequent work.
  other_rows <- history[as.integer(unique(unlist(history_by_pin[pins], use.names = FALSE)))]
  same_assessment <- other_rows[source_family == "residential" & tax_year %in% selected_years &
    is.finite(building_sqft) & building_sqft > 0]
  selected_cards <- paste(selected$pin, selected$card_num, selected$tax_year)
  extra_cards <- same_assessment[!paste(pin, card_num, tax_year) %in% selected_cards]
  comparable_rows <- same_year_changes <- area_stable_unit_changes <- measurement_stable_year_changes <- 0L
  for (j in seq_len(nrow(selected))) {
    s <- selected[j]
    other <- other_rows[source_family == s$source_family & pin == s$pin & tax_year != s$tax_year &
      (card_num == s$card_num | (is.na(card_num) & is.na(s$card_num)))]
    if (!nrow(other)) next
    old_units <- other$dwelling_units
    selected_unit <- s$dwelling_units
    if (p$project_kind == "single_pin_single_card") {
      old_units[other$class %in% single_family_assessor_classes] <- 1
      if (s$class %in% single_family_assessor_classes) selected_unit <- 1
    } else if (p$project_kind == "same_pin_multiple_cards") {
      old_units[!other$class %in% c("211", "212")] <- 1
      if (!s$class %in% c("211", "212")) selected_unit <- 1
    }
    valid <- is.finite(old_units) & old_units > 0 & is.finite(other$building_sqft) & other$building_sqft > 0 &
      is.finite(other$land_sqft) & other$land_sqft > 0 & is.finite(other$construction_year)
    count_same <- same_value(old_units, selected_unit)
    floor_same <- same_value(other$building_sqft, s$building_sqft)
    land_same <- same_value(other$land_sqft, s$land_sqft)
    year_same <- same_value(other$construction_year, s$construction_year)
    comparable_rows <- comparable_rows + sum(valid)
    same_year_changes <- same_year_changes + sum(valid & year_same & !(count_same & floor_same & land_same))
    area_stable_unit_changes <- area_stable_unit_changes + sum(valid & floor_same & land_same & !count_same)
    measurement_stable_year_changes <- measurement_stable_year_changes + sum(valid & count_same & floor_same & land_same & !year_same)
  }
  checks[[i]] <- data.table(project_id = p$project_id, route, source_family = p$source_family,
    in_main_500ft_sample = p$in_main_500ft_sample, recorded_correction = recorded, recorded_fields_match,
    identified_source_rows = length(ids), found_source_rows = n_found,
    identified_rows_complete = length(ids) > 0L && n_found == length(ids),
    source_assessment_years = paste(sort(selected_years), collapse = "/"),
    one_assessment_year = length(selected_years) == 1L, complete_components,
    reported_units = expected_units, reported_floor_sqft = expected_floor,
    reported_land_sqft = expected_land, reported_construction_year = expected_year,
    units_match = same_value(p$dwelling_units, expected_units), floor_match = same_value(p$building_sqft, expected_floor),
    land_match = same_value(p$land_sqft, expected_land), year_match = same_value(p$construction_year, expected_year),
    earlier_area_used, earlier_area_values_match, earlier_area_aggregate, earlier_area_units_agree, permit_count_fill, permit_year_adjustment, bedroom_total_conflict,
    source_year_label_matches = length(selected_years) == 1L && isTRUE(selected_years == p$tax_year),
    source_assessment_lag = if (length(selected_years) == 1L) selected_years - p$construction_year else NA_real_,
    unselected_same_assessment_cards = nrow(unique(extra_cards[, .(pin, card_num, tax_year)])),
    unselected_card_construction_years = paste(sort(unique(extra_cards$construction_year)), collapse = "/"),
    comparable_history_rows = comparable_rows, same_year_measurement_change_rows = same_year_changes,
    same_areas_unit_change_rows = area_stable_unit_changes, same_measurements_year_change_rows = measurement_stable_year_changes)
}
checks <- rbindlist(checks)
checks[, all_four_match := identified_rows_complete & units_match & floor_match & land_match & year_match]
checks[, numbers_follow_documented_rule := recorded_fields_match %in% TRUE |
  (identified_rows_complete & (units_match | permit_count_fill) &
    (year_match | permit_year_adjustment) & ((floor_match & land_match) | earlier_area_values_match | earlier_area_aggregate))]

# Keep the full permit timeline for each recorded parcel. These are candidates
# for the same construction, not automatic proof of completion or conversion.
# No candidate is selected by the property's alderman, score or regression effect.
permits <- fread("../input/building_permits_full.csv", colClasses = "character",
  select = c("id", "permit_", "permit_type", "permit_status", "issue_date", "pin_list", "work_description"))
stopifnot(!anyDuplicated(permits$id))
permits[, issue_year := as.integer(substr(issue_date, 1, 4))]
description <- toupper(permits$work_description)
residential_text <- grepl("DWELLING|APARTMENT|RESIDENTI|SINGLE[ -]?FAMILY|\\bSFR\\b|\\bD\\.?U\\.?\\b", description)
permits[, full_new_building_text := permit_type == "PERMIT - NEW CONSTRUCTION" & residential_text &
  grepl("\\bERECT\\b|NEW CONSTRUCTION|NEW [0-9]+.?STOR", description) &
  !grepl("^.{0,70}\\bREVISION\\b|FOUNDATION ONLY|GARAGE ONLY|ALTERATION|CONVERT|ADDITION|EXISTING", description)]
permits[, conversion_or_count_change_text := residential_text &
  grepl("\\b(CONVERT|CONVERSION|CONVERTING|REDUC[A-Z]*|INCREAS[A-Z]*)\\b", description)]
pin_lists <- regmatches(permits$pin_list, gregexpr("[0-9]{10,14}", permits$pin_list))
pin_index <- unique(data.table(permit_row = rep(seq_len(nrow(permits)), lengths(pin_lists)),
  pin10 = substr(unlist(pin_lists), 1, 10)))
setkey(pin_index, pin10)
timelines <- vector("list", nrow(projects))
for (i in seq_len(nrow(projects))) {
  p <- projects[i]
  pins <- unique(substr(strsplit(p$component_pins, "/", fixed = TRUE)[[1]], 1, 10))
  rows <- unique(pin_index[.(pins), permit_row])
  candidates <- permits[rows[!is.na(rows)]]
  new <- candidates[full_new_building_text == TRUE]
  late <- candidates[conversion_or_count_change_text == TRUE & issue_year > p$construction_year]
  timelines[[i]] <- data.table(project_id = p$project_id, exact_parcel_permits = nrow(candidates),
    original_building_permit_candidates = nrow(new),
    single_complete_permit_after_assigned_year = nrow(new) == 1L &&
      isTRUE(new$permit_status == "COMPLETE") && isTRUE(new$issue_year > p$construction_year),
    original_permit_numbers = paste(sort(unique(new$permit_)), collapse = "/"),
    later_conversion_or_count_change_candidates = nrow(late),
    later_change_permit_numbers = paste(sort(unique(late$permit_)), collapse = "/"))
}
timelines <- rbindlist(timelines)
stopifnot(!anyDuplicated(timelines$project_id))
checks <- merge(checks, timelines, by = "project_id", all.x = TRUE, sort = FALSE)
setorder(checks, project_id)
SaveData(checks, "project_id", "../output/source_rule_checks.csv")
summary <- checks[, .(projects = .N, main_sample = sum(in_main_500ft_sample),
  source_rows_found = sum(identified_rows_complete), one_assessment = sum(one_assessment_year),
  all_four_match = sum(all_four_match), recorded_values_applied = sum(recorded_fields_match %in% TRUE),
  numbers_follow_rule = sum(numbers_follow_documented_rule),
  earlier_area_used = sum(earlier_area_used), earlier_area_aggregate = sum(earlier_area_aggregate),
  permit_count_fills = sum(permit_count_fill), permit_year_adjustments = sum(permit_year_adjustment),
  bedroom_total_disagreements = sum(bedroom_total_conflict),
  has_unselected_same_assessment_cards = sum(unselected_same_assessment_cards > 0),
  source_year_label_differences = sum(identified_rows_complete & !source_year_label_matches),
  comparable_histories = sum(comparable_history_rows > 0),
  same_year_measurement_changes = sum(same_year_measurement_change_rows > 0),
  same_areas_unit_changes = sum(same_areas_unit_change_rows > 0),
  same_measurements_year_changes = sum(same_measurements_year_change_rows > 0),
  has_exact_parcel_permits = sum(exact_parcel_permits > 0),
  has_original_permit_candidates = sum(original_building_permit_candidates > 0),
  single_complete_permit_after_year = sum(single_complete_permit_after_assigned_year),
  has_later_change_candidates = sum(later_conversion_or_count_change_candidates > 0)), by = route]
SaveData(summary[order(route)], "route", "../output/source_rule_summary.csv")
