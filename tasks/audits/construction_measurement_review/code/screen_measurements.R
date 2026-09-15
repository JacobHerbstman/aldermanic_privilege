# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/construction_measurement_review/code")
# first_year <- 2006
# last_year <- 2022
# main_window_ft <- 500
# nearby_ft <- 1000
# year_gap <- 2
# repeated_units_min <- 8
# repeated_land_min <- 20000
# small_sqft_per_unit <- 300
# large_sqft_per_unit <- 5000
library(data.table)
source("../../../shared/code/assessor_classification.R")
source("../../../shared/code/save_data.R")
args <- commandArgs(trailingOnly = TRUE)
if (!interactive()) {
  stopifnot(length(args) == 9L)
  first_year <- as.integer(args[1]); last_year <- as.integer(args[2])
  main_window_ft <- as.numeric(args[3]); nearby_ft <- as.numeric(args[4])
  year_gap <- as.integer(args[5]); repeated_units_min <- as.numeric(args[6])
  repeated_land_min <- as.numeric(args[7]); small_sqft_per_unit <- as.numeric(args[8])
  large_sqft_per_unit <- as.numeric(args[9])
}

# Screen every retained citywide construction project before looking at aldermen.
projects <- fread("../input/project_ledger.csv", na.strings = c("", "NA"))
buildings <- fread("../input/assessor_buildings.csv", na.strings = c("", "NA"))
history <- fread("../input/measurement_history.csv", colClasses = c(source_row_id = "character", pin = "character", class = "character"))
scope <- fread("../input/boundary_scope.csv", colClasses = c(ward_pair = "character"))
analysis <- fread("../input/analysis_data.csv", na.strings = c("", "NA"))
prior <- fread("../input/prior_unit_review.csv")
stopifnot(!anyDuplicated(projects$project_id), !anyDuplicated(buildings$project_id),
  !anyDuplicated(scope$project_id), !anyDuplicated(analysis$project_id), !anyDuplicated(prior$project_id),
  !anyDuplicated(history[, .(source_family, source_row_id)]))
projects <- projects[construction_year >= first_year & construction_year <= last_year]
stopifnot(all(analysis$project_id %in% projects$project_id))
for (field in c("dwelling_units", "building_sqft", "land_sqft", "construction_year")) {
  stopifnot(isTRUE(all.equal(analysis[[field]], projects[[field]][match(analysis$project_id, projects$project_id)])))
}
projects <- merge(projects, buildings[, .(project_id, earlier_units, earlier_building, earlier_land,
  earlier_pins, stable_membership, permit_units)], by = "project_id", all.x = TRUE, sort = FALSE)
projects <- merge(projects, scope[, .(project_id, ward, neighbor_ward, distance_to_boundary_ft)],
  by = "project_id", all.x = TRUE, sort = FALSE)
prior[, cleared_without_unit_support := changed_two_vintages & !permit_supports_2021 &
  !permit_supports_2024 & !unit_review_required]
projects <- merge(projects, prior[, .(project_id, cleared_without_unit_support, recommended_units,
  permit_supports_2021, permit_supports_2024)], by = "project_id", all.x = TRUE, sort = FALSE)
projects[is.na(cleared_without_unit_support), cleared_without_unit_support := FALSE]
projects[, `:=`(unit_change = is.finite(earlier_units) & earlier_units > 0 &
    is.finite(dwelling_units) & dwelling_units > 0 & dwelling_units != earlier_units,
  same_building = is.finite(earlier_building) & building_sqft == earlier_building,
  same_land = is.finite(earlier_land) & land_sqft == earlier_land,
  earlier_review_still_selected = cleared_without_unit_support & dwelling_units == recommended_units,
  unit_ratio = dwelling_units / earlier_units, sqft_per_unit = building_sqft / dwelling_units)]
projects[, stable_area_unit_conflict := unit_change & same_building & same_land & stable_membership == TRUE]
projects[, development_permit_conflict := project_kind == "class_297" & is.finite(permit_units) &
  permit_units > 0 & dwelling_units != permit_units]

# For residential records, compare the same card and floor/lot measurements.
# Do not sum cards into a new project or infer missing lot areas for this audit.
history[source_family == "residential" & class %in% single_family_assessor_classes, dwelling_units := 1]
setkey(history, pin)
residential_checks <- vector("list", nrow(projects))
for (i in which(projects$source_family == "residential")) {
  p <- projects[i]
  pins <- unique(strsplit(p$component_pins, "/", fixed = TRUE)[[1]])
  h <- history[.(pins), nomatch = 0L][source_family == "residential"]
  selected_rows <- strsplit(p$source_row_ids, "/", fixed = TRUE)[[1]]
  selected <- h[source_row_id %in% selected_rows]
  comparisons <- list()
  for (j in seq_len(nrow(selected))) {
    s <- selected[j]
    matches <- h[pin == s$pin & card_num == s$card_num & tax_year != s$tax_year &
      is.finite(building_sqft) & building_sqft == s$building_sqft &
      is.finite(land_sqft) & land_sqft == s$land_sqft &
      is.finite(dwelling_units) & dwelling_units > 0 &
      abs(construction_year - s$construction_year) <= year_gap]
    comparisons[[j]] <- matches[, .(source_row_id, dwelling_units, selected_units = s$dwelling_units)]
  }
  matched <- rbindlist(comparisons)
  conflicts <- if (NROW(matched)) matched[dwelling_units != selected_units] else matched
  residential_checks[[i]] <- data.table(project_id = p$project_id,
    residential_source_rows_found = nrow(selected), residential_selected_rows = length(selected_rows),
    comparable_residential_history = NROW(matched) > 0L,
    residential_unit_conflict = NROW(conflicts) > 0L,
    conflicting_residential_rows = if (NROW(conflicts)) paste(sort(unique(conflicts$source_row_id)), collapse = "/") else NA_character_,
    conflicting_residential_units = if (NROW(conflicts)) paste(sort(unique(conflicts$dwelling_units)), collapse = "/") else NA_character_)
}
projects <- merge(projects, rbindlist(residential_checks), by = "project_id", all.x = TRUE, sort = FALSE)

# Find nearby records sharing unusually informative measurements. Identical
# townhouses can be legitimate, so these are leads, never automatic exclusions.
eligible <- projects[is.finite(x_3435) & is.finite(y_3435) & dwelling_units >= 2]
pairs <- list()
for (i in seq_len(nrow(eligible))) {
  p <- eligible[i]
  d <- sqrt((eligible$x_3435 - p$x_3435)^2 + (eligible$y_3435 - p$y_3435)^2)
  other <- which(seq_len(nrow(eligible)) > i & d < nearby_ft)
  q <- eligible[other]
  if (!nrow(q)) next
  same_units <- q$dwelling_units == p$dwelling_units & p$dwelling_units >= repeated_units_min &
    abs(q$construction_year - p$construction_year) <= year_gap
  same_land <- is.finite(q$land_sqft) & is.finite(p$land_sqft) & q$land_sqft == p$land_sqft & p$land_sqft >= repeated_land_min
  keep <- same_units | same_land
  if (!any(keep)) next
  pairs[[i]] <- data.table(project_a = p$project_id, project_b = q$project_id[keep],
    distance_ft = d[other][keep], repeated_units = same_units[keep], repeated_large_land = same_land[keep],
    units_a = p$dwelling_units, units_b = q$dwelling_units[keep],
    building_a = p$building_sqft, building_b = q$building_sqft[keep],
    land_a = p$land_sqft, land_b = q$land_sqft[keep])
}
pairs <- rbindlist(pairs)
projects[, `:=`(nearby_repeated_units = project_id %in% c(pairs[repeated_units == TRUE, project_a], pairs[repeated_units == TRUE, project_b]),
  nearby_repeated_land = project_id %in% c(pairs[repeated_large_land == TRUE, project_a], pairs[repeated_large_land == TRUE, project_b]),
  unusual_floor_per_unit = dwelling_units >= 2 & is.finite(sqft_per_unit) &
    (sqft_per_unit < small_sqft_per_unit | sqft_per_unit > large_sqft_per_unit))]
components <- projects[, .(pin = strsplit(component_pins, "/", fixed = TRUE)[[1]]), by = project_id]
stopifnot(!anyDuplicated(components[, .(project_id, pin)]))
shared_pins <- components[, .N, by = pin][N > 1, pin]
projects[, shared_parcel := project_id %in% components[pin %in% shared_pins, project_id]]

# Add readable addresses and sample membership after defining every screen.
addresses <- fread("../input/parcel_addresses.csv", select = c("pin", "prop_address_full"), colClasses = "character")
addresses <- addresses[, .(parcel_address = paste(sort(unique(na.omit(prop_address_full))), collapse = " / ")), by = pin]
components <- merge(components, addresses, by = "pin", all.x = TRUE, sort = FALSE)
display <- components[, .(parcel_addresses = paste(sort(unique(na.omit(parcel_address))), collapse = " / ")), by = project_id]
projects <- merge(projects, display, by = "project_id", all.x = TRUE, sort = FALSE)
projects[, address := fifelse(!is.na(source_addresses) & source_addresses != "", source_addresses, parcel_addresses)]
controls <- c("share_white_own", "share_black_own", "median_hh_income_own", "share_bach_plus_own", "homeownership_rate_own")
main <- analysis[density_eligible == TRUE & is.finite(signed_distance_m) & abs(signed_distance_m / .3048) < main_window_ft &
  !is.na(zone_group) & !is.na(segment_id) & segment_id != "" & !is.na(ward_pair) &
  Reduce(`&`, lapply(analysis[, ..controls], is.finite)), project_id]
projects[, in_main_500ft_sample := project_id %in% main]
flags <- c("unit_change", "stable_area_unit_conflict", "cleared_without_unit_support", "residential_unit_conflict", "development_permit_conflict",
  "nearby_repeated_units", "nearby_repeated_land", "unusual_floor_per_unit", "shared_parcel")
for (field in flags) set(projects, which(is.na(projects[[field]])), field, FALSE)
projects[, flagged := Reduce(`|`, .SD), .SDcols = flags]
projects[, screen_reasons := apply(.SD, 1, function(x) paste(flags[x], collapse = "; ")), .SDcols = flags]
setorder(projects, project_id)
SaveData(projects, "project_id", "../output/project_screen.csv")
SaveData(pairs[order(project_a, project_b)], c("project_a", "project_b"), "../output/measurement_pairs.csv")

# Preserve the actual Assessor fields rather than replacing them with the audit's interpretation.
raw <- fread("../input/commercial_valuation_data.csv", colClasses = "character")
raw[, source_data_row := .I]
raw[, source_project_id := paste0("commercial_", gsub("[^0-9]", "", keypin))]
flagged_sources <- unique(unlist(strsplit(projects[flagged == TRUE, source_project_ids], "/", fixed = TRUE)))
source_rows <- raw[source_project_id %in% flagged_sources, .(source_data_row, source_project_id,
  keypin, pins, year, address, studiounits, `_1brunits`, `_2brunits`, `_3brunits`, `_4brunits`,
  tot_units, bldgsf, landsf, yearbuilt, property_type_use)]
SaveData(source_rows, "source_data_row", "../output/commercial_source_rows.csv")
permit_links <- fread("../input/building_permit_evidence.csv", colClasses = "character")
permits <- fread("../input/building_permits_full.csv", colClasses = "character", select = c("id", "permit_",
  "permit_type", "permit_status", "issue_date", "application_start_date", "street_number", "street_direction",
  "street_name", "pin_list", "work_description"))
stopifnot(!anyDuplicated(permits$id))
permits[, address := paste(street_number, street_direction, street_name)]
permit_pin_lists <- regmatches(permits$pin_list, gregexpr("[0-9]{10,14}", permits$pin_list))
pin_index <- unique(data.table(id = rep(permits$id, lengths(permit_pin_lists)),
  pin10 = substr(unlist(permit_pin_lists), 1, 10)))
setkey(pin_index, pin10)
permit_project_lists <- strsplit(permit_links$project_ids, "/", fixed = TRUE)
all_permits <- list()
for (i in which(projects$flagged)) {
  p <- projects[i]
  source_ids <- strsplit(p$source_project_ids, "/", fixed = TRUE)[[1]]
  linked <- vapply(permit_project_lists, function(x) any(x %in% source_ids), logical(1))
  numbers <- permit_links[linked, permit_number]
  addresses <- unique(permits[permit_ %in% numbers, address])
  project_pin10 <- unique(substr(strsplit(p$component_pins, "/", fixed = TRUE)[[1]], 1, 10))
  exact_pin <- permits$id %in% pin_index[.(project_pin10), id]
  selected <- exact_pin | permits$address %in% addresses
  if (!any(selected)) next
  all_permits[[i]] <- cbind(data.table(project_id = p$project_id, exact_component_pin = exact_pin[selected]), permits[selected])
}
SaveData(rbindlist(all_permits), c("project_id", "id"), "../output/flagged_permits.csv")
cat(nrow(projects), "citywide projects;", length(main), "main-sample projects;", sum(projects$flagged), "flagged projects.\n")
