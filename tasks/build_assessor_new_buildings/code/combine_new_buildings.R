# setwd("tasks/build_assessor_new_buildings/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("new_building_rules.R")
sf_use_s2(FALSE)

transit_years <- 3  # an event on a parcel retired within three years is the building moving to the parcels that replace it
follow_years <- 4   # the replacing parcels' events up to four years after it are that building
merge_years <- 3    # events on one parcel (or one house record card) within three years are one building
touch_ft <- 1       # apartment-class parcels this close and improved in the same year are one building on assembled lots

# Events of the three rules: house record cards (houses and 2-6 flats), new condominium buildings and apartment-class buildings.
lineage <- fread("../output/parcel_lineage.csv", colClasses = c(pin10 = "character", old_pin10 = "character"))[new_share >= lineage_share, .(pin10, old_pin10)]
houses <- fread("../output/house_events.csv", colClasses = c(pin = "character", card_num = "character"))[, .(pin, card = card_num, pin10 = substr(pin, 1, 10),
  year = tax_year, type = fifelse(units >= 2, "2-6 flat", "house"), route = paste("house:", rule), units = as.numeric(units), record = NA_integer_)]
condominiums <- fread("../output/condominium_buildings.csv", colClasses = c(pin10 = "character"))[kind == "new", .(pin = NA_character_, card = NA_character_, pin10,
  year = first_year, type = "condominium", route = "condominium: units first appear", units = as.numeric(units), record = NA_integer_)]
apartments <- fread("../output/apartment_events.csv", colClasses = c(pin = "character", pin10 = "character"))[apartment_building == TRUE, .(pin, card = NA_character_, pin10,
  year, type = "apartment class", route = paste("apartment:", route), units = valuation_units, record = valuation_record)]
events <- rbind(houses, condominiums, apartments)[year %in% 1999:2025][, `:=`(eid = .I, first_date = year, date_from = "own record")]
message("events: ", nrow(events)); print(events[, .N, by = type])
assessed <- as.data.table(read_parquet("../input/assessed_values.parquet", col_select = c("pin", "year")))[year <= 2025]
last_year <- assessed[, .(last_year = max(year)), by = .(pin10 = substr(pin, 1, 10))]

# Step 1: a building on a parcel retired within transit_years, followed by events on the parcels replacing it, is counted
# on those parcels with its earlier date. Repeated so a chain (old lot, intermediate parcel, final parcel) collapses.
for (pass in 1:3) {
  moving <- merge(events[, .(eid, pin10, year)], last_year, by = "pin10")[last_year < 2025 & last_year <= year + transit_years]
  moving <- merge(moving, lineage, by.x = "pin10", by.y = "old_pin10", allow.cartesian = TRUE, suffixes = c("", "_new"))
  moving <- merge(moving, events[, .(successor = eid, pin10_new = pin10, successor_year = year)], by = "pin10_new", allow.cartesian = TRUE)[
    successor_year >= year & successor_year <= year + follow_years]
  if (!nrow(moving)) break
  inherit <- moving[, .(earliest = min(year)), by = successor]
  events <- merge(events, inherit, by.x = "eid", by.y = "successor", all.x = TRUE)
  events[!is.na(earliest) & earliest < first_date, `:=`(first_date = earliest, date_from = "earlier record on the old parcel")][, earliest := NULL]
  message(sprintf("pass %d: events moved to the parcels replacing theirs: %d", pass, uniqueN(moving$eid)))
  events <- events[!eid %in% moving$eid]
}

# Step 2: events on one parcel within merge_years are one building (house records only with the same card), dated by the
# earliest. A condominium or apartment-class event takes in house events on its parcel within merge_years (a house record
# of that building).
events[, key := fifelse(type %in% c("house", "2-6 flat"), paste(pin, card, sep = "-"), pin10)]
setorder(events, key, year)
events[, building_id := paste(key, cumsum(c(TRUE, diff(year) > merge_years)), sep = "-"), by = key]
big <- events[type %in% c("condominium", "apartment class"), .(pin10, big_year = year, big_building = building_id)]
absorb <- merge(events[type %in% c("house", "2-6 flat"), .(eid, pin10, year)], big, by = "pin10", allow.cartesian = TRUE)[abs(year - big_year) <= merge_years][,
  .SD[which.min(abs(year - big_year))], by = eid]
events[absorb, on = "eid", building_id := i.big_building]

# Step 3: apartment-class buildings improved in the same year on touching parcels, or sharing a valuation record within a
# year, are one building on assembled lots (or one development of several buildings), named by its first building.
apt <- events[type == "apartment class", .(building_id, pin10, year, record)]
parcels <- st_read("../input/parcel_history.gpkg", quiet = TRUE) |> filter(pin10 %in% apt$pin10) |> group_by(pin10) |> summarise(.groups = "drop")
links <- rbindlist(lapply(split(apt, apt$year), \(same_year) {
  g <- parcels[parcels$pin10 %in% same_year$pin10, ]
  if (nrow(g) < 2) return(NULL)
  near <- st_is_within_distance(g, g, touch_ft)
  touching <- rbindlist(lapply(seq_along(near), \(i) data.table(from = g$pin10[i], to = g$pin10[near[[i]]])))[from < to]
  merge(merge(touching, same_year[, .(from = pin10, b1 = building_id)], by = "from"), same_year[, .(to = pin10, b2 = building_id)], by = "to")[, .(b1, b2)]
}))
shared <- merge(apt[!is.na(record), .(record, b1 = building_id, y1 = year)], apt[!is.na(record), .(record, b2 = building_id, y2 = year)],
  by = "record", allow.cartesian = TRUE)[b1 < b2 & abs(y1 - y2) <= 1, .(b1, b2)]
graph <- graph_from_data_frame(unique(rbind(links, shared)), directed = FALSE, vertices = data.table(name = unique(events$building_id)))
component <- data.table(building_id = names(components(graph)$membership), component = components(graph)$membership)
component[, development_id := min(building_id), by = component]
events[component, on = "building_id", development_id := i.development_id]

# One row per building: dated by its earliest event, typed condominium or apartment class if it has such an event (the
# latest), else 2-6 flat, else house.
buildings <- events[order(year)][, .(development_id = development_id[1], year = min(first_date),
  type = if (any(type %in% c("condominium", "apartment class"))) tail(type[type %in% c("condominium", "apartment class")], 1) else if (any(type == "2-6 flat")) "2-6 flat" else "house",
  route = route[1], routes = paste(unique(route), collapse = "; "), pin10 = pin10[.N], pin10s = paste(unique(pin10), collapse = ";"),
  units = units[.N], events = .N, date_from = date_from[which.min(first_date)]), by = building_id]

# Units of apartment-class developments: the latest valuation year with unit counts on the development's parcels, summed
# over that year's records.
valuations <- fread("../input/commercial_valuation_data.csv", colClasses = "character", select = c("pins", "year", "tot_units"))
valuations[, `:=`(record = .I, valuation_year = as.integer(year), units = as.numeric(tot_units))]
valuations <- valuations[!is.na(units), .(pin10 = unique(substr(gsub("[^0-9]", "", unlist(strsplit(pins, ","))), 1, 10))), by = .(record, valuation_year, units)]
development_units <- merge(events[type == "apartment class", .(development_id, pin10)], valuations, by = "pin10", allow.cartesian = TRUE)
development_units <- development_units[, .SD[valuation_year == max(valuation_year)], by = development_id][, .(valuation_units = sum(units[!duplicated(record)])), by = development_id]
buildings <- merge(buildings, development_units, by = "development_id", all.x = TRUE)
buildings[type == "apartment class", units := valuation_units][, valuation_units := NULL]
message(sprintf("buildings: %d, developments: %d", nrow(buildings), uniqueN(buildings$development_id)))
print(buildings[, .(buildings = .N, developments = uniqueN(development_id)), by = type])
SaveData(buildings[order(year, building_id), .(building_id, development_id, year, type, route, routes, pin10, pin10s, units, events, date_from)],
  "building_id", "../output/new_buildings.csv")
