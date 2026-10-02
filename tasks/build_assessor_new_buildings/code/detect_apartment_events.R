# setwd("tasks/build_assessor_new_buildings/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("new_building_rules.R")

small_ratio <- 0.1      # a building value at most this share of the new building's is no building
existing_ratio <- 0.25  # land carrying more than this share of the new building's value held an existing building
look_back <- 3          # years before a new parcel appeared in which the land under it is examined
min_coverage <- 0.5     # the old parcels must cover at least half the new parcel to judge it
earlier_years <- 4      # an event on the old parcels up to four years before the new parcel is the same building
new_years <- 5          # a building the valuations date more than five years before its event is an existing building
exempt_years <- 15      # on exempt land, up to fifteen years: a building built while exempt is new
same_years <- 2         # a valuation whose year built is within two years of the event describes the event's building
type_years <- 2         # without such a valuation, a building is apartment class if its site holds that class within two years
condo_years <- 3        # a site with condominium units within three years is a condominium building (counted by that rule)
successor_share <- 0.5  # a replacing parcel speaks for the event's building only if it holds at least half the event parcel's land
flip_years <- 3         # exempt land taxable at most three years before exempt again, without a valuation, is not counted
repeat_years <- 10      # route 3 on a parcel with another new-building event in the ten years before is the same building

# Assessed values by PIN and year, with each PIN's state: exempt, vacant (vacant land or a minor improvement) or improved.
assessed <- as.data.table(read_parquet("../input/assessed_values.parquet", col_select = c("pin", "year", "class", "board_bldg")))[year <= 2025]
setorder(assessed, pin, year)
assessed[, `:=`(pin10 = substr(pin, 1, 10), state = fifelse(class == "EX", "exempt", fifelse(class %in% vacant_classes, "vacant", "improved")))]
assessed[, new_bldg := pmax(board_bldg, shift(board_bldg, -1), shift(board_bldg, -2), na.rm = TRUE), by = pin]

# Route 1: a PIN becomes improved the year after a vacant or exempt year, with a building far larger than before.
assessed[, `:=`(prev_state = shift(state), prev_year = shift(year), prev_bldg = shift(board_bldg), prev_class = shift(class)), by = pin]
assessed[, improved_value := fifelse(state == "improved", board_bldg, NA_real_)]
assessed[, last_improved_value := shift(nafill(improved_value, "locf")), by = pin]
route1 <- assessed[state == "improved" & prev_state %in% c("vacant", "exempt") & prev_year == year - 1 & new_bldg > 0 & prev_bldg <= small_ratio * new_bldg]
route1[, route := fcase(prev_state == "vacant", "same parcel, after vacant",
  is.na(last_improved_value) | last_improved_value <= existing_ratio * new_bldg, "same parcel, after exempt",
  default = "same parcel, after exempt, earlier building")]
route1 <- route1[, .(pin, pin10, year, route)]

# Route 3: a PIN enters an apartment class from another class (kept below only when the valuations date its building new).
route3 <- assessed[is_large(class) & !is.na(prev_class) & !is_large(prev_class) & prev_year == year - 1,
  .(pin, pin10, year, route = "same parcel, into a large class, valuation dates it new")]

# Each 10-digit parcel and year: building value and state (condominium if any unit is 299 or 399).
parcel_years <- assessed[, .(bldg = sum(board_bldg, na.rm = TRUE), large = any(is_large(class)), condo = any(class %in% c("299", "399")),
  state = fifelse(any(class %in% c("299", "399")), "condo", fifelse(all(state != "improved"), fifelse(all(state == "exempt"), "exempt", "vacant"), "improved"))),
  by = .(pin10, year)]
setorder(parcel_years, pin10, year)
parcel_years[, new_bldg := pmax(bldg, shift(bldg, -1), shift(bldg, -2), na.rm = TRUE), by = pin10]
new_parcels <- parcel_years[, .(first_year = min(year)), by = pin10][first_year >= 2000]
# A new parcel's building year: its first year in a building class with a building value above small_ratio of the next years'.
built <- parcel_years[pin10 %in% new_parcels$pin10 & state %in% c("improved", "condo") & new_bldg > 0 & bldg > small_ratio * new_bldg, .SD[1], by = pin10][,
  .(pin10, built_year = year, new_bldg)]
new_parcels <- merge(new_parcels, built, by = "pin10", all.x = TRUE)

# Route 2: a new parcel judged by the land under it in the look_back years before it appeared. Each old parcel alive in a
# year adds its share of the new parcel's area to the coverage, and its building value in proportion to its own area
# under the new parcel.
lineage <- fread("../output/parcel_lineage.csv", colClasses = c(pin10 = "character", old_pin10 = "character"))
land <- merge(lineage, new_parcels[, .(pin10, first_year)], by = "pin10")
land <- land[, .(t = first_year - seq_len(look_back)), by = .(pin10, old_pin10, new_share, old_share, first_year)]
land <- merge(land, parcel_years[, .(old_pin10 = pin10, t = year, state, bldg)], by = c("old_pin10", "t"))
land <- land[, .(coverage = sum(new_share), cleared = sum(new_share[state %in% c("vacant", "exempt")]), condo = sum(new_share[state == "condo"]),
  value = sum(old_share * bldg)), by = .(pin10, t)]
land <- land[, .(coverage = max(coverage), cleared_year = any(coverage >= min_coverage & cleared >= 0.5 * coverage),
  condo_before = condo[which.max(t)] >= 0.5 * coverage[which.max(t)], value_before = value[which.max(t)]), by = pin10]
# A route 1 event or a new condominium building on the old parcels shortly before is the same building.
condominiums <- fread("../output/condominium_buildings.csv", colClasses = c(pin10 = "character"))[kind == "new", .(old_pin10 = pin10, event_year = first_year)]
old_events <- rbind(route1[, .(old_pin10 = pin10, event_year = year)], condominiums)
earlier <- merge(lineage[new_share >= lineage_share], old_events, by = "old_pin10", allow.cartesian = TRUE)
earlier <- merge(earlier, new_parcels[, .(pin10, first_year)], by = "pin10")[event_year >= first_year - earlier_years & event_year <= first_year, unique(pin10)]
route2 <- merge(new_parcels, land, by = "pin10", all.x = TRUE)
route2[, route := fcase(is.na(built_year), "new parcel, never built", pin10 %in% earlier, "new parcel, building counted on its old parcels",
  is.na(coverage) | coverage < min_coverage, "new parcel, old parcels cover under half", condo_before %in% TRUE, "new parcel, from a condominium building",
  cleared_year %in% TRUE, "new parcel, cleared land", value_before <= existing_ratio * new_bldg, "new parcel, land with little building value",
  default = "new parcel, existing building renumbered")]
message("new parcels by route:"); print(route2[, .N, by = route][order(-N)])
route2 <- route2[route %in% c("new parcel, cleared land", "new parcel, land with little building value")]
route2 <- assessed[route2[, .(pin10, year = built_year, route)], on = .(pin10, year), nomatch = NULL][order(-board_bldg)][, .SD[1], by = .(pin10, year)][, .(pin, pin10, year, route)]

# Events, one per parcel and year (route 1 first, then route 2, then route 3).
events <- rbind(route1, route2, route3)[order(pin10, year)][, .SD[1], by = .(pin10, year)][, eid := .I]
# Each event's site: its parcel, and the later parcels that replace it holding at least half its land.
site <- rbind(events[, .(eid, pin10, own = TRUE)], merge(events[, .(eid, old_pin10 = pin10)], lineage[old_share >= successor_share, .(old_pin10, pin10)],
  by = "old_pin10", allow.cartesian = TRUE)[, .(eid, pin10, own = FALSE)])

# The Assessor's 2021+ commercial valuations: year built, units and class of the building each record describes. A parcel
# in several records of one year takes the record naming it as the key PIN, else the record with the most units.
valuations <- fread("../input/commercial_valuation_data.csv", colClasses = "character", select = c("keypin", "pins", "year", "class_es", "tot_units", "yearbuilt"))
valuations[, `:=`(record = .I, valuation_year = as.integer(year), yb = as.integer(as.numeric(yearbuilt)), units = as.numeric(tot_units),
  record_large = is_large(gsub("[^0-9]", "", substr(class_es, 1, 4))), key10 = substr(gsub("[^0-9]", "", keypin), 1, 10))]
valuations <- valuations[!is.na(yb), .(pin10 = unique(substr(gsub("[^0-9]", "", unlist(strsplit(pins, ","))), 1, 10))),
  by = .(record, key10, valuation_year, yb, units, record_large)][nchar(pin10) == 10]
valuations[, is_key := pin10 == key10]
setorder(valuations, pin10, valuation_year, -is_key, -units, na.last = TRUE)
valuations <- valuations[, .SD[1], by = .(pin10, valuation_year)]
# An event takes the earliest valuation at or after it on its site, its own parcel first.
matched <- merge(merge(site, events[, .(eid, year)], by = "eid"), valuations[, .(pin10, valuation_year, yb, units, record_large, record)],
  by = "pin10", allow.cartesian = TRUE)[valuation_year >= year]
setorder(matched, eid, valuation_year, -own)
matched <- matched[, .SD[1], by = eid][, .(eid, valuation_year_built = yb, valuation_units = units, valuation_large = record_large, valuation_record = record)]
events <- merge(events, matched, by = "eid", all.x = TRUE)
events[, same_building := !is.na(valuation_year_built) & abs(year - valuation_year_built) <= same_years]

# Classes on the site after the event: condominium units within condo_years, an apartment class within type_years. The
# building's type is its valuation's class when the valuation describes it, else the classes on its site.
held <- merge(merge(site, events[, .(eid, ey = year)], by = "eid"), parcel_years[, .(pin10, y = year, large, condo)], by = "pin10", allow.cartesian = TRUE)[y >= ey]
held <- held[, .(condo = any(condo & y <= ey + condo_years), large_class = any(large & y <= ey + type_years)), by = eid]
events <- merge(events, held, by = "eid", all.x = TRUE)
events[, large := fifelse(same_building, valuation_large, large_class %in% TRUE)]

# Route 3 counts only a building the valuations date new, on a parcel without another event in the repeat_years before.
setorder(events, pin10, year)
events[, earlier_event := vapply(seq_len(.N), \(i) any(route[-i] != "same parcel, into a large class, valuation dates it new" & year[-i] < year[i] &
  year[-i] >= year[i] - repeat_years), TRUE), by = pin10]
events <- events[!(route == "same parcel, into a large class, valuation dates it new" & (!same_building | earlier_event))]

# Exempt land: the year a parcel becomes taxable is not when its building was built. A building the valuations date up to
# exempt_years before is new, dated the earlier of its first taxable year and the year after its year built; older is
# existing. Elsewhere a building the valuations date more than new_years before its event is existing.
events[, exempt_route := route == "same parcel, after exempt"]
events[, existing_building := !is.na(valuation_year_built) & year - valuation_year_built > fifelse(exempt_route, exempt_years, new_years)]
events[, `:=`(event_year = year, date_source = "first assessment")]
events[exempt_route & !is.na(valuation_year_built) & !existing_building & valuation_year_built + 1L < year,
  `:=`(year = valuation_year_built + 1L, date_source = "valuation year built (built on exempt land)")]
events[exempt_route & is.na(valuation_year_built), date_source := "first assessment (exempt land, no valuation)"]
# Exempt land taxable only briefly (exempt again within flip_years) and without a valuation: an unverifiable status change.
exempt_again <- parcel_years[state == "exempt", .(pin10, exempt_year = year)]
flip <- merge(events[exempt_route & is.na(valuation_year_built), .(eid, pin10, event_year)], exempt_again, by = "pin10", allow.cartesian = TRUE)[
  exempt_year > event_year & exempt_year <= event_year + flip_years, unique(eid)]
events[eid %in% flip, route := "same parcel, after exempt, taxable briefly"]

# Counted as apartment-class buildings: apartment type, not a condominium building, not an existing building, and not on
# exempt land with an earlier building or taxable only briefly.
events[, apartment_building := large & !condo %in% TRUE & !existing_building &
  !route %in% c("same parcel, after exempt, earlier building", "same parcel, after exempt, taxable briefly")]
message("events by route:"); print(events[, .(events = .N, apartment_class = sum(large), condominium = sum(condo %in% TRUE), counted = sum(apartment_building)), by = route])
events <- events[, .(pin, pin10, event_year, year, route, date_source, apartment_building, large, condo = condo %in% TRUE, existing_building,
  same_building, valuation_year_built, valuation_units, valuation_large, valuation_record)][order(pin10, event_year)]
SaveData(events, c("pin10", "event_year"), "../output/apartment_events.csv")
