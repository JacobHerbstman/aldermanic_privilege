# setwd("tasks/prepare_permit_construction/code")
# first_year_built <- 2006
# last_year_built <- 2022
# last_unpermitted_year_built <- 2007
# assessor_year_lead <- 2
# max_build_lag_years <- 4
# unit_tolerance <- 0.2
# min_sqft_per_unit <- 300
# max_land_sqft_per_unit <- 43560
# lot_distance_ft <- 150
# townhouse_distance_ft <- 300
# large_building_units <- 20
# large_lot_distance_ft <- 300
# measurement_years <- 3
# min_land_sqft <- 100
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/assessor_classification.R")
source("../../shared/code/normalize_chicago_address.R")
source("../../shared/code/street_key.R")
source("construction_rules.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(first_year_built, last_year_built, last_unpermitted_year_built, assessor_year_lead,
  max_build_lag_years, unit_tolerance, min_sqft_per_unit, max_land_sqft_per_unit, lot_distance_ft, townhouse_distance_ft,
  large_building_units, large_lot_distance_ft, measurement_years, min_land_sqft)
stopifnot(length(args) == 14L)
first_year_built <- as.integer(args[1])
last_year_built <- as.integer(args[2])
last_unpermitted_year_built <- as.integer(args[3])
assessor_year_lead <- as.integer(args[4])
max_build_lag_years <- as.integer(args[5])
unit_tolerance <- as.numeric(args[6])
min_sqft_per_unit <- as.numeric(args[7])
max_land_sqft_per_unit <- as.numeric(args[8])
lot_distance_ft <- as.numeric(args[9])
townhouse_distance_ft <- as.numeric(args[10])
large_building_units <- as.integer(args[11])
large_lot_distance_ft <- as.numeric(args[12])
measurement_years <- as.integer(args[13])
min_land_sqft <- as.numeric(args[14])

permit_buildings <- read_csv("../output/permit_buildings.csv", col_types = cols(building_id = "c", permit_number = "c",
  member_permit_numbers = "c", superseded_permit_numbers = "c", parcel_pin10s = "c", record_ids = "c", issue_date = "D",
  .default = col_guess()))

# Whether a permit in "id:year/id:year", other than `except`, was issued within the construction lag before, or the lead
# after, a building's reported year built.
permit_in_window <- function(claims, year_built, except = NA_character_) {
  pmap_lgl(list(coalesce(claims, ""), year_built, coalesce(except, "")), \(claims, built, except) {
    claims <- str_split_1(claims, "/")
    claims <- claims[claims != ""]
    years <- as.integer(str_extract(claims, "[0-9]+$"))
    any(str_extract(claims, "^[^:]+") != except & years >= built - max_build_lag_years & years <= built + assessor_year_lead,
      na.rm = TRUE)
  })
}

# Parcels that already belong to the permit arm: measured records, and every parcel of a new residential permit.
claimed_records <- permit_buildings |> filter(status == "measured") |> select(record_ids) |>
  separate_longer_delim(record_ids, "/") |> distinct(record_id = record_ids)
permitted_parcels <- permit_buildings |> select(permit_number, issue_year, parcel_pin10s) |> filter(!is.na(parcel_pin10s)) |>
  separate_longer_delim(parcel_pin10s, "/") |> group_by(pin10 = parcel_pin10s) |>
  summarise(parcel_permits = paste(permit_number, issue_year, sep = ":", collapse = "/"), .groups = "drop")
# Chicago parcels assessed in any year from 1999, with centroids, so parcels later retired by condominium declarations
# and subdivisions keep a location.
parcels <- read_csv("../input/parcel_centroids.csv", col_types = cols(pin10 = "c", .default = "d")) |>
  filter(is.finite(x_3435), is.finite(y_3435))
centroids <- parcels |> select(pin10, x = x_3435, y = y_3435)

con <- DBI::dbConnect(duckdb::duckdb())

# Residential parcels whose records first show a card built in the window, persisting the next year, after either
# no record or a record without it. As in the permit arm, the building is measured on the new cards the parcel holds
# most often in its first MEASUREMENT_YEARS years with new cards.
cards <- DBI::dbGetQuery(con, sprintf("
  WITH h AS (SELECT * FROM read_parquet('../input/residential_assessor_history.parquet')),
  first_new AS (
    SELECT pin, min(tax_year) AS first_year FROM h
    WHERE year_built >= %d GROUP BY pin),
  checked AS (
    SELECT f.pin, f.first_year,
      bool_or(h.tax_year = f.first_year + 1 AND h.year_built >= %d) OR max(h.tax_year) = f.first_year AS persists
    FROM first_new f JOIN h USING (pin) GROUP BY f.pin, f.first_year),
  prior AS (
    SELECT c.pin, sum(h.building_sqft) AS prior_sqft FROM checked c JOIN h ON h.pin = c.pin AND h.tax_year = c.first_year - 1
    GROUP BY c.pin)
  SELECT h.pin, substr(h.pin, 1, 10) AS pin10, c.first_year, h.tax_year, h.card_num, h.class, h.year_built, h.building_sqft,
    h.land_sqft, h.num_apartments, h.pin_proration_rate, h.proration_key_pin, coalesce(h.year_built >= %d, false) AS new_card,
    prior.prior_sqft
  FROM checked c JOIN h ON h.pin = c.pin AND h.tax_year BETWEEN c.first_year AND c.first_year + %d
  LEFT JOIN prior ON prior.pin = c.pin
  WHERE c.persists", first_year_built - assessor_year_lead, first_year_built - assessor_year_lead,
  first_year_built - assessor_year_lead, measurement_years - 1)) |>
  # A reported year built that changes while the parcel keeps last year's floor area is a revised record, not a building.
  # A building on prorated parcels is keyed by its proration as first assessed.
  group_by(pin) |>
  mutate(record_id = first(if_else(coalesce(pin_proration_rate, 1) < 1 & !is.na(proration_key_pin), proration_key_pin,
    pin)[tax_year == first_year]),
    same_floor_area = coalesce(abs(first(prior_sqft) - sum(building_sqft[new_card & tax_year == first_year])) <= 1, FALSE)) |>
  group_by(pin, tax_year) |> filter(any(new_card)) |>
  mutate(signature = if_else(anyNA(building_sqft[new_card]), NA_character_,
    paste(sum(building_sqft[new_card]), sum(new_card), sum(num_apartments[new_card], na.rm = TRUE)))) |>
  group_by(pin) |> filter(tax_year == stable_year(tax_year, signature, measurement_years)) |>
  mutate(old_card_on_parcel = any(!new_card)) |> filter(new_card) |> ungroup() |>
  mutate(card_key = paste(record_id, card_num),
    card_units = case_when(class %in% single_family_assessor_classes ~ 1, num_apartments > 0 ~ num_apartments))
residential <- cards |> group_by(record_id) |> summarise(
    pin10s = paste(sort(unique(pin10)), collapse = "/"), first_year = min(first_year), year_built = min(year_built),
    classes = paste(sort(unique(class)), collapse = "/"),
    units = sum(card_units[!duplicated(card_key)]), building_sqft = sum(building_sqft[!duplicated(card_key)]),
    land_sqft = sum(land_sqft[!duplicated(pin)]), older_building = any(old_card_on_parcel),
    same_floor_area = any(same_floor_area), single_family = all(class %in% single_family_assessor_classes), .groups = "drop") |>
  mutate(source = "residential")

# Condominium buildings first appearing with a reported year built in the window, measured in the year of their first
# MEASUREMENT_YEARS they hold most often.
condominiums <- DBI::dbGetQuery(con, sprintf("
  WITH c AS (
    SELECT pin10, try_cast(try_cast(year AS DOUBLE) AS INTEGER) AS tax_year, is_parking_space, is_common_area,
      try_cast(char_building_sf AS DOUBLE) AS building_sqft, try_cast(char_land_sf AS DOUBLE) AS land_sqft,
      try_cast(try_cast(char_yrblt AS DOUBLE) AS INTEGER) AS year_built
    FROM read_csv('../input/condominium_characteristics.csv', all_varchar = true)),
  first_year AS (SELECT pin10, min(tax_year) AS first_year FROM c WHERE year_built >= %d GROUP BY pin10)
  SELECT c.pin10 AS record_id, c.pin10 AS pin10s, f.first_year, c.tax_year, min(c.year_built) AS year_built,
    count(*) FILTER (WHERE c.is_parking_space <> 'true' AND c.is_common_area <> 'true') AS units,
    max(c.building_sqft) AS building_sqft, max(c.land_sqft) AS land_sqft
  FROM c JOIN first_year f ON c.pin10 = f.pin10 AND c.tax_year BETWEEN f.first_year AND f.first_year + %d
  GROUP BY 1, 2, 3, 4", first_year_built - assessor_year_lead, measurement_years - 1)) |>
  group_by(record_id) |> filter(tax_year == stable_year(tax_year, if_else(is.na(units) | is.na(building_sqft), NA_character_, paste(units, building_sqft, land_sqft)), measurement_years)) |>
  ungroup() |> select(-tax_year) |>
  mutate(classes = "299", older_building = FALSE, single_family = FALSE, source = "condominium")
DBI::dbDisconnect(con, shutdown = TRUE)

# Commercial apartment valuations (2021 onward) of buildings reported built in the window.
commercial <- read_commercial_valuations() |> filter(year_built >= first_year_built - assessor_year_lead) |>
  group_by(record_id) |> mutate(first_year = min(year)) |> ungroup() |>
  distinct(record_id, pin10s, first_year, year, year_built, classes, units, building_sqft, land_sqft) |>
  # A valuation reporting different measurements in the same year has no usable measurement.
  group_by(record_id, year) |> mutate(across(c(units, building_sqft, land_sqft), \(x) if (n() > 1) NA_real_ else x)) |>
  slice(1) |> group_by(record_id) |>
  filter(year == stable_year(year, if_else(is.na(units) | is.na(building_sqft), NA_character_, paste(units, building_sqft, land_sqft)), measurement_years)) |> ungroup() |>
  select(-year) |> mutate(older_building = FALSE, single_family = FALSE, source = "commercial")
stopifnot(!anyDuplicated(commercial$record_id), !anyDuplicated(residential$record_id), !anyDuplicated(condominiums$record_id))

# Assessor-only buildings: on Chicago parcels, in the window, not measured by a permit, and on no parcel of a new
# residential permit issued within the construction lag before, or the lead after, the reported year built.
# Condominium records take a parcel before residential cards, and both before commercial valuations, as in the permit arm.
# Buildings reported built after the window are kept only as possible matches for late permits.
candidates <- bind_rows(condominiums, residential, commercial) |>
  filter(year_built >= first_year_built, year_built <= last_year_built + max_build_lag_years,
    !record_id %in% claimed_records$record_id) |>
  mutate(priority = match(source, c("condominium", "residential", "commercial")),
    flags = str_c(
      if_else(building_sqft / units < min_sqft_per_unit, "area_per_unit_implausible;", "", ""),
      if_else(land_sqft / units > max_land_sqft_per_unit | land_sqft < min_land_sqft, "land_implausible;", "", ""),
      if_else(older_building, "older_building_on_parcel;", "", ""),
      if_else(same_floor_area %in% TRUE, "year_built_revised_same_floor_area;", "", "")) |> coalesce(""))
parcel_candidates <- candidates |> select(source, record_id, priority, year_built, pin10s) |>
  separate_longer_delim(pin10s, "/") |> rename(pin10 = pin10s) |>
  left_join(permitted_parcels, by = "pin10", relationship = "many-to-one") |>
  group_by(pin10) |> mutate(best_priority = min(priority)) |> ungroup() |>
  group_by(source, record_id) |>
  summarise(permitted = any(permit_in_window(parcel_permits, year_built)), outranked = any(priority > best_priority),
    chicago = any(pin10 %in% centroids$pin10), .groups = "drop")
addresses <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(.default = col_character()),
  col_select = c(pin10, prop_address_full)) |> group_by(pin10) |> summarise(address = first(prop_address_full), .groups = "drop")
unpermitted <- candidates |> left_join(parcel_candidates, by = c("source", "record_id"), relationship = "one-to-one") |>
  filter(chicago, !permitted, !outranked) |>
  mutate(pin10 = substr(pin10s, 1, 10)) |> left_join(addresses, by = "pin10", relationship = "many-to-one") |>
  mutate(
    building_id = paste0("assessor_", source, "_", record_id), route = "assessor_only",
    status = case_when(year_built <= last_unpermitted_year_built ~ "measured_without_permit",
      year_built <= last_year_built ~ "no_permit_found", TRUE ~ "built_after_window")) |>
  transmute(building_id, route, status, flags, address, source, record_ids = record_id, parcel_pin10s = pin10s, classes,
    first_assessment_year = first_year, assessor_year_built = year_built, dwelling_units = units, building_sqft, land_sqft,
    multifamily = units >= 2 & !single_family)

# One row per building: every new residential permit's outcome, and every Assessor-only building.
# Each building sits at the centroid of its measured parcels; permit coordinates are geocoded at the street frontage.
buildings <- bind_rows(permit_buildings |> mutate(route = "permit"), unpermitted)
building_centroids <- buildings |> filter(!is.na(record_ids)) |> select(building_id, record_ids) |>
  separate_longer_delim(record_ids, "/") |> distinct(building_id, pin10 = substr(record_ids, 1, 10)) |>
  inner_join(centroids, by = "pin10", relationship = "many-to-one") |>
  group_by(building_id) |> summarise(parcel_x = mean(x), parcel_y = mean(y), .groups = "drop")
permit_points <- buildings |> filter(is.finite(latitude), is.finite(longitude)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
permit_points <- tibble(building_id = permit_points$building_id,
  permit_x = st_coordinates(permit_points)[, 1], permit_y = st_coordinates(permit_points)[, 2])
buildings <- buildings |>
  left_join(building_centroids, by = "building_id", relationship = "one-to-one") |>
  left_join(permit_points, by = "building_id", relationship = "one-to-one") |>
  mutate(location_source = case_when(!is.na(parcel_x) ~ "parcel_centroid", !is.na(permit_x) ~ "permit_point"),
    x_3435 = coalesce(parcel_x, permit_x), y_3435 = coalesce(parcel_y, permit_y), flags = coalesce(flags, ""))


# A condominium declaration or subdivision retires a parcel number, and the building reappears on the successor
# parcels. An Assessor-only record on a parcel whose successors hold a new building reported built within its lead and
# lag is that building counted twice, and is removed.
descendants <- parcel_descendants(parcels)
successor_years <- buildings |> filter(!is.na(record_ids)) |> select(record_ids, assessor_year_built) |>
  separate_longer_delim(record_ids, "/") |> group_by(pin10 = substr(record_ids, 1, 10)) |>
  summarise(earliest_built = min(assessor_year_built), latest_built = max(assessor_year_built), .groups = "drop")
counted_twice <- buildings |> filter(route == "assessor_only") |> select(building_id, record_ids, assessor_year_built) |>
  separate_longer_delim(record_ids, "/") |> mutate(pin10 = substr(record_ids, 1, 10)) |>
  inner_join(descendants, by = c("pin10" = "ancestor"), relationship = "many-to-one") |>
  separate_longer_delim(descendants, "/") |>
  inner_join(successor_years, by = c("descendants" = "pin10"), relationship = "many-to-one") |>
  filter(latest_built >= assessor_year_built - assessor_year_lead, earliest_built <= assessor_year_built + max_build_lag_years) |>
  distinct(building_id)
buildings <- buildings |> filter(!building_id %in% counted_twice$building_id)

# Lot rule: a permit reaching no new building on its own parcels takes the unclaimed new building within
# LOT_DISTANCE_FT of its geocoded point (LARGE_LOT_DISTANCE_FT for LARGE_BUILDING_UNITS or more dwellings, whose lots
# reach farther from the street frontage) that first appears after the permit, within the construction lag, and
# matches its dwelling count.
# Each must be the other's only such match; every other qualifying pair is listed for review.
# A permit whose own parcel changed floor area after the permit is held for review instead. Lots on the opposite
# side of the permit's street, or at an address with its own new-construction permit, are not candidates.
lot_permits <- read_csv("../output/construction_permits.csv", col_types = cols(permit_number = "c", issue_year = "i",
    .default = col_character()), col_select = c(permit_number, scope, issue_year, address)) |>
  filter(!scope %in% c("revision", "temporary_structure"))
address_permits <- bind_cols(lot_permits, address_parts(lot_permits$address)) |> filter(!is.na(number)) |>
  group_by(l_number = number, l_street = street) |>
  summarise(address_permits = paste(permit_number, issue_year, sep = ":", collapse = "/"), .groups = "drop")
open_permits <- buildings |> filter(route == "permit", status %in% c("no_parcel", "no_new_building"), !is.na(permit_x),
  !parcel_changed_after_permit %in% TRUE)
lots <- buildings |> filter(route == "assessor_only", flags == "", !is.na(x_3435))
near <- st_is_within_distance(st_as_sf(open_permits, coords = c("permit_x", "permit_y"), crs = 3435),
  st_as_sf(lots, coords = c("x_3435", "y_3435"), crs = 3435), dist = max(lot_distance_ft, large_lot_distance_ft))
lot_pairs <- tibble(permit_id = open_permits$building_id[rep(seq_along(near), lengths(near))],
    lot_id = lots$building_id[unlist(near)]) |>
  left_join(open_permits |> select(permit_id = building_id, permit_number, issue_year, permit_units, permit_x, permit_y,
    permit_address = address), by = "permit_id", relationship = "many-to-one") |>
  left_join(lots |> select(lot_id = building_id, assessor_year_built, first_assessment_year, dwelling_units, lot_x = x_3435,
    lot_y = y_3435, lot_address = address), by = "lot_id", relationship = "many-to-one") |>
  filter(sqrt((permit_x - lot_x)^2 + (permit_y - lot_y)^2) <=
      if_else(permit_units >= large_building_units, large_lot_distance_ft, lot_distance_ft, lot_distance_ft),
    assessor_year_built >= issue_year - assessor_year_lead, assessor_year_built <= issue_year + max_build_lag_years,
    first_assessment_year >= issue_year,
    units_agree(dwelling_units, permit_units, unit_tolerance) %in% TRUE) |>
  mutate(p = address_parts(permit_address), l = address_parts(lot_address)) |> unpack(c(p, l), names_sep = "_") |>
  filter(!(coalesce(p_street == l_street, FALSE) & (p_number %% 2L) != (l_number %% 2L))) |>
  left_join(address_permits, by = c("l_number", "l_street"), relationship = "many-to-one") |>
  filter(coalesce(l_number == p_number & l_street == p_street, FALSE) |
    !permit_in_window(address_permits, assessor_year_built, except = permit_number)) |>
  add_count(permit_id, name = "lots_for_permit") |> add_count(lot_id, name = "permits_for_lot")
lot_links <- lot_pairs |> filter(lots_for_permit == 1, permits_for_lot == 1)
lot_review <- bind_rows(
  lot_pairs |> filter(lots_for_permit > 1 | permits_for_lot > 1) |> group_by(building_id = permit_id) |>
    summarise(lot_rule_candidates = paste(sort(lot_id), collapse = "/"), .groups = "drop"),
  lot_pairs |> filter(lots_for_permit > 1 | permits_for_lot > 1) |> group_by(building_id = lot_id) |>
    summarise(lot_rule_candidates = paste(sort(permit_number), collapse = "/"), .groups = "drop"))
# A linked permit takes the lot's measurement, and the lot leaves the data as a separate row.
linked <- lot_links |> select(building_id = permit_id, lot_id) |>
  left_join(buildings |> select(lot_id = building_id, source, record_ids, classes, first_assessment_year, assessor_year_built,
    dwelling_units, building_sqft, land_sqft, multifamily, x_3435, y_3435), by = "lot_id", relationship = "one-to-one") |>
  mutate(status = "measured", match_basis = "nearby_lot", location_source = "parcel_centroid") |> select(-lot_id)
buildings <- buildings |> rows_update(linked, by = "building_id") |> filter(!building_id %in% lot_links$lot_id) |>
  left_join(lot_review, by = "building_id", relationship = "one-to-one")


# Townhouse rule: a permit for several townhouses or houses (or a group of single-house permits) reaching fewer homes
# than it authorizes takes the unclaimed new single-family parcels that complete one run of consecutive parcel numbers
# on a block, first assessed within a year of its measured homes, exactly matching its count. A permit without a
# measured home takes the one such run within TOWNHOUSE_DISTANCE_FT. Parcels of other permits in the construction
# window are excluded, as is a home two permits would take. Permits with other qualifying homes wait for review.
single_family_record <- function(classes) {
  map_lgl(str_split(coalesce(classes, ""), "/"), \(x) all(x %in% single_family_assessor_classes))
}
permits_by_parcel <- permit_buildings |> filter(!is.na(parcel_pin10s)) |>
  select(permit_id = building_id, permit_year = issue_year, parcel_pin10s) |>
  separate_longer_delim(parcel_pin10s, "/") |> distinct(pin10 = parcel_pin10s, permit_id, permit_year) |>
  group_by(pin10) |> summarise(parcel_permits = paste(permit_id, permit_year, sep = ":", collapse = "/"), .groups = "drop")
taken_rows <- setdiff(unpermitted$building_id, buildings$building_id)
homes <- candidates |>
  left_join(parcel_candidates |> select(source, record_id, outranked), by = c("source", "record_id"), relationship = "one-to-one") |>
  filter(source == "residential", units == 1, flags == "", single_family, !outranked, str_detect(record_id, "^[0-9]{14}$"),
    !paste0("assessor_residential_", record_id) %in% taken_rows) |>
  transmute(home_id = record_id, pin10 = substr(record_id, 1, 10), block = substr(record_id, 1, 7),
    parcel = as.integer(substr(record_id, 8, 10)), home_first_year = first_year, home_year_built = year_built,
    home_classes = classes, home_sqft = building_sqft, home_land = land_sqft) |>
  inner_join(centroids, by = "pin10", relationship = "many-to-one") |>
  left_join(permits_by_parcel, by = "pin10", relationship = "many-to-one")
targets <- buildings |>
  filter(route == "permit", permit_units >= 2,
    str_detect(str_to_upper(coalesce(description, "")),
      "TOWN ?HOUSES?|TOWN ?HOMES?|ROW ?HOUSES?|ROW ?HOMES?|\\bHOMES\\b|\\bHOUSES\\b|\\bRESIDENCES\\b") |
      str_count(member_permit_numbers, "/") + 1 == permit_units,
    (status == "measured" & source %in% "residential" & dwelling_units < permit_units & single_family_record(classes) &
      !str_detect(flags, "area_|land_|older_")) |
    (status %in% c("no_parcel", "no_new_building") & !parcel_changed_after_permit %in% TRUE & !is.na(permit_x)))
measured_homes <- targets |> filter(status == "measured") |> select(building_id, record_ids, first_assessment_year) |>
  separate_longer_delim(record_ids, "/") |>
  transmute(building_id, block = substr(record_ids, 1, 7), parcel = as.integer(substr(record_ids, 8, 10)),
    anchor_year = first_assessment_year)
open_targets <- targets |> filter(status != "measured")
near <- st_is_within_distance(st_as_sf(open_targets, coords = c("permit_x", "permit_y"), crs = 3435),
  st_as_sf(homes, coords = c("x", "y"), crs = 3435), dist = townhouse_distance_ft)
home_pairs <- bind_rows(
    measured_homes |> distinct(building_id, block, anchor_year) |>
      left_join(homes |> group_by(block) |> summarise(home_ids = paste(home_id, collapse = "/"), .groups = "drop"),
        by = "block", relationship = "many-to-one") |>
      filter(!is.na(home_ids)) |> separate_longer_delim(home_ids, "/") |> select(building_id, home_id = home_ids, anchor_year),
    tibble(building_id = open_targets$building_id[rep(seq_along(near), lengths(near))], home_id = homes$home_id[unlist(near)])) |>
  left_join(homes, by = "home_id", relationship = "many-to-one") |>
  left_join(targets |> select(building_id, issue_year), by = "building_id", relationship = "many-to-one") |>
  filter(is.na(anchor_year) | abs(home_first_year - anchor_year) <= 1,
    home_year_built >= issue_year - assessor_year_lead, home_year_built <= issue_year + max_build_lag_years,
    home_first_year >= issue_year, !permit_in_window(parcel_permits, home_year_built, except = building_id))

# Runs of consecutive parcel numbers: measured homes plus candidates for permits with measured homes, candidates alone
# (within a year of each other) for permits without.
runs <- bind_rows(
    measured_homes |> filter(building_id %in% home_pairs$building_id) |> transmute(building_id, block, parcel, home_id = NA_character_,
      home_first_year = anchor_year),
    home_pairs |> select(building_id, block, parcel, home_id, home_first_year)) |>
  arrange(building_id, block, parcel) |>
  group_by(building_id, block) |>
  mutate(run = cumsum(c(TRUE, diff(parcel) != 1L | abs(diff(home_first_year)) > 1L))) |> ungroup() |>
  left_join(targets |> select(building_id, permit_units, status), by = "building_id", relationship = "many-to-one") |>
  group_by(building_id, block, run) |>
  mutate(run_size = n(), run_measured = sum(is.na(home_id))) |> ungroup() |>
  group_by(building_id) |>
  mutate(measured_runs = n_distinct(paste(block, run)[is.na(home_id)]),
    exact_runs = n_distinct(paste(block, run)[run_size == permit_units &
      (status != "measured" | run_measured == sum(is.na(home_id)))])) |> ungroup()
townhouse_links <- runs |>
  filter(!is.na(home_id), run_size == permit_units, exact_runs == 1,
    (status == "measured" & measured_runs == 1 & run_measured > 0) | (status != "measured" & run_measured == 0)) |>
  add_count(home_id, name = "permits_for_home") |>
  group_by(building_id) |> filter(all(permits_for_home == 1)) |> ungroup() |>
  select(building_id, home_id)
townhouse_review <- home_pairs |> filter(!building_id %in% townhouse_links$building_id) |>
  group_by(building_id) |> summarise(townhouse_candidates = paste(sort(unique(home_id)), collapse = "/"), .groups = "drop")
added <- townhouse_links |> left_join(homes, by = "home_id", relationship = "one-to-one") |>
  group_by(building_id) |>
  summarise(add_ids = paste(home_id, collapse = "/"), add_units = n(), add_sqft = sum(home_sqft), add_land = sum(home_land),
    add_classes = paste(sort(unique(home_classes)), collapse = "/"), add_first = min(home_first_year),
    add_built = min(home_year_built), add_x = mean(x), add_y = mean(y), .groups = "drop")
buildings <- buildings |>
  left_join(added, by = "building_id", relationship = "one-to-one") |>
  left_join(townhouse_review, by = "building_id", relationship = "one-to-one") |>
  filter(!building_id %in% paste0("assessor_residential_", townhouse_links$home_id)) |>
  mutate(townhouse = !is.na(add_ids),
    x_3435 = if_else(townhouse, (coalesce(x_3435 * dwelling_units, 0) + add_x * add_units) / (coalesce(dwelling_units, 0) + add_units), x_3435),
    y_3435 = if_else(townhouse, (coalesce(y_3435 * dwelling_units, 0) + add_y * add_units) / (coalesce(dwelling_units, 0) + add_units), y_3435),
    location_source = if_else(townhouse, "parcel_centroid", location_source),
    record_ids = if_else(townhouse, if_else(is.na(record_ids), add_ids, paste(record_ids, add_ids, sep = "/")), record_ids),
    classes = if_else(townhouse, if_else(is.na(classes), add_classes, paste(classes, add_classes, sep = "/")), classes),
    first_assessment_year = if_else(townhouse, pmin(first_assessment_year, add_first, na.rm = TRUE), first_assessment_year),
    assessor_year_built = if_else(townhouse, pmin(assessor_year_built, add_built, na.rm = TRUE), assessor_year_built),
    building_sqft = if_else(townhouse, coalesce(building_sqft, 0) + add_sqft, building_sqft),
    land_sqft = if_else(townhouse, coalesce(land_sqft, 0) + add_land, land_sqft),
    dwelling_units = if_else(townhouse, coalesce(dwelling_units, 0) + add_units, dwelling_units),
    flags = if_else(townhouse, str_remove(flags, "units_disagree;"), flags),
    match_basis = if_else(townhouse, if_else(is.na(match_basis), "townhouse_lots", paste0(match_basis, "+townhouse_lots")), match_basis),
    source = if_else(townhouse, "residential", source), status = if_else(townhouse, "measured", status),
    multifamily = if_else(townhouse, FALSE, multifamily))

# Assessor-only townhouses: homes on consecutive parcel numbers of a block, first assessed in the same year with the
# same year built, on the same side of the same street, are one building, as a townhouse permit is one row. The row keeps its first home's identifier and sums the homes' measurements.
townhouse_homes <- buildings |>
  filter(route == "assessor_only", source == "residential", classes == "295", dwelling_units == 1, flags == "") |>
  mutate(a = address_parts(address), block = substr(record_ids, 1, 7), parcel = as.integer(substr(record_ids, 8, 10))) |>
  unpack(a, names_sep = "_") |>
  arrange(block, parcel) |> group_by(block) |>
  mutate(next_home = parcel == lag(parcel) + 1L & first_assessment_year == lag(first_assessment_year) &
      assessor_year_built == lag(assessor_year_built) &
      coalesce(a_street == lag(a_street) & a_number %% 2L == lag(a_number) %% 2L, TRUE),
    row_id = building_id[cummax(if_else(coalesce(next_home, FALSE), 0L, row_number()))]) |> ungroup()
townhouse_rows <- townhouse_homes |> group_by(building_id = row_id) |> filter(n() > 1) |>
  summarise(record_ids = paste(record_ids, collapse = "/"), parcel_pin10s = paste(parcel_pin10s, collapse = "/"),
    dwelling_units = n(), building_sqft = sum(building_sqft), land_sqft = sum(land_sqft),
    x_3435 = mean(x_3435), y_3435 = mean(y_3435), .groups = "drop")
buildings <- buildings |>
  filter(!building_id %in% townhouse_homes$building_id[townhouse_homes$building_id != townhouse_homes$row_id]) |>
  rows_update(townhouse_rows, by = "building_id")

# Hand-checked links from adjudication/manual_decisions.csv. Named lots and homes (assign_lot, add_homes,
# replace_homes) joined their permit's parcels in measure_buildings.R; each must now be measured in the row holding its
# permit (as the row's permit, a member or a superseded alternative). Here a permit is another phase of a named
# permit's building (same_building), or has no qualifying candidate (no_match).
permit_rows <- buildings |> filter(route == "permit") |>
  transmute(building_id, permit_number = paste(member_permit_numbers, coalesce(superseded_permit_numbers, ""), sep = "/")) |>
  separate_longer_delim(permit_number, "/") |> filter(permit_number != "") |> distinct()
stopifnot(!anyDuplicated(permit_rows$permit_number))
manual <- read_csv("../adjudication/manual_decisions.csv", col_types = cols(.default = col_character())) |>
  filter(field %in% c("assign_lot", "add_homes", "replace_homes", "same_building", "no_match")) |>
  left_join(permit_rows, by = "permit_number", relationship = "many-to-one")
stopifnot(!anyNA(manual$building_id))
named_records <- manual |> filter(field %in% c("assign_lot", "add_homes", "replace_homes")) |>
  separate_longer_delim(value, "/") |> transmute(building_id, record_id = sub("^assessor_[a-z]+_", "", value))
row_records <- buildings |> filter(!is.na(record_ids)) |> select(building_id, record_id = record_ids) |>
  separate_longer_delim(record_id, "/")
stopifnot(nrow(anti_join(named_records, row_records, by = c("building_id", "record_id"))) == 0)
same_building <- manual |> filter(field == "same_building") |>
  left_join(permit_rows |> rename(target_id = building_id, value = permit_number), by = "value", relationship = "many-to-one") |>
  filter(target_id != building_id) |>
  left_join(buildings |> select(building_id, a_members = member_permit_numbers, a_date = issue_date), by = "building_id",
    relationship = "one-to-one")
stopifnot(!anyNA(same_building$target_id), !any(same_building$target_id %in% same_building$building_id))
buildings <- buildings |> filter(!building_id %in% same_building$building_id) |>
  left_join(same_building |> group_by(building_id = target_id) |>
    summarise(add_members = paste(a_members, collapse = "/"), add_date = min(a_date), .groups = "drop"),
    by = "building_id", relationship = "one-to-one") |>
  mutate(member_permit_numbers = if_else(is.na(add_members), member_permit_numbers, paste(member_permit_numbers, add_members, sep = "/")),
    issue_date = if_else(is.na(add_date), issue_date, pmin(issue_date, add_date)),
    issue_year = as.integer(format(issue_date, "%Y")),
    reviewed = building_id %in% manual$building_id,
    lot_rule_candidates = if_else(reviewed, NA_character_, lot_rule_candidates),
    townhouse_candidates = if_else(reviewed, NA_character_, townhouse_candidates))

# No Assessor record measures two rows of the same building era.
record_rows <- buildings |> filter(!is.na(record_ids), status != "built_after_window") |>
  select(building_id, record_ids, first_assessment_year) |> separate_longer_delim(record_ids, "/")
stopifnot(!anyDuplicated(record_rows[c("record_ids", "first_assessment_year")]))
buildings <- buildings |>
  filter(status != "built_after_window") |>
  mutate(allow_dupac = coalesce(status %in% c("measured", "measured_without_permit") & flags == "" &
      dwelling_units > 0 & land_sqft > 0, FALSE),
    allow_far = allow_dupac & coalesce(building_sqft > 0, FALSE),
    dupac = if_else(allow_dupac, dwelling_units / (land_sqft / 43560), NA_real_),
    far = if_else(allow_far, building_sqft / land_sqft, NA_real_)) |>
  select(building_id, route, status, match_basis, flags, lot_rule_candidates, townhouse_candidates, permit_number,
    member_permit_numbers, superseded_permit_numbers, issue_date, issue_year, address, permit_units, stated_counts,
    permit_status, any_permit_complete, parcel_pin10s, parcel_changed_after_permit, source, record_ids, classes,
    first_assessment_year, assessor_year_built, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac, far, dupac,
    multifamily, location_source, x_3435, y_3435, description) |>
  arrange(route, coalesce(issue_date, make_date(assessor_year_built, 6L, 15L)), building_id)
SaveData(buildings, "building_id", "../output/construction_buildings.csv", na = "")
