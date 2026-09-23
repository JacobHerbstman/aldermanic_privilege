# setwd("tasks/prepare_permit_construction/code")
# assessor_year_lead <- 2
# max_build_lag_years <- 4
# unit_tolerance <- 0.2
# min_sqft_per_unit <- 300
# max_land_sqft_per_unit <- 43560
# rebuilt_area_growth <- 0.25
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/normalize_chicago_address.R")
source("../../shared/code/street_key.R")
source("../../shared/code/assessor_classification.R")
source("construction_rules.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(assessor_year_lead, max_build_lag_years, unit_tolerance, min_sqft_per_unit, max_land_sqft_per_unit,
  rebuilt_area_growth)
stopifnot(length(args) == 6L)
assessor_year_lead <- as.integer(args[1])
max_build_lag_years <- as.integer(args[2])
unit_tolerance <- as.numeric(args[3])
min_sqft_per_unit <- as.numeric(args[4])
max_land_sqft_per_unit <- as.numeric(args[5])
rebuilt_area_growth <- as.numeric(args[6])

permits <- read_csv("../output/construction_permits.csv",
  col_types = cols(permit_id = "c", permit_number = "c", permit_pin10s = "c", permit_units = "i", .default = col_guess())) |>
  filter(scope %in% c("new_residential", "building_use_not_stated"))

# Parcels: every PIN listed on the permit, and every 2025 parcel at the permit's house number and street.
street_parcels <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(.default = col_character()),
  col_select = c(pin10, prop_address_full)) |>
  mutate(street = street_key(prop_address_full)) |> filter(!is.na(street)) |>
  group_by(street) |> summarise(address_pin10s = paste(sort(unique(pin10)), collapse = "/"), .groups = "drop")
parcels <- bind_rows(
  permits |> select(permit_id, pin10 = permit_pin10s) |> separate_longer_delim(pin10, "/") |>
    filter(!is.na(pin10), pin10 != "") |> mutate(permit_pin = TRUE),
  permits |> transmute(permit_id, street = street_key(address)) |>
    inner_join(street_parcels, by = "street", relationship = "many-to-one") |>
    separate_longer_delim(address_pin10s, "/") |> transmute(permit_id, pin10 = address_pin10s, permit_pin = FALSE)) |>
  group_by(permit_id, pin10) |> summarise(permit_pin = any(permit_pin), .groups = "drop") |>
  left_join(permits |> select(permit_id, issue_year), by = "permit_id", relationship = "many-to-one")

con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "parcels", parcels)

# Residential cards are new when their reported year built is between the permit year minus the lead and the permit
# year plus the longest construction lag.
# A parcel already showing a new card before the permit year holds an earlier building, not this one.
# Otherwise each parcel is measured in the first tax year after the permit that shows a new card.
cards <- DBI::dbGetQuery(con, sprintf("
  SELECT p.permit_id, p.permit_pin, p.issue_year, h.pin, h.tax_year, h.card_num, h.class, h.year_built, h.building_sqft,
    h.land_sqft, h.num_apartments, h.pin_proration_rate, h.proration_key_pin,
    coalesce(h.year_built BETWEEN p.issue_year - %d AND p.issue_year + %d, false) AS new_card,
    h.tax_year < p.issue_year AS before_permit
  FROM parcels p JOIN read_parquet('../input/residential_assessor_history.parquet') h ON substr(h.pin, 1, 10) = p.pin10",
  assessor_year_lead, max_build_lag_years)) |>
  group_by(permit_id, pin) |> mutate(predates_permit = any(new_card & before_permit)) |> ungroup()
predates <- cards |> filter(predates_permit) |> distinct(permit_id)
# A parcel still holding a building of a different floor area in its latest record may hold the new building under
# an old reported year built. A demolished building leaves no later record.
parcel_changes <- cards |> group_by(permit_id, pin, tax_year, before_permit) |>
  summarise(sqft = sum(building_sqft, na.rm = TRUE), .groups = "drop") |> arrange(permit_id, pin, tax_year) |>
  group_by(permit_id, pin) |> filter(any(before_permit), any(!before_permit)) |>
  summarise(changed = last(sqft) > 0 & abs(last(sqft) - last(sqft[before_permit])) > 1 &
    max(tax_year) >= max(cards$tax_year) - 1, .groups = "drop") |>
  group_by(permit_id) |> summarise(parcel_changed_after_permit = any(changed), .groups = "drop")
# Some new buildings keep the old reported year built: a parcel without a new card whose floor area first rises by
# REBUILT_AREA_GROWTH within the construction lag and keeps it is measured in that year.
rebuilt_parcels <- cards |> group_by(permit_id, pin) |> filter(!any(new_card), any(before_permit)) |>
  group_by(permit_id, pin, issue_year, tax_year, before_permit) |>
  summarise(sqft = sum(building_sqft, na.rm = TRUE), .groups = "drop") |> arrange(permit_id, pin, tax_year) |>
  group_by(permit_id, pin) |>
  mutate(threshold = (1 + rebuilt_area_growth) * last(sqft[before_permit]), latest_sqft = last(sqft)) |>
  filter(!before_permit, tax_year <= issue_year + max_build_lag_years, sqft > 0, sqft >= threshold, latest_sqft >= threshold) |>
  summarise(tax_year = min(tax_year), .groups = "drop") |> mutate(rebuilt = TRUE)
cards <- cards |> left_join(rebuilt_parcels, by = c("permit_id", "pin", "tax_year"), relationship = "many-to-one") |>
  mutate(rebuilt = coalesce(rebuilt, FALSE), new_card = new_card | rebuilt)
cards <- cards |> filter(!predates_permit, !before_permit) |>
  group_by(permit_id, pin) |> filter(any(new_card)) |> filter(tax_year == min(tax_year[new_card])) |>
  mutate(old_card_on_parcel = any(!new_card)) |> filter(new_card) |> ungroup()

# A building on prorated parcels repeats its characteristics on each parcel; land is reported per parcel.
residential <- cards |>
  mutate(record_id = if_else(coalesce(pin_proration_rate, 1) < 1 & !is.na(proration_key_pin), proration_key_pin, pin),
    card_key = paste(record_id, card_num),
    card_units = case_when(class %in% single_family_assessor_classes ~ 1, num_apartments > 0 ~ num_apartments)) |>
  group_by(permit_id, record_id) |> summarise(
    permit_pin = any(permit_pin), first_year = min(tax_year), year_built = min(year_built),
    classes = paste(sort(unique(class)), collapse = "/"),
    units = sum(card_units[!duplicated(card_key)]), building_sqft = sum(building_sqft[!duplicated(card_key)]),
    land_sqft = sum(land_sqft[!duplicated(pin)]), older_building = any(old_card_on_parcel),
    single_family = all(class %in% single_family_assessor_classes), rebuilt = any(rebuilt), .groups = "drop") |>
  mutate(source = "residential")

# Condominium buildings: the first year after the permit in which a new building's unit parcels appear.
condominiums <- DBI::dbGetQuery(con, sprintf("
  SELECT p.permit_id, p.permit_pin, p.issue_year, c.pin10 AS record_id, try_cast(try_cast(c.year AS DOUBLE) AS INTEGER) AS first_year,
    count(*) FILTER (WHERE c.is_parking_space <> 'true' AND c.is_common_area <> 'true') AS units,
    max(try_cast(c.char_building_sf AS DOUBLE)) AS building_sqft, max(try_cast(c.char_land_sf AS DOUBLE)) AS land_sqft,
    min(try_cast(try_cast(c.char_yrblt AS DOUBLE) AS INTEGER)) AS year_built
  FROM parcels p JOIN read_csv('../input/condominium_characteristics.csv', all_varchar = true) c ON c.pin10 = p.pin10
  GROUP BY 1, 2, 3, 4, 5
  HAVING min(try_cast(try_cast(c.char_yrblt AS DOUBLE) AS INTEGER)) BETWEEN p.issue_year - %d AND p.issue_year + %d",
  assessor_year_lead, max_build_lag_years)) |>
  group_by(permit_id, record_id) |> mutate(predates_permit = any(first_year < issue_year)) |> ungroup()
predates <- bind_rows(predates, condominiums |> filter(predates_permit) |> distinct(permit_id))
condominiums <- condominiums |> filter(!predates_permit) |>
  group_by(permit_id, record_id) |> filter(first_year == min(first_year)) |> ungroup() |>
  transmute(permit_id, record_id, permit_pin, first_year, year_built, classes = "299", units, building_sqft, land_sqft,
    older_building = FALSE, single_family = FALSE, rebuilt = FALSE, source = "condominium")

# Commercial apartment valuations (2021 onward): the earliest valuation of a new building with dwelling units.
commercial <- read_commercial_valuations() |> separate_longer_delim(pin10s, "/") |> rename(pin10 = pin10s) |>
  inner_join(parcels |> group_by(pin10) |> summarise(permit_id = paste(permit_id, collapse = "/"), .groups = "drop"),
    by = "pin10", relationship = "many-to-one") |>
  separate_longer_delim(permit_id, "/") |>
  left_join(parcels |> select(permit_id, pin10, permit_pin, issue_year), by = c("permit_id", "pin10"), relationship = "many-to-one") |>
  filter(year_built >= issue_year - assessor_year_lead, year_built <= issue_year + max_build_lag_years) |>
  group_by(permit_id, record_id) |> mutate(permit_pin = any(permit_pin)) |> filter(year == min(year)) |> ungroup() |>
  distinct(permit_id, record_id, permit_pin, first_year = year, year_built, classes, units, building_sqft, land_sqft) |>
  # A valuation reporting different measurements in the same year has no usable measurement.
  group_by(permit_id, record_id) |> mutate(across(c(units, building_sqft, land_sqft), \(x) if (n() > 1) NA_real_ else x)) |>
  slice(1) |> ungroup() |>
  mutate(older_building = FALSE, single_family = FALSE, rebuilt = FALSE, source = "commercial")
stopifnot(!anyDuplicated(commercial[c("permit_id", "record_id")]))
DBI::dbDisconnect(con, shutdown = TRUE)

# Evidence for a permit comes from its listed parcels when they show a new building, otherwise from its address.
# One source per permit, never mixed: condominium records describe the residential building above any shop.
records <- bind_rows(condominiums, residential, commercial) |>
  group_by(permit_id) |> filter(permit_pin | !any(permit_pin)) |>
  filter(match(source, c("condominium", "residential", "commercial")) ==
    min(match(source, c("condominium", "residential", "commercial")))) |> ungroup() |>
  left_join(permits |> select(permit_id, issue_date, issue_year, address, scope), by = "permit_id", relationship = "many-to-one")
# A permit that does not state a residential use measures only buildings that no residential permit reaches.
residential_claims <- records |> filter(scope == "new_residential") |> distinct(source, record_id, first_year)
records <- records |> anti_join(residential_claims |> mutate(scope = "building_use_not_stated"),
  by = c("source", "record_id", "first_year", "scope"))

# Permits at one address that reach the same new building are alternative authorizations:
# the latest permit issued before the building first appears keeps it.
keepers <- records |> group_by(source, record_id, first_year, address) |>
  mutate(keeper = permit_id[order(issue_year > first_year, desc(issue_date), permit_id)][1]) |> ungroup()
superseded <- keepers |> filter(permit_id != keeper) |> distinct(permit_id, keeper)
records <- records |> filter(!permit_id %in% superseded$permit_id)

# Permits at different addresses that reach the same new building are parts of one development.
links <- records |> transmute(permit_id, record = paste(source, record_id, first_year))
components <- igraph::components(igraph::graph_from_data_frame(links, directed = FALSE))$membership
groups <- tibble(permit_id = unique(links$permit_id), group = components[unique(links$permit_id)]) |>
  left_join(permits |> select(permit_id, permit_number, issue_date, permit_units), by = "permit_id", relationship = "one-to-one") |>
  group_by(group) |> arrange(issue_date, permit_id, .by_group = TRUE) |>
  mutate(building_id = first(permit_id), member_permit_numbers = paste(permit_number, collapse = "/"),
    group_permit_units = sum(permit_units)) |> ungroup()
buildings <- records |> left_join(groups |> select(permit_id, building_id), by = "permit_id", relationship = "many-to-one") |>
  distinct(building_id, source, record_id, first_year, .keep_all = TRUE) |>
  group_by(building_id) |> summarise(source = first(source),
    record_ids = paste(sort(record_id), collapse = "/"), classes = paste(sort(unique(unlist(str_split(classes, "/")))), collapse = "/"),
    first_assessment_year = min(first_year), assessor_year_built = min(year_built),
    dwelling_units = sum(units), building_sqft = sum(building_sqft), land_sqft = sum(land_sqft),
    older_building = any(older_building), single_family = all(single_family), rebuilt = any(rebuilt), .groups = "drop") |>
  left_join(groups |> filter(permit_id == building_id) |> select(building_id, member_permit_numbers, group_permit_units),
    by = "building_id", relationship = "one-to-one") |>
  left_join(superseded |> left_join(groups |> select(permit_id, building_id), by = c("keeper" = "permit_id"),
      relationship = "many-to-one") |>
    left_join(permits |> select(permit_id, permit_number), by = "permit_id", relationship = "one-to-one") |>
    group_by(building_id) |> summarise(superseded_permit_numbers = paste(sort(permit_number), collapse = "/"), .groups = "drop"),
    by = "building_id", relationship = "one-to-one")

# Measurement decisions apply here; building links (assign_lot and the others) apply in build_construction_buildings.R.
decisions <- read_csv("../adjudication/manual_decisions.csv", col_types = cols(.default = col_character()))
stopifnot(!anyDuplicated(decisions[c("permit_number", "field")]),
  all(decisions$field %in% c("exclude", "accept", "dwelling_units", "building_sqft", "land_sqft",
    "assign_lot", "add_homes", "replace_homes", "same_building", "no_match")),
  !anyNA(decisions$source), all(decisions$permit_number %in% permits$permit_number))
decisions <- decisions |> filter(field %in% c("exclude", "accept", "dwelling_units", "building_sqft", "land_sqft")) |>
  select(permit_number, field, value) |>
  pivot_wider(names_from = field, values_from = value, names_prefix = "manual_")
for (field in c("manual_exclude", "manual_accept", "manual_dwelling_units", "manual_building_sqft", "manual_land_sqft")) {
  if (!field %in% names(decisions)) decisions[[field]] <- NA_character_
}

# One row per measured building (identified by its first permit) and per residential permit that reached no building.
buildings <- permits |>
  filter(!permit_id %in% superseded$permit_id, !permit_id %in% setdiff(groups$permit_id, groups$building_id),
    scope == "new_residential" | permit_id %in% groups$building_id) |>
  left_join(parcels |> group_by(permit_id) |> summarise(parcel_pin10s = paste(sort(unique(pin10)), collapse = "/"), .groups = "drop"),
    by = "permit_id", relationship = "one-to-one") |>
  left_join(buildings, by = c("permit_id" = "building_id"), relationship = "one-to-one") |>
  left_join(parcel_changes, by = "permit_id", relationship = "one-to-one") |>
  left_join(decisions, by = "permit_number", relationship = "one-to-one") |>
  mutate(
    permit_units = if_else(is.na(source), permit_units, group_permit_units),
    parcel_changed_after_permit = coalesce(parcel_changed_after_permit, FALSE),
    member_permit_numbers = coalesce(member_permit_numbers, permit_number),
    dwelling_units = coalesce(as.numeric(manual_dwelling_units), dwelling_units),
    building_sqft = coalesce(as.numeric(manual_building_sqft), building_sqft),
    land_sqft = coalesce(as.numeric(manual_land_sqft), land_sqft),
    status = case_when(
      manual_exclude %in% "TRUE" ~ "manual_exclusion",
      !is.na(source) ~ "measured",
      is.na(parcel_pin10s) ~ "no_parcel",
      permit_id %in% predates$permit_id ~ "building_predates_permit",
      TRUE ~ "no_new_building"),
    match_basis = case_when(rebuilt ~ "floor_area_change", status == "measured" ~ "permit_parcels"),
    flags = if_else(status != "measured" | manual_accept %in% "TRUE", "", str_c(
      if_else(units_agree(dwelling_units, permit_units, unit_tolerance) %in% FALSE, "units_disagree;", ""),
      if_else(building_sqft / dwelling_units < min_sqft_per_unit, "area_per_unit_implausible;", "", ""),
      if_else(land_sqft / dwelling_units > max_land_sqft_per_unit, "land_per_unit_implausible;", "", ""),
      if_else(older_building, "older_building_on_parcel;", "", ""))),
    allow_dupac = coalesce(status == "measured" & flags == "" & dwelling_units > 0 & land_sqft > 0, FALSE),
    allow_far = allow_dupac & coalesce(building_sqft > 0, FALSE),
    dupac = if_else(allow_dupac, dwelling_units / (land_sqft / 43560), NA_real_),
    far = if_else(allow_far, building_sqft / land_sqft, NA_real_),
    multifamily = dwelling_units >= 2 & !single_family) |>
  select(building_id = permit_id, permit_number, member_permit_numbers, superseded_permit_numbers, issue_date, issue_year,
    address, latitude, longitude, permit_units, stated_counts, permit_status, any_permit_complete, parcel_pin10s,
    parcel_changed_after_permit, status,
    source, record_ids, classes, first_assessment_year, assessor_year_built, dwelling_units, building_sqft, land_sqft,
    match_basis, flags, allow_far, allow_dupac, far, dupac, multifamily, description) |>
  arrange(issue_date, building_id)

# A permit that reached no building, followed by a measured permit on one of its parcels or at its address, was not
# built under this permit.
built_later <- bind_rows(
    buildings |> filter(status == "measured") |> select(later_date = issue_date, key = parcel_pin10s) |>
      separate_longer_delim(key, "/"),
    buildings |> filter(status == "measured") |> select(later_date = issue_date, key = address)) |>
  filter(!is.na(key)) |> group_by(key) |> summarise(later_date = max(later_date), .groups = "drop")
not_built <- bind_rows(
    buildings |> filter(status %in% c("no_new_building", "no_parcel")) |> select(building_id, issue_date, key = parcel_pin10s) |>
      separate_longer_delim(key, "/"),
    buildings |> filter(status %in% c("no_new_building", "no_parcel")) |> select(building_id, issue_date, key = address)) |>
  inner_join(built_later, by = "key", relationship = "many-to-one") |> filter(later_date > issue_date) |> distinct(building_id)
buildings <- buildings |> mutate(status = if_else(building_id %in% not_built$building_id, "later_permit_built", status))
SaveData(buildings, "building_id", "../output/permit_buildings.csv", na = "")
