# setwd("tasks/prepare_permit_construction/code")
# first_year_built <- 2006
# last_year_built <- 2022
# last_unpermitted_year_built <- 2007
# assessor_year_lead <- 2
# min_sqft_per_unit <- 300
# max_land_sqft_per_unit <- 43560
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
source("../../shared/code/assessor_classification.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(first_year_built, last_year_built, last_unpermitted_year_built, assessor_year_lead,
  min_sqft_per_unit, max_land_sqft_per_unit)
stopifnot(length(args) == 6L)
first_year_built <- as.integer(args[1])
last_year_built <- as.integer(args[2])
last_unpermitted_year_built <- as.integer(args[3])
assessor_year_lead <- as.integer(args[4])
min_sqft_per_unit <- as.numeric(args[5])
max_land_sqft_per_unit <- as.numeric(args[6])

permit_buildings <- read_csv("../output/permit_buildings.csv", col_types = cols(building_id = "c", permit_number = "c",
  member_permit_numbers = "c", superseded_permit_numbers = "c", parcel_pin10s = "c", record_ids = "c", issue_date = "D",
  .default = col_guess()))

# Parcels that already belong to the permit arm: measured records, and every parcel of a new residential permit.
claimed_records <- permit_buildings |> filter(status == "measured") |> select(record_ids) |>
  separate_longer_delim(record_ids, "/") |> distinct(record_id = record_ids)
permitted_parcels <- permit_buildings |> select(issue_year, parcel_pin10s) |> filter(!is.na(parcel_pin10s)) |>
  separate_longer_delim(parcel_pin10s, "/") |> group_by(pin10 = parcel_pin10s) |>
  summarise(permit_years = paste(sort(unique(issue_year)), collapse = "/"), .groups = "drop")

con <- DBI::dbConnect(duckdb::duckdb())

# Residential parcels whose records first show a card built in the window, persisting the next year, after either
# no record or a record without it. The building is measured in that first year, as in the permit arm.
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
  SELECT h.pin, substr(h.pin, 1, 10) AS pin10, h.tax_year, h.card_num, h.class, h.year_built, h.building_sqft, h.land_sqft,
    h.num_apartments, h.pin_proration_rate, h.proration_key_pin, coalesce(h.year_built >= %d, false) AS new_card, prior.prior_sqft
  FROM checked c JOIN h ON h.pin = c.pin AND h.tax_year = c.first_year LEFT JOIN prior ON prior.pin = c.pin
  WHERE c.persists", first_year_built - assessor_year_lead, first_year_built - assessor_year_lead,
  first_year_built - assessor_year_lead)) |>
  group_by(pin) |> mutate(old_card_on_parcel = any(!new_card)) |> filter(new_card) |>
  # A reported year built that changes while the parcel keeps last year's floor area is a revised record, not a building.
  mutate(same_floor_area = coalesce(abs(first(prior_sqft) - sum(building_sqft)) <= 1, FALSE)) |> ungroup() |>
  mutate(record_id = if_else(coalesce(pin_proration_rate, 1) < 1 & !is.na(proration_key_pin), proration_key_pin, pin),
    card_key = paste(record_id, card_num),
    card_units = case_when(class %in% single_family_assessor_classes ~ 1, num_apartments > 0 ~ num_apartments))
residential <- cards |> group_by(record_id) |> summarise(
    pin10s = paste(sort(unique(pin10)), collapse = "/"), first_year = min(tax_year), year_built = min(year_built),
    classes = paste(sort(unique(class)), collapse = "/"),
    units = sum(card_units[!duplicated(card_key)]), building_sqft = sum(building_sqft[!duplicated(card_key)]),
    land_sqft = sum(land_sqft[!duplicated(pin)]), older_building = any(old_card_on_parcel),
    same_floor_area = any(same_floor_area), single_family = all(class %in% single_family_assessor_classes), .groups = "drop") |>
  mutate(source = "residential")

# Condominium buildings first appearing with a reported year built in the window.
condominiums <- DBI::dbGetQuery(con, sprintf("
  WITH c AS (
    SELECT pin10, try_cast(try_cast(year AS DOUBLE) AS INTEGER) AS tax_year, is_parking_space, is_common_area,
      try_cast(char_building_sf AS DOUBLE) AS building_sqft, try_cast(char_land_sf AS DOUBLE) AS land_sqft,
      try_cast(try_cast(char_yrblt AS DOUBLE) AS INTEGER) AS year_built
    FROM read_csv('../input/condominium_characteristics.csv', all_varchar = true)),
  first_year AS (SELECT pin10, min(tax_year) AS first_year FROM c WHERE year_built >= %d GROUP BY pin10)
  SELECT c.pin10 AS record_id, c.pin10 AS pin10s, c.tax_year AS first_year, min(c.year_built) AS year_built,
    count(*) FILTER (WHERE c.is_parking_space <> 'true' AND c.is_common_area <> 'true') AS units,
    max(c.building_sqft) AS building_sqft, max(c.land_sqft) AS land_sqft
  FROM c JOIN first_year f ON c.pin10 = f.pin10 AND c.tax_year = f.first_year
  GROUP BY 1, 2, 3", first_year_built - assessor_year_lead)) |>
  mutate(classes = "299", older_building = FALSE, single_family = FALSE, source = "condominium")
DBI::dbDisconnect(con, shutdown = TRUE)

# Commercial apartment valuations (2021 onward) of buildings reported built in the window.
commercial <- read_csv("../input/commercial_valuation_data.csv", col_types = cols(.default = col_character()),
  col_select = c(keypin, pins, year, class_es, tot_units, bldgsf, landsf, yearbuilt)) |>
  mutate(across(c(year, tot_units, bldgsf, landsf, yearbuilt), as.numeric), record_id = str_remove_all(keypin, "-")) |>
  filter(tot_units > 0, yearbuilt >= first_year_built - assessor_year_lead) |>
  group_by(record_id) |> filter(year == min(year)) |> ungroup() |>
  mutate(pin10s = map_chr(str_extract_all(str_remove_all(pins, "-"), "[0-9]{14}"), \(x) paste(sort(unique(substr(x, 1, 10))), collapse = "/"))) |>
  distinct(record_id, pin10s, first_year = year, year_built = yearbuilt, classes = class_es,
    units = tot_units, building_sqft = bldgsf, land_sqft = landsf) |>
  # A valuation reporting different measurements in the same year has no usable measurement.
  group_by(record_id) |> mutate(across(c(units, building_sqft, land_sqft), \(x) if (n() > 1) NA_real_ else x)) |>
  slice(1) |> ungroup() |>
  mutate(older_building = FALSE, single_family = FALSE, source = "commercial")
stopifnot(!anyDuplicated(commercial$record_id), !anyDuplicated(residential$record_id), !anyDuplicated(condominiums$record_id))

# Assessor-only buildings: in the window, not measured by a permit, and on no parcel of a new residential permit
# issued within the lead years before the building appears. Condominium records take a parcel before residential
# cards, and both before commercial valuations, as in the permit arm.
candidates <- bind_rows(condominiums, residential, commercial) |>
  filter(year_built >= first_year_built, year_built <= last_year_built, !record_id %in% claimed_records$record_id) |>
  mutate(priority = match(source, c("condominium", "residential", "commercial")))
parcel_candidates <- candidates |> select(source, record_id, priority, first_year, pin10s) |>
  separate_longer_delim(pin10s, "/") |> rename(pin10 = pin10s) |>
  left_join(permitted_parcels, by = "pin10", relationship = "many-to-one") |>
  group_by(pin10) |> mutate(best_priority = min(priority)) |> ungroup() |>
  group_by(source, record_id) |> summarise(
    permitted = any(map2_lgl(permit_years, first_year, \(years, year) {
      years <- as.integer(str_split_1(coalesce(years, ""), "/"))
      any(years <= year & years >= year - assessor_year_lead - 2, na.rm = TRUE)
    })),
    outranked = any(priority > best_priority), .groups = "drop")
addresses <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(.default = col_character()),
  col_select = c(pin10, prop_address_full)) |> group_by(pin10) |> summarise(address = first(prop_address_full), .groups = "drop")
unpermitted <- candidates |> left_join(parcel_candidates, by = c("source", "record_id"), relationship = "one-to-one") |>
  filter(!permitted, !outranked) |>
  mutate(pin10 = substr(pin10s, 1, 10)) |> left_join(addresses, by = "pin10", relationship = "many-to-one") |>
  mutate(
    building_id = paste0("assessor_", source, "_", record_id), route = "assessor_only",
    status = if_else(year_built <= last_unpermitted_year_built, "measured_without_permit", "no_permit_found"),
    flags = str_c(
      if_else(building_sqft / units < min_sqft_per_unit, "area_per_unit_implausible;", "", ""),
      if_else(land_sqft / units > max_land_sqft_per_unit, "land_per_unit_implausible;", "", ""),
      if_else(older_building, "older_building_on_parcel;", "", ""),
      if_else(same_floor_area %in% TRUE, "year_built_revised_same_floor_area;", "", "")),
    allow_dupac = coalesce(status == "measured_without_permit" & flags == "" & units > 0 & land_sqft > 0, FALSE),
    allow_far = allow_dupac & coalesce(building_sqft > 0, FALSE)) |>
  transmute(building_id, route, status, flags, address, source, record_ids = record_id, parcel_pin10s = pin10s, classes,
    first_assessment_year = first_year, assessor_year_built = year_built, dwelling_units = units, building_sqft, land_sqft,
    allow_far, allow_dupac, multifamily = units >= 2 & !single_family)

# One row per building: every new residential permit's outcome, and every Assessor-only building.
buildings <- bind_rows(
  permit_buildings |> mutate(route = "permit") |> select(-far, -dupac),
  unpermitted) |>
  mutate(dupac = if_else(allow_dupac, dwelling_units / (land_sqft / 43560), NA_real_),
    far = if_else(allow_far, building_sqft / land_sqft, NA_real_))

# Each building sits at the centroid of its measured parcels; permit coordinates are geocoded at the street frontage.
centroids <- read_csv("../input/parcel_universe_2025_city.csv", col_types = cols(pin10 = "c", .default = "d"),
    col_select = c(pin10, centroid_x_crs_3435, centroid_y_crs_3435)) |>
  filter(is.finite(centroid_x_crs_3435), is.finite(centroid_y_crs_3435)) |>
  group_by(pin10) |> summarise(x = mean(centroid_x_crs_3435), y = mean(centroid_y_crs_3435), .groups = "drop")
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
    x_3435 = coalesce(parcel_x, permit_x), y_3435 = coalesce(parcel_y, permit_y)) |>
  select(building_id, route, status, flags, permit_number, member_permit_numbers, superseded_permit_numbers, issue_date,
    issue_year, address, permit_units, stated_counts, permit_status, any_permit_complete, parcel_pin10s, source, record_ids,
    classes, first_assessment_year, assessor_year_built, dwelling_units, building_sqft, land_sqft, allow_far, allow_dupac,
    far, dupac, multifamily, location_source, x_3435, y_3435, description) |>
  arrange(route, coalesce(issue_date, make_date(assessor_year_built, 6L, 15L)), building_id)
SaveData(buildings, "building_id", "../output/construction_buildings.csv", na = "")
