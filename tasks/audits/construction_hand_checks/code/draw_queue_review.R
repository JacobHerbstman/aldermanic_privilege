# setwd("tasks/audits/construction_hand_checks/code")
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("../../../shared/code/normalize_chicago_address.R")

buildings <- read_csv("../input/construction_buildings.csv", col_types = cols(building_id = "c", permit_number = "c",
  member_permit_numbers = "c", superseded_permit_numbers = "c", lot_rule_candidates = "c", townhouse_candidates = "c",
  record_ids = "c", parcel_pin10s = "c", .default = col_guess()))
street_key <- function(x) {
  x <- normalize_address(x) |> str_replace_all("\\bPKY\\b", "PKWY") |> str_replace_all("\\bAV\\b", "AVE") |>
    str_replace_all("\\bSAINT\\b", "ST") |>
    str_replace_all("\\b(?:DR )?(?:MARTIN L(?:UTHER)?|M L) KING(?: JR)?\\b", "MARTIN LUTHER KING")
  coalesce(str_match(x, "^([0-9]+ [NSEW] .+?) (?:AVE|ST|RD|BLVD|DR|PL|CT|PKWY|TER|HWY|LN|WAY|SQ|CIR)\\b")[, 2],
    str_extract(x, "^[0-9]+ [NSEW] [A-Z]+(?: [A-Z]+)*"))
}
addresses <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(.default = col_character()),
  col_select = c(pin, pin10, prop_address_full))
pin10_addresses <- addresses |> group_by(pin10) |> summarise(address = first(prop_address_full), .groups = "drop")

# Every permit with several qualifying lots or homes, with each candidate: lots are Assessor-only buildings,
# homes are single-family parcels.
queue <- buildings |> filter(route == "permit", !is.na(lot_rule_candidates) | !is.na(townhouse_candidates)) |>
  mutate(case = if_else(!is.na(townhouse_candidates), "townhouse", "lot"))
lot_candidates <- queue |> filter(case == "lot") |> select(permit_number, candidate = lot_rule_candidates) |>
  separate_longer_delim(candidate, "/") |>
  left_join(buildings |> select(candidate = building_id, record_ids, source, dwelling_units, building_sqft, land_sqft,
    assessor_year_built, first_assessment_year, x_3435, y_3435, other_permits = lot_rule_candidates),
    by = "candidate", relationship = "many-to-one") |>
  mutate(pin10 = substr(record_ids, 1, 10))
home_candidates <- queue |> filter(case == "townhouse") |>
  transmute(permit_number, candidate = paste(coalesce(record_ids, ""), townhouse_candidates, sep = "/"),
    measured = coalesce(record_ids, "")) |>
  separate_longer_delim(candidate, "/") |> filter(candidate != "") |>
  mutate(measured_home = map2_lgl(candidate, measured, \(x, m) x %in% str_split_1(m, "/")), pin10 = substr(candidate, 1, 10))
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "home_pins", home_candidates |> distinct(pin = candidate))
home_records <- DBI::dbGetQuery(con, "
  SELECT pin, min(tax_year) FILTER (WHERE year_built >= 2004) AS first_year,
    arg_min(year_built, tax_year) FILTER (WHERE year_built >= 2004) AS year_built,
    arg_min(building_sqft, tax_year) FILTER (WHERE year_built >= 2004) AS sqft,
    arg_min(land_sqft, tax_year) FILTER (WHERE year_built >= 2004) AS land, arg_min(class, tax_year) FILTER (WHERE year_built >= 2004) AS class
  FROM read_parquet('../input/residential_assessor_history.parquet') WHERE pin IN (SELECT pin FROM home_pins) GROUP BY 1")
DBI::dbDisconnect(con, shutdown = TRUE)

# Permits without a measured building are located at their geocoded permit point.
permit_points <- queue |> transmute(permit_number, px = x_3435, py = y_3435)
candidates <- bind_rows(
    lot_candidates |> left_join(pin10_addresses, by = "pin10", relationship = "many-to-one") |>
      left_join(permit_points, by = "permit_number", relationship = "many-to-one") |>
      transmute(permit_number, candidate, address, entry = paste0(candidate, " at ", coalesce(address, "no address"), ": ",
        source, ", ", dwelling_units, " units, ", building_sqft, " sq ft, land ", land_sqft, ", built ", assessor_year_built,
        ", first assessed ", first_assessment_year, ", ", round(sqrt((x_3435 - px)^2 + (y_3435 - py)^2)), " ft away",
        if_else(is.na(other_permits), "", paste0(", also qualifies for ", other_permits)))),
    home_candidates |> left_join(addresses |> select(candidate = pin, address = prop_address_full), by = "candidate",
        relationship = "many-to-one") |>
      left_join(home_records, by = c("candidate" = "pin"), relationship = "many-to-one") |>
      transmute(permit_number, candidate, address, entry = paste0(if_else(measured_home, "MEASURED ", "candidate "),
        candidate, " at ", coalesce(address, "no address"), ": class ", class, ", built ", year_built, ", first assessed ",
        first_year, ", ", sqft, " sq ft, land ", land)))

# Every permit on record at the permit's address and at each candidate's address, oldest first.
keys <- unique(na.omit(c(street_key(queue$address), street_key(candidates$address))))
permits <- read_csv("../input/building_permits_full.csv", col_types = cols(.default = col_character()),
    col_select = c(permit_ = permit_, permit_type, issue_date, street_number, street_direction, street_name, work_description)) |>
  mutate(street = street_key(paste(str_remove(street_number, "^0+"), street_direction, street_name))) |>
  filter(street %in% keys) |> arrange(issue_date) |>
  mutate(entry = paste0(substr(issue_date, 1, 10), " ", permit_, " ", str_remove(permit_type, "^PERMIT - "), ": ",
    str_sub(str_squish(coalesce(work_description, "")), 1, 80))) |>
  group_by(street) |> summarise(permits = paste(entry, collapse = " | "), .groups = "drop")
candidate_permits <- candidates |> mutate(street = street_key(address)) |> filter(!is.na(street)) |>
  distinct(permit_number, street) |> inner_join(permits, by = "street", relationship = "many-to-one") |>
  group_by(permit_number) |> summarise(permits_at_candidate_addresses = paste0("[", street, "] ", permits, collapse = " || "),
    .groups = "drop")

review <- queue |>
  mutate(street = street_key(address)) |>
  left_join(permits |> rename(permits_at_permit_address = permits), by = "street", relationship = "many-to-one",
    na_matches = "never") |>
  left_join(candidates |> group_by(permit_number) |> summarise(candidates = paste(entry, collapse = " | "), .groups = "drop"),
    by = "permit_number", relationship = "one-to-one") |>
  left_join(candidate_permits, by = "permit_number", relationship = "one-to-one") |>
  transmute(case, permit_number, member_permit_numbers, issue_date, permit_status, address, permit_units, status,
    dwelling_units, description, candidates, permits_at_permit_address, permits_at_candidate_addresses) |>
  arrange(case, permit_number)
SaveData(review, "permit_number", "../output/queue_review.csv", na = "")
