# setwd("tasks/audits/construction_hand_checks/code")
# review_seed <- 20260922
# sample_size <- 40
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("../../../shared/code/normalize_chicago_address.R")
source("../../../shared/code/street_key.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(review_seed, sample_size)
stopifnot(length(args) == 2L)
review_seed <- as.integer(args[1])
sample_size <- as.integer(args[2])

buildings <- read_csv("../input/construction_buildings.csv", col_types = cols(building_id = "c", permit_number = "c",
  member_permit_numbers = "c", superseded_permit_numbers = "c", lot_rule_candidates = "c", record_ids = "c",
  parcel_pin10s = "c", .default = col_guess()))
addresses <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(.default = col_character()),
  col_select = c(pin10, prop_address_full)) |> group_by(pin10) |> summarise(lot_address = first(prop_address_full), .groups = "drop")

# Cases: a seeded random sample of lot-rule links not reviewed before, and every permit with several qualifying lots.
reviewed <- read_csv("../lot_link_reviews.csv", col_types = cols(.default = col_character()))
set.seed(review_seed)
links <- buildings |> filter(match_basis == "nearby_lot", !permit_number %in% reviewed$permit_number) |>
  slice_sample(n = sample_size) |> mutate(case = "lot_link_sample")
queue <- buildings |> filter(route == "permit", !is.na(lot_rule_candidates)) |> mutate(case = "ambiguous_permit")
cases <- bind_rows(links, queue) |>
  mutate(lot_pin10 = if_else(case == "lot_link_sample", substr(record_ids, 1, 10), NA_character_)) |>
  left_join(addresses, by = c("lot_pin10" = "pin10"), relationship = "many-to-one")

# Every permit on record at the permit's address and at the linked lot's address, oldest first.
permits <- read_csv("../input/building_permits_full.csv", col_types = cols(.default = col_character()),
    col_select = c(permit_ = permit_, permit_type, issue_date, street_number, street_direction, street_name, work_description)) |>
  mutate(street = street_key(paste(str_remove(street_number, "^0+"), street_direction, street_name)),
    entry = paste0(substr(issue_date, 1, 10), " ", permit_, " ", str_remove(permit_type, "^PERMIT - "), ": ",
      str_sub(str_squish(coalesce(work_description, "")), 1, 90))) |>
  filter(!is.na(street), street %in% c(street_key(cases$address), street_key(cases$lot_address))) |>
  arrange(issue_date) |> group_by(street) |> summarise(permits = paste(entry, collapse = " | "), .groups = "drop")

# The lot's residential Assessor history, one entry per change in class, year built, floor area or apartments.
lot_pins <- cases |> filter(!is.na(lot_pin10)) |> distinct(lot_pin10)
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "lot_pins", lot_pins)
history <- DBI::dbGetQuery(con, "
  SELECT substr(pin, 1, 10) AS lot_pin10, tax_year, string_agg(DISTINCT class, '+') AS class,
    min(year_built) AS year_built, sum(building_sqft) AS sqft, sum(num_apartments) AS apartments
  FROM read_parquet('../input/residential_assessor_history.parquet') WHERE substr(pin, 1, 10) IN (SELECT lot_pin10 FROM lot_pins)
  GROUP BY 1, 2 ORDER BY 1, 2")
DBI::dbDisconnect(con, shutdown = TRUE)
history <- history |> mutate(state = paste(class, year_built, sqft, apartments)) |>
  group_by(lot_pin10) |> filter(state != lag(state, default = "")) |>
  summarise(lot_history = paste0(tax_year, ": class ", class, ", built ", year_built, ", ", sqft, " sq ft", collapse = " | "),
    .groups = "drop")

points <- st_as_sf(cases |> filter(is.finite(x_3435)), coords = c("x_3435", "y_3435"), crs = 3435) |> st_transform(4326)
review <- cases |>
  left_join(tibble(building_id = points$building_id, lat = st_coordinates(points)[, 2], lon = st_coordinates(points)[, 1]),
    by = "building_id", relationship = "one-to-one") |>
  left_join(history, by = "lot_pin10", relationship = "many-to-one") |>
  mutate(permit_street = street_key(address), lot_street = street_key(lot_address)) |>
  left_join(permits |> rename(permits_at_permit_address = permits), by = c("permit_street" = "street"),
    relationship = "many-to-one", na_matches = "never") |>
  left_join(permits |> rename(permits_at_lot_address = permits), by = c("lot_street" = "street"),
    relationship = "many-to-one", na_matches = "never") |>
  transmute(case, permit_number, issue_date, permit_status, address, permit_units, description,
    lot_address, lot_pin10, source, dwelling_units, building_sqft, land_sqft, assessor_year_built, first_assessment_year,
    lot_rule_candidates, lot_history, permits_at_permit_address, permits_at_lot_address,
    assessor_url = if_else(is.na(lot_pin10), NA_character_, paste0("https://www.cookcountyassessor.com/pin/", lot_pin10, "0000")),
    map_url = if_else(is.na(lat), NA_character_, sprintf("https://www.google.com/maps/search/?api=1&query=%.6f,%.6f", lat, lon))) |>
  arrange(case, permit_number)
SaveData(review, c("case", "permit_number"), "../output/lot_review.csv", na = "")
