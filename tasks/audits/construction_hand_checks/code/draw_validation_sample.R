# setwd("tasks/audits/construction_hand_checks/code")
# review_seed <- 20260923
# stratum_size <- 25
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("../../../shared/code/normalize_chicago_address.R")
source("../../../shared/code/street_key.R")

args <- commandArgs(trailingOnly = TRUE)
if (interactive()) args <- c(review_seed, stratum_size)
stopifnot(length(args) == 2L)
review_seed <- as.integer(args[1])
stratum_size <- as.integer(args[2])

# Strata: eligible buildings by how they were linked, and the three groups held out of the density sample.
buildings <- read_csv("../input/construction_buildings.csv", col_types = cols(.default = col_character())) |>
  mutate(eligible = coalesce(allow_far == "TRUE" & allow_dupac == "TRUE", FALSE),
    stratum = case_when(
      eligible & route == "assessor_only" ~ "eligible_assessor_only",
      eligible & str_detect(coalesce(match_basis, ""), "townhouse|homes") ~ "eligible_added_homes",
      eligible & str_detect(coalesce(match_basis, ""), "parcel_successor") ~ "eligible_parcel_successor",
      eligible & str_detect(coalesce(match_basis, ""), "floor_area_change") ~ "eligible_floor_area_change",
      eligible & str_detect(coalesce(match_basis, ""), "lot") ~ "eligible_lot_link",
      eligible ~ "eligible_permit_parcels",
      status == "measured" & str_detect(coalesce(flags, ""), "units_disagree") ~ "held_units_disagree",
      status == "no_permit_found" ~ "held_no_permit_found",
      status %in% c("no_new_building", "no_parcel") ~ "held_permit_without_building"))
set.seed(review_seed)
sample <- buildings |> filter(!is.na(stratum)) |> add_count(stratum, name = "stratum_population") |>
  group_by(stratum) |> slice_sample(n = stratum_size) |> ungroup() |> arrange(stratum, building_id)

# Evidence for each case: the Assessor's yearly records for its parcels, and every permit at its address or parcels.
parcels <- sample |> transmute(building_id, pin10 = paste(coalesce(record_ids, ""), coalesce(parcel_pin10s, ""), sep = "/")) |>
  separate_longer_delim(pin10, "/") |> mutate(pin10 = substr(pin10, 1, 10)) |> filter(str_detect(pin10, "^[0-9]{10}$")) |>
  distinct()
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "parcels", parcels |> distinct(pin10))
history <- DBI::dbGetQuery(con, "
  SELECT substr(pin, 1, 10) AS pin10, tax_year, string_agg(DISTINCT class, '+') AS classes, max(year_built) AS year_built,
    sum(building_sqft) AS sqft, sum(num_apartments) AS apartments
  FROM read_parquet('../input/residential_assessor_history.parquet') WHERE substr(pin, 1, 10) IN (SELECT pin10 FROM parcels)
  GROUP BY 1, 2 ORDER BY 1, 2")
condominiums <- DBI::dbGetQuery(con, "
  SELECT pin10, try_cast(try_cast(year AS DOUBLE) AS INTEGER) AS tax_year, count(*) AS unit_parcels,
    min(try_cast(try_cast(char_yrblt AS DOUBLE) AS INTEGER)) AS year_built, max(try_cast(char_building_sf AS DOUBLE)) AS sqft
  FROM read_csv('../input/condominium_characteristics.csv', all_varchar = true) WHERE pin10 IN (SELECT pin10 FROM parcels)
  GROUP BY 1, 2 ORDER BY 1, 2")
DBI::dbDisconnect(con, shutdown = TRUE)

# Consecutive years with the same record collapse into one span: "2007-2012 class 211 built 1924 1848 sqft 2 apt".
records <- bind_rows(
  history |> transmute(pin10, kind = "residential", tax_year, record = paste0("class ", classes, " built ", year_built, " ",
    sqft, " sqft ", coalesce(apartments, 0), " apt")),
  condominiums |> transmute(pin10, kind = "condominium", tax_year, record = paste0("condo ", unit_parcels,
    " unit parcels built ", year_built, " ", coalesce(sqft, 0), " sqft")))
spans <- records |> arrange(pin10, kind, tax_year) |> group_by(pin10, kind) |>
  mutate(span = cumsum(record != lag(record, default = ""))) |>
  group_by(pin10, kind, span, record) |>
  summarise(years = if_else(min(tax_year) == max(tax_year), as.character(min(tax_year)), paste0(min(tax_year), "-", max(tax_year))),
    .groups = "drop") |>
  group_by(pin10) |> summarise(history = paste0(pin10[1], ": ", paste(years, record, collapse = "; ")), .groups = "drop")
assessor_history <- parcels |> inner_join(spans, by = "pin10", relationship = "many-to-one") |>
  group_by(building_id) |> summarise(assessor_history = paste(history, collapse = " || "), .groups = "drop")

# Permits keyed by street and by listed parcel, one row per key.
permits <- read_csv("../input/building_permits_full.csv", col_types = cols(.default = col_character()),
    col_select = c(permit_ = permit_, permit_type, issue_date, street_number, street_direction, street_name, work_description, pin_list)) |>
  mutate(entry = paste0(substr(issue_date, 1, 10), " ", permit_, " ", str_remove(permit_type, "^PERMIT - "), ": ",
    str_sub(str_squish(coalesce(work_description, "")), 1, 120)))
permits_by_key <- bind_rows(
    permits |> transmute(key = street_key(paste(str_remove(street_number, "^0+"), street_direction, street_name)), entry),
    permits |> transmute(key = str_extract_all(coalesce(pin_list, ""), "[0-9]{10}"), entry) |> unnest_longer(key)) |>
  filter(!is.na(key)) |> distinct(key, entry) |> group_by(key) |> summarise(entries = list(entry), .groups = "drop")
case_permits <- bind_rows(sample |> transmute(building_id, key = street_key(address)), parcels |> transmute(building_id, key = pin10)) |>
  filter(!is.na(key)) |> distinct() |>
  inner_join(permits_by_key, by = "key", relationship = "many-to-one") |>
  unnest_longer(entries) |> distinct(building_id, entry = entries) |> arrange(building_id, entry) |>
  group_by(building_id) |> summarise(permits_on_record = paste(entry, collapse = " | "), .groups = "drop")

validation <- sample |>
  left_join(assessor_history, by = "building_id", relationship = "one-to-one") |>
  left_join(case_permits, by = "building_id", relationship = "one-to-one") |>
  transmute(stratum, stratum_population, building_id, route, status, match_basis, flags, permit_number, member_permit_numbers,
    issue_date, address, permit_units, description, source, record_ids, parcel_pin10s, first_assessment_year,
    assessor_year_built, dwelling_units, building_sqft, land_sqft, dupac, far, assessor_history, permits_on_record)
SaveData(validation, "building_id", "../output/validation_sample.csv", na = "")
