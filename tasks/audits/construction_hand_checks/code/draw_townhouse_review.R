# setwd("tasks/audits/construction_hand_checks/code")
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")

review_seed <- 20260922L
townhouse_sample_size <- 25L
first_record_year_built <- 2004L  # reported year built from which Assessor records are reviewed

buildings <- read_csv("../input/construction_buildings.csv", col_types = cols(building_id = "c", permit_number = "c",
  member_permit_numbers = "c", superseded_permit_numbers = "c", lot_rule_candidates = "c", townhouse_candidates = "c",
  record_ids = "c", parcel_pin10s = "c", .default = col_guess()))
addresses <- read_csv("../input/parcel_addresses_2025_chicago.csv", col_types = cols(.default = col_character()),
  col_select = c(pin, prop_address_full)) |> distinct(pin, .keep_all = TRUE)

# A seeded random sample of townhouse-rule links, with each home's 2025 address and first residential record.
set.seed(review_seed)
links <- buildings |> filter(str_detect(coalesce(match_basis, ""), "townhouse_lots")) |> slice_sample(n = townhouse_sample_size)
homes <- links |> select(permit_number, record_ids) |> separate_longer_delim(record_ids, "/") |> rename(pin = record_ids)
con <- DBI::dbConnect(duckdb::duckdb())
duckdb::duckdb_register(con, "homes", homes)
first_records <- DBI::dbGetQuery(con, sprintf("
  SELECT h.pin, min(h.tax_year) AS first_year, arg_min(h.year_built, h.tax_year) AS year_built,
    arg_min(h.building_sqft, h.tax_year) AS sqft, arg_min(h.class, h.tax_year) AS class,
    min(h.tax_year) FILTER (WHERE h.year_built >= %1$d) AS new_year,
    arg_min(h.year_built, h.tax_year) FILTER (WHERE h.year_built >= %1$d) AS new_year_built,
    arg_min(h.building_sqft, h.tax_year) FILTER (WHERE h.year_built >= %1$d) AS new_sqft
  FROM read_parquet('../input/residential_assessor_history.parquet') h WHERE h.pin IN (SELECT pin FROM homes)
  GROUP BY 1", first_record_year_built))
DBI::dbDisconnect(con, shutdown = TRUE)
review <- homes |>
  left_join(addresses, by = "pin", relationship = "many-to-one") |>
  left_join(first_records, by = "pin", relationship = "many-to-one") |>
  group_by(permit_number) |>
  summarise(homes = paste0(pin, " ", coalesce(prop_address_full, "no address"), " (new building ", new_year_built,
    ", first assessed ", new_year, ", ", new_sqft, " sq ft; earliest record ", first_year, ": built ", year_built, ", ",
    sqft, " sq ft)", collapse = " | "), .groups = "drop") |>
  right_join(links |> select(permit_number, member_permit_numbers, issue_date, permit_status, address, permit_units,
    match_basis, description),
    by = "permit_number", relationship = "one-to-one") |>
  relocate(homes, .after = last_col()) |>
  arrange(permit_number)
SaveData(review, "permit_number", "../output/townhouse_review.csv", na = "")
