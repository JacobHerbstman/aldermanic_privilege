# process_improvements.R
# Creates an all-buildings PIN x tax_year panel of residential property characteristics.
# This panel is used for temporal matching to home sales, so it must not apply
# the new-construction year-built restriction used by density tasks.

# setwd("tasks/process_residential_improvements_full/code")
source("../../setup_environment/code/packages.R")

message("Loading full residential improvements data using DuckDB...")
message("(This may take a few minutes for ~12M Chicago improvement-year rows)")

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

message("Reading CSV...")
invisible(dbExecute(con, "
CREATE TABLE improvements_raw AS
SELECT
  trim(pin) AS pin,
  trim(year) AS tax_year_raw,
  trim(township_code) AS township_code_raw,
  trim(char_yrblt) AS year_built_raw,
  trim(char_bldg_sf) AS building_sqft_raw,
  trim(char_land_sf) AS land_sqft_raw,
  trim(char_rooms) AS num_rooms_raw,
  trim(char_beds) AS num_bedrooms_raw,
  trim(char_fbath) AS num_full_baths_raw,
  trim(char_hbath) AS num_half_baths_raw,
  trim(char_frpl) AS num_fireplaces_raw,
  trim(char_apts) AS num_apartments_raw,
  trim(char_gar1_size) AS garage_size_raw,
  trim(row_id) AS row_id_raw
FROM read_csv('../input/residential_improvement_characteristics_full.csv',
              ignore_errors = true,
              all_varchar = true,
              header = true,
              auto_detect = true,
              max_line_size = 10000000)
"))

n_loaded <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM improvements_raw")$n
message(sprintf("Loaded %s rows", format(n_loaded, big.mark = ",")))

message("Cleaning columns and keeping Chicago townships...")
invisible(dbExecute(con, "
CREATE TABLE improvements_clean AS
SELECT
  pin,
  try_cast(numeric_text(tax_year_raw) AS INTEGER) AS tax_year,
  try_cast(numeric_text(township_code_raw) AS INTEGER) AS township_code,
  try_cast(numeric_text(year_built_raw) AS INTEGER) AS year_built,
  try_cast(numeric_text(building_sqft_raw) AS DOUBLE) AS building_sqft,
  try_cast(numeric_text(land_sqft_raw) AS DOUBLE) AS land_sqft,
  try_cast(numeric_text(num_rooms_raw) AS DOUBLE) AS num_rooms,
  try_cast(numeric_text(num_bedrooms_raw) AS DOUBLE) AS num_bedrooms,
  try_cast(numeric_text(num_full_baths_raw) AS DOUBLE) AS num_full_baths,
  try_cast(numeric_text(num_half_baths_raw) AS DOUBLE) AS num_half_baths,
  try_cast(numeric_text(num_fireplaces_raw) AS DOUBLE) AS num_fireplaces,
  CASE
    WHEN lower(trim(num_apartments_raw)) IN ('none', 'zero') THEN 0
    WHEN lower(trim(num_apartments_raw)) = 'one' THEN 1
    WHEN lower(trim(num_apartments_raw)) = 'two' THEN 2
    WHEN lower(trim(num_apartments_raw)) = 'three' THEN 3
    WHEN lower(trim(num_apartments_raw)) = 'four' THEN 4
    WHEN lower(trim(num_apartments_raw)) = 'five' THEN 5
    WHEN lower(trim(num_apartments_raw)) = 'six' THEN 6
    ELSE try_cast(numeric_text(num_apartments_raw) AS INTEGER)
  END AS num_apartments,
  try_cast(numeric_text(garage_size_raw) AS DOUBLE) AS garage_size,
  try_cast(numeric_text(row_id_raw) AS BIGINT) AS row_id
FROM improvements_raw
WHERE try_cast(numeric_text(township_code_raw) AS INTEGER) IN (70, 71, 72, 73, 74, 75, 76, 77)
"))

n_chicago <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM improvements_clean")$n
n_pre_1999 <- dbGetQuery(con, "
SELECT COUNT(*) AS n
FROM improvements_clean
WHERE year_built < 1999
")$n
message(sprintf("Chicago township rows: %s", format(n_chicago, big.mark = ",")))
message(sprintf("Pre-1999 building-year rows retained: %s", format(n_pre_1999, big.mark = ",")))

if (n_chicago == 0) {
  stop("No Chicago township residential improvement rows were retained.", call. = FALSE)
}
if (n_pre_1999 == 0) {
  stop("Full residential improvements panel has no pre-1999 buildings; sales hedonics would be new-construction-only.", call. = FALSE)
}

message("Handling duplicates within PIN-year...")
n_duplicate_groups <- dbGetQuery(con, "
SELECT COUNT(*) AS n
FROM (
  SELECT pin, tax_year, COUNT(*) AS n_records
  FROM improvements_clean
  GROUP BY pin, tax_year
  HAVING COUNT(*) > 1
)
")$n
message(sprintf("PIN-years with multiple records: %s", format(n_duplicate_groups, big.mark = ",")))

invisible(dbExecute(con, "
CREATE TABLE improvements_panel AS
SELECT
  pin,
  tax_year,
  year_built,
  building_sqft,
  land_sqft_pin_year AS land_sqft,
  num_rooms,
  num_bedrooms,
  num_full_baths,
  num_half_baths,
  num_fireplaces,
  num_apartments,
  garage_size
FROM (
  SELECT
    *,
    max(land_sqft) OVER (PARTITION BY pin, tax_year) AS land_sqft_pin_year,
    row_number() OVER (
      PARTITION BY pin, tax_year
      ORDER BY building_sqft DESC NULLS LAST, row_id ASC NULLS LAST
    ) AS keep_row
  FROM improvements_clean
)
WHERE keep_row = 1
"))

n_panel <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM improvements_panel")$n
message(sprintf(
  "Rows after deduplication: %s (dropped %s)",
  format(n_panel, big.mark = ","),
  format(n_chicago - n_panel, big.mark = ",")
))

message("\n=== PANEL DIAGNOSTICS ===")

panel_summary <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,
  COUNT(DISTINCT pin) AS unique_pins,
  MIN(tax_year) AS min_tax_year,
  MAX(tax_year) AS max_tax_year,
  COUNT(*) FILTER (WHERE year_built < 1999) AS pre_1999_rows,
  COUNT(DISTINCT pin) FILTER (WHERE year_built < 1999) AS pre_1999_pins
FROM improvements_panel
")
print(panel_summary)

if (panel_summary$pre_1999_rows == 0 || panel_summary$pre_1999_pins == 0) {
  stop("Deduplicated full residential improvements panel lost all pre-1999 buildings.", call. = FALSE)
}

records_per_pin <- dbGetQuery(con, "
SELECT
  median(n_records) AS median_records,
  AVG(n_records) AS mean_records,
  MAX(n_records) AS max_records
FROM (
  SELECT pin, COUNT(*) AS n_records
  FROM improvements_panel
  GROUP BY pin
)
")
print(records_per_pin)

message("\nVariable coverage (% non-missing):")
coverage <- dbGetQuery(con, "
SELECT
  100.0 * AVG(CASE WHEN building_sqft IS NOT NULL THEN 1 ELSE 0 END) AS building_sqft,
  100.0 * AVG(CASE WHEN land_sqft IS NOT NULL THEN 1 ELSE 0 END) AS land_sqft,
  100.0 * AVG(CASE WHEN year_built IS NOT NULL THEN 1 ELSE 0 END) AS year_built,
  100.0 * AVG(CASE WHEN num_bedrooms IS NOT NULL THEN 1 ELSE 0 END) AS num_bedrooms,
  100.0 * AVG(CASE WHEN num_full_baths IS NOT NULL THEN 1 ELSE 0 END) AS num_full_baths,
  100.0 * AVG(CASE WHEN garage_size IS NOT NULL THEN 1 ELSE 0 END) AS garage_size
FROM improvements_panel
")
print(coverage)

message("\nObservations by tax year:")
tax_year_counts <- dbGetQuery(con, "
SELECT tax_year, COUNT(*) AS n
FROM improvements_panel
GROUP BY tax_year
ORDER BY tax_year
")
print(tax_year_counts)

message("\nYear built distribution:")
decade_counts <- dbGetQuery(con, "
SELECT floor(year_built / 10) * 10 AS decade, COUNT(DISTINCT pin) AS n_properties
FROM improvements_panel
WHERE year_built IS NOT NULL
GROUP BY decade
ORDER BY decade
")
print(decade_counts)

message("\nSaving panel...")
invisible(dbExecute(con, "
COPY (
  SELECT *
  FROM improvements_panel
  ORDER BY pin, tax_year
) TO '../output/residential_improvements_panel.parquet' (FORMAT PARQUET)
"))

message(sprintf(
  "Saved: ../output/residential_improvements_panel.parquet (%s rows)",
  format(n_panel, big.mark = ",")
))

message("\nDone!")
