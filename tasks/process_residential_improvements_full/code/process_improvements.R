# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/process_residential_improvements_full/code")
source("../../setup_environment/code/packages.R")

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

invisible(dbExecute(con, "
CREATE OR REPLACE MACRO numeric_text(x) AS
  nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '');
"))

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

if (n_chicago == 0) {
  stop("No Chicago township residential improvement rows were retained.", call. = FALSE)
}
if (n_pre_1999 == 0) {
  stop("Full residential improvements panel has no pre-1999 buildings; sales hedonics would be new-construction-only.", call. = FALSE)
}

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

if (panel_summary$pre_1999_rows == 0 || panel_summary$pre_1999_pins == 0) {
  stop("Deduplicated full residential improvements panel lost all pre-1999 buildings.", call. = FALSE)
}

invisible(dbExecute(con, "
COPY (
  SELECT *
  FROM improvements_panel
  ORDER BY pin, tax_year
) TO '../output/residential_improvements_panel.parquet' (FORMAT PARQUET)
"))
