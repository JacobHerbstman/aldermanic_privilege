# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/process_residential_improvements_full/code")
# start_year <- 2006
# end_year <- 2022

source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (interactive()) {
  cli_args <- c(start_year, end_year)
}
if (length(cli_args) != 2) {
  stop("Script requires start and end tax years.", call. = FALSE)
}

start_year <- as.integer(cli_args[1])
end_year <- as.integer(cli_args[2])
if (any(!is.finite(c(start_year, end_year))) || start_year > end_year) {
  stop("Tax-year range is invalid.", call. = FALSE)
}

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
  trim(card) AS card_raw,
  trim(class) AS improvement_class_raw,
  trim(township_code) AS township_code_raw,
  trim(pin_is_multicard) AS pin_is_multicard_raw,
  trim(pin_num_cards) AS pin_num_cards_raw,
  trim(pin_is_multiland) AS pin_is_multiland_raw,
  trim(pin_num_landlines) AS pin_num_landlines_raw,
  trim(tieback_proration_rate) AS tieback_proration_rate_raw,
  trim(card_proration_rate) AS card_proration_rate_raw,
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
              ignore_errors = false,
              all_varchar = true,
              header = true,
              auto_detect = true,
              max_line_size = 10000000)
"))

invisible(dbExecute(con, sprintf("
CREATE TABLE improvements_clean AS
SELECT
  pin,
  try_cast(numeric_text(tax_year_raw) AS INTEGER) AS tax_year,
  try_cast(numeric_text(card_raw) AS INTEGER) AS card,
  try_cast(numeric_text(improvement_class_raw) AS INTEGER) AS improvement_class,
  try_cast(numeric_text(township_code_raw) AS INTEGER) AS township_code,
  lower(pin_is_multicard_raw) = 'true' AS pin_is_multicard,
  try_cast(numeric_text(pin_num_cards_raw) AS INTEGER) AS pin_num_cards,
  lower(pin_is_multiland_raw) = 'true' AS pin_is_multiland,
  try_cast(numeric_text(pin_num_landlines_raw) AS INTEGER) AS pin_num_landlines,
  try_cast(numeric_text(tieback_proration_rate_raw) AS DOUBLE) AS tieback_proration_rate,
  try_cast(numeric_text(card_proration_rate_raw) AS DOUBLE) AS card_proration_rate,
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
  AND try_cast(numeric_text(tax_year_raw) AS INTEGER) BETWEEN %d AND %d
", start_year, end_year)))

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
  stop("Residential improvements panel has no pre-1999 buildings; sales hedonics would be new-construction-only.", call. = FALSE)
}

invisible(dbExecute(con, "
CREATE TABLE improvements_panel AS
SELECT
  pin,
  tax_year,
  improvement_class,
  num_buildings,
  is_multibuilding,
  is_multiland,
  num_landlines,
  tieback_proration_rate,
  card_proration_rate,
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
    coalesce(
      max(pin_num_cards) OVER (PARTITION BY pin, tax_year),
      count(*) OVER (PARTITION BY pin, tax_year)
    ) AS num_buildings,
    coalesce(
      bool_or(pin_is_multicard) OVER (PARTITION BY pin, tax_year),
      count(*) OVER (PARTITION BY pin, tax_year) > 1
    ) AS is_multibuilding,
    coalesce(
      bool_or(pin_is_multiland) OVER (PARTITION BY pin, tax_year),
      false
    ) AS is_multiland,
    max(pin_num_landlines) OVER (PARTITION BY pin, tax_year) AS num_landlines,
    max(tieback_proration_rate) OVER (PARTITION BY pin, tax_year) AS tieback_proration_rate,
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
  stop("Deduplicated residential improvements panel lost all pre-1999 buildings.", call. = FALSE)
}

n_invalid_structure <- dbGetQuery(con, "
SELECT COUNT(*) AS n
FROM improvements_panel
WHERE num_buildings IS NULL
  OR num_buildings < 1
  OR is_multibuilding <> (num_buildings > 1)
")$n
if (n_invalid_structure > 0) {
  stop("Residential improvements contain inconsistent official building counts.", call. = FALSE)
}

invisible(dbExecute(con, "
COPY (
  SELECT *
  FROM improvements_panel
  ORDER BY pin, tax_year
) TO '../output/residential_improvements_panel.parquet' (FORMAT PARQUET)
"))

ReportData("../output/residential_improvements_panel.parquet", c("pin", "tax_year"))
