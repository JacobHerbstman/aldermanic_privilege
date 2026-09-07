# setwd("tasks/prepare_construction_assessor_history/code")

source("../../setup_environment/code/packages.R")

con <- DBI::dbConnect(duckdb::duckdb())
invisible(DBI::dbExecute(con, "
CREATE MACRO numeric_value(x) AS
  try_cast(nullif(regexp_replace(cast(x AS VARCHAR), '[^0-9.-]', '', 'g'), '') AS DOUBLE);
"))

# Keep source reports before selecting years, linking projects, or adding units.
invisible(DBI::dbExecute(con, "
CREATE TABLE history AS
SELECT
  trim(pin) AS pin,
  cast(trunc(numeric_value(year)) AS INTEGER) AS tax_year,
  cast(trunc(numeric_value(card)) AS INTEGER) AS card_num,
  trim(class) AS class,
  trim(township_code) AS township_code,
  trim(tieback_key_pin) AS proration_key_pin,
  numeric_value(tieback_proration_rate) AS pin_proration_rate,
  numeric_value(card_proration_rate) AS card_proration_rate,
  trim(cdu) AS cdu,
  lower(trim(pin_is_multicard)) = 'true' AS pin_is_multicard,
  cast(trunc(numeric_value(pin_num_cards)) AS INTEGER) AS pin_num_cards,
  lower(trim(pin_is_multiland)) = 'true' AS pin_is_multiland,
  cast(trunc(numeric_value(pin_num_landlines)) AS INTEGER) AS pin_num_landlines,
  cast(trunc(numeric_value(char_yrblt)) AS INTEGER) AS year_built,
  numeric_value(char_bldg_sf) AS building_sqft,
  numeric_value(char_land_sf) AS land_sqft,
  numeric_value(char_beds) AS num_bedrooms,
  numeric_value(char_rooms) AS num_rooms,
  numeric_value(char_fbath) AS num_full_baths,
  numeric_value(char_hbath) AS num_half_baths,
  numeric_value(char_frpl) AS num_fireplaces,
  trim(char_type_resd) AS type_of_residence,
  trim(char_cnst_qlty) AS construction_quality,
  CASE lower(trim(char_apts)) WHEN 'none' THEN 0 WHEN 'zero' THEN 0 WHEN 'one' THEN 1 WHEN 'two' THEN 2 WHEN 'three' THEN 3 WHEN 'four' THEN 4 WHEN 'five' THEN 5 WHEN 'six' THEN 6 ELSE numeric_value(char_apts) END AS num_apartments,
  trim(char_attic_fnsh) AS attic_finish,
  trim(char_gar1_att) AS garage_attached,
  trim(char_gar1_area) AS garage_area_included,
  numeric_value(char_gar1_size) AS garage_size,
  trim(char_gar1_cnst) AS garage_ext_wall_material,
  trim(char_attic_type) AS attic_type,
  trim(char_bsmt) AS basement_type,
  trim(char_ext_wall) AS ext_wall_material,
  trim(char_heat) AS central_heating,
  trim(char_repair_cnd) AS repair_condition,
  trim(char_bsmt_fin) AS basement_finish,
  trim(char_roof_cnst) AS roof_material,
  trim(char_use) AS single_v_multi_family,
  trim(char_site) AS site_desirability,
  cast(trunc(numeric_value(char_ncu)) AS INTEGER) AS num_commercial_units,
  trim(char_renovation) AS renovation,
  trim(char_porch) AS porch,
  trim(char_air) AS central_air,
  trim(char_tp_plan) AS design_plan,
  trim(row_id) AS row_id,
  trim(char_apts) AS apartments_text,
  row_number() OVER () AS source_row_order
FROM read_csv(
  '../input/residential_improvement_characteristics_full.csv',
  all_varchar = true, header = true, ignore_errors = false,
  max_line_size = 10000000
)
WHERE numeric_value(township_code) BETWEEN 70 AND 77;
"))

keys <- DBI::dbGetQuery(con, "
SELECT count(*) AS records, count(DISTINCT row_id) AS source_ids,
  count(*) FILTER (WHERE row_id IS NULL OR row_id = '' OR
    NOT regexp_full_match(pin, '[0-9]{14}') OR pin IS NULL OR
    card_num IS NULL OR tax_year IS NULL) AS invalid_keys
FROM history
")
stopifnot(keys$records > 0, keys$records == keys$source_ids, keys$invalid_keys == 0)

# Fix Parquet row-group layout as well as row order across repeated builds.
invisible(DBI::dbExecute(con, "SET threads = 1"))
invisible(DBI::dbExecute(con, "
COPY (SELECT * FROM history ORDER BY source_row_order)
TO '../output/residential_assessor_history.parquet' (FORMAT PARQUET, COMPRESSION ZSTD);
"))
DBI::dbDisconnect(con, shutdown = TRUE)
