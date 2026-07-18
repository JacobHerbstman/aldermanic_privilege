# setwd("tasks/audits/density_multicard_manual_review/code")

library(DBI)
library(duckdb)
library(readr)

pins <- c(
  "11303200330000", "13141270250000", "13322180280000",
  "14074070280000", "14074070290000", "14301010390000",
  "17173330340000", "19083090620000", "20211200220000",
  "20211200230000", "20211200290000", "20231160220000",
  "25194060030000"
)

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

pin_sql <- paste0("'", pins, "'", collapse = ",")

card_history <- dbGetQuery(con, paste0("
SELECT
  trim(pin) AS pin,
  try_cast(year AS INTEGER) AS tax_year,
  try_cast(card AS INTEGER) AS card_num,
  try_cast(pin_num_cards AS INTEGER) AS pin_num_cards,
  try_cast(char_yrblt AS INTEGER) AS year_built,
  try_cast(char_bldg_sf AS DOUBLE) AS building_sqft,
  trim(char_apts) AS apartments,
  trim(char_type_resd) AS residence_type,
  trim(char_use) AS family_type,
  trim(tieback_key_pin) AS tieback_key_pin,
  try_cast(tieback_proration_rate AS DOUBLE) AS tieback_proration_rate,
  try_cast(card_proration_rate AS DOUBLE) AS card_proration_rate
FROM read_csv(
  '../../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv',
  all_varchar = true,
  header = true,
  ignore_errors = true,
  max_line_size = 10000000
)
WHERE pin IN (", pin_sql, ")
ORDER BY pin, tax_year, card_num
"))

write_csv(card_history, "../output/card_history.csv")
