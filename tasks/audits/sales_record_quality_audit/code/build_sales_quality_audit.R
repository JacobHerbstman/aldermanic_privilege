# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_record_quality_audit/code")
# start_year <- 2006
# end_year <- 2022
# bandwidth_ft <- 500
# high_ppsf <- 5000
# review_ppsf <- 2000
# low_ppsf <- 5

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("../../../shared/code/canonical_geometry_helpers.R")
args <- if (interactive()) {
  c(start_year, end_year, bandwidth_ft, high_ppsf, review_ppsf, low_ppsf)
} else commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 6L, all(is.finite(as.numeric(args))))
start_year <- as.integer(args[1])
end_year <- as.integer(args[2])
bandwidth_ft <- as.numeric(args[3])
high_ppsf <- as.numeric(args[4])
review_ppsf <- as.numeric(args[5])
low_ppsf <- as.numeric(args[6])
stopifnot(start_year <= end_year, bandwidth_ft > 0,
          high_ppsf > review_ppsf, review_ppsf > low_ppsf, low_ppsf > 0)

sales <- as.data.table(read_parquet("../input/sales_with_hedonics_amenities.parquet"))
clean <- as.data.table(read_parquet("../input/residential_sales_clean.parquet"))
sales[, `:=`(row_id = as.character(row_id), sale_document_num = as.character(sale_document_num))]
clean[, row_id := as.character(row_id)]
stopifnot(!anyDuplicated(sales$row_id), !anyDuplicated(clean$row_id))
matched <- match(sales$row_id, clean$row_id)
stopifnot(!anyNA(matched))
sales[, `:=`(
  is_mydec_date = clean$is_mydec_date[matched],
  sale_deed_type = clean$sale_deed_type[matched],
  mydec_deed_type = clean$mydec_deed_type[matched],
  sale_type = clean$sale_type[matched],
  signed_dist_ft = signed_dist_m / 0.3048,
  era = canonical_era_from_date(sale_date, allow_pre_2003 = TRUE),
  nominal_ppsf = sale_price_nominal / building_sqft
)]
stopifnot(!anyNA(sales$is_mydec_date),
          all(sales$hedonic_tax_year == sales$year),
          all(sales$sale_price_nominal == clean$sale_price_nominal[matched]))

# Reproduce the paper's observation filters without estimating a regression.
controls <- c("log_sqft", "log_land_sqft", "log_building_age", "log_bedrooms",
              "log_baths", "has_garage", "nearest_school_dist_ft",
              "nearest_park_dist_ft", "nearest_major_road_dist_ft",
              "nearest_cta_stop_dist_ft", "lake_michigan_dist_ft")
sales[, complete_controls := Reduce(`&`, lapply(.SD, is.finite)), .SDcols = controls]
sales[, in_main_rd :=
  !is.na(sale_price) & sale_price > 0 & year >= start_year & year <= end_year &
  is.finite(signed_dist_ft) & abs(signed_dist_ft) < bandwidth_ft &
  is.finite(strictness_own) & is.finite(strictness_neighbor) &
  strictness_own != strictness_neighbor &
  !is.na(segment_id) & segment_id != "" &
  !is.na(ward_pair_id) & ward_pair_id != "" & !is.na(era) &
  is.finite(longitude) & is.finite(latitude) & complete_controls]
sales[is.na(in_main_rd), in_main_rd := FALSE]
estimates <- fread("../input/price_boundary_property_type_fe_estimates.csv")
production <- as.data.table(read_parquet(
  "../input/clean_sales_with_hedonics_amenities.parquet", col_select = "row_id"
))
stopifnot(!anyDuplicated(production$row_id))
sales[, in_current_main_rd := in_main_rd & row_id %in% as.character(production$row_id)]
if (start_year == 2006L && end_year == 2022L && bandwidth_ft == 500) {
  stopifnot(sum(sales$in_current_main_rd) == estimates[market == "sales" & property_type_fe, n])
}
sales[, `:=`(
  flag_high_ppsf = nominal_ppsf > high_ppsf,
  flag_review_ppsf = nominal_ppsf > review_ppsf,
  flag_low_ppsf = nominal_ppsf < low_ppsf,
  flag_rooms_below_bedrooms = !is.na(num_rooms) & !is.na(num_bedrooms) & num_rooms < num_bedrooms,
  flag_future_building_year = !is.na(year_built) & year_built > year,
  flag_negative_characteristic = Reduce(`|`, lapply(.SD, function(x) !is.na(x) & x < 0)),
  flag_missing_rooms = is.na(num_rooms),
  flag_missing_half_baths = is.na(num_half_baths),
  flag_missing_garage = is.na(garage_size),
  flag_unknown_apartments = class == 211L & !num_apartments %in% 2:6,
  flag_subclass_mismatch = improvement_class_mismatch,
  flag_month_precision = !is_mydec_date
), .SDcols = c("num_rooms", "num_bedrooms", "num_full_baths", "num_half_baths")]

# Check source cards directly, before the production producer selects a card.
con <- dbConnect(duckdb())
invisible(dbExecute(con, "SET threads = 4"))
invisible(dbExecute(con, "SET memory_limit = '2GB'"))
audit_keys <- unique(sales[, .(pin, year)])
duckdb_register(con, "audit_keys", audit_keys)
source_cards <- as.data.table(dbGetQuery(con, "
SELECT pin, try_cast(year AS INTEGER) AS year, card,
       class AS raw_improvement_class, pin_num_cards, pin_is_multicard,
       char_bldg_sf, char_rooms, char_beds, char_fbath, char_hbath, char_apts,
       char_yrblt, char_type_resd, char_cnst_qlty, char_repair_cnd, row_id AS card_row_id
FROM read_csv('../input/residential_improvement_characteristics_full.csv',
              all_varchar = true, header = true)
WHERE (pin, try_cast(year AS INTEGER)) IN (SELECT pin, year FROM audit_keys)
"))
stopifnot(!anyDuplicated(source_cards[, .(pin, year, card)]))
source_counts <- source_cards[, .(actual_card_rows = .N), by = .(pin, year)]
sales <- merge(sales, source_counts, by = c("pin", "year"), all.x = TRUE, sort = FALSE)
stopifnot(!anyDuplicated(sales$row_id), !anyNA(sales$actual_card_rows))
sales[, flag_multiple_source_cards := actual_card_rows != 1L]
sole_card <- source_cards[source_counts[actual_card_rows == 1L], on = .(pin, year)]
stopifnot(!anyDuplicated(sole_card[, .(pin, year)]))
sales <- merge(sales, sole_card[, .(pin, year, char_rooms, char_beds, char_fbath,
                                    char_hbath, char_apts, char_type_resd,
                                    char_cnst_qlty, char_repair_cnd)],
               by = c("pin", "year"), all.x = TRUE, sort = FALSE)
sales[, flag_missing_quality_fields :=
  is.na(char_type_resd) | trimws(char_type_resd) == "" |
  is.na(char_cnst_qlty) | trimws(char_cnst_qlty) == "" |
  is.na(char_repair_cnd) | trimws(char_repair_cnd) == ""]

property_cases <- sales[flag_review_ppsf | flag_rooms_below_bedrooms |
                         flag_negative_characteristic | flag_future_building_year]
review_pins <- unique(sales[flag_unknown_apartments | row_id %in% property_cases$row_id, .(pin)])
duckdb_register(con, "review_pins", review_pins)
history <- as.data.table(dbGetQuery(con, "
SELECT pin, try_cast(year AS INTEGER) AS year, card, class, pin_num_cards,
       pin_is_multicard, tieback_proration_rate, char_bldg_sf, char_land_sf,
       char_rooms, char_beds, char_fbath, char_hbath, char_apts, char_yrblt,
       char_type_resd, char_cnst_qlty, char_repair_cnd, row_id
FROM read_csv('../input/residential_improvement_characteristics_full.csv',
              all_varchar = true, header = true)
WHERE pin IN (SELECT pin FROM review_pins)
ORDER BY pin, year, card
"))
dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(history[, .(pin, year, card)]))
fwrite(history, "../output/property_improvement_history.csv")
ReportData("../output/property_improvement_history.csv")

# Historical corroboration is evidence only: no characteristics are carried across years.
history[, apartment_count := fcase(
  char_apts == "Two", 2L, char_apts == "Three", 3L, char_apts == "Four", 4L,
  char_apts == "Five", 5L, char_apts == "Six", 6L, default = NA_integer_
)]
history[, source_cards_in_year := .N, by = .(pin, year)]
unknown <- sales[flag_unknown_apartments == TRUE]
apartment_evidence <- vector("list", nrow(unknown))
setkey(history, pin)
for (i in seq_len(nrow(unknown))) {
  row <- unknown[i]
  h <- history[.(row$pin)][source_cards_in_year == 1L & class == "211" &
                            is.finite(as.numeric(tieback_proration_rate)) &
                            abs(as.numeric(tieback_proration_rate) - 1) < 1e-6]
  known <- h[!is.na(apartment_count)]
  before <- known[year < row$year][order(-year)][1]
  after <- known[year > row$year][order(year)][1]
  evidence <- if (nrow(known) == 0L) {
    "No recorded two-to-six count in available single-building history"
  } else if (uniqueN(known$apartment_count) > 1L) {
    "Historical unit counts disagree across years"
  } else if (!is.na(before$year) && !is.na(after$year)) {
    "Same count recorded both before and after sale year"
  } else {
    "Count recorded only before or only after sale year"
  }
  apartment_evidence[[i]] <- data.table(
    row_id = row$row_id, pin = row$pin, year = row$year,
    in_main_rd = row$in_main_rd, signed_dist_ft = row$signed_dist_ft,
    raw_apartment_label = row$char_apts, evidence = evidence,
    prior_year = before$year, prior_count = before$apartment_count,
    subsequent_year = after$year, subsequent_count = after$apartment_count,
    available_history_years = uniqueN(h$year)
  )
}
apartment_evidence <- rbindlist(apartment_evidence)
fwrite(apartment_evidence, "../output/apartment_count_evidence.csv")
ReportData("../output/apartment_count_evidence.csv")
apartment_summary <- apartment_evidence[, .(
  panel_sales = .N, main_rd_sales = sum(in_main_rd)
), by = evidence]
fwrite(apartment_summary, "../output/apartment_count_summary.csv")
ReportData("../output/apartment_count_summary.csv")
fwrite(sales[class == 211L, .(
  panel_sales = .N,
  unknown_count = sum(flag_unknown_apartments),
  main_rd_sales = sum(in_main_rd),
  main_rd_unknown_count = sum(in_main_rd & flag_unknown_apartments)
), by = year][order(year)], "../output/apartment_count_by_year.csv")
ReportData("../output/apartment_count_by_year.csv")
fwrite(sales[class == 211L & in_main_rd, .(
  sales = .N, median_nominal_price = as.numeric(median(sale_price_nominal)),
  median_building_sqft = as.numeric(median(building_sqft)),
  median_bedrooms = as.numeric(median(num_bedrooms)),
  median_rooms = as.numeric(median(num_rooms, na.rm = TRUE)),
  share_more_stringent_side = mean(signed_dist_ft > 0)
), by = flag_unknown_apartments], "../output/apartment_count_composition.csv")
ReportData("../output/apartment_count_composition.csv")

property_history <- history[pin %in% property_cases$pin]
setorder(property_history, pin, card, year)
property_history[, spell := rleid(char_rooms, char_beds, char_bldg_sf, char_apts), by = .(pin, card)]
fwrite(property_history[, .(
  first_year = min(year), last_year = max(year),
  rooms = first(char_rooms), bedrooms = first(char_beds),
  building_sqft = first(char_bldg_sf), apartment_label = first(char_apts)
), by = .(pin, card, spell)], "../output/property_history_spells.csv")
ReportData("../output/property_history_spells.csv")

raw <- fread("../input/parcel_sales_city.csv", colClasses = list(
  character = c("pin", "row_id", "sale_document_num")))
raw[, pin := gsub("[^0-9]", "", pin)]
raw[nchar(pin) == 13L, pin := paste0("0", pin)]
stopifnot(!anyDuplicated(raw$row_id))
raw_match <- match(sales$row_id, raw$row_id)
stopifnot(!anyNA(raw_match),
          all(sales$sale_price_nominal == raw$sale_price[raw_match]))
doc_counts <- raw[, .(raw_document_rows = .N, raw_document_pins = uniqueN(pin)),
                  by = sale_document_num]
sales <- merge(sales, doc_counts, by = "sale_document_num", all.x = TRUE, sort = FALSE)
sales[, flag_document_multiple_pins := raw_document_pins > 1L]

sale_history <- raw[pin %in% property_cases$pin | sale_document_num %in% property_cases$sale_document_num]
sale_history[, in_clean_transaction_sample := row_id %in% clean$row_id]
sale_history[, in_main_rd := row_id %in% sales[in_main_rd == TRUE, row_id]]
setorder(sale_history, pin, sale_date, row_id)
fwrite(sale_history, "../output/property_sale_history.csv")
ReportData("../output/property_sale_history.csv")

setorder(sales, year, sale_date, row_id)
write_parquet(sales, "../output/quality_records.parquet")
ReportData("../output/quality_records.parquet")
fwrite(sales[row_id %in% property_cases$row_id, .(
  row_id, pin, sale_document_num, sale_date, class, in_main_rd,
  signed_dist_ft, sale_price_nominal, building_sqft, nominal_ppsf,
  num_rooms, num_bedrooms, num_full_baths, num_half_baths,
  char_rooms, char_beds, char_fbath, char_hbath, char_apts,
  raw_document_rows, raw_document_pins, actual_card_rows,
  sale_deed_type, mydec_deed_type, is_mydec_date,
  flag_high_ppsf, flag_review_ppsf, flag_rooms_below_bedrooms,
  flag_negative_characteristic, flag_future_building_year
)], "../output/property_review.csv")
ReportData("../output/property_review.csv")

flags <- grep("^flag_", names(sales), value = TRUE)
summary <- rbindlist(lapply(flags, function(flag) data.table(
  issue = flag,
  panel_sales = sum(sales[[flag]], na.rm = TRUE),
  main_rd_sales = sum(sales[[flag]] & sales$in_main_rd, na.rm = TRUE),
  current_main_rd_sales = sum(sales[[flag]] & sales$in_current_main_rd, na.rm = TRUE)
)))
summary <- rbind(data.table(issue = "All observations", panel_sales = nrow(sales),
                           main_rd_sales = sum(sales$in_main_rd),
                           current_main_rd_sales = sum(sales$in_current_main_rd)), summary)
fwrite(summary, "../output/quality_summary.csv")
ReportData("../output/quality_summary.csv")
