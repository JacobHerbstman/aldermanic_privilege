# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_price_tail_audit/code")
# tail_probability <- 0.999
# area_change_ratio <- 1.5
# review_seed <- 42
# review_per_group <- 4

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
args <- if (interactive()) c(tail_probability, area_change_ratio, review_seed, review_per_group) else commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 4L, all(is.finite(as.numeric(args))))
tail_probability <- as.numeric(args[1])
area_change_ratio <- as.numeric(args[2])
review_seed <- as.integer(args[3])
review_per_group <- as.integer(args[4])
stopifnot(tail_probability > 0 & tail_probability < 1, area_change_ratio > 1, review_per_group > 0)

# The reference population is citywide, after the approved rooms rule, before RD restrictions.
sales <- as.data.table(read_parquet("../input/quality_records.parquet"))
production <- as.data.table(read_parquet("../input/clean_sales_with_hedonics_amenities.parquet", col_select = "row_id"))
stopifnot(!anyDuplicated(sales$row_id), !anyDuplicated(production$row_id))
sales <- sales[row_id %in% as.character(production$row_id)]
stopifnot(nrow(sales) == nrow(production), !any(sales$flag_rooms_below_bedrooms))
sales[, real_ppsf := sale_price / building_sqft]
stopifnot(all(is.finite(sales$real_ppsf)), all(sales$real_ppsf > 0),
          all(abs(sales$sale_price - sales$sale_price_real_2022_raw) < 1e-6))
pooled_cutoff <- unname(quantile(sales$real_ppsf, tail_probability, type = 7))
cutoffs <- sales[, .(reference_sales = .N,
  annual_cutoff_real = unname(quantile(real_ppsf, tail_probability, type = 7)),
  annual_cutoff_nominal = unname(quantile(nominal_ppsf, tail_probability, type = 7))), by = year]
stopifnot(!anyDuplicated(cutoffs$year))
sales[, annual_cutoff_real := cutoffs$annual_cutoff_real[match(year, cutoffs$year)]]
sales[, `:=`(pooled_cutoff_real = pooled_cutoff,
  flag_annual = real_ppsf > annual_cutoff_real, flag_pooled = real_ppsf > pooled_cutoff,
  flag_annual_nominal = nominal_ppsf > cutoffs$annual_cutoff_nominal[match(year, cutoffs$year)])]
cutoffs <- merge(cutoffs, sales[, .(annual_excluded = sum(flag_annual),
  pooled_excluded = sum(flag_pooled), annual_rd_excluded = sum(flag_annual & in_current_main_rd),
  pooled_rd_excluded = sum(flag_pooled & in_current_main_rd),
  nominal_real_disagreements = sum(flag_annual != flag_annual_nominal)), by = year], by = "year")
cutoffs[, `:=`(tail_probability = tail_probability, pooled_cutoff_real = pooled_cutoff)]
fwrite(cutoffs[order(year)], "../output/tail_cutoffs.csv")
ReportData("../output/tail_cutoffs.csv")
sales[, selection := fcase(flag_annual & flag_pooled, "Both rules",
  flag_annual, "Annual only", flag_pooled, "Pooled only", default = "Neither")]
fwrite(sales[, .(citywide_sales = .N, main_rd_sales = sum(in_current_main_rd),
  more_stringent_main = sum(in_current_main_rd & signed_dist_ft > 0, na.rm = TRUE),
  less_stringent_main = sum(in_current_main_rd & signed_dist_ft < 0, na.rm = TRUE)),
  by = selection], "../output/tail_counts.csv")
ReportData("../output/tail_counts.csv")
review <- sales[flag_annual | flag_pooled]
review[, tail_probability := tail_probability]
setorder(review, selection, in_current_main_rd, row_id)
set.seed(review_seed)
review[, sampled_for_reading := FALSE]
chosen <- review[, .(selected_row = sample(row_id, min(.N, review_per_group))),
                 by = .(selection, in_current_main_rd)]$selected_row
review[row_id %in% chosen, sampled_for_reading := TRUE]

# Reuse frozen raw sources; histories are evidence, never replacement characteristics.
con <- dbConnect(duckdb())
invisible(dbExecute(con, "SET threads = 4"))
invisible(dbExecute(con, "SET memory_limit = '2GB'"))
review_pins <- unique(review[, .(pin)])
duckdb_register(con, "review_pins", review_pins)
history <- as.data.table(dbGetQuery(con, "
SELECT pin, try_cast(year AS INTEGER) AS year, card, class, pin_num_cards,
       pin_is_multicard, tieback_proration_rate, char_bldg_sf, char_land_sf,
       char_rooms, char_beds, char_fbath, char_hbath, char_apts, char_yrblt
FROM read_csv('../input/residential_improvement_characteristics_full.csv', all_varchar = true, header = true)
WHERE pin IN (SELECT pin FROM review_pins)
ORDER BY pin, year, card"))
dbDisconnect(con, shutdown = TRUE)
stopifnot(!anyDuplicated(history[, .(pin, year, card)]))
history[, `:=`(cards_in_year = .N), by = .(pin, year)]
history[, `:=`(area = as.numeric(char_bldg_sf), built = as.numeric(char_yrblt),
              proration = as.numeric(tieback_proration_rate))]
fwrite(history, "../output/tail_improvement_history.csv")
ReportData("../output/tail_improvement_history.csv")
raw <- fread("../input/parcel_sales_city.csv", colClasses = list(character = c("pin", "row_id", "sale_document_num")))
raw[, pin := gsub("[^0-9]", "", pin)]
raw[nchar(pin) == 13L, pin := paste0("0", pin)]
stopifnot(!anyDuplicated(raw$row_id))
raw <- raw[pin %in% review$pin | sale_document_num %in% review$sale_document_num]
raw[, in_current_citywide_sample := row_id %in% sales$row_id]
raw[, flagged_for_review := row_id %in% review$row_id]
raw[, recorded_date := as.Date(sale_date)]
stopifnot(!anyNA(match(review$row_id, raw$row_id)),
          all(review$sale_price_nominal == raw$sale_price[match(review$row_id, raw$row_id)]))
fwrite(raw[order(pin, recorded_date, row_id)], "../output/tail_sale_history.csv")
ReportData("../output/tail_sale_history.csv")

evidence <- vector("list", nrow(review))
setkey(history, pin)
setkey(raw, pin)
for (i in seq_len(nrow(review))) {
  row <- review[i]
  h <- history[.(row$pin)][cards_in_year == 1 & is.finite(area) & area > 0 & proration == 1]
  sale_year <- h[year == row$year]
  stopifnot(nrow(sale_year) == 1L, sale_year$area == row$building_sqft)
  later_larger <- h[year > row$year & area > row$building_sqft * area_change_ratio][order(year)]
  other_sales <- raw[.(row$pin)][row_id != row$row_id & sale_price > 10000]
  previous <- other_sales[recorded_date < row$sale_date][order(-as.numeric(recorded_date), row_id)]
  subsequent <- other_sales[recorded_date > row$sale_date][order(recorded_date, row_id)]
  evidence[[i]] <- data.table(row_id = row$row_id,
    history_first_year = min(h$year), history_last_year = max(h$year),
    history_min_sqft = min(h$area), history_max_sqft = max(h$area),
    first_larger_year = later_larger$year[1], first_larger_sqft = later_larger$area[1],
    first_larger_year_built = later_larger$built[1],
    prior_sale_date = previous$recorded_date[1], prior_sale_price = previous$sale_price[1],
    prior_sale_retained = previous$in_current_citywide_sample[1],
    next_sale_date = subsequent$recorded_date[1], next_sale_price = subsequent$sale_price[1],
    next_sale_retained = subsequent$in_current_citywide_sample[1])
}
evidence <- rbindlist(evidence)
stopifnot(!anyDuplicated(evidence$row_id), identical(review$row_id, evidence$row_id))
review <- merge(review, evidence, by = "row_id", all.x = TRUE, sort = FALSE)
review[, `:=`(sale_seller_name = raw$sale_seller_name[match(row_id, raw$row_id)],
             sale_buyer_name = raw$sale_buyer_name[match(row_id, raw$row_id)])]
fwrite(review[order(year, -real_ppsf), .(row_id, pin, sale_document_num, sale_date, year, class,
  selection, tail_probability, flag_annual, flag_pooled, flag_annual_nominal, sampled_for_reading,
  in_current_main_rd, signed_dist_ft, ward, neighbor_ward, segment_id,
  sale_price_nominal, building_sqft, real_ppsf, nominal_ppsf, annual_cutoff_real, pooled_cutoff_real,
  land_sqft, year_built, num_rooms, num_bedrooms, num_full_baths, num_half_baths,
  sale_deed_type, mydec_deed_type, is_mydec_date, sale_seller_name, sale_buyer_name,
  raw_document_pins, actual_card_rows, improvement_class_mismatch,
  history_first_year, history_last_year, history_min_sqft, history_max_sqft,
  first_larger_year, first_larger_sqft, first_larger_year_built,
  prior_sale_date, prior_sale_price, prior_sale_retained,
  next_sale_date, next_sale_price, next_sale_retained)], "../output/tail_review.csv")
ReportData("../output/tail_review.csv")
