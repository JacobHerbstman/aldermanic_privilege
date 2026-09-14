# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_record_quality_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
sales <- as.data.table(read_parquet("../output/quality_records.parquet"))
dates <- fread("../output/date_assignment_review.csv", colClasses = c(row_id = "character"))
review <- fread("../output/property_review.csv", colClasses = c(row_id = "character", pin = "character"))
requested <- sales[row_id %in% c(dates$row_id, review$row_id) |
                     (in_current_main_rd & (ward == 26L | neighbor_ward == 26L) &
                        sale_date >= as.Date("2009-05-01") & sale_date <= as.Date("2009-07-31"))]
stopifnot(!anyDuplicated(requested$row_id), nrow(requested) > 0)
responses <- list()
for (chunk in split(requested$row_id, ceiling(seq_len(nrow(requested)) / 100))) {
  query <- paste0(
    "https://datacatalog.cookcountyil.gov/resource/wvhk-k5uv.json?",
    "%24where=", URLencode(paste0("row_id in('", paste(chunk, collapse = "','"), "')"), reserved = TRUE),
    "&%24select=pin,year,row_id,doc_no,sale_price,sale_date,is_mydec_date,deed_type,mydec_deed_type,num_parcels_sale,is_multisale",
    "&%24limit=50000&%24order=row_id"
  )
  response <- curl::curl_fetch_memory(query, handle = curl::new_handle(timeout = 120))
  stopifnot(response$status_code == 200L)
  records <- jsonlite::fromJSON(rawToChar(response$content))
  if (length(records) > 0) responses[[length(responses) + 1L]] <- as.data.table(records)
}
current <- rbindlist(responses, fill = TRUE)
stopifnot(!anyDuplicated(current$row_id), all(current$row_id %in% requested$row_id))
current[, fetched_at_utc := format(Sys.time(), tz = "UTC", usetz = TRUE)]
fwrite(current, "../output/current_county_sales.csv")
ReportData("../output/current_county_sales.csv")
idx <- match(requested$row_id, current$row_id)
comparison <- requested[, .(row_id, pin, sale_document_num, sale_date, sale_price_nominal,
                            is_mydec_date, in_main_rd)]
comparison[, `:=`(
  current_record_found = !is.na(idx),
  current_pin = current$pin[idx],
  current_document = current$doc_no[idx],
  current_price = as.numeric(current$sale_price[idx]),
  current_date = as.Date(current$sale_date[idx]),
  current_is_mydec_date = current$is_mydec_date[idx]
)]
comparison[, `:=`(
  identifier_agrees = current_record_found & pin == current_pin & sale_document_num == current_document,
  price_changed = current_record_found & sale_price_nominal != current_price,
  date_changed = current_record_found & sale_date != current_date,
  refined_date_now_available = current_record_found & !is_mydec_date & current_is_mydec_date
)]

# For price/characteristic cases, also inspect later property descriptions.
# Later characteristics are diagnostic evidence, never replacements for sale-year data.
query <- paste0(
  "https://datacatalog.cookcountyil.gov/resource/x54s-btds.json?",
  "%24where=", URLencode(paste0("pin in('", paste(unique(review$pin), collapse = "','"), "')"), reserved = TRUE),
  "&%24select=pin,year,card,class,char_yrblt,char_bldg_sf,char_rooms,char_beds,char_fbath,char_hbath,char_apts,pin_num_cards,pin_is_multicard,tieback_proration_rate",
  "&%24limit=50000&%24order=pin,year,card"
)
response <- curl::curl_fetch_memory(query, handle = curl::new_handle(timeout = 120))
stopifnot(response$status_code == 200L)
current_improvements <- as.data.table(jsonlite::fromJSON(rawToChar(response$content)))
stopifnot(nrow(current_improvements) < 50000L,
          !anyDuplicated(current_improvements[, .(pin, year, card)]),
          all(current_improvements$pin %in% review$pin))
current_improvements[, fetched_at_utc := format(Sys.time(), tz = "UTC", usetz = TRUE)]
fwrite(current_improvements, "../output/current_county_improvements.csv")
ReportData("../output/current_county_improvements.csv")
current_improvements[, `:=`(year = as.integer(year),
                             char_bldg_sf = as.numeric(char_bldg_sf),
                             char_yrblt = as.integer(char_yrblt),
                             char_rooms = as.numeric(char_rooms),
                             char_beds = as.numeric(char_beds))]
current_improvements[, actual_cards := .N, by = .(pin, year)]
property_comparisons <- list()
for (i in seq_len(nrow(review))) {
  row <- sales[row_id == review$row_id[i]]
  h <- current_improvements[pin == row$pin & actual_cards == 1L]
  exact <- h[year == row$year]
  later <- h[year > row$year & (
    char_bldg_sf != row$building_sqft | char_yrblt != row$year_built |
      char_rooms != row$num_rooms | char_beds != row$num_bedrooms
  )][order(year)][1]
  property_comparisons[[i]] <- data.table(
    row_id = row$row_id, pin = row$pin, sale_year = row$year,
    in_main_rd = row$in_main_rd, nominal_price = row$sale_price_nominal,
    original_sqft = row$building_sqft, original_year_built = row$year_built,
    original_rooms = row$num_rooms, original_bedrooms = row$num_bedrooms,
    current_exact_year_found = nrow(exact) == 1L,
    current_exact_year_changed = if (nrow(exact) == 1L) {
      any(c(exact$char_bldg_sf != row$building_sqft, exact$char_yrblt != row$year_built,
            exact$char_rooms != row$num_rooms, exact$char_beds != row$num_bedrooms), na.rm = TRUE)
    } else NA,
    first_later_change_year = later$year, later_sqft = later$char_bldg_sf,
    later_year_built = later$char_yrblt, later_rooms = later$char_rooms,
    later_bedrooms = later$char_beds
  )
}
fwrite(rbindlist(property_comparisons), "../output/current_characteristic_comparison.csv")
ReportData("../output/current_characteristic_comparison.csv")
fwrite(comparison, "../output/current_county_comparison.csv")
ReportData("../output/current_county_comparison.csv")
