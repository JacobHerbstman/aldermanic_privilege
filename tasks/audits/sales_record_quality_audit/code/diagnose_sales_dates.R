# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_record_quality_audit/code")
# bandwidth_ft <- 500
# bin_width_ft <- 100

source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
args <- if (interactive()) c(bandwidth_ft, bin_width_ft) else commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L, all(is.finite(as.numeric(args))))
bandwidth_ft <- as.numeric(args[1])
bin_width_ft <- as.numeric(args[2])
stopifnot(bandwidth_ft > 0, bin_width_ft > 0, bandwidth_ft %% bin_width_ft == 0)

sales <- as.data.table(read_parquet("../output/quality_records.parquet"))[in_current_main_rd == TRUE]
cases <- fread("../output/date_assignment_review.csv", colClasses = c(row_id = "character", pin = "character"))
alternatives <- fread("../output/date_assignment_possibilities.csv", colClasses = c(row_id = "character", pin = "character"))
county <- fread("../output/current_county_comparison.csv", colClasses = c(row_id = "character", pin = "character"))
stopifnot(!anyDuplicated(sales$row_id), !anyDuplicated(cases$row_id), !anyDuplicated(county$row_id),
          all(abs(sales$signed_dist_ft) < bandwidth_ft))
matched <- match(sales$row_id, cases$row_id)
for (flag in c("any_person_change", "affects_rd_assignment", "sign_changes_with_both_eligible",
               "score_eligibility_changes")) {
  set(sales, j = flag, value = !is.na(matched) & cases[[flag]][matched])
}

rows <- sales[any_person_change == TRUE,
              .(row_id, pin, sale_document_num, sale_date, is_mydec_date,
                sale_price_nominal, class, ward, neighbor_ward, signed_dist_ft,
                alderman_own, alderman_neighbor, strictness_own, strictness_neighbor,
                affects_rd_assignment, sign_changes_with_both_eligible,
                score_eligibility_changes)]
matched <- match(rows$row_id, cases$row_id)
rows[, `:=`(possible_own_aldermen = cases$possible_own_aldermen[matched],
             possible_neighbor_aldermen = cases$possible_neighbor_aldermen[matched])]
matched <- match(rows$row_id, county$row_id)
rows[, `:=`(county_record_checked = !is.na(matched),
             county_identifier_agrees = county$identifier_agrees[matched],
             county_date = county$current_date[matched],
             county_date_changed = county$date_changed[matched],
             county_refined_date_now_available = county$refined_date_now_available[matched])]
rows[, diagnosis := fcase(
  sign_changes_with_both_eligible, "Missing day can reverse which side is more stringent",
  score_eligibility_changes, "Missing day can place a ward in a verified vacancy",
  default = "Possible alderman changes, but binned-RD sign and eligibility do not"
)]
setorder(rows, sale_date, ward, neighbor_ward, row_id)

# Public index dates are evidence, not automatic replacements for Assessor dates.
# Search by document number and independently check the indexed PIN. Leading
# zeros normalize identifiers; no dates are decoded from document numbers.
if (nrow(rows) > 0L) {
  clerk_records <- list()
  for (i in seq_len(nrow(rows))) {
  document <- str_pad(rows$sale_document_num[i], width = 10, side = "left", pad = "0")
  result_url <- paste0("https://crs.cookcountyclerkil.gov/Search/Result?id1=", document)
  response <- curl::curl_fetch_memory(result_url, curl::new_handle(timeout = 30))
  stopifnot(response$status_code == 200L)
  page <- xml2::read_html(rawToChar(response$content))
  headers <- trimws(xml2::xml_text(xml2::xml_find_all(page, "//table[@id='tblData']//th")))
  stopifnot(all(c("Doc Number", "Doc Recorded", "Doc Executed", "1st PIN") %in% headers))
  matches <- xml2::xml_find_all(page, paste0(
    "//table[@id='tblData']//tr[normalize-space(td[3])='", document, "']"))
  stopifnot(length(matches) <= 1L)
  record <- data.table(row_id = rows$row_id[i], clerk_document = document,
                       clerk_index_found = length(matches) == 1L, clerk_pin = NA_character_,
                       clerk_recorded = as.Date(NA), clerk_executed = as.Date(NA),
                       clerk_document_type = NA_character_, clerk_result_url = result_url,
                       clerk_detail_url = NA_character_, clerk_under_pins = NA_character_,
                       clerk_property_type = NA_character_,
                       clerk_fetched_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  if (length(matches) == 1L) {
    fields <- trimws(xml2::xml_text(xml2::xml_find_all(matches[[1]], "./td")))
    stopifnot(length(fields) == 10L, fields[3] == document)
    pin_link <- xml2::xml_attr(xml2::xml_find_first(matches[[1]],
                              ".//a[contains(@href, '/Search/ResultByPin?id1=')]"), "href")
    record[, `:=`(clerk_pin = sub(".*id1=", "", pin_link),
                   clerk_recorded = as.Date(fields[4], format = "%m/%d/%Y"),
                   clerk_executed = as.Date(fields[5], format = "%m/%d/%Y"),
                   clerk_document_type = fields[6])]
    stopifnot(!is.na(record$clerk_recorded))
    if (is.na(record$clerk_pin) || record$clerk_pin != rows$pin[i]) {
      detail_link <- xml2::xml_attr(xml2::xml_find_first(matches[[1]],
                                    ".//a[contains(@href, '/Document/Detail')]"), "href")
      stopifnot(!is.na(detail_link))
      record[, clerk_detail_url := paste0("https://crs.cookcountyclerkil.gov", detail_link)]
      detail_response <- curl::curl_fetch_memory(record$clerk_detail_url, curl::new_handle(timeout = 30))
      stopifnot(detail_response$status_code == 200L)
      detail <- xml2::read_html(rawToChar(detail_response$content))
      under_pins <- xml2::xml_text(xml2::xml_find_all(detail,
        "//table[.//th[normalize-space(.)='Under PIN']]//tr[td]/td[2]"))
      property_type <- trimws(xml2::xml_text(xml2::xml_find_all(detail,
        "//table[.//th[normalize-space(.)='PropType']]//tr[td]/td[3]")))
      record[, `:=`(clerk_under_pins = paste(gsub("[^0-9]", "", under_pins), collapse = ";"),
                     clerk_property_type = paste(unique(property_type), collapse = ";"))]
    }
  }
    clerk_records[[i]] <- record
  }
  clerk_records <- rbindlist(clerk_records)
  stopifnot(!anyDuplicated(clerk_records$row_id), identical(rows$row_id, clerk_records$row_id))
  rows <- cbind(rows, clerk_records[, !"row_id"])
  rows[, `:=`(clerk_pin_agrees = clerk_index_found & !is.na(clerk_pin) & pin == clerk_pin,
               recording_month_agrees = format(sale_date, "%Y-%m") == format(clerk_recorded, "%Y-%m"),
               execution_month_agrees = format(sale_date, "%Y-%m") == format(clerk_executed, "%Y-%m"),
               recording_quarter_agrees = year(sale_date) == year(clerk_recorded) &
                 quarter(sale_date) == quarter(clerk_recorded))]
  rows[, clerk_under_pin_agrees := mapply(function(pin, under_pins) {
    !is.na(under_pins) && pin %in% strsplit(under_pins, ";", fixed = TRUE)[[1]]
  }, pin, clerk_under_pins)]
  rows[, clerk_linkage := fcase(
    clerk_pin_agrees, "Exact indexed PIN",
    clerk_under_pin_agrees, "Matches Under PIN only; property identity/characteristics need review",
    default = "Indexed PIN mismatch unresolved"
  )]
} else {
  rows[, `:=`(
    clerk_document = character(), clerk_index_found = logical(), clerk_pin = character(),
    clerk_recorded = as.Date(character()), clerk_executed = as.Date(character()),
    clerk_document_type = character(), clerk_result_url = character(),
    clerk_detail_url = character(), clerk_under_pins = character(),
    clerk_property_type = character(), clerk_fetched_at_utc = character(),
    clerk_pin_agrees = logical(), recording_month_agrees = logical(),
    execution_month_agrees = logical(), recording_quarter_agrees = logical(),
    clerk_under_pin_agrees = logical(), clerk_linkage = character()
  )]
}
fwrite(rows, "../output/date_row_diagnosis.csv")
ReportData("../output/date_row_diagnosis.csv")
fwrite(sales[any_person_change == TRUE, .(
  possible_person_changes = sum(any_person_change),
  assignment_sensitive = sum(affects_rd_assignment),
  side_can_flip = sum(sign_changes_with_both_eligible),
  eligibility_can_change = sum(score_eligibility_changes)
), by = .(month = format(sale_date, "%Y-%m"))][order(month)],
"../output/date_diagnosis_by_month.csv")
ReportData("../output/date_diagnosis_by_month.csv")

# This is a fixed-current-cohort stress test, not a rebuilt timing sample:
# formerly excluded sales cannot enter, and stringency scores are held fixed.
setorder(alternatives, row_id, interval_start)
latest <- alternatives[, .SD[.N], by = row_id]
stopifnot(!anyDuplicated(latest$row_id))
matched <- match(sales$row_id, latest$row_id)
sales[, `:=`(latest_sign = sign, latest_eligible = TRUE)]
sales[!is.na(matched), `:=`(latest_sign = latest$possible_sign[matched[!is.na(matched)]],
                           latest_eligible = latest$score_eligible[matched[!is.na(matched)]])]
stopifnot(!anyNA(sales$latest_eligible))
specifications <- c("Current production", "Exclude possible side flips", "Exclude possible eligibility changes",
                    "Exclude all assignment-sensitive dates", "Exclude all possible person changes",
                    "Assign month-end on current cohort")
fits <- list()
for (specification in specifications) {
  sample <- copy(sales)
  if (specification == specifications[2]) sample <- sample[sign_changes_with_both_eligible == FALSE]
  if (specification == specifications[3]) sample <- sample[score_eligibility_changes == FALSE]
  if (specification == specifications[4]) sample <- sample[affects_rd_assignment == FALSE]
  if (specification == specifications[5]) sample <- sample[any_person_change == FALSE]
  if (specification == specifications[6]) {
    sample <- sample[latest_eligible == TRUE]
    sample[, signed_dist_ft := abs(signed_dist_ft) * latest_sign]
  }
  sample[, `:=`(property_class_factor = factor(class),
                 distance_bin = cut(signed_dist_ft, breaks = seq(-bandwidth_ft, bandwidth_ft, bin_width_ft),
                                    labels = FALSE, right = FALSE, include.lowest = TRUE))]
  reference <- bandwidth_ft / bin_width_ft
  model <- feols(
    log(sale_price) ~ i(distance_bin, ref = reference) +
      log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage +
      nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft +
      nearest_cta_stop_dist_ft + lake_michigan_dist_ft + property_class_factor |
      segment_id^year_quarter,
    data = sample, cluster = ~ward_pair_id, notes = FALSE, warn = FALSE
  )
  result <- coeftable(model)[paste0("distance_bin::", reference + 1), ]
  fits[[length(fits) + 1L]] <- data.table(
    specification, observations = nobs(model), excluded = nrow(sales) - nobs(model),
    estimate = unname(result[1]), std_error = unname(result[2]), p_value = unname(result[4]),
    percent_difference = 100 * expm1(unname(result[1]))
  )
}
fits <- rbindlist(fits)
official <- fread("../input/price_boundary_property_type_fe_estimates.csv")[market == "sales" & property_type_fe]
stopifnot(nrow(official) == 1L, fits$observations[1] == official$n,
          abs(fits$estimate[1] - official$estimate) < 1e-8,
          abs(fits$std_error[1] - official$std_error) < 1e-8)
fwrite(fits, "../output/date_rd_sensitivity.csv")
ReportData("../output/date_rd_sensitivity.csv")
