# Audit raw residential construction sources against the active density sample.

# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/new_construction_completeness_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

library(data.table)
library(sf)

audit_generated_on <- as.character(Sys.Date())
min_construction_year <- 2006L
max_construction_year <- 2022L
crs_projected <- 3435
commercial_large_unit_threshold <- 20
commercial_large_sqft_threshold <- 50000

normalize_pin <- function(x) {
  x <- as.character(x)
  out <- gsub("[^0-9]", "", x)
  short_leading_zero_pin <- !is.na(out) & nchar(out) == 13
  out[short_leading_zero_pin] <- paste0("0", out[short_leading_zero_pin])
  out[nchar(out) < 14] <- NA_character_
  out <- substr(out, 1, 14)
  out[out == "" | out == "NA"] <- NA_character_
  out
}

pin10_from_pin <- function(x) {
  out <- substr(as.character(x), 1, 10)
  out[is.na(x) | nchar(out) < 10] <- NA_character_
  out
}

as_num_clean <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

as_int_year <- function(x) {
  suppressWarnings(as.integer(as_num_clean(x)))
}

parse_unit_words <- function(x) {
  x_clean <- tolower(trimws(as.character(x)))
  out <- suppressWarnings(as.integer(as_num_clean(x_clean)))
  out[x_clean %in% c("none", "zero", "no", "na", "n/a", "")] <- 0L
  out[x_clean == "one"] <- 1L
  out[x_clean == "two"] <- 2L
  out[x_clean == "three"] <- 3L
  out[x_clean == "four"] <- 4L
  out[x_clean == "five"] <- 5L
  out[x_clean == "six"] <- 6L
  out[is.na(x)] <- NA_integer_
  out
}

parse_mdy <- function(x) {
  as.Date(as.character(x), format = "%m/%d/%Y")
}

date_year <- function(x) {
  out <- suppressWarnings(as.integer(format(as.Date(x), "%Y")))
  out
}

make_june_date <- function(year_value) {
  year_value <- suppressWarnings(as.integer(year_value))
  out <- rep(as.Date(NA), length(year_value))
  ok <- !is.na(year_value)
  out[ok] <- as.Date(sprintf("%04d-06-15", year_value[ok]))
  out
}

first_nonmissing <- function(...) {
  vals <- list(...)
  Reduce(function(a, b) fifelse(!is.na(a) & a != "", a, b), vals)
}

clean_text <- function(...) {
  paste(vapply(list(...), function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    x
  }, character(length(list(...)[[1]]))), collapse = " ")
}

unit_bin <- function(units) {
  fifelse(
    is.na(units), "unknown",
    fifelse(units <= 0, "0",
      fifelse(units == 1, "1",
        fifelse(units <= 3, "2-3",
          fifelse(units <= 6, "4-6",
            fifelse(units <= 20, "7-20", "21+")
          )
        )
      )
    )
  )
}

google_maps_url <- function(lat, lon) {
  ok <- is.finite(lat) & is.finite(lon)
  out <- rep(NA_character_, length(lat))
  out[ok] <- paste0(
    "https://www.google.com/maps/search/?api=1&query=",
    sprintf("%.7f", lat[ok]), ",", sprintf("%.7f", lon[ok])
  )
  out
}

google_search_url <- function(address, pin, lat, lon) {
  query <- ifelse(
    !is.na(address) & nzchar(address),
    paste(address, "Chicago IL"),
    ifelse(!is.na(pin) & nzchar(pin), paste("Cook County PIN", pin), paste(lat, lon, "Chicago"))
  )
  paste0(
    "https://www.google.com/search?q=",
    vapply(query, utils::URLencode, character(1), reserved = TRUE)
  )
}

extract_permit_pins <- function(x) {
  x <- as.character(x)
  hits <- stringr::str_extract_all(x, "\\d{2}-?\\d{2}-?\\d{3}-?\\d{3}-?\\d{4}")[[1]]
  hits <- normalize_pin(hits)
  unique(hits[!is.na(hits)])
}

read_bps_chicago_year <- function(year_value) {
  url <- sprintf(
    "https://www2.census.gov/econ/bps/Place/Midwest%%20Region/mw%02d12y.txt",
    year_value %% 100
  )
  raw <- fread(url, header = FALSE, fill = TRUE, colClasses = "character", showProgress = FALSE)
  city_row <- NA_integer_
  city_col <- NA_integer_
  for (j in seq_along(raw)) {
    matches <- which(trimws(raw[[j]]) == "Chicago" & trimws(raw[["V2"]]) == "17")
    if (length(matches) > 0) {
      city_row <- matches[1]
      city_col <- j
      break
    }
  }
  if (is.na(city_row) || is.na(city_col)) {
    stop(sprintf("Could not find Chicago row in Census BPS file for %d.", year_value), call. = FALSE)
  }
  one_value <- function(offset) as_num_clean(raw[[city_col + offset]][city_row])
  data.table(
    year = year_value,
    bps_source_url = url,
    bps_1unit_buildings = one_value(1),
    bps_1unit_units = one_value(2),
    bps_1unit_value = one_value(3),
    bps_2unit_buildings = one_value(4),
    bps_2unit_units = one_value(5),
    bps_2unit_value = one_value(6),
    bps_3to4unit_buildings = one_value(7),
    bps_3to4unit_units = one_value(8),
    bps_3to4unit_value = one_value(9),
    bps_5plus_buildings = one_value(10),
    bps_5plus_units = one_value(11),
    bps_5plus_value = one_value(12)
  )
}

safe_fread <- function(path, select = NULL) {
  fread(path, select = select, na.strings = c("", "NA", "N/A", "NULL"))
}

candidate_year_in_window <- function(year_value) {
  !is.na(year_value) & year_value >= min_construction_year & year_value <= max_construction_year
}

message("Loading parcel coordinates...")
parcel_locs <- safe_fread(
  "../input/parcel_universe.csv",
  select = c("pin", "pin10", "latitude", "longitude")
)
parcel_locs[, pin := normalize_pin(pin)]
parcel_locs[, pin10 := fifelse(is.na(pin10) | pin10 == "", pin10_from_pin(pin), as.character(pin10))]
parcel_locs[, latitude := as_num_clean(latitude)]
parcel_locs[, longitude := as_num_clean(longitude)]
parcel_locs <- parcel_locs[
  !is.na(pin),
  .(
    parcel_pin10 = first_nonmissing(pin10),
    parcel_latitude = latitude[which.max(is.finite(latitude) & is.finite(longitude))],
    parcel_longitude = longitude[which.max(is.finite(latitude) & is.finite(longitude))]
  ),
  by = pin
]
parcel_locs[!is.finite(parcel_latitude) | !is.finite(parcel_longitude), `:=`(
  parcel_latitude = NA_real_,
  parcel_longitude = NA_real_
)]

message("Loading cleaned and final construction pipeline outputs...")
res_clean <- safe_fread("../input/residential_cross_section.csv")
res_clean[, pin := normalize_pin(pin)]
res_clean[, clean_residential_row := TRUE]
res_clean[, clean_res_row_id := as.character(row_id)]
res_clean[, clean_res_tax_year := suppressWarnings(as.integer(tax_year))]
res_clean[, clean_res_card_num := suppressWarnings(as.integer(card_num))]
res_clean[, clean_res_year := as_int_year(year_built)]
res_clean <- res_clean[
  !is.na(pin),
  .(
    clean_residential_row = TRUE,
    clean_res_row_id = first_nonmissing(clean_res_row_id),
    clean_res_tax_year = clean_res_tax_year[1],
    clean_res_card_num = clean_res_card_num[1],
    clean_res_year = clean_res_year[1]
  ),
  by = pin
]

comm_clean <- safe_fread("../input/multifamily_data_cleaned.csv")
if (!"source_row_id" %in% names(comm_clean)) {
  stop("Cleaned commercial data must include source_row_id for source-row audit diagnostics.", call. = FALSE)
}
comm_clean[, pin := normalize_pin(pin)]
comm_clean[, clean_commercial_row := TRUE]
comm_clean[, clean_comm_source_row_id := as.character(source_row_id)]
comm_clean[, clean_comm_year := as_int_year(yearbuilt)]
comm_clean[, clean_comm_units := as_num_clean(tot_units)]
comm_clean[, clean_comm_address := as.character(address)]
comm_clean_source_sizes <- comm_clean[
  !is.na(clean_comm_source_row_id),
  .(
    source_row_id = clean_comm_source_row_id,
    clean_comm_building_sqft = as_num_clean(bldgsf),
    clean_comm_land_sqft = as_num_clean(landsf)
  )
]
comm_clean <- comm_clean[
  !is.na(pin),
  .(
    clean_commercial_row = TRUE,
    clean_comm_source_row_id = first_nonmissing(clean_comm_source_row_id),
    clean_comm_year = clean_comm_year[1],
    clean_comm_units = clean_comm_units[1],
    clean_comm_address = first_nonmissing(clean_comm_address)
  ),
  by = pin
]

pre_scores <- safe_fread("../input/parcels_pre_scores.csv")
pre_scores[, pin := normalize_pin(pin)]
pre_scores <- pre_scores[
  !is.na(pin),
  .(
    pre_scores_row = TRUE,
    pre_construction_year = suppressWarnings(as.integer(construction_year[1])),
    pre_dist_to_boundary = as_num_clean(dist_to_boundary[1]),
    pre_ward_pair = as.character(ward_pair[1]),
    pre_ward = suppressWarnings(as.integer(ward[1])),
    pre_unitscount = as_num_clean(unitscount[1]),
    pre_arealotsf = as_num_clean(arealotsf[1]),
    pre_areabuilding = as_num_clean(areabuilding[1])
  ),
  by = pin
]

segment_lookup <- safe_fread("../input/parcel_segment_ids.csv")
segment_lookup[, pin := normalize_pin(pin)]
segment_lookup <- segment_lookup[
  !is.na(pin),
  .(
    segment_lookup_row = TRUE,
    segment_lookup_id = as.character(segment_id[1]),
    segment_lookup_reason = as.character(segment_reason[1])
  ),
  by = pin
]

final <- safe_fread("../input/parcels_with_ward_distances.csv")
final[, pin := normalize_pin(pin)]
final[, construction_year := suppressWarnings(as.integer(construction_year))]
final[, unitscount := as_num_clean(unitscount)]
final[, arealotsf := as_num_clean(arealotsf)]
final[, areabuilding := as_num_clean(areabuilding)]
final[, dist_to_boundary := as_num_clean(dist_to_boundary)]
final[, segment_id := as.character(segment_id)]
final[, final_all_base := arealotsf > 1 & areabuilding > 1 & unitscount > 0 & construction_year >= min_construction_year]
final[, final_multifamily_base := final_all_base & unitscount > 1]
final[, final_all_250 := final_all_base & dist_to_boundary <= 250]
final[, final_multifamily_500 := final_multifamily_base & dist_to_boundary <= 500]
final[, final_multifamily_1000 := final_multifamily_base & dist_to_boundary <= 1000]
final[, final_multifamily_500_segment := final_multifamily_500 & !is.na(segment_id) & segment_id != ""]

final_counts <- final[, .(
  final_all_base = sum(final_all_base, na.rm = TRUE),
  final_multifamily_base = sum(final_multifamily_base, na.rm = TRUE),
  final_multifamily_500 = sum(final_multifamily_500, na.rm = TRUE)
)]

if (final_counts$final_all_base != 12572L ||
    final_counts$final_multifamily_base != 2278L ||
    final_counts$final_multifamily_500 != 741L) {
  stop(sprintf(
    "Final sample count validation failed: all=%d, multifamily=%d, mf500=%d.",
    final_counts$final_all_base,
    final_counts$final_multifamily_base,
    final_counts$final_multifamily_500
  ), call. = FALSE)
}

final_lookup <- final[
  !is.na(pin),
  .(
    final_scored_row = TRUE,
    final_construction_year = construction_year[1],
    final_dist_to_boundary = dist_to_boundary[1],
    final_ward_pair = as.character(ward_pair[1]),
    final_ward = suppressWarnings(as.integer(ward[1])),
    final_unitscount = unitscount[1],
    final_arealotsf = arealotsf[1],
    final_areabuilding = areabuilding[1],
    final_segment_id = as.character(segment_id[1]),
    final_all_base = final_all_base[1],
    final_multifamily_base = final_multifamily_base[1],
    final_all_250 = final_all_250[1],
    final_multifamily_500 = final_multifamily_500[1],
    final_multifamily_1000 = final_multifamily_1000[1],
    final_multifamily_500_segment = final_multifamily_500_segment[1],
    final_signed_distance = as_num_clean(signed_distance[1])
  ),
  by = pin
]

message("Building Census BPS benchmark...")
bps_chicago <- rbindlist(lapply(
  min_construction_year:max_construction_year,
  read_bps_chicago_year
))
bps_chicago[, `:=`(
  bps_total_buildings = bps_1unit_buildings + bps_2unit_buildings +
    bps_3to4unit_buildings + bps_5plus_buildings,
  bps_total_units = bps_1unit_units + bps_2unit_units +
    bps_3to4unit_units + bps_5plus_units,
  bps_multifamily_buildings = bps_2unit_buildings + bps_3to4unit_buildings +
    bps_5plus_buildings,
  bps_multifamily_units = bps_2unit_units + bps_3to4unit_units +
    bps_5plus_units
)]
setcolorder(bps_chicago, c(
  "year", "bps_source_url", "bps_total_buildings", "bps_total_units",
  "bps_multifamily_buildings", "bps_multifamily_units",
  setdiff(names(bps_chicago), c(
    "year", "bps_source_url", "bps_total_buildings", "bps_total_units",
    "bps_multifamily_buildings", "bps_multifamily_units"
  ))
))
fwrite(bps_chicago, "../output/census_bps_chicago_benchmark.csv", na = "")

outcome_by_year <- final[
  final_all_base == TRUE,
  .(
    outcome_total_buildings = .N,
    outcome_total_units = sum(unitscount, na.rm = TRUE),
    outcome_multifamily_buildings = sum(unitscount > 1, na.rm = TRUE),
    outcome_multifamily_units = sum(fifelse(unitscount > 1, unitscount, 0), na.rm = TRUE),
    outcome_1unit_buildings = sum(unitscount == 1, na.rm = TRUE),
    outcome_1unit_units = sum(fifelse(unitscount == 1, unitscount, 0), na.rm = TRUE),
    outcome_2unit_buildings = sum(unitscount == 2, na.rm = TRUE),
    outcome_2unit_units = sum(fifelse(unitscount == 2, unitscount, 0), na.rm = TRUE),
    outcome_3to4unit_buildings = sum(unitscount >= 3 & unitscount <= 4, na.rm = TRUE),
    outcome_3to4unit_units = sum(fifelse(unitscount >= 3 & unitscount <= 4, unitscount, 0), na.rm = TRUE),
    outcome_5plus_buildings = sum(unitscount >= 5, na.rm = TRUE),
    outcome_5plus_units = sum(fifelse(unitscount >= 5, unitscount, 0), na.rm = TRUE)
  ),
  by = .(year = construction_year)
]
bps_gap <- merge(bps_chicago, outcome_by_year, by = "year", all.x = TRUE, sort = FALSE)
for (v in names(outcome_by_year)[names(outcome_by_year) != "year"]) {
  bps_gap[is.na(get(v)), (v) := 0]
}
bps_gap[, `:=`(
  bps_minus_outcome_total_buildings = bps_total_buildings - outcome_total_buildings,
  bps_minus_outcome_total_units = bps_total_units - outcome_total_units,
  bps_minus_outcome_multifamily_buildings = bps_multifamily_buildings - outcome_multifamily_buildings,
  bps_minus_outcome_multifamily_units = bps_multifamily_units - outcome_multifamily_units,
  bps_minus_outcome_5plus_buildings = bps_5plus_buildings - outcome_5plus_buildings,
  bps_minus_outcome_5plus_units = bps_5plus_units - outcome_5plus_units,
  outcome_share_of_bps_total_units = outcome_total_units / bps_total_units,
  outcome_share_of_bps_multifamily_units = outcome_multifamily_units / bps_multifamily_units,
  outcome_share_of_bps_5plus_units = outcome_5plus_units / bps_5plus_units
)]
fwrite(bps_gap, "../output/outcome_vs_bps_yearly_gap.csv", na = "")

message("Building residential-improvements candidates from current raw rows...")
res_raw <- safe_fread(
  "../input/residential_improvement_characteristics.csv",
  select = c(
    "pin", "tax_year", "card_num", "class", "township_code", "year_built",
    "building_sqft", "land_sqft", "num_apartments", "single_v_multi_family",
    "type_of_residence", "row_id"
  )
)
res_raw[, raw_row_n := .I]
res_raw[, pin := normalize_pin(pin)]
res_raw[, pin10 := pin10_from_pin(pin)]
res_raw[, candidate_year := as_int_year(year_built)]
res_raw[, source_building_sqft := as_num_clean(building_sqft)]
res_raw[, source_land_sqft := as_num_clean(land_sqft)]
res_raw[, candidate_units := parse_unit_words(num_apartments)]
res_raw[, source_chicago := as.character(township_code) %in% c("70", "71", "72", "73", "74", "75", "76", "77")]
res_raw[, is_single_family_text := (
  grepl("^single", as.character(single_v_multi_family), ignore.case = TRUE) |
    as.character(type_of_residence) %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level")
)]
res_raw[is_single_family_text == TRUE & (is.na(candidate_units) | candidate_units == 0), candidate_units := 1L]
res_raw <- merge(res_raw, res_clean, by = "pin", all.x = TRUE, sort = FALSE)
res_raw[, selected_source_row := clean_residential_row == TRUE &
  mapply(
    function(raw_row_id, clean_row_ids) {
      raw_row_id <- as.character(raw_row_id)
      clean_row_ids <- trimws(unlist(strsplit(as.character(clean_row_ids), ",")))
      raw_row_id %in% clean_row_ids
    },
    row_id,
    clean_res_row_id
  )]
res_raw[is.na(selected_source_row), selected_source_row := FALSE]
res_raw[, source_pin_duplicate_n := .N, by = pin]
res_current_candidates <- res_raw[, .(
  source = "residential_current_raw",
  source_row_id = paste0("res_current_raw:", raw_row_n),
  source_record_id = paste(pin, tax_year, card_num, row_id, sep = "|"),
  pin,
  pin10,
  candidate_year,
  candidate_date = make_june_date(candidate_year),
  candidate_units = as.numeric(candidate_units),
  source_building_sqft,
  source_land_sqft,
  source_address = NA_character_,
  source_latitude = NA_real_,
  source_longitude = NA_real_,
  source_text = paste("class", class, "type", type_of_residence, "single_multi", single_v_multi_family),
  source_township = as.character(township_code),
  source_chicago,
  selected_source_row,
  source_pin_duplicate_n,
  candidate_residential_signal = TRUE,
  source_false_positive_tier = "assessor_residential",
  permit_issued = NA,
  permit_residential_text = NA,
  permit_pin_count = NA_integer_
)]

message("Building residential-improvements candidates from full historical panel...")
res_panel <- as.data.table(arrow::read_parquet("../input/residential_improvements_panel.parquet"))
res_panel[, pin := normalize_pin(pin)]
res_panel[, candidate_year := suppressWarnings(as.integer(year_built))]
res_panel[, candidate_units := as.numeric(num_apartments)]
res_panel[, source_building_sqft := as_num_clean(building_sqft)]
res_panel[, source_land_sqft := as_num_clean(land_sqft)]
res_panel[, panel_records_for_pin := .N, by = pin]
res_panel <- res_panel[!is.na(pin)]
setorder(res_panel, pin, candidate_year, tax_year, -source_building_sqft)
res_panel <- res_panel[, .SD[1], by = pin]
res_panel_candidates <- res_panel[, .(
  source = "residential_historical_panel",
  source_row_id = paste0("res_historical_panel:", pin),
  source_record_id = paste(pin, tax_year, sep = "|"),
  pin,
  pin10 = pin10_from_pin(pin),
  candidate_year,
  candidate_date = make_june_date(candidate_year),
  candidate_units,
  source_building_sqft,
  source_land_sqft,
  source_address = NA_character_,
  source_latitude = NA_real_,
  source_longitude = NA_real_,
  source_text = "historical residential improvements panel",
  source_township = NA_character_,
  source_chicago = TRUE,
  selected_source_row = TRUE,
  source_pin_duplicate_n = panel_records_for_pin,
  candidate_residential_signal = TRUE,
  source_false_positive_tier = "assessor_residential_panel",
  permit_issued = NA,
  permit_residential_text = NA,
  permit_pin_count = NA_integer_
)]

message("Building broad commercial multifamily candidates...")
comm_raw <- safe_fread(
  "../input/commercial_value_raw.csv",
  select = c(
    "keypin", "pins", "yearbuilt", "township", "modelgroup", "class(es)",
    "property_type/use", "studiounits", "1brunits", "2brunits", "3brunits",
    "4brunits", "tot_units", "address", "bldgsf", "landsf",
    "property_name/description", "owner", "taxpayer", "stories",
    "finalmarketvalue", "aprx_comm_sf", "netrentablesf", "category", "subclass2"
  )
)
setnames(
  comm_raw,
  old = c(
    "class(es)", "property_type/use", "1brunits", "2brunits", "3brunits",
    "4brunits", "property_name/description"
  ),
  new = c(
    "class_es", "property_type_use", "x1brunits", "x2brunits", "x3brunits",
    "x4brunits", "property_name_description"
  )
)
comm_raw[, raw_row_n := .I]
comm_raw[, source_row_id := paste0("commercial_raw:", raw_row_n)]
comm_raw[, pin := normalize_pin(keypin)]
comm_raw[, pin10 := pin10_from_pin(pin)]
comm_raw[, candidate_year := as_int_year(yearbuilt)]
for (v in c(
  "tot_units", "studiounits", "x1brunits", "x2brunits", "x3brunits",
  "x4brunits", "bldgsf", "landsf", "stories", "finalmarketvalue",
  "aprx_comm_sf", "netrentablesf"
)) {
  comm_raw[, (v) := as_num_clean(get(v))]
}
comm_raw[, calculated_units := rowSums(.SD, na.rm = TRUE), .SDcols = c("studiounits", "x1brunits", "x2brunits", "x3brunits", "x4brunits")]
comm_raw[, candidate_units := fcoalesce(tot_units, fifelse(calculated_units > 0, calculated_units, NA_real_))]
comm_raw[, source_building_sqft := bldgsf]
comm_raw[, source_land_sqft := landsf]
comm_raw <- merge(comm_raw, comm_clean_source_sizes, by = "source_row_id", all.x = TRUE, sort = FALSE)
comm_raw[!is.na(clean_comm_building_sqft), source_building_sqft := clean_comm_building_sqft]
comm_raw[!is.na(clean_comm_land_sqft), source_land_sqft := clean_comm_land_sqft]
comm_raw[, c("clean_comm_building_sqft", "clean_comm_land_sqft") := NULL]
comm_raw[, source_chicago := as.character(township) %in% c(
  "West Chicago", "South Chicago", "Jefferson", "North Chicago",
  "Lake View", "Rogers Park", "Hyde Park", "Lake"
)]
comm_raw[, commercial_primary_multifamily := grepl("Multifamily|Class3|Class9|Condos", as.character(modelgroup), ignore.case = TRUE)]
comm_raw[, commercial_text_blob := paste(modelgroup, class_es, property_type_use)]
comm_raw[, commercial_residential_text := grepl(
  "apartment|apartments|multifamily|multi-family|condo|condos|residential|dwelling|class3|class9",
  commercial_text_blob,
  ignore.case = TRUE
)]
comm_raw[, retail_multi_tenant_false_positive := grepl("Retail-Multi Tenant", as.character(property_type_use), ignore.case = TRUE) &
  !commercial_primary_multifamily]
comm_raw[, candidate_residential_signal := commercial_primary_multifamily |
  (commercial_residential_text & !retail_multi_tenant_false_positive)]
comm_raw[, source_false_positive_tier := fifelse(
  commercial_primary_multifamily,
  "primary_multifamily_modelgroup",
  fifelse(
    commercial_residential_text & !retail_multi_tenant_false_positive,
    "secondary_residential_text",
    fifelse(
      retail_multi_tenant_false_positive,
      "likely_retail_multi_tenant_false_positive",
      "no_residential_signal"
    )
  )
)]

comm_selected <- comm_clean[
  !is.na(clean_comm_source_row_id),
  .(pin, selected_comm_source_row_id = clean_comm_source_row_id)
]
comm_raw <- merge(comm_raw, comm_selected, by = "pin", all.x = TRUE, sort = FALSE)
comm_raw[, selected_source_row := source_row_id == selected_comm_source_row_id]
comm_raw[is.na(selected_source_row), selected_source_row := FALSE]
comm_raw[, source_pin_duplicate_n := .N, by = pin]
comm_candidates <- comm_raw[, .(
  source = "commercial_value_raw",
  source_row_id,
  source_record_id = paste(pin, yearbuilt, modelgroup, class_es, sep = "|"),
  pin,
  pin10,
  candidate_year,
  candidate_date = make_june_date(candidate_year),
  candidate_units,
  source_building_sqft,
  source_land_sqft,
  source_address = as.character(address),
  source_latitude = NA_real_,
  source_longitude = NA_real_,
  source_text = commercial_text_blob,
  source_township = as.character(township),
  source_chicago,
  selected_source_row,
  source_pin_duplicate_n,
  candidate_residential_signal,
  source_false_positive_tier,
  permit_issued = NA,
  permit_residential_text = NA,
  permit_pin_count = NA_integer_
)]

message("Building issued new-construction permit candidates...")
permit_raw <- safe_fread(
  "../input/building_permits_raw.csv",
  select = c(
    "ID", "PERMIT#", "PERMIT_STATUS", "PERMIT_TYPE", "APPLICATION_START_DATE",
    "ISSUE_DATE", "STREET_NUMBER", "STREET_DIRECTION", "STREET_NAME",
    "WORK_TYPE", "WORK_DESCRIPTION", "REPORTED_COST", "PIN_LIST",
    "WARD", "XCOORDINATE", "YCOORDINATE", "LATITUDE", "LONGITUDE"
  )
)
permit_raw[, raw_row_n := .I]
permit_raw <- permit_raw[PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION"]
permit_raw[, application_date := parse_mdy(APPLICATION_START_DATE)]
permit_raw[, issue_date := parse_mdy(ISSUE_DATE)]
permit_raw[, candidate_year := date_year(issue_date)]
permit_raw[, permit_issued := !is.na(issue_date) &
  PERMIT_STATUS %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING")]
permit_raw[, permit_latitude := as_num_clean(LATITUDE)]
permit_raw[, permit_longitude := as_num_clean(LONGITUDE)]
permit_raw[, xcoordinate := as_num_clean(XCOORDINATE)]
permit_raw[, ycoordinate := as_num_clean(YCOORDINATE)]

needs_xy_conversion <- (
  !is.finite(permit_raw$permit_latitude) |
    !is.finite(permit_raw$permit_longitude)
) &
  is.finite(permit_raw$xcoordinate) &
  is.finite(permit_raw$ycoordinate)
if (any(needs_xy_conversion)) {
  converted_sf <- st_as_sf(
    permit_raw[needs_xy_conversion],
    coords = c("xcoordinate", "ycoordinate"),
    crs = crs_projected,
    remove = FALSE
  ) |>
    st_transform(4326)
  converted_coords <- st_coordinates(converted_sf)
  permit_raw$permit_longitude[needs_xy_conversion] <- converted_coords[, "X"]
  permit_raw$permit_latitude[needs_xy_conversion] <- converted_coords[, "Y"]
}

permit_raw[, source_address := trimws(paste(STREET_NUMBER, STREET_DIRECTION, STREET_NAME))]
permit_raw[, source_address := fifelse(source_address == "" | is.na(source_address), NA_character_, source_address)]
permit_raw[, work_blob := paste(WORK_TYPE, WORK_DESCRIPTION)]
permit_raw[, permit_residential_text := grepl(
  "residential|dwelling|apartment|apartments|unit|units|multi-family|multifamily|single family|townhome|condo|duplex|two flat|three flat|coach house",
  work_blob,
  ignore.case = TRUE
)]
permit_raw[, parsed_units := as.numeric(stringr::str_match(
  tolower(work_blob),
  "\\b([0-9]{1,4})\\s*(dwelling\\s*units?|d\\.?\\s*u\\.?|du\\b|units?\\b|apartments?\\b|apts?\\b)"
)[, 2])]
permit_raw[, permit_pins := lapply(PIN_LIST, extract_permit_pins)]

permit_pin_rows <- permit_raw[, {
  pins <- permit_pins[[1]]
  if (length(pins) == 0) pins <- NA_character_
  .(pin = pins)
}, by = raw_row_n]

permit_candidates <- merge(
  permit_raw[, .(
    raw_row_n,
    permit_id = as.character(ID),
    permit_number = as.character(`PERMIT#`),
    candidate_year,
    candidate_date = issue_date,
    permit_issued,
    permit_residential_text,
    parsed_units,
    source_address,
    permit_latitude,
    permit_longitude,
    work_blob,
    permit_status = as.character(PERMIT_STATUS),
    permit_pin_count = lengths(permit_pins)
  )],
  permit_pin_rows,
  by = "raw_row_n",
  all.x = TRUE,
  sort = FALSE
)
permit_candidates[, pin := normalize_pin(pin)]
permit_candidates[, pin10 := pin10_from_pin(pin)]
permit_candidates[, selected_source_row := permit_issued == TRUE]
permit_candidates[, source_pin_duplicate_n := .N, by = permit_id]
permit_candidates[, source_chicago := TRUE]
permit_candidates[, candidate_residential_signal := permit_residential_text]
permit_candidates[, source_false_positive_tier := fifelse(
  permit_residential_text,
  "permit_residential_text",
  "permit_no_residential_text"
)]
permit_candidates <- permit_candidates[, .(
  source = "permit_new_construction",
  source_row_id = paste0("permit_new_construction:", raw_row_n, ":", fifelse(is.na(pin), "nopin", pin)),
  source_record_id = paste(permit_id, permit_number, pin, sep = "|"),
  pin,
  pin10,
  candidate_year,
  candidate_date,
  candidate_units = parsed_units,
  source_building_sqft = NA_real_,
  source_land_sqft = NA_real_,
  source_address,
  source_latitude = permit_latitude,
  source_longitude = permit_longitude,
  source_text = work_blob,
  source_township = NA_character_,
  source_chicago,
  selected_source_row,
  source_pin_duplicate_n,
  candidate_residential_signal,
  source_false_positive_tier,
  permit_issued,
  permit_residential_text,
  permit_pin_count
)]

message("Combining source candidates and joining pipeline stages...")
all_candidates <- rbindlist(
  list(res_current_candidates, res_panel_candidates, comm_candidates, permit_candidates),
  use.names = TRUE,
  fill = TRUE
)
all_candidates[, candidate_uid := paste(source, source_row_id, sep = "::")]
all_candidates[, candidate_year := suppressWarnings(as.integer(candidate_year))]
all_candidates[, candidate_units := as.numeric(candidate_units)]
all_candidates[, source_building_sqft := as.numeric(source_building_sqft)]
all_candidates[, source_land_sqft := as.numeric(source_land_sqft)]
all_candidates[, pin10 := fifelse(is.na(pin10) | pin10 == "", pin10_from_pin(pin), pin10)]

all_candidates <- merge(all_candidates, parcel_locs, by = "pin", all.x = TRUE, sort = FALSE)
all_candidates[, source_latitude := fcoalesce(source_latitude, parcel_latitude)]
all_candidates[, source_longitude := fcoalesce(source_longitude, parcel_longitude)]
all_candidates[, pin10 := fcoalesce(pin10, parcel_pin10)]

all_candidates <- merge(all_candidates, res_clean[, .(pin, clean_residential_row)], by = "pin", all.x = TRUE, sort = FALSE)
all_candidates <- merge(all_candidates, comm_clean[, .(pin, clean_commercial_row, clean_comm_address)], by = "pin", all.x = TRUE, sort = FALSE)
all_candidates <- merge(all_candidates, pre_scores, by = "pin", all.x = TRUE, sort = FALSE)
all_candidates <- merge(all_candidates, segment_lookup, by = "pin", all.x = TRUE, sort = FALSE)
all_candidates <- merge(all_candidates, final_lookup, by = "pin", all.x = TRUE, sort = FALSE)

for (v in c("clean_residential_row", "clean_commercial_row", "pre_scores_row", "segment_lookup_row", "final_scored_row")) {
  all_candidates[is.na(get(v)), (v) := FALSE]
}
for (v in c("final_all_base", "final_multifamily_base", "final_all_250", "final_multifamily_500", "final_multifamily_1000", "final_multifamily_500_segment")) {
  all_candidates[is.na(get(v)), (v) := FALSE]
}

all_candidates[, has_pin := !is.na(pin)]
all_candidates[, has_coordinates := is.finite(source_latitude) & is.finite(source_longitude)]
all_candidates[, in_year_window := candidate_year_in_window(candidate_year)]
all_candidates[, in_cleaned_source := fifelse(
  source %in% c("residential_current_raw", "residential_historical_panel"),
  clean_residential_row,
  fifelse(source == "commercial_value_raw", clean_commercial_row, FALSE)
)]
all_candidates[, active_row := selected_source_row == TRUE]
all_candidates[, active_final_all_base := active_row & final_all_base]
all_candidates[, active_final_multifamily_base := active_row & final_multifamily_base]
all_candidates[, active_final_all_250 := active_row & final_all_250]
all_candidates[, active_final_multifamily_500 := active_row & final_multifamily_500]
all_candidates[, active_final_multifamily_1000 := active_row & final_multifamily_1000]
all_candidates[, active_final_multifamily_500_segment := active_row & final_multifamily_500_segment]
all_candidates[, active_pre_scores := active_row & pre_scores_row]
all_candidates[, active_cleaned_source := active_row & in_cleaned_source]

message("Checking permit spatial/year fuzzy matches...")
final_coords <- merge(
  final_lookup[final_all_base == TRUE, .(pin, final_construction_year, final_dist_to_boundary)],
  parcel_locs,
  by = "pin",
  all.x = TRUE,
  sort = FALSE
)
final_coords <- final_coords[is.finite(parcel_latitude) & is.finite(parcel_longitude)]
permit_for_nearest <- unique(
  all_candidates[
    source == "permit_new_construction" &
      active_row == TRUE &
      in_year_window == TRUE &
      has_coordinates == TRUE &
      !final_scored_row,
    .(candidate_uid, source_latitude, source_longitude, candidate_year)
  ],
  by = "candidate_uid"
)
permit_nearest <- data.table(
  candidate_uid = character(),
  permit_nearest_final_pin = character(),
  permit_nearest_final_dist_ft = numeric(),
  permit_nearest_final_year_gap = integer(),
  permit_spatial_year_match_final = logical()
)
if (nrow(permit_for_nearest) > 0 && nrow(final_coords) > 0) {
  permit_sf <- st_as_sf(
    permit_for_nearest,
    coords = c("source_longitude", "source_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    st_transform(crs_projected)
  final_sf <- st_as_sf(
    final_coords,
    coords = c("parcel_longitude", "parcel_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    st_transform(crs_projected)
  nearest_idx <- st_nearest_feature(permit_sf, final_sf)
  nearest_dist <- as.numeric(st_distance(permit_sf, final_sf[nearest_idx, ], by_element = TRUE))
  permit_nearest <- data.table(
    candidate_uid = permit_for_nearest$candidate_uid,
    permit_nearest_final_pin = final_coords$pin[nearest_idx],
    permit_nearest_final_dist_ft = nearest_dist,
    permit_nearest_final_year_gap = abs(permit_for_nearest$candidate_year - final_coords$final_construction_year[nearest_idx])
  )
  permit_nearest[, permit_spatial_year_match_final := permit_nearest_final_dist_ft <= 250 &
    permit_nearest_final_year_gap <= 3]
}
all_candidates <- merge(all_candidates, permit_nearest, by = "candidate_uid", all.x = TRUE, sort = FALSE)
all_candidates[is.na(permit_spatial_year_match_final), permit_spatial_year_match_final := FALSE]
all_candidates[, permit_pin_match_final := source == "permit_new_construction" & final_scored_row == TRUE]
all_candidates[source == "permit_new_construction" & (permit_pin_match_final | permit_spatial_year_match_final), candidate_residential_signal := TRUE]

message("Assigning independent boundary distances to selected omitted candidates with coordinates...")
distance_needs <- unique(
  all_candidates[
    active_row == TRUE &
      in_year_window == TRUE &
      candidate_residential_signal == TRUE &
      final_scored_row == FALSE &
      has_coordinates == TRUE,
    .(candidate_uid, source_latitude, source_longitude, candidate_date)
  ],
  by = "candidate_uid"
)
independent_distance <- data.table(
  candidate_uid = character(),
  audit_ward = integer(),
  audit_ward_pair = character(),
  audit_dist_to_boundary = numeric()
)
if (nrow(distance_needs) > 0) {
  ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
  ward_maps <- load_canonical_ward_maps(ward_panel)
  boundary_layers <- load_boundary_layers("../input/ward_pair_boundaries.gpkg")
  distance_sf <- st_as_sf(
    distance_needs,
    coords = c("source_longitude", "source_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    st_transform(st_crs(ward_panel))
  distance_era <- canonical_era_from_date(distance_needs$candidate_date)
  distance_assignment <- assign_points_to_boundaries(
    points_sf = distance_sf,
    era_values = distance_era,
    ward_maps = ward_maps,
    boundary_lines = boundary_layers,
    chunk_n = 5000L
  )
  independent_distance <- data.table(
    candidate_uid = distance_needs$candidate_uid,
    audit_ward = distance_assignment$ward,
    audit_ward_pair = distance_assignment$ward_pair_id,
    audit_dist_to_boundary = distance_assignment$dist_ft
  )
}
all_candidates <- merge(all_candidates, independent_distance, by = "candidate_uid", all.x = TRUE, sort = FALSE)
all_candidates[, best_dist_to_boundary := fcoalesce(final_dist_to_boundary, pre_dist_to_boundary, audit_dist_to_boundary)]
all_candidates[, best_ward_pair := fcoalesce(final_ward_pair, pre_ward_pair, audit_ward_pair)]

message("Assigning terminal audit reasons...")
all_candidates[, terminal_reason := fifelse(
  active_row == FALSE,
  "duplicate_lower_priority_source_row",
  fifelse(
    source == "permit_new_construction" & permit_issued != TRUE,
    "permit_not_issued",
    fifelse(
      source_chicago != TRUE,
      "non_chicago_township",
      fifelse(
        !in_year_window,
        "out_of_year_window",
        fifelse(
          source == "commercial_value_raw" & candidate_residential_signal != TRUE,
          "nonresidential_commercial_false_positive",
          fifelse(
            source == "permit_new_construction" &
              candidate_residential_signal != TRUE &
              permit_pin_match_final != TRUE &
              permit_spatial_year_match_final != TRUE,
            "permit_new_construction_no_residential_signal",
            fifelse(
              active_final_multifamily_500 == TRUE,
              "final_bw500_multifamily",
              fifelse(
                active_final_all_250 == TRUE,
                "final_bw250_all",
                fifelse(
                  active_final_multifamily_base == TRUE,
                  "final_multifamily_outside_main_band",
                  fifelse(
                    active_final_all_base == TRUE,
                    "final_all_outside_appendix_band",
                    fifelse(
                      has_pin != TRUE & source != "permit_new_construction",
                      "missing_pin",
                      fifelse(
                        source != "permit_new_construction" & (is.na(candidate_units) | candidate_units <= 0),
                        "missing_unit_count",
                        fifelse(
                          source != "permit_new_construction" &
                            (is.na(source_building_sqft) | is.na(source_land_sqft) |
                               source_building_sqft <= 1 | source_land_sqft <= 1),
                          "missing_lot_or_building_size",
                          fifelse(
                            has_coordinates != TRUE,
                            "missing_coordinate",
                            fifelse(
                              active_pre_scores == TRUE & final_scored_row != TRUE,
                              "no_score_or_signed_distance",
                              fifelse(
                                source == "permit_new_construction",
                                "permit_only_needs_manual_review",
                                "possible_missed_residential_new_construction"
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)]

if (any(is.na(all_candidates$terminal_reason) | all_candidates$terminal_reason == "")) {
  stop("At least one candidate lacks a terminal audit reason.", call. = FALSE)
}

all_candidates[, missed_review_flag := terminal_reason %in% c(
  "possible_missed_residential_new_construction",
  "permit_only_needs_manual_review",
  "missing_unit_count",
  "missing_lot_or_building_size",
  "missing_coordinate",
  "no_score_or_signed_distance"
) & active_row == TRUE & in_year_window == TRUE & candidate_residential_signal == TRUE]

all_candidates[, missed_priority := fifelse(
  missed_review_flag == TRUE & (
    (!is.na(candidate_units) & candidate_units > 1) |
      (!is.na(best_dist_to_boundary) & best_dist_to_boundary <= 1000) |
      source == "permit_new_construction"
  ),
  "high",
  fifelse(missed_review_flag == TRUE, "medium", NA_character_)
)]

all_candidates[, google_maps_url := google_maps_url(source_latitude, source_longitude)]
all_candidates[, google_search_url := google_search_url(source_address, pin, source_latitude, source_longitude)]
all_candidates[, geocode_review_status := fifelse(has_coordinates == TRUE, "coordinate_backed", "needs_geocode")]
all_candidates[, candidate_unit_bin := unit_bin(candidate_units)]

message("Writing large commercial residential-candidate review...")
commercial_audit_status <- all_candidates[
  source == "commercial_value_raw",
  .(
    source_row_id,
    candidate_uid,
    audit_selected_source_row = selected_source_row,
    terminal_reason,
    missed_review_flag,
    missed_priority,
    clean_commercial_row,
    in_cleaned_source,
    pre_scores_row,
    final_scored_row,
    final_all_base,
    final_multifamily_base,
    final_construction_year,
    final_unitscount,
    final_arealotsf,
    final_areabuilding,
    source_latitude,
    source_longitude,
    google_maps_url,
    google_search_url,
    geocode_review_status
  )
]
commercial_large_review <- merge(
  comm_raw[
    source_chicago == TRUE &
      candidate_year_in_window(candidate_year) &
      (
        (!is.na(candidate_units) & candidate_units >= commercial_large_unit_threshold) |
          (!is.na(source_building_sqft) & source_building_sqft >= commercial_large_sqft_threshold)
      )
  ],
  commercial_audit_status,
  by = "source_row_id",
  all.x = TRUE,
  sort = FALSE
)
commercial_large_review <- merge(
  commercial_large_review,
  final_lookup[, .(
    pin,
    pin_final_scored_row = final_scored_row,
    pin_final_all_base = final_all_base,
    pin_final_multifamily_base = final_multifamily_base,
    pin_final_construction_year = final_construction_year,
    pin_final_unitscount = final_unitscount,
    pin_final_arealotsf = final_arealotsf,
    pin_final_areabuilding = final_areabuilding,
    pin_final_dist_to_boundary = final_dist_to_boundary
  )],
  by = "pin",
  all.x = TRUE,
  sort = FALSE
)
commercial_large_review[pin_final_scored_row == TRUE, `:=`(
  final_scored_row = TRUE,
  final_construction_year = fifelse(is.na(final_construction_year), pin_final_construction_year, final_construction_year),
  final_unitscount = fifelse(is.na(final_unitscount), pin_final_unitscount, final_unitscount),
  final_arealotsf = fifelse(is.na(final_arealotsf), pin_final_arealotsf, final_arealotsf),
  final_areabuilding = fifelse(is.na(final_areabuilding), pin_final_areabuilding, final_areabuilding)
)]
commercial_large_review[pin_final_all_base == TRUE, `:=`(
  final_all_base = TRUE,
  final_multifamily_base = pin_final_multifamily_base,
  terminal_reason = fifelse(
    terminal_reason %in% c("final_bw500_multifamily", "final_multifamily_outside_main_band"),
    terminal_reason,
    "final_same_pin"
  )
)]
commercial_large_review[, final_all_base := final_all_base == TRUE]
commercial_large_review[, final_multifamily_base := final_multifamily_base == TRUE]
commercial_large_review[, commercial_large_reason := fifelse(
  !is.na(candidate_units) & candidate_units >= commercial_large_unit_threshold &
    !is.na(source_building_sqft) & source_building_sqft >= commercial_large_sqft_threshold,
  "large_units_and_building_sqft",
  fifelse(
    !is.na(candidate_units) & candidate_units >= commercial_large_unit_threshold,
    "large_units",
    "large_building_sqft"
  )
)]
commercial_large_review[, manual_review_recommended := final_all_base != TRUE &
  candidate_residential_signal == TRUE]
commercial_large_review[, commercial_review_priority := fifelse(
  final_all_base == TRUE,
  "already_in_final_sample",
  fifelse(
    audit_selected_source_row == TRUE & commercial_primary_multifamily == TRUE,
    "review_first_active_multifamily_not_final",
    fifelse(
      commercial_primary_multifamily == TRUE,
      "review_duplicate_multifamily_source_row_not_final",
      fifelse(
        commercial_residential_text == TRUE & retail_multi_tenant_false_positive != TRUE,
        "review_secondary_residential_text_not_final",
        fifelse(
          retail_multi_tenant_false_positive == TRUE,
          "low_priority_retail_multi_tenant",
          "low_priority_large_no_residential_signal"
        )
      )
    )
  )
)]
commercial_large_review[, commercial_review_priority_rank := fcase(
  commercial_review_priority == "review_first_active_multifamily_not_final", 1L,
  commercial_review_priority == "review_secondary_residential_text_not_final", 2L,
  commercial_review_priority == "review_duplicate_multifamily_source_row_not_final", 3L,
  commercial_review_priority == "low_priority_large_no_residential_signal", 4L,
  commercial_review_priority == "low_priority_retail_multi_tenant", 5L,
  commercial_review_priority == "already_in_final_sample", 6L,
  default = 9L
)]
commercial_large_review[, raw_large_rows_for_pin := .N, by = pin]
setorder(
  commercial_large_review,
  pin,
  commercial_review_priority_rank,
  -candidate_units,
  -source_building_sqft,
  candidate_year
)
commercial_large_review <- commercial_large_review[, .SD[1], by = pin]
setorder(
  commercial_large_review,
  commercial_review_priority_rank,
  -candidate_residential_signal,
  -candidate_units,
  -source_building_sqft,
  candidate_year
)
commercial_large_review[, `:=`(
  visible_on_maps = "",
  residential_confirmed = "",
  new_construction_confirmed = "",
  multifamily_confirmed = "",
  unit_count_notes = "",
  action_needed = ""
)]
fwrite(
  commercial_large_review[, .(
    commercial_review_priority_rank,
    commercial_review_priority,
    manual_review_recommended,
    commercial_large_reason,
    raw_large_rows_for_pin,
    source_row_id,
    candidate_uid,
    pin,
    pin10,
    candidate_year,
    candidate_units,
    source_building_sqft,
    source_land_sqft,
    address,
    source_latitude,
    source_longitude,
    township,
    modelgroup,
    class_es,
    property_type_use,
    property_name_description,
    owner,
    taxpayer,
    stories,
    finalmarketvalue,
    aprx_comm_sf,
    netrentablesf,
    category,
    subclass2,
    commercial_primary_multifamily,
    commercial_residential_text,
    retail_multi_tenant_false_positive,
    candidate_residential_signal,
    source_false_positive_tier,
    audit_selected_source_row,
    terminal_reason,
    missed_review_flag,
    missed_priority,
    clean_commercial_row,
    in_cleaned_source,
    pre_scores_row,
    final_scored_row,
    final_all_base,
    final_multifamily_base,
    final_construction_year,
    final_unitscount,
    final_arealotsf,
    final_areabuilding,
    google_maps_url,
    google_search_url,
    geocode_review_status,
    visible_on_maps,
    residential_confirmed,
    new_construction_confirmed,
    multifamily_confirmed,
    unit_count_notes,
    action_needed
  )],
  "../output/commercial_large_residential_review.csv",
  na = ""
)

commercial_gap_denominators <- bps_gap[, .(
  total_gap_units = sum(bps_minus_outcome_total_units, na.rm = TRUE),
  multifamily_gap_units = sum(bps_minus_outcome_multifamily_units, na.rm = TRUE),
  fiveplus_gap_units = sum(bps_minus_outcome_5plus_units, na.rm = TRUE)
)]
commercial_gap_scenarios <- list(
  active_large_multifamily_not_final = commercial_large_review[
    commercial_review_priority == "review_first_active_multifamily_not_final"
  ],
  active_large_missing_lot_or_building_size = commercial_large_review[
    commercial_review_priority == "review_first_active_multifamily_not_final" &
      terminal_reason == "missing_lot_or_building_size"
  ],
  active_large_missing_coordinate = commercial_large_review[
    commercial_review_priority == "review_first_active_multifamily_not_final" &
      terminal_reason == "missing_coordinate"
  ],
  manual_recommended_large_commercial = commercial_large_review[
    manual_review_recommended == TRUE
  ],
  all_large_not_final_including_low_priority = commercial_large_review[
    final_all_base != TRUE
  ]
)
commercial_gap_closure <- rbindlist(lapply(names(commercial_gap_scenarios), function(scenario_name) {
  rows <- commercial_gap_scenarios[[scenario_name]]
  units_added <- sum(rows$candidate_units, na.rm = TRUE)
  data.table(
    scenario = scenario_name,
    n_pins = uniqueN(rows$pin[!is.na(rows$pin)]),
    candidate_units_added = units_added,
    total_gap_units = commercial_gap_denominators$total_gap_units,
    multifamily_gap_units = commercial_gap_denominators$multifamily_gap_units,
    fiveplus_gap_units = commercial_gap_denominators$fiveplus_gap_units,
    total_gap_closed_pct = 100 * units_added / commercial_gap_denominators$total_gap_units,
    multifamily_gap_closed_pct = 100 * units_added / commercial_gap_denominators$multifamily_gap_units,
    fiveplus_gap_closed_pct = 100 * units_added / commercial_gap_denominators$fiveplus_gap_units
  )
}))
fwrite(commercial_gap_closure, "../output/commercial_large_gap_closure_scenarios.csv", na = "")

message("Writing large permit-only residential review...")
permit_large_review_rows <- copy(all_candidates[
  source == "permit_new_construction" &
    terminal_reason == "permit_only_needs_manual_review" &
    permit_spatial_year_match_final != TRUE &
    candidate_year_in_window(candidate_year) &
    !is.na(candidate_units) &
    candidate_units >= commercial_large_unit_threshold
])
permit_large_review_rows[, permit_address_key := toupper(gsub("[^A-Z0-9]", "", source_address))]
permit_large_review_rows <- permit_large_review_rows[
  !is.na(permit_address_key) & permit_address_key != ""
]
permit_large_review_rows[, permit_text_lower := tolower(source_text)]
permit_large_review_rows[, permit_strong_residential_text := grepl(
  "dwelling|residential|apartment|apartments|condominium|condo|du",
  permit_text_lower
)]
permit_large_review_rows[, permit_hotel_institution_text := grepl(
  "hotel|residence hall|dorm|assisted living|supportive living|hospital|nursing|memory care",
  permit_text_lower
)]
permit_large_review_rows[, permit_review_priority := fifelse(
  permit_strong_residential_text == TRUE & permit_hotel_institution_text != TRUE,
  "review_first_strong_residential_permit",
  fifelse(
    permit_strong_residential_text == TRUE & permit_hotel_institution_text == TRUE,
    "review_mixed_residential_hotel_or_institution",
    fifelse(
      permit_hotel_institution_text == TRUE,
      "low_priority_hotel_or_institution",
      "review_ambiguous_large_units_text"
    )
  )
)]
permit_large_review_rows[, permit_review_priority_rank := fcase(
  permit_review_priority == "review_first_strong_residential_permit", 1L,
  permit_review_priority == "review_ambiguous_large_units_text", 2L,
  permit_review_priority == "review_mixed_residential_hotel_or_institution", 3L,
  permit_review_priority == "low_priority_hotel_or_institution", 4L,
  default = 9L
)]
setorder(
  permit_large_review_rows,
  permit_address_key,
  permit_review_priority_rank,
  -candidate_units,
  candidate_year
)
permit_large_review <- permit_large_review_rows[, .(
  permit_review_priority_rank = permit_review_priority_rank[1],
  permit_review_priority = permit_review_priority[1],
  permit_address_key = permit_address_key[1],
  permit_candidate_rows_at_address = .N,
  permit_candidate_years_at_address = paste(sort(unique(candidate_year)), collapse = ";"),
  permit_candidate_units_at_address = paste(sort(unique(candidate_units)), collapse = ";"),
  candidate_uid = candidate_uid[1],
  source_row_id = source_row_id[1],
  candidate_year = candidate_year[1],
  candidate_units = candidate_units[1],
  source_address = source_address[1],
  source_latitude = source_latitude[1],
  source_longitude = source_longitude[1],
  source_text = source_text[1],
  permit_strong_residential_text = permit_strong_residential_text[1],
  permit_hotel_institution_text = permit_hotel_institution_text[1],
  google_maps_url = google_maps_url[1],
  google_search_url = google_search_url[1],
  geocode_review_status = geocode_review_status[1]
), by = permit_address_key]
setorder(
  permit_large_review,
  permit_review_priority_rank,
  -candidate_units,
  candidate_year
)
permit_large_review[, `:=`(
  visible_on_maps = "",
  residential_confirmed = "",
  new_construction_confirmed = "",
  multifamily_confirmed = "",
  unit_count_notes = "",
  duplicate_or_already_in_assessor = "",
  action_needed = ""
)]
fwrite(permit_large_review, "../output/permit_large_residential_review.csv", na = "")

message("Tracing large permit-only rows to Assessor candidates...")
permit_trace_base <- permit_large_review[
  is.finite(source_latitude) & is.finite(source_longitude),
  .(
    permit_review_priority_rank,
    permit_review_priority,
    permit_address_key,
    permit_candidate_rows_at_address,
    permit_candidate_years_at_address,
    permit_candidate_units_at_address,
    permit_candidate_uid = candidate_uid,
    permit_source_row_id = source_row_id,
    permit_year = candidate_year,
    permit_units = candidate_units,
    permit_address = source_address,
    permit_latitude = source_latitude,
    permit_longitude = source_longitude,
    permit_text = source_text,
    permit_strong_residential_text,
    permit_hotel_institution_text,
    permit_google_maps_url = google_maps_url,
    permit_google_search_url = google_search_url,
    permit_geocode_review_status = geocode_review_status
  )
]
final_trace_targets <- merge(
  final_lookup[
    final_all_base == TRUE,
    .(
      pin,
      final_construction_year,
      final_unitscount,
      final_areabuilding,
      final_arealotsf,
      final_dist_to_boundary,
      final_ward_pair,
      final_multifamily_base
    )
  ],
  parcel_locs[, .(
    pin,
    source_latitude = parcel_latitude,
    source_longitude = parcel_longitude
  )],
  by = "pin",
  all.x = TRUE,
  sort = FALSE
)
final_trace_targets <- final_trace_targets[
  is.finite(source_latitude) & is.finite(source_longitude)
]
assessor_trace_cols <- c(
  "candidate_uid", "source", "source_row_id", "source_record_id", "pin", "pin10",
  "candidate_year", "candidate_units", "source_address", "source_text",
  "source_false_positive_tier", "source_latitude", "source_longitude",
  "selected_source_row", "terminal_reason", "clean_residential_row",
  "clean_commercial_row", "in_cleaned_source", "final_scored_row",
  "final_all_base", "final_multifamily_base", "final_construction_year",
  "final_unitscount", "final_areabuilding", "final_arealotsf",
  "google_search_url"
)
selected_assessor_targets <- unique(
  all_candidates[
    source != "permit_new_construction" &
      selected_source_row == TRUE &
      candidate_residential_signal == TRUE &
      has_coordinates == TRUE,
    ..assessor_trace_cols
  ],
  by = "candidate_uid"
)
raw_assessor_targets <- unique(
  all_candidates[
    source != "permit_new_construction" &
      candidate_residential_signal == TRUE &
      has_coordinates == TRUE,
    ..assessor_trace_cols
  ],
  by = "candidate_uid"
)

nearest_trace_table <- function(points_dt, targets_dt, prefix) {
  points_sf <- st_as_sf(
    points_dt,
    coords = c("permit_longitude", "permit_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    st_transform(crs_projected)
  targets_sf <- st_as_sf(
    targets_dt,
    coords = c("source_longitude", "source_latitude"),
    crs = 4326,
    remove = FALSE
  ) |>
    st_transform(crs_projected)
  nearest_idx <- st_nearest_feature(points_sf, targets_sf)
  nearest_dist <- as.numeric(st_distance(points_sf, targets_sf[nearest_idx, ], by_element = TRUE))
  out <- copy(targets_dt[nearest_idx])
  setnames(out, names(out), paste0(prefix, names(out)))
  out[, (paste0(prefix, "dist_ft")) := nearest_dist]
  out
}

permit_large_assessor_trace <- data.table()
if (nrow(permit_trace_base) > 0) {
  permit_large_assessor_trace <- cbind(
    permit_trace_base,
    nearest_trace_table(permit_trace_base, final_trace_targets, "nearest_final_"),
    nearest_trace_table(permit_trace_base, selected_assessor_targets, "nearest_selected_"),
    nearest_trace_table(permit_trace_base, raw_assessor_targets, "nearest_raw_")
  )
  permit_large_assessor_trace[, `:=`(
    nearest_final_year_gap = abs(permit_year - nearest_final_final_construction_year),
    nearest_selected_year_gap = abs(permit_year - nearest_selected_candidate_year),
    nearest_raw_year_gap = abs(permit_year - nearest_raw_candidate_year)
  )]
  permit_large_assessor_trace[, assessor_trace_class := fcase(
    nearest_final_dist_ft <= 250 & nearest_final_year_gap <= 5,
    "near_final_within_5_years",
    nearest_selected_dist_ft <= 250 &
      nearest_selected_terminal_reason == "out_of_year_window" &
      nearest_selected_candidate_year > max_construction_year,
    "assessor_after_window",
    nearest_selected_dist_ft <= 250 &
      nearest_selected_terminal_reason == "out_of_year_window" &
      nearest_selected_candidate_year < min_construction_year,
    "assessor_old_year_possible_stale_pin",
    nearest_selected_dist_ft <= 250 &
      nearest_selected_terminal_reason == "missing_lot_or_building_size",
    "assessor_selected_missing_lot_or_building_size",
    nearest_selected_dist_ft <= 250 &
      nearest_selected_terminal_reason == "missing_coordinate",
    "assessor_selected_missing_coordinate",
    nearest_selected_dist_ft <= 250 &
      nearest_selected_terminal_reason == "missing_unit_count",
    "assessor_selected_missing_unit_count",
    nearest_selected_dist_ft <= 250 &
      nearest_selected_terminal_reason == "possible_missed_residential_new_construction",
    "assessor_selected_possible_missed",
    nearest_selected_dist_ft <= 250 & nearest_selected_final_all_base == TRUE,
    "assessor_selected_final_year_or_duplicate_mismatch",
    nearest_selected_dist_ft <= 250,
    paste0("assessor_selected_other_", nearest_selected_terminal_reason),
    nearest_raw_dist_ft <= 250,
    "raw_assessor_nearby_not_selected",
    nearest_final_dist_ft <= 500,
    "near_final_beyond_strict_match",
    nearest_selected_dist_ft <= 500 &
      nearest_selected_terminal_reason == "out_of_year_window" &
      nearest_selected_candidate_year > max_construction_year,
    "assessor_after_window_beyond_strict_match",
    nearest_selected_dist_ft <= 500 &
      nearest_selected_terminal_reason == "out_of_year_window" &
      nearest_selected_candidate_year < min_construction_year,
    "assessor_old_year_possible_stale_pin_beyond_strict_match",
    nearest_selected_dist_ft <= 500,
    "assessor_selected_nearby_beyond_strict_match",
    nearest_raw_dist_ft <= 500,
    "raw_assessor_nearby_beyond_strict_match",
    default = "no_assessor_candidate_within_500ft"
  )]
  permit_large_assessor_trace[, correction_priority_rank := fcase(
    assessor_trace_class == "assessor_old_year_possible_stale_pin", 1L,
    assessor_trace_class == "assessor_selected_missing_lot_or_building_size", 2L,
    assessor_trace_class == "raw_assessor_nearby_not_selected", 3L,
    assessor_trace_class %in% c(
      "assessor_selected_missing_coordinate",
      "assessor_selected_missing_unit_count",
      "assessor_selected_possible_missed"
    ), 4L,
    assessor_trace_class == "no_assessor_candidate_within_500ft", 5L,
    assessor_trace_class == "assessor_after_window", 6L,
    assessor_trace_class %in% c(
      "near_final_within_5_years",
      "assessor_selected_final_year_or_duplicate_mismatch",
      "near_final_beyond_strict_match"
    ), 7L,
    default = 8L
  )]
  permit_large_assessor_trace[, correction_priority := fcase(
    correction_priority_rank == 1L, "review_assessor_old_year_for_manual_year_pin_correction",
    correction_priority_rank == 2L, "review_missing_size_fields_for_manual_fill",
    correction_priority_rank == 3L, "review_raw_assessor_selection_loss",
    correction_priority_rank == 4L, "review_other_assessor_blocker",
    correction_priority_rank == 5L, "review_no_nearby_assessor_candidate",
    correction_priority_rank == 6L, "likely_after_window_completion_or_assessment",
    correction_priority_rank == 7L, "likely_already_in_outcome_or_timing_mismatch",
    default = "review_low_priority_match_ambiguity"
  )]
  permit_large_assessor_trace[, `:=`(
    manual_visible_project = "",
    manual_residential_confirmed = "",
    manual_new_construction_confirmed = "",
    manual_correct_assessor_pin = "",
    manual_correct_year = "",
    manual_correct_units = "",
    manual_correct_building_sqft = "",
    manual_correct_land_sqft = "",
    manual_assessor_action = "",
    manual_notes = ""
  )]
}
fwrite(permit_large_assessor_trace, "../output/permit_large_assessor_trace.csv", na = "")

message("Writing crosswalk and possible-missed outputs...")
crosswalk_cols <- c(
  "candidate_uid", "source", "source_row_id", "source_record_id", "pin", "pin10",
  "candidate_year", "candidate_date", "candidate_units", "candidate_unit_bin",
  "source_building_sqft", "source_land_sqft", "source_address",
  "source_latitude", "source_longitude", "source_township", "source_chicago",
  "source_text", "source_false_positive_tier", "candidate_residential_signal",
  "selected_source_row", "source_pin_duplicate_n", "permit_issued",
  "permit_residential_text", "permit_pin_count", "permit_pin_match_final",
  "permit_nearest_final_pin", "permit_nearest_final_dist_ft",
  "permit_nearest_final_year_gap", "permit_spatial_year_match_final",
  "clean_residential_row", "clean_commercial_row", "in_cleaned_source",
  "pre_scores_row", "segment_lookup_row", "segment_lookup_id",
  "segment_lookup_reason", "final_scored_row", "final_all_base",
  "final_multifamily_base", "final_all_250", "final_multifamily_500",
  "final_multifamily_1000", "final_multifamily_500_segment",
  "final_construction_year", "final_unitscount", "final_arealotsf",
  "final_areabuilding", "final_dist_to_boundary", "final_ward_pair",
  "pre_dist_to_boundary", "pre_ward_pair", "audit_dist_to_boundary",
  "audit_ward_pair", "best_dist_to_boundary", "best_ward_pair",
  "terminal_reason", "missed_review_flag", "missed_priority",
  "google_maps_url", "google_search_url", "geocode_review_status"
)
fwrite(all_candidates[, ..crosswalk_cols], "../output/raw_candidate_to_final_crosswalk.csv", na = "")

possible_missed <- all_candidates[
  missed_review_flag == TRUE,
  ..crosswalk_cols
][order(
  factor(missed_priority, levels = c("high", "medium")),
  source != "permit_new_construction",
  -candidate_units,
  best_dist_to_boundary
)]
fwrite(possible_missed, "../output/possible_missed_new_residential.csv", na = "")

message("Writing source attrition ladder...")
stage_defs <- list(
  raw_candidate = rep(TRUE, nrow(all_candidates)),
  selected_source_row = all_candidates$active_row == TRUE,
  in_year_window = all_candidates$active_row == TRUE & all_candidates$in_year_window == TRUE,
  residential_signal = all_candidates$active_row == TRUE & all_candidates$in_year_window == TRUE & all_candidates$candidate_residential_signal == TRUE,
  has_pin = all_candidates$active_row == TRUE & all_candidates$in_year_window == TRUE & all_candidates$has_pin == TRUE,
  has_coordinates = all_candidates$active_row == TRUE & all_candidates$in_year_window == TRUE & all_candidates$has_coordinates == TRUE,
  in_cleaned_source = all_candidates$active_cleaned_source == TRUE,
  in_pre_scores = all_candidates$active_pre_scores == TRUE,
  in_final_scored = all_candidates$active_row == TRUE & all_candidates$final_scored_row == TRUE,
  final_all_base = all_candidates$active_final_all_base == TRUE,
  final_multifamily_base = all_candidates$active_final_multifamily_base == TRUE,
  final_all_250 = all_candidates$active_final_all_250 == TRUE,
  final_multifamily_500 = all_candidates$active_final_multifamily_500 == TRUE,
  possible_missed_review = all_candidates$missed_review_flag == TRUE
)
stage_ladder <- rbindlist(lapply(names(stage_defs), function(stage_name) {
  idx <- stage_defs[[stage_name]]
  all_candidates[idx, .(
    n_candidate_rows = .N,
    n_pins = uniqueN(pin[!is.na(pin)]),
    n_source_records = uniqueN(source_record_id),
    n_high_priority_missed = sum(missed_priority == "high", na.rm = TRUE)
  ), by = .(
    source,
    candidate_year,
    candidate_unit_bin
  )][, sample_stage := stage_name]
}), fill = TRUE)
setcolorder(stage_ladder, c("source", "candidate_year", "candidate_unit_bin", "sample_stage"))
setorder(stage_ladder, source, candidate_year, candidate_unit_bin, sample_stage)
fwrite(stage_ladder, "../output/source_attrition_ladder.csv", na = "")

message("Building manual Google review queue...")
final_review <- merge(
  final[
    final_multifamily_500 == TRUE | final_all_250 == TRUE,
    .(
      pin,
      candidate_year = construction_year,
      candidate_units = unitscount,
      source_building_sqft = areabuilding,
      source_land_sqft = arealotsf,
      dist_to_boundary,
      ward_pair = as.character(ward_pair),
      final_multifamily_500,
      final_all_250
    )
  ],
  parcel_locs,
  by = "pin",
  all.x = TRUE,
  sort = FALSE
)
final_review <- merge(final_review, comm_clean[, .(pin, clean_comm_address)], by = "pin", all.x = TRUE, sort = FALSE)
final_review[, review_scope := fifelse(
  final_multifamily_500 == TRUE & final_all_250 == TRUE,
  "final_bw500_multifamily;final_bw250_all",
  fifelse(final_multifamily_500 == TRUE, "final_bw500_multifamily", "final_bw250_all")
)]
final_review[, review_priority_order := fifelse(grepl("final_bw500_multifamily", review_scope), 2L, 3L)]
final_review[, candidate_uid := paste("final_sample", pin, sep = "::")]
final_review[, source := "final_sample"]
final_review[, source_address := clean_comm_address]
final_review[, source_latitude := parcel_latitude]
final_review[, source_longitude := parcel_longitude]
final_review[, terminal_reason := review_scope]
final_review[, missed_priority := NA_character_]
final_review[, google_maps_url := google_maps_url(source_latitude, source_longitude)]
final_review[, google_search_url := google_search_url(source_address, pin, source_latitude, source_longitude)]
final_review[, geocode_review_status := fifelse(is.finite(source_latitude) & is.finite(source_longitude), "coordinate_backed", "needs_geocode")]

missed_review <- copy(possible_missed)
missed_review[, review_scope := "suspected_omitted_candidate"]
missed_review[, review_priority_order := fifelse(missed_priority == "high", 1L, 4L)]
missed_review[, dist_to_boundary := best_dist_to_boundary]
missed_review[, ward_pair := best_ward_pair]

manual_queue <- rbindlist(list(
  final_review[, .(
    review_priority_order, review_scope, candidate_uid, source, pin, pin10 = pin10_from_pin(pin),
    candidate_year, candidate_units, source_building_sqft, source_land_sqft,
    source_address, source_latitude, source_longitude,
    dist_to_boundary, ward_pair, terminal_reason, missed_priority,
    google_maps_url, google_search_url, geocode_review_status
  )],
  missed_review[, .(
    review_priority_order, review_scope, candidate_uid, source, pin, pin10,
    candidate_year, candidate_units, source_building_sqft, source_land_sqft,
    source_address, source_latitude, source_longitude,
    dist_to_boundary, ward_pair, terminal_reason, missed_priority,
    google_maps_url, google_search_url, geocode_review_status
  )]
), fill = TRUE)

manual_queue[, visible_on_maps := ""]
manual_queue[, residential_confirmed := ""]
manual_queue[, new_construction_confirmed := ""]
manual_queue[, multifamily_confirmed := ""]
manual_queue[, unit_count_notes := ""]
manual_queue[, ward_border_relevance := ""]
manual_queue[, action_needed := ""]
setorder(manual_queue, review_priority_order, dist_to_boundary, source, candidate_year)

bad_queue <- manual_queue[
  (is.na(google_maps_url) | google_maps_url == "") &
    geocode_review_status != "needs_geocode"
]
if (nrow(bad_queue) > 0) {
  stop("Manual queue has rows without a maps URL and without needs_geocode status.", call. = FALSE)
}
fwrite(manual_queue, "../output/manual_google_review_queue.csv", na = "")

message("Writing report...")
terminal_summary <- all_candidates[, .(
  n_candidate_rows = .N,
  n_pins = uniqueN(pin[!is.na(pin)]),
  high_priority_missed = sum(missed_priority == "high", na.rm = TRUE)
), by = terminal_reason][order(-n_candidate_rows)]

source_summary <- all_candidates[, .(
  n_candidate_rows = .N,
  n_selected_rows = sum(active_row, na.rm = TRUE),
  n_in_year_selected_rows = sum(active_row & in_year_window, na.rm = TRUE),
  n_final_all_base_rows = sum(active_final_all_base, na.rm = TRUE),
  n_final_multifamily_500_rows = sum(active_final_multifamily_500, na.rm = TRUE),
  n_missed_review_rows = sum(missed_review_flag, na.rm = TRUE),
  n_high_priority_missed = sum(missed_priority == "high", na.rm = TRUE)
), by = source][order(source)]

bps_totals <- bps_gap[, .(
  bps_total_buildings = sum(bps_total_buildings, na.rm = TRUE),
  bps_total_units = sum(bps_total_units, na.rm = TRUE),
  bps_multifamily_units = sum(bps_multifamily_units, na.rm = TRUE),
  bps_5plus_units = sum(bps_5plus_units, na.rm = TRUE),
  outcome_total_buildings = sum(outcome_total_buildings, na.rm = TRUE),
  outcome_total_units = sum(outcome_total_units, na.rm = TRUE),
  outcome_multifamily_units = sum(outcome_multifamily_units, na.rm = TRUE),
  outcome_5plus_units = sum(outcome_5plus_units, na.rm = TRUE)
)]
bps_totals[, `:=`(
  outcome_share_total_units = outcome_total_units / bps_total_units,
  outcome_share_multifamily_units = outcome_multifamily_units / bps_multifamily_units,
  outcome_share_5plus_units = outcome_5plus_units / bps_5plus_units
)]

commercial_review_summary <- commercial_large_review[, .(
  n_pins = .N,
  n_manual_review_recommended = sum(manual_review_recommended == TRUE, na.rm = TRUE),
  candidate_units = sum(candidate_units, na.rm = TRUE)
), by = commercial_review_priority][order(match(
  commercial_review_priority,
  c(
    "review_first_active_multifamily_not_final",
    "review_secondary_residential_text_not_final",
    "review_duplicate_multifamily_source_row_not_final",
    "low_priority_large_no_residential_signal",
    "low_priority_retail_multi_tenant",
    "already_in_final_sample"
  )
))]
commercial_active_not_final <- commercial_large_review[
  commercial_review_priority == "review_first_active_multifamily_not_final",
  .(
    n_pins = .N,
    candidate_units = sum(candidate_units, na.rm = TRUE)
  ),
  by = terminal_reason
][order(-candidate_units)]
commercial_gap_active <- commercial_gap_closure[scenario == "active_large_multifamily_not_final"]
commercial_gap_missing_size <- commercial_gap_closure[scenario == "active_large_missing_lot_or_building_size"]
permit_review_summary <- permit_large_review[, .(
  n_addresses = .N,
  candidate_units = sum(candidate_units, na.rm = TRUE)
), by = permit_review_priority][order(permit_review_priority)]
permit_strong_nonhotel <- permit_large_review[
  permit_review_priority == "review_first_strong_residential_permit",
  .(
    n_addresses = .N,
    candidate_units = sum(candidate_units, na.rm = TRUE)
  )
]
permit_strong_nonhotel[, remaining_net_gap_after_active_commercial := commercial_gap_denominators$total_gap_units -
  commercial_gap_active$candidate_units_added]
permit_strong_nonhotel[, share_of_remaining_net_gap_after_active_commercial := candidate_units /
  remaining_net_gap_after_active_commercial]
permit_trace_summary <- permit_large_assessor_trace[, .(
  n_addresses = .N,
  permit_units = sum(permit_units, na.rm = TRUE)
), by = .(
  correction_priority_rank,
  correction_priority,
  assessor_trace_class
)][order(correction_priority_rank, -permit_units)]

report_lines <- c(
  "# New Construction Completeness Audit",
  "",
  sprintf("Generated on %s.", audit_generated_on),
  "",
  "## Final Sample Validation",
  "",
  sprintf("- All new construction base count: %s.", format(final_counts$final_all_base, big.mark = ",")),
  sprintf("- Multifamily base count: %s.", format(final_counts$final_multifamily_base, big.mark = ",")),
  sprintf("- Main 500-foot multifamily count: %s.", format(final_counts$final_multifamily_500, big.mark = ",")),
  "",
  "## Source Summary",
  "",
  paste(
    apply(source_summary, 1, function(row) {
      sprintf(
        "- %s: %s candidate rows, %s selected rows, %s final all-base rows, %s final 500-foot multifamily rows, %s review rows (%s high priority).",
        row[["source"]],
        format(as.integer(row[["n_candidate_rows"]]), big.mark = ","),
        format(as.integer(row[["n_selected_rows"]]), big.mark = ","),
        format(as.integer(row[["n_final_all_base_rows"]]), big.mark = ","),
        format(as.integer(row[["n_final_multifamily_500_rows"]]), big.mark = ","),
        format(as.integer(row[["n_missed_review_rows"]]), big.mark = ","),
        format(as.integer(row[["n_high_priority_missed"]]), big.mark = ",")
      )
    }),
    collapse = "\n"
  ),
  "",
  "## Terminal Reasons",
  "",
  paste(
    apply(terminal_summary, 1, function(row) {
      sprintf(
        "- %s: %s rows across %s PINs; %s high-priority review rows.",
        row[["terminal_reason"]],
        format(as.integer(row[["n_candidate_rows"]]), big.mark = ","),
        format(as.integer(row[["n_pins"]]), big.mark = ","),
        format(as.integer(row[["high_priority_missed"]]), big.mark = ",")
      )
    }),
    collapse = "\n"
  ),
  "",
  "## Review Queue",
  "",
  sprintf("- Manual Google review queue rows: %s.", format(nrow(manual_queue), big.mark = ",")),
  sprintf("- Possible missed new-residential rows: %s.", format(nrow(possible_missed), big.mark = ",")),
  sprintf("- High-priority possible missed rows: %s.", format(sum(possible_missed$missed_priority == "high", na.rm = TRUE), big.mark = ",")),
  "",
  "## Census BPS Benchmark",
  "",
  sprintf(
    "- Census BPS 2006-2022 Chicago residential permits authorized: %s buildings and %s units.",
    format(as.integer(bps_totals$bps_total_buildings), big.mark = ","),
    format(as.integer(bps_totals$bps_total_units), big.mark = ",")
  ),
  sprintf(
    "- Current outcome universe 2006-2022: %s buildings and %s units.",
    format(as.integer(bps_totals$outcome_total_buildings), big.mark = ","),
    format(as.integer(bps_totals$outcome_total_units), big.mark = ",")
  ),
  sprintf(
    "- Outcome share of BPS units: %.1f%% overall, %.1f%% multifamily, %.1f%% five-plus-unit buildings.",
    100 * bps_totals$outcome_share_total_units,
    100 * bps_totals$outcome_share_multifamily_units,
    100 * bps_totals$outcome_share_5plus_units
  ),
  "",
  "## Large Commercial Residential Review",
  "",
  sprintf(
    "- Large commercial review rows: %s unique PINs from commercial valuation data.",
    format(nrow(commercial_large_review), big.mark = ",")
  ),
  paste(
    apply(commercial_review_summary, 1, function(row) {
      sprintf(
        "- %s: %s PINs; %s manual-review recommended; %s candidate units.",
        row[["commercial_review_priority"]],
        format(as.integer(row[["n_pins"]]), big.mark = ","),
        format(as.integer(row[["n_manual_review_recommended"]]), big.mark = ","),
        format(as.integer(row[["candidate_units"]]), big.mark = ",")
      )
    }),
    collapse = "\n"
  ),
  if (nrow(commercial_active_not_final) > 0) {
    paste(
      apply(commercial_active_not_final, 1, function(row) {
        sprintf(
          "- Active large multifamily not-final, %s: %s PINs; %s candidate units.",
          row[["terminal_reason"]],
          format(as.integer(row[["n_pins"]]), big.mark = ","),
          format(as.integer(row[["candidate_units"]]), big.mark = ",")
        )
      }),
      collapse = "\n"
    )
  } else {
    "- No active large multifamily commercial rows were outside the final sample."
  },
  sprintf(
    "- Adding every active large multifamily not-final commercial row would add %s units, closing %.1f%% of the total BPS unit gap and %.1f%% of the five-plus-unit gap.",
    format(as.integer(commercial_gap_active$candidate_units_added), big.mark = ","),
    commercial_gap_active$total_gap_closed_pct,
    commercial_gap_active$fiveplus_gap_closed_pct
  ),
  sprintf(
    "- Hand-filling only active large rows missing lot/building size would add %s units, closing %.1f%% of the total BPS unit gap and %.1f%% of the five-plus-unit gap.",
    format(as.integer(commercial_gap_missing_size$candidate_units_added), big.mark = ","),
    commercial_gap_missing_size$total_gap_closed_pct,
    commercial_gap_missing_size$fiveplus_gap_closed_pct
  ),
  "",
  "## Large Permit-Only Residential Review",
  "",
  sprintf(
    "- Large permit-only review rows: %s address-collapsed candidates with parsed unit counts of at least %s.",
    format(nrow(permit_large_review), big.mark = ","),
    commercial_large_unit_threshold
  ),
  paste(
    apply(permit_review_summary, 1, function(row) {
      sprintf(
        "- %s: %s addresses; %s parsed units.",
        row[["permit_review_priority"]],
        format(as.integer(row[["n_addresses"]]), big.mark = ","),
        format(as.integer(row[["candidate_units"]]), big.mark = ",")
      )
    }),
    collapse = "\n"
  ),
  sprintf(
    "- Strong residential, non-hotel permit-only candidates sum to %s parsed units, or %.1f%% of the remaining net BPS unit gap after active large commercial omissions.",
    format(as.integer(permit_strong_nonhotel$candidate_units), big.mark = ","),
    100 * permit_strong_nonhotel$share_of_remaining_net_gap_after_active_commercial
  ),
  "",
  "## Permit-to-Assessor Trace",
  "",
  sprintf(
    "- Traced %s large permit-only address candidates to nearest final, selected Assessor, and raw Assessor rows.",
    format(nrow(permit_large_assessor_trace), big.mark = ",")
  ),
  paste(
    apply(permit_trace_summary, 1, function(row) {
      sprintf(
        "- %s / %s: %s addresses; %s permit units.",
        row[["correction_priority"]],
        row[["assessor_trace_class"]],
        format(as.integer(row[["n_addresses"]]), big.mark = ","),
        format(as.integer(row[["permit_units"]]), big.mark = ",")
      )
    }),
    collapse = "\n"
  ),
  "",
  "## Interpretation",
  "",
  "This task is audit-only. It does not change the production construction universe, density regressions, figures, tables, or paper text.",
  "Rows in `possible_missed_new_residential.csv`, `commercial_large_residential_review.csv`, and `permit_large_assessor_trace.csv` require manual review before any production-sample fix. The Census BPS comparison is an aggregate recall benchmark, not a project-level replacement for assessor data."
)
writeLines(report_lines, "../output/new_construction_completeness_audit_report.md")

message("Audit complete.")
