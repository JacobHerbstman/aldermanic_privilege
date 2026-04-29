# Audit-only investigation of assessor residential new-construction source universe.
# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/new_construction_universe_investigation/code")

source("../../../setup_environment/code/packages.R")

library(data.table)

min_construction_year <- 2006L
max_construction_year <- 2022L

normalize_pin <- function(x) {
  out <- substr(gsub("[^0-9]", "", as.character(x)), 1, 14)
  out[nchar(out) < 14] <- NA_character_
  out[out == "" | out == "NA"] <- NA_character_
  out
}

as_num_clean <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

safe_max <- function(x) {
  out <- suppressWarnings(max(x, na.rm = TRUE))
  if (!is.finite(out)) {
    NA_real_
  } else {
    out
  }
}

parse_unit_words <- function(x) {
  x_clean <- tolower(trimws(as.character(x)))
  out <- suppressWarnings(as.integer(gsub("[^0-9.-]", "", x_clean)))
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

normalize_class_codes <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  pieces <- strsplit(x, ",")
  vapply(pieces, function(vals) {
    vals <- trimws(vals)
    vals <- vals[vals != ""]
    vals <- gsub("[.]0$", "", vals)

    idx <- grepl("^[0-9]{3}$", vals)
    vals[idx] <- paste0(substr(vals[idx], 1, 1), "-", substr(vals[idx], 2, 3))

    idx <- grepl("^[0-9]{2}-May$", vals)
    vals[idx] <- paste0("5-", sprintf("%02d", as.integer(substr(vals[idx], 1, 2))))

    idx <- grepl("^May-[0-9]{2}$", vals)
    vals[idx] <- paste0("5-", substr(vals[idx], 5, 6))

    idx <- grepl("^[0-9]{2}-Mar$", vals)
    vals[idx] <- paste0("3-", sprintf("%02d", as.integer(substr(vals[idx], 1, 2))))

    idx <- grepl("^Mar-[0-9]{2}$", vals)
    vals[idx] <- paste0("3-", substr(vals[idx], 5, 6))

    idx <- grepl("^[0-9]{2}-Sep$", vals)
    vals[idx] <- paste0("9-", sprintf("%02d", as.integer(substr(vals[idx], 1, 2))))

    idx <- grepl("^Sep-[0-9]{2}$", vals)
    vals[idx] <- paste0("9-", substr(vals[idx], 5, 6))

    vals <- vals[vals != ""]
    if (length(vals) == 0) {
      NA_character_
    } else {
      paste(unique(vals), collapse = ", ")
    }
  }, character(1))
}

first_class_code <- function(x) {
  out <- sub(",.*$", "", as.character(x))
  out[out == "" | out == "NA"] <- NA_character_
  out
}

has_class_code <- function(x, codes) {
  x <- paste0(", ", as.character(x), ", ")
  Reduce(`|`, lapply(codes, function(code) grepl(paste0("(^|, )", code, "(,|$)"), x)))
}

unit_bin <- function(units) {
  fifelse(
    is.na(units), "unknown",
    fifelse(units <= 0, "0",
      fifelse(units == 1, "1",
        fifelse(units == 2, "2",
          fifelse(units >= 3 & units <= 4, "3-4", "5+")
        )
      )
    )
  )
}

in_window <- function(year_value) {
  !is.na(year_value) & year_value >= min_construction_year & year_value <= max_construction_year
}

candidate_summary <- function(dt, scenario_name) {
  dt[, .(
    scenario = scenario_name,
    buildings = .N,
    units = sum(units, na.rm = TRUE),
    residential_buildings = sum(source == "residential"),
    residential_units = sum(fifelse(source == "residential", units, 0), na.rm = TRUE),
    commercial_buildings = sum(source == "commercial"),
    commercial_units = sum(fifelse(source == "commercial", units, 0), na.rm = TRUE)
  )]
}

make_scenario <- function(dt, scenario_name) {
  out <- copy(dt)
  out <- out[in_window(year_built) & units > 0 & !is.na(pin)]
  out[, scenario := scenario_name]
  out[, unit_bin := unit_bin(units)]
  out[]
}

combine_high_units <- function(res_rows, comm_rows, scenario_name) {
  combined <- rbindlist(list(res_rows, comm_rows), fill = TRUE)
  combined <- combined[in_window(year_built) & units > 0 & !is.na(pin)]
  setorder(combined, pin, -units, source, year_built)
  combined <- combined[, .SD[1], by = pin]
  combined[, scenario := scenario_name]
  combined[, unit_bin := unit_bin(units)]
  combined[]
}

read_bps_long <- function(path) {
  bps <- fread(path)
  rbindlist(list(
    bps[, .(year, unit_bin = "1", bps_buildings = bps_1unit_buildings, bps_units = bps_1unit_units)],
    bps[, .(year, unit_bin = "2", bps_buildings = bps_2unit_buildings, bps_units = bps_2unit_units)],
    bps[, .(year, unit_bin = "3-4", bps_buildings = bps_3to4unit_buildings, bps_units = bps_3to4unit_units)],
    bps[, .(year, unit_bin = "5+", bps_buildings = bps_5plus_buildings, bps_units = bps_5plus_units)]
  ))
}

message("Loading BPS and production baseline...")
bps_long <- read_bps_long("../input/census_bps_chicago_benchmark.csv")
bps_totals <- bps_long[, .(
  bps_buildings = sum(bps_buildings, na.rm = TRUE),
  bps_units = sum(bps_units, na.rm = TRUE)
)]

production_gap <- fread("../input/outcome_vs_bps_yearly_gap.csv")
production_baseline <- production_gap[, .(
  scenario = "production_density_output",
  buildings = sum(outcome_total_buildings, na.rm = TRUE),
  units = sum(outcome_total_units, na.rm = TRUE),
  bps_buildings = sum(bps_total_buildings, na.rm = TRUE),
  bps_units = sum(bps_total_units, na.rm = TRUE)
)]
production_baseline[, `:=`(
  unit_gap = bps_units - units,
  unit_share_of_bps = units / bps_units,
  building_gap = bps_buildings - buildings,
  building_share_of_bps = buildings / bps_buildings
)]

message("Loading current residential cleaned source...")
res_current <- fread("../input/residential_cross_section.csv", colClasses = "character", na.strings = c("", "NA", "N/A"))
res_current[, pin := normalize_pin(pin)]
for (v in c("tax_year", "card_num", "year_built", "building_sqft", "land_sqft", "num_commercial_units")) {
  if (v %in% names(res_current)) {
    res_current[, (v) := as_num_clean(get(v))]
  }
}
res_current[, class_codes := normalize_class_codes(class)]
res_current[, source_class_group := first_class_code(class_codes)]
res_current[, units := as.numeric(parse_unit_words(num_apartments))]
res_current[, is_single_family := grepl("^single", as.character(single_v_multi_family), ignore.case = TRUE) |
  as.character(type_of_residence) %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level")]
res_current[is_single_family == TRUE & (is.na(units) | units == 0), units := 1]
res_current[, known_condo_signal := has_class_code(class_codes, c("2-99")) |
  grepl("condo|condominium", paste(class, type_of_residence, single_v_multi_family), ignore.case = TRUE)]
res_current[, possible_condo_signal := has_class_code(class_codes, c("2-97"))]
res_current[, exclusion_flag := fifelse(
  known_condo_signal == TRUE, "known_condo",
  fifelse(possible_condo_signal == TRUE, "possible_condo_2_97_review", "included_noncondo")
)]
res_current_rows <- res_current[, .(
  pin,
  year_built,
  units,
  building_sqft,
  land_sqft,
  source = "residential",
  source_rule = "current_cleaned_residential",
  source_class_group,
  class_codes,
  modelgroup = NA_character_,
  property_type_use = NA_character_,
  exclusion_flag,
  review_flag = fifelse(exclusion_flag == "included_noncondo", FALSE, TRUE),
  n_cards = NA_integer_,
  n_distinct_card_years = NA_integer_
)]
res_actual_cleaned_rows <- copy(res_current_rows)

message("Building residential card-aggregated alternatives...")
res_raw <- fread("../input/residential_improvement_characteristics.csv", colClasses = "character", na.strings = c("", "NA", "N/A"))
res_raw[, raw_row_n := .I]
res_raw[, pin := normalize_pin(pin)]
for (v in c("tax_year", "card_num", "year_built", "building_sqft", "land_sqft", "num_commercial_units")) {
  if (v %in% names(res_raw)) {
    res_raw[, (v) := as_num_clean(get(v))]
  }
}
res_raw <- res_raw[township_code %in% c("70", "71", "72", "73", "74", "75", "76", "77") & !is.na(pin)]
res_raw[, class_codes := normalize_class_codes(class)]
res_raw[, source_class_group := first_class_code(class_codes)]
res_raw[, units := as.numeric(parse_unit_words(num_apartments))]
res_raw[, is_single_family := grepl("^single", as.character(single_v_multi_family), ignore.case = TRUE) |
  as.character(type_of_residence) %in% c("1 Story", "1.5 Story", "2 Story", "3 Story +", "Split Level")]
res_raw[is_single_family == TRUE & (is.na(units) | units == 0), units := 1]
res_raw[, known_condo_signal := has_class_code(class_codes, c("2-99")) |
  grepl("condo|condominium", paste(class, type_of_residence, single_v_multi_family), ignore.case = TRUE)]
res_raw[, possible_condo_signal := has_class_code(class_codes, c("2-97"))]
res_raw[, exclusion_flag := fifelse(
  known_condo_signal == TRUE, "known_condo",
  fifelse(possible_condo_signal == TRUE, "possible_condo_2_97_review", "included_noncondo")
)]
res_raw[, year_order := fifelse(is.na(year_built), Inf, year_built)]
setorder(res_raw, pin, card_num, year_order, tax_year, -building_sqft, raw_row_n)
res_card_selected <- res_raw[, .SD[1], by = .(pin, card_num)]
res_card_aggregate <- res_card_selected[!is.na(year_built), .(
  min_year_built = min(year_built, na.rm = TRUE),
  max_year_built = max(year_built, na.rm = TRUE),
  units = sum(units, na.rm = TRUE),
  building_sqft = sum(building_sqft, na.rm = TRUE),
  land_sqft = safe_max(land_sqft),
  source_class_group = paste(unique(na.omit(source_class_group)), collapse = "; "),
  class_codes = paste(unique(na.omit(class_codes)), collapse = "; "),
  exclusion_flag = fifelse(any(exclusion_flag == "known_condo", na.rm = TRUE), "known_condo",
    fifelse(any(exclusion_flag == "possible_condo_2_97_review", na.rm = TRUE), "possible_condo_2_97_review", "included_noncondo")
  ),
  n_cards = .N,
  n_distinct_card_years = uniqueN(year_built, na.rm = TRUE),
  any_class_2_12 = any(has_class_code(class_codes, c("2-12")), na.rm = TRUE),
  class_2_12_units = sum(fifelse(has_class_code(class_codes, c("2-12")), units, 0), na.rm = TRUE)
), by = pin]

setorder(res_raw, pin, year_order, tax_year, -building_sqft, raw_row_n)
res_legacy_current_selected <- res_raw[, .SD[1], by = pin][!is.na(year_built)]
res_current_rows <- res_legacy_current_selected[, .(
  pin,
  year_built,
  units,
  building_sqft,
  land_sqft,
  source = "residential",
  source_rule = "legacy_current_cleaned_residential_emulation",
  source_class_group,
  class_codes,
  modelgroup = NA_character_,
  property_type_use = NA_character_,
  exclusion_flag,
  review_flag = fifelse(exclusion_flag == "included_noncondo", FALSE, TRUE),
  n_cards = NA_integer_,
  n_distinct_card_years = NA_integer_
)]

res_card_min_rows <- res_card_aggregate[, .(
  pin,
  year_built = min_year_built,
  units,
  building_sqft,
  land_sqft,
  source = "residential",
  source_rule = "residential_card_aggregate_min_year",
  source_class_group,
  class_codes,
  modelgroup = NA_character_,
  property_type_use = NA_character_,
  exclusion_flag,
  review_flag = exclusion_flag != "included_noncondo",
  n_cards,
  n_distinct_card_years
)]
res_card_max_rows <- res_card_aggregate[, .(
  pin,
  year_built = max_year_built,
  units,
  building_sqft,
  land_sqft,
  source = "residential",
  source_rule = "residential_card_aggregate_max_year",
  source_class_group,
  class_codes,
  modelgroup = NA_character_,
  property_type_use = NA_character_,
  exclusion_flag,
  review_flag = TRUE,
  n_cards,
  n_distinct_card_years
)]

message("Loading commercial raw source and rebuilding current/expanded alternatives...")
comm_raw <- fread("../input/commercial_value_raw.csv", colClasses = "character", na.strings = c("", "NA", "N/A"))
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
comm_raw[, pin := normalize_pin(keypin)]
for (v in c(
  "year", "yearbuilt", "tot_units", "studiounits", "x1brunits", "x2brunits",
  "x3brunits", "x4brunits", "bldgsf", "landsf"
)) {
  comm_raw[, (v) := as_num_clean(get(v))]
}
comm_raw[, unit_mix_sum := rowSums(.SD, na.rm = TRUE), .SDcols = c("studiounits", "x1brunits", "x2brunits", "x3brunits", "x4brunits")]
comm_raw[, units_current_script := fcoalesce(tot_units, fifelse(unit_mix_sum > 0, unit_mix_sum, NA_real_))]
comm_raw[, units_positive_rule := fifelse(!is.na(tot_units) & tot_units > 0, tot_units, fifelse(unit_mix_sum > 0, unit_mix_sum, NA_real_))]
comm_raw[, class_codes := normalize_class_codes(class_es)]
comm_raw[, source_class_group := first_class_code(class_codes)]
comm_raw[, text_all := paste(modelgroup, class_es, property_type_use, property_name_description, category, subclass2, owner, taxpayer)]
comm_raw[, chicago := township %in% c(
  "West Chicago", "South Chicago", "Jefferson", "North Chicago",
  "Lake View", "Rogers Park", "Hyde Park", "Lake"
)]
comm_raw[, current_model_rule := grepl("Multifamily|Class3|Class9|Condos", as.character(modelgroup), ignore.case = TRUE)]
comm_raw[, known_condo_signal := grepl("condo|condominium", text_all, ignore.case = TRUE) |
  has_class_code(class_codes, c("2-99", "3-99", "4-99", "5-99", "7-99", "8-99", "9-59"))]
comm_raw[, hotel_motel_signal := grepl(
  "hotel|motel|lodging|inn|hostel|rooming|boarding|dorm|hospital|nursing|assisted living|shelter|hyatt|aloft|waldorf",
  text_all,
  ignore.case = TRUE
) | has_class_code(class_codes, c("5-16", "5-29", "7-16", "7-29", "8-16", "8-29"))]
comm_raw[, class_multifamily_signal := current_model_rule |
  grepl("(^|, )(3|9)-", paste0(class_codes, ",")) |
  grepl("apartment|apartments|multifamily|multi-family|mixed-use|mixed use", text_all, ignore.case = TRUE)]
comm_raw[, exclusion_flag := fifelse(
  known_condo_signal == TRUE, "known_condo",
  fifelse(hotel_motel_signal == TRUE, "hotel_motel_or_institution", "included_noncondo_nonhotel")
)]
comm_raw[, has_land := !is.na(landsf) & landsf > 0]
comm_equivalent_unit_fills <- comm_raw[
  chicago == TRUE &
    units_positive_rule > 0 &
    !is.na(pin) &
    !is.na(yearbuilt) &
    !is.na(bldgsf),
  .(equivalent_row_units = max(units_positive_rule, na.rm = TRUE)),
  by = .(pin, yearbuilt, bldgsf)
]

comm_current_pool <- comm_raw[
  chicago == TRUE &
    yearbuilt >= 1999 &
    current_model_rule == TRUE &
    !is.na(units_current_script)
]
setorder(comm_current_pool, pin, -has_land, -units_current_script, raw_row_n)
comm_current_selected <- comm_current_pool[, .SD[1], by = pin]
comm_current_rows <- comm_current_selected[, .(
  pin,
  year_built = yearbuilt,
  units = units_current_script,
  building_sqft = bldgsf,
  land_sqft = landsf,
  source = "commercial",
  source_rule = "commercial_current_rule_script_dedup",
  source_class_group,
  class_codes,
  modelgroup = as.character(modelgroup),
  property_type_use = as.character(property_type_use),
  exclusion_flag,
  review_flag = exclusion_flag != "included_noncondo_nonhotel",
  n_cards = NA_integer_,
  n_distinct_card_years = NA_integer_
)]

comm_current_year_repair_pool <- comm_raw[
  chicago == TRUE &
    in_window(yearbuilt) &
    current_model_rule == TRUE &
    units_positive_rule > 0
]
setorder(comm_current_year_repair_pool, pin, -has_land, -units_positive_rule, yearbuilt, -bldgsf, raw_row_n)
comm_current_year_repair_rows <- comm_current_year_repair_pool[, .SD[1], by = pin][, .(
  pin,
  year_built = yearbuilt,
  units = units_positive_rule,
  building_sqft = bldgsf,
  land_sqft = landsf,
  source = "commercial",
  source_rule = "commercial_current_rule_year_window_repaired",
  source_class_group,
  class_codes,
  modelgroup = as.character(modelgroup),
  property_type_use = as.character(property_type_use),
  exclusion_flag,
  review_flag = exclusion_flag != "included_noncondo_nonhotel",
  n_cards = NA_integer_,
  n_distinct_card_years = NA_integer_
)]
comm_current_year_repair_rows <- merge(
  comm_current_year_repair_rows,
  comm_equivalent_unit_fills,
  by.x = c("pin", "year_built", "building_sqft"),
  by.y = c("pin", "yearbuilt", "bldgsf"),
  all.x = TRUE,
  sort = FALSE
)
comm_current_year_repair_rows[, units := fifelse(!is.na(equivalent_row_units) & equivalent_row_units > units, equivalent_row_units, units)]
comm_current_year_repair_rows[, equivalent_row_units := NULL]

comm_expanded_pool <- comm_raw[
  chicago == TRUE &
    in_window(yearbuilt) &
    class_multifamily_signal == TRUE &
    units_positive_rule > 0
]
setorder(comm_expanded_pool, pin, -has_land, -units_positive_rule, yearbuilt, -bldgsf, raw_row_n)
comm_expanded_rows <- comm_expanded_pool[, .SD[1], by = pin][, .(
  pin,
  year_built = yearbuilt,
  units = units_positive_rule,
  building_sqft = bldgsf,
  land_sqft = landsf,
  source = "commercial",
  source_rule = "commercial_expanded_noncondo_nonhotel",
  source_class_group,
  class_codes,
  modelgroup = as.character(modelgroup),
  property_type_use = as.character(property_type_use),
  exclusion_flag,
  review_flag = exclusion_flag != "included_noncondo_nonhotel",
  n_cards = NA_integer_,
  n_distinct_card_years = NA_integer_
)]
comm_expanded_rows <- merge(
  comm_expanded_rows,
  comm_equivalent_unit_fills,
  by.x = c("pin", "year_built", "building_sqft"),
  by.y = c("pin", "yearbuilt", "bldgsf"),
  all.x = TRUE,
  sort = FALSE
)
comm_expanded_rows[, units := fifelse(!is.na(equivalent_row_units) & equivalent_row_units > units, equivalent_row_units, units)]
comm_expanded_rows[, equivalent_row_units := NULL]

message("Validating rebuilt commercial current-rule output against cleaned file...")
comm_clean <- fread("../input/multifamily_data_cleaned.csv", colClasses = "character", na.strings = c("", "NA", "N/A"))
comm_clean[, pin := normalize_pin(pin)]
for (v in c("yearbuilt", "tot_units", "bldgsf", "landsf")) {
  comm_clean[, (v) := as_num_clean(get(v))]
}
comm_clean[, class_codes := normalize_class_codes(class_es)]
comm_clean[, source_class_group := first_class_code(class_codes)]
comm_clean[, text_all := paste(modelgroup, class_es, address)]
comm_clean[, known_condo_signal := grepl("condo|condominium", text_all, ignore.case = TRUE) |
  has_class_code(class_codes, c("2-99", "3-99", "4-99", "5-99", "7-99", "8-99", "9-59"))]
comm_clean[, hotel_motel_signal := grepl(
  "hotel|motel|lodging|inn|hostel|rooming|boarding|dorm|hospital|nursing|assisted living|shelter|hyatt|aloft|waldorf",
  text_all,
  ignore.case = TRUE
) | has_class_code(class_codes, c("5-16", "5-29", "7-16", "7-29", "8-16", "8-29"))]
comm_clean[, exclusion_flag := fifelse(
  known_condo_signal == TRUE, "known_condo",
  fifelse(hotel_motel_signal == TRUE, "hotel_motel_or_institution", "included_noncondo_nonhotel")
)]
comm_clean_rows <- comm_clean[, .(
  pin,
  year_built = yearbuilt,
  units = tot_units,
  building_sqft = bldgsf,
  land_sqft = landsf,
  source = "commercial",
  source_rule = "commercial_current_cleaned_output",
  source_class_group,
  class_codes,
  modelgroup = as.character(modelgroup),
  property_type_use = NA_character_,
  exclusion_flag,
  review_flag = exclusion_flag != "included_noncondo_nonhotel",
  n_cards = NA_integer_,
  n_distinct_card_years = NA_integer_
)]
comm_actual_cleaned_rows <- copy(comm_clean_rows)
comm_clean_validation <- data.table(
  metric = c(
    "cleaned_output_rows_2006_2022_positive_units",
    "current_source_rows_from_cleaned_output_2006_2022_positive_units",
    "raw_rebuilt_current_rule_rows_2006_2022_positive_units",
    "cleaned_output_units_2006_2022_positive_units",
    "current_source_units_from_cleaned_output_2006_2022_positive_units",
    "raw_rebuilt_current_rule_units_2006_2022_positive_units"
  ),
  value = c(
    nrow(comm_clean[in_window(yearbuilt) & tot_units > 0]),
    nrow(comm_clean_rows[in_window(year_built) & units > 0]),
    nrow(comm_current_rows[in_window(year_built) & units > 0]),
    sum(comm_clean[in_window(yearbuilt) & tot_units > 0, tot_units], na.rm = TRUE),
    sum(comm_clean_rows[in_window(year_built) & units > 0, units], na.rm = TRUE),
    sum(comm_current_rows[in_window(year_built) & units > 0, units], na.rm = TRUE)
  )
)

message("Constructing audit-only scenarios...")
res_current_as_is <- make_scenario(res_current_rows, "current_cleaned_source_as_is")[source == "residential"]
comm_current_as_is <- make_scenario(comm_current_rows, "current_cleaned_source_as_is")[source == "commercial"]

res_current_noncondo <- make_scenario(
  res_current_rows[exclusion_flag == "included_noncondo"],
  "current_cleaned_source_noncondo_nonhotel"
)[source == "residential"]
comm_current_noncondo <- make_scenario(
  comm_current_rows[exclusion_flag == "included_noncondo_nonhotel"],
  "current_cleaned_source_noncondo_nonhotel"
)[source == "commercial"]

res_actual_cleaned_noncondo <- make_scenario(
  res_actual_cleaned_rows[exclusion_flag == "included_noncondo"],
  "updated_cleaned_source_noncondo_nonhotel"
)[source == "residential"]
comm_actual_cleaned_noncondo <- make_scenario(
  comm_actual_cleaned_rows[exclusion_flag == "included_noncondo_nonhotel"],
  "updated_cleaned_source_noncondo_nonhotel"
)[source == "commercial"]

res_card_min_noncondo <- make_scenario(
  res_card_min_rows[exclusion_flag == "included_noncondo"],
  "residential_card_aggregate_min_year_noncondo"
)
res_card_max_noncondo <- make_scenario(
  res_card_max_rows[exclusion_flag == "included_noncondo"],
  "residential_card_aggregate_max_year_noncondo_sensitivity"
)
comm_current_rule_noncondo <- make_scenario(
  comm_current_rows[exclusion_flag == "included_noncondo_nonhotel"],
  "commercial_current_rule_noncondo_nonhotel"
)
comm_current_year_repair_noncondo <- make_scenario(
  comm_current_year_repair_rows[exclusion_flag == "included_noncondo_nonhotel"],
  "commercial_current_rule_year_window_repaired_noncondo"
)
comm_expanded_noncondo <- make_scenario(
  comm_expanded_rows[exclusion_flag == "included_noncondo_nonhotel"],
  "commercial_expanded_noncondo_nonhotel"
)

combined_current_as_is <- combine_high_units(
  res_current_as_is,
  comm_current_as_is,
  "current_cleaned_source_as_is"
)
combined_current_noncondo <- combine_high_units(
  res_current_noncondo,
  comm_current_noncondo,
  "current_cleaned_source_noncondo_nonhotel"
)
combined_updated_cleaned_source <- combine_high_units(
  res_actual_cleaned_noncondo,
  comm_actual_cleaned_noncondo,
  "updated_cleaned_source_noncondo_nonhotel"
)
combined_card_min_current_comm <- combine_high_units(
  res_card_min_noncondo,
  comm_current_rule_noncondo,
  "card_min_res_plus_current_comm_noncondo"
)
combined_card_min_year_repair_comm <- combine_high_units(
  res_card_min_noncondo,
  comm_current_year_repair_noncondo,
  "card_min_res_plus_year_repaired_comm_noncondo"
)
combined_card_min_expanded_comm <- combine_high_units(
  res_card_min_noncondo,
  comm_expanded_noncondo,
  "card_min_res_plus_expanded_comm_noncondo"
)
combined_card_max_expanded_comm <- combine_high_units(
  res_card_max_noncondo,
  comm_expanded_noncondo,
  "card_max_res_plus_expanded_comm_noncondo_sensitivity"
)

scenario_candidates <- rbindlist(list(
  combined_current_as_is,
  combined_current_noncondo,
  combined_updated_cleaned_source,
  res_card_min_noncondo,
  res_card_max_noncondo,
  comm_current_rule_noncondo,
  comm_current_year_repair_noncondo,
  comm_expanded_noncondo,
  combined_card_min_current_comm,
  combined_card_min_year_repair_comm,
  combined_card_min_expanded_comm,
  combined_card_max_expanded_comm
), fill = TRUE)

scenario_candidates[, unit_bin := factor(unit_bin, levels = c("1", "2", "3-4", "5+", "0", "unknown"))]
setorder(scenario_candidates, scenario, source, year_built, pin)

scenario_summary <- scenario_candidates[, .(
  buildings = .N,
  units = sum(units, na.rm = TRUE),
  residential_buildings = sum(source == "residential"),
  residential_units = sum(fifelse(source == "residential", units, 0), na.rm = TRUE),
  commercial_buildings = sum(source == "commercial"),
  commercial_units = sum(fifelse(source == "commercial", units, 0), na.rm = TRUE)
), by = scenario]
scenario_summary[, `:=`(
  bps_buildings = bps_totals$bps_buildings,
  bps_units = bps_totals$bps_units
)]
scenario_summary[, `:=`(
  building_gap = bps_buildings - buildings,
  unit_gap = bps_units - units,
  building_share_of_bps = buildings / bps_buildings,
  unit_share_of_bps = units / bps_units
)]
setorder(scenario_summary, scenario)

scenario_by_year_unit_bin <- scenario_candidates[, .(
  buildings = .N,
  units = sum(units, na.rm = TRUE)
), by = .(scenario, year = as.integer(year_built), unit_bin = as.character(unit_bin))]
scenario_by_year_unit_bin <- merge(
  scenario_by_year_unit_bin,
  bps_long,
  by = c("year", "unit_bin"),
  all.x = TRUE,
  sort = FALSE
)
scenario_by_year_unit_bin[, `:=`(
  bps_buildings = fifelse(is.na(bps_buildings), 0, as.numeric(bps_buildings)),
  bps_units = fifelse(is.na(bps_units), 0, as.numeric(bps_units))
)]
scenario_by_year_unit_bin[, `:=`(
  building_gap = bps_buildings - buildings,
  unit_gap = bps_units - units,
  building_share_of_bps = fifelse(bps_buildings > 0, buildings / bps_buildings, NA_real_),
  unit_share_of_bps = fifelse(bps_units > 0, units / bps_units, NA_real_)
)]
setorder(scenario_by_year_unit_bin, scenario, year, unit_bin)

scenario_detail_summary <- scenario_candidates[, .(
  buildings = .N,
  units = sum(units, na.rm = TRUE),
  pins = uniqueN(pin)
), by = .(
  scenario,
  year = as.integer(year_built),
  unit_bin = as.character(unit_bin),
  source,
  source_rule,
  source_class_group,
  modelgroup,
  property_type_use,
  exclusion_flag
)]
setorder(scenario_detail_summary, scenario, year, source, source_class_group, unit_bin)

scenario_order <- c(
  "production_density_output",
  "current_cleaned_source_as_is",
  "current_cleaned_source_noncondo_nonhotel",
  "card_min_res_plus_current_comm_noncondo",
  "card_min_res_plus_year_repaired_comm_noncondo",
  "card_min_res_plus_expanded_comm_noncondo",
  "card_max_res_plus_expanded_comm_noncondo_sensitivity"
)
closure_base <- rbindlist(list(
  production_baseline[, .(scenario, buildings, units, bps_buildings, bps_units, building_gap, unit_gap, building_share_of_bps, unit_share_of_bps)],
  scenario_summary[scenario %in% scenario_order, .(scenario, buildings, units, bps_buildings, bps_units, building_gap, unit_gap, building_share_of_bps, unit_share_of_bps)]
), fill = TRUE)
closure_base[, scenario := factor(scenario, levels = scenario_order)]
setorder(closure_base, scenario)
closure_base[, scenario := as.character(scenario)]

scenario_gap_closure <- closure_base[, .(
  step = c(
    "production_to_current_source",
    "exclude_known_or_ambiguous_condo_hotel",
    "residential_card_aggregation",
    "commercial_dedup_year_repair",
    "broader_commercial_class_model",
    "max_year_sensitivity"
  ),
  from_scenario = head(scenario, -1),
  to_scenario = tail(scenario, -1),
  from_units = head(units, -1),
  to_units = tail(units, -1),
  units_delta = tail(units, -1) - head(units, -1),
  from_gap_units = head(unit_gap, -1),
  to_gap_units = tail(unit_gap, -1),
  gap_closed_pct_of_initial_current_source_gap = (tail(units, -1) - head(units, -1)) /
    unit_gap[scenario == "current_cleaned_source_as_is"]
)]

updated_check <- cbind(
  scenario_summary[scenario == "card_min_res_plus_year_repaired_comm_noncondo", .(
    reference_scenario = scenario,
    reference_units = units,
    reference_buildings = buildings
  )],
  scenario_summary[scenario == "updated_cleaned_source_noncondo_nonhotel", .(
    updated_scenario = scenario,
    updated_units = units,
    updated_buildings = buildings
  )]
)

unit_addition_ledger <- rbindlist(list(
  scenario_gap_closure[, .(
    tier = "source_universe",
    step,
    from_scenario,
    to_scenario,
    units_delta,
    from_units,
    to_units,
    notes = fifelse(
      step == "broader_commercial_class_model",
      "Expected to remain zero because the only outside-regex positive-unit row is parking.",
      ""
    )
  )],
  data.table(
    tier = "manual_correction_layer",
    step = "approved_manual_corrections",
    from_scenario = "card_min_res_plus_year_repaired_comm_noncondo",
    to_scenario = "manual_approved_corrections_applied",
    units_delta = 0L,
    from_units = scenario_summary[scenario == "card_min_res_plus_year_repaired_comm_noncondo", units],
    to_units = scenario_summary[scenario == "card_min_res_plus_year_repaired_comm_noncondo", units],
    notes = "No manual corrections are applied until approved rows exist in tasks/new_construction_manual_corrections/manual/approved_assessor_corrections.csv."
  ),
  data.table(
    tier = "updated_source_validation",
    step = "rebuilt_source_files_match_main_scenario",
    from_scenario = updated_check$reference_scenario,
    to_scenario = updated_check$updated_scenario,
    units_delta = updated_check$updated_units - updated_check$reference_units,
    from_units = updated_check$reference_units,
    to_units = updated_check$updated_units,
    notes = "This should be zero after the residential and commercial source fixes are rebuilt."
  )
), fill = TRUE)

message("Building targeted review queues...")
res_current_compare <- res_current_rows[, .(
  pin,
  current_year = year_built,
  current_units = units,
  current_building_sqft = building_sqft,
  current_class_group = source_class_group,
  current_exclusion_flag = exclusion_flag
)]
res_card_compare <- res_card_aggregate[, .(
  pin,
  card_min_year = min_year_built,
  card_max_year = max_year_built,
  card_units = units,
  card_building_sqft = building_sqft,
  n_cards,
  n_distinct_card_years,
  card_class_group = source_class_group,
  card_class_codes = class_codes,
  card_exclusion_flag = exclusion_flag,
  any_class_2_12,
  class_2_12_units
)]
residential_card_aggregation_review <- merge(res_current_compare, res_card_compare, by = "pin", all = TRUE)
residential_card_aggregation_review[, `:=`(
  current_in_window = in_window(current_year),
  card_min_in_window = in_window(card_min_year),
  card_max_in_window = in_window(card_max_year),
  units_delta_card_minus_current = card_units - current_units,
  sqft_delta_card_minus_current = card_building_sqft - current_building_sqft
)]
residential_card_aggregation_review <- residential_card_aggregation_review[
  (current_in_window == TRUE | card_min_in_window == TRUE | card_max_in_window == TRUE) &
    (
      n_cards > 1 |
        abs(fcoalesce(units_delta_card_minus_current, 0)) > 0 |
        abs(fcoalesce(sqft_delta_card_minus_current, 0)) > 1
    )
]
residential_card_aggregation_review[, `:=`(
  abs_units_delta_card_minus_current = abs(fcoalesce(units_delta_card_minus_current, 0)),
  abs_sqft_delta_card_minus_current = abs(fcoalesce(sqft_delta_card_minus_current, 0))
)]
setorder(residential_card_aggregation_review, -abs_units_delta_card_minus_current, -abs_sqft_delta_card_minus_current, pin)

residential_year_window_review <- residential_card_aggregation_review[
  fcoalesce(card_min_in_window, FALSE) != fcoalesce(card_max_in_window, FALSE) |
    fcoalesce(current_in_window, FALSE) != fcoalesce(card_min_in_window, FALSE),
  .(
    pin,
    current_year,
    card_min_year,
    card_max_year,
    current_units,
    card_units,
    n_cards,
    n_distinct_card_years,
    current_in_window,
    card_min_in_window,
    card_max_in_window,
    current_class_group,
    card_class_group,
    card_exclusion_flag
  )
]
setorder(residential_year_window_review, card_min_year, card_max_year, pin)

comm_selected_compare <- comm_current_selected[, .(
  pin,
  selected_year_built = yearbuilt,
  selected_units = units_current_script,
  selected_building_sqft = bldgsf,
  selected_land_sqft = landsf,
  selected_modelgroup = as.character(modelgroup),
  selected_class_codes = class_codes,
  selected_property_type_use = as.character(property_type_use),
  selected_exclusion_flag = exclusion_flag
)]
comm_eligible_inside <- comm_expanded_pool[exclusion_flag == "included_noncondo_nonhotel"]
setorder(comm_eligible_inside, pin, -has_land, -units_positive_rule, yearbuilt, -bldgsf, raw_row_n)
comm_eligible_inside <- comm_eligible_inside[, .SD[1], by = pin]
commercial_dedup_year_repair_review <- merge(
  comm_eligible_inside[, .(
    pin,
    candidate_year_built = yearbuilt,
    candidate_units = units_positive_rule,
    candidate_building_sqft = bldgsf,
    candidate_land_sqft = landsf,
    candidate_modelgroup = as.character(modelgroup),
    candidate_class_codes = class_codes,
    candidate_property_type_use = as.character(property_type_use),
    candidate_address = as.character(address),
    candidate_current_model_rule = current_model_rule
  )],
  comm_selected_compare,
  by = "pin",
  all.x = TRUE
)
commercial_dedup_year_repair_review[, review_reason := fifelse(
  is.na(selected_year_built), "not_selected_by_current_rule",
  fifelse(!in_window(selected_year_built), "selected_row_outside_2006_2022", "selected_row_inside_2006_2022")
)]
commercial_dedup_year_repair_review <- commercial_dedup_year_repair_review[
  review_reason != "selected_row_inside_2006_2022"
]
setorder(commercial_dedup_year_repair_review, -candidate_units, pin)

commercial_outside_regex_review <- comm_raw[
  chicago == TRUE &
    in_window(yearbuilt) &
    units_positive_rule > 0 &
    current_model_rule != TRUE &
    known_condo_signal != TRUE &
    hotel_motel_signal != TRUE,
  .(
    pin,
    year_built = yearbuilt,
    units = units_positive_rule,
    building_sqft = bldgsf,
    land_sqft = landsf,
    modelgroup = as.character(modelgroup),
    class_codes,
    property_type_use = as.character(property_type_use),
    address = as.character(address),
    property_name_description = as.character(property_name_description),
    class_multifamily_signal,
    broad_rule_included = class_multifamily_signal == TRUE,
    review_reason = fifelse(class_multifamily_signal == TRUE, "outside_regex_but_broad_residential_signal", "outside_regex_positive_units_manual_review")
  )
]
setorder(commercial_outside_regex_review, -units, pin)

possible_condo_review <- rbindlist(list(
  res_current_rows[exclusion_flag != "included_noncondo" & in_window(year_built), .(
    source_rule,
    pin,
    year_built,
    units,
    source_class_group,
    class_codes,
    modelgroup,
    property_type_use,
    exclusion_flag
  )],
  comm_current_rows[exclusion_flag != "included_noncondo_nonhotel" & in_window(year_built), .(
    source_rule,
    pin,
    year_built,
    units,
    source_class_group,
    class_codes,
    modelgroup,
    property_type_use,
    exclusion_flag
  )]
), fill = TRUE)
setorder(possible_condo_review, source_rule, exclusion_flag, pin)

message("Writing audit outputs...")
fwrite(production_baseline, "../output/production_baseline_validation.csv", na = "")
fwrite(comm_clean_validation, "../output/commercial_current_rebuild_validation.csv", na = "")
fwrite(scenario_candidates, "../output/scenario_pin_candidates.csv", na = "")
fwrite(scenario_summary, "../output/scenario_summary.csv", na = "")
fwrite(scenario_by_year_unit_bin, "../output/scenario_bps_gap_by_year_unit_bin.csv", na = "")
fwrite(scenario_detail_summary, "../output/scenario_detail_summary.csv", na = "")
fwrite(scenario_gap_closure, "../output/scenario_gap_closure.csv", na = "")
fwrite(unit_addition_ledger, "../output/source_universe_unit_addition_ledger.csv", na = "")
fwrite(residential_card_aggregation_review, "../output/residential_card_aggregation_review.csv", na = "")
fwrite(residential_year_window_review, "../output/residential_year_window_review.csv", na = "")
fwrite(commercial_dedup_year_repair_review, "../output/commercial_dedup_year_repair_review.csv", na = "")
fwrite(commercial_outside_regex_review, "../output/commercial_outside_regex_review.csv", na = "")
fwrite(possible_condo_review, "../output/condo_hotel_exclusion_review.csv", na = "")

fmt_int <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_pct <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")

headline_scenarios <- scenario_summary[scenario %in% c(
  "current_cleaned_source_as_is",
  "current_cleaned_source_noncondo_nonhotel",
  "card_min_res_plus_current_comm_noncondo",
  "card_min_res_plus_year_repaired_comm_noncondo",
  "card_min_res_plus_expanded_comm_noncondo",
  "card_max_res_plus_expanded_comm_noncondo_sensitivity",
  "updated_cleaned_source_noncondo_nonhotel"
)]
headline_order <- c(
  "current_cleaned_source_as_is",
  "current_cleaned_source_noncondo_nonhotel",
  "card_min_res_plus_current_comm_noncondo",
  "card_min_res_plus_year_repaired_comm_noncondo",
  "card_min_res_plus_expanded_comm_noncondo",
  "card_max_res_plus_expanded_comm_noncondo_sensitivity",
  "updated_cleaned_source_noncondo_nonhotel"
)
headline_scenarios[, headline_order_id := match(scenario, headline_order)]
setorder(headline_scenarios, headline_order_id)

scenario_lines <- apply(headline_scenarios, 1, function(row) {
  paste0(
    "- ", row[["scenario"]], ": ",
    fmt_int(as.numeric(row[["buildings"]])), " PINs/buildings, ",
    fmt_int(as.numeric(row[["units"]])), " units, ",
    fmt_pct(as.numeric(row[["unit_share_of_bps"]])), " of BPS units."
  )
})

closure_lines <- apply(scenario_gap_closure, 1, function(row) {
  paste0(
    "- ", row[["step"]], ": ",
    fmt_int(as.numeric(row[["units_delta"]])), " units delta; gap moves from ",
    fmt_int(as.numeric(row[["from_gap_units"]])), " to ",
    fmt_int(as.numeric(row[["to_gap_units"]])), " units."
  )
})

report <- c(
  "# New Construction Universe Investigation",
  "",
  paste0("Generated on ", Sys.Date(), "."),
  "",
  "## Scope",
  "",
  "This task produces audit-only source-universe diagnostics. It does not itself change density samples, paper tables, figures, or text.",
  "",
  "Target comparison is assessor-observed residential units built 2006-2022, excluding known condo and hotel/motel/institutional signals, against Census BPS as an aggregate benchmark.",
  "",
  "## Production Baseline",
  "",
  paste0(
    "- Existing production density output: ",
    fmt_int(production_baseline$buildings), " buildings and ",
    fmt_int(production_baseline$units), " units."
  ),
  paste0(
    "- BPS benchmark: ",
    fmt_int(production_baseline$bps_buildings), " buildings and ",
    fmt_int(production_baseline$bps_units), " units."
  ),
  paste0("- Production output share of BPS units: ", fmt_pct(production_baseline$unit_share_of_bps), "."),
  "",
  "## Source-Level Scenarios",
  "",
  scenario_lines,
  "",
  "## Gap Closure",
  "",
  closure_lines,
  "",
  "## Review Queues",
  "",
  paste0("- Residential card aggregation review: ", fmt_int(nrow(residential_card_aggregation_review)), " PINs."),
  paste0("- Residential year-window review: ", fmt_int(nrow(residential_year_window_review)), " PINs."),
  paste0("- Commercial selected-old-row repair review: ", fmt_int(nrow(commercial_dedup_year_repair_review)), " PINs."),
  paste0("- Commercial outside-current-regex review: ", fmt_int(nrow(commercial_outside_regex_review)), " rows."),
  paste0("- Condo/hotel exclusion review: ", fmt_int(nrow(possible_condo_review)), " source rows."),
  "",
  "## Interpretation Notes",
  "",
  "- `current_cleaned_source_as_is` is a source-level reconstruction before geocoding/final density filters; it is not the same object as the production density output.",
  "- `card_min_res_plus_expanded_comm_noncondo` is the main audit scenario for the proposed assessor-source universe, still not a production sample.",
  "- `updated_cleaned_source_noncondo_nonhotel` reads the branch's rebuilt source files and should match the main card-min plus year-repaired commercial scenario.",
  "- `card_max_res_plus_expanded_comm_noncondo_sensitivity` is a sensitivity flag for year-built ambiguity, not an endorsed construction-year rule.",
  "- Rows in the review queues should be manually checked before any production-sample change."
)
writeLines(report, "../output/new_construction_universe_investigation_report.md")

message("Done.")
