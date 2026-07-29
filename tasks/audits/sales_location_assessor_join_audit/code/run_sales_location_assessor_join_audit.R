# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_location_assessor_join_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/canonical_geometry_helpers.R")

sales_raw <- fread(
  "../input/parcel_sales.csv",
  select = c(
    "pin", "year", "class", "sale_date", "sale_price", "sale_deed_type",
    "sale_seller_name", "num_parcels_sale", "sale_buyer_name", "sale_type", "row_id"
  ),
  colClasses = list(character = c("pin", "sale_date", "sale_price", "row_id"))
)

sales_raw[, `:=`(
  year = suppressWarnings(as.integer(year)),
  sale_price_nominal = suppressWarnings(as.numeric(gsub("[$,]", "", sale_price))),
  pin_digits = gsub("[^0-9]", "", trimws(pin)),
  pin_length_raw = nchar(gsub("[^0-9]", "", trimws(pin)))
)]
sales_raw[, pin_norm := fcase(
  pin_length_raw == 14L, pin_digits,
  pin_length_raw == 13L, paste0("0", pin_digits),
  default = NA_character_
)]
sales_raw[, pin10_norm := substr(pin_norm, 1L, 10L)]

sales <- sales_raw[
  class %in% c(202, 203, 204, 205, 206, 207, 208, 209, 210, 211) &
    !is.na(sale_price_nominal) & sale_price_nominal > 10000 &
    !is.na(year) & year >= 1999 & year <= 2025 &
    sale_deed_type %in% c("Warranty", "Trustee") &
    sale_type != "LAND" &
    !sale_seller_name %in% c("", "-", "UNKNOWN", "..") &
    sale_seller_name != sale_buyer_name &
    num_parcels_sale == 1
]

if (anyDuplicated(sales$row_id) > 0) {
  stop("Filtered raw sales must be unique by row_id.", call. = FALSE)
}

parcels <- fread(
  "../input/parcel_universe.csv",
  select = c(
    "pin", "pin10", "longitude", "latitude", "centroid_x_crs_3435",
    "centroid_y_crs_3435", "ward_num"
  ),
  colClasses = list(character = c("pin", "pin10"))
)
parcels[, `:=`(
  pin_digits = gsub("[^0-9]", "", trimws(pin)),
  pin10_digits = gsub("[^0-9]", "", trimws(pin10))
)]
parcels[, pin_norm := fcase(
  nchar(pin_digits) == 14L, pin_digits,
  nchar(pin_digits) == 13L, paste0("0", pin_digits),
  default = NA_character_
)]
parcels[, pin10_norm := fcase(
  nchar(pin10_digits) == 10L, pin10_digits,
  nchar(pin10_digits) == 9L, paste0("0", pin10_digits),
  default = NA_character_
)]
parcels[, valid_coordinate :=
  is.finite(centroid_x_crs_3435) & is.finite(centroid_y_crs_3435) &
    is.finite(longitude) & is.finite(latitude)]

if (anyDuplicated(parcels[!is.na(pin_norm), pin_norm]) > 0) {
  stop("Parcel universe is not unique by normalized 14-digit PIN.", call. = FALSE)
}
if (parcels[!is.na(pin_norm) & !is.na(pin10_norm), any(pin10_norm != substr(pin_norm, 1L, 10L))]) {
  stop("Parcel PIN10 field disagrees with the first 10 digits of the normalized PIN.", call. = FALSE)
}

parcels_exact <- parcels[, .(
  pin_norm,
  exact_pin_found = TRUE,
  exact_coordinate_found = valid_coordinate
)]
sales_location <- merge(sales, parcels_exact, by = "pin_norm", all.x = TRUE, sort = FALSE)
if (nrow(sales_location) != nrow(sales)) {
  stop("Exact parcel join changed the number of filtered sales.", call. = FALSE)
}
sales_location[is.na(exact_pin_found), exact_pin_found := FALSE]
sales_location[is.na(exact_coordinate_found), exact_coordinate_found := FALSE]

relevant_pin10 <- unique(sales_location[
  year >= 2006 & year <= 2022 & !exact_coordinate_found & !is.na(pin10_norm),
  pin10_norm
])

candidate_coordinates <- unique(
  parcels[
    pin10_norm %in% relevant_pin10 & valid_coordinate == TRUE,
    .(
      pin10_norm,
      centroid_x_crs_3435,
      centroid_y_crs_3435,
      ward_num = suppressWarnings(as.integer(ward_num))
    )
  ],
  by = c("pin10_norm", "centroid_x_crs_3435", "centroid_y_crs_3435")
)

candidate_coordinates[, `:=`(ward_2003 = NA_integer_, ward_2015 = NA_integer_)]
if (nrow(candidate_coordinates) > 0) {
  ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE) %>%
    st_transform(3435)
  ward_maps <- load_canonical_ward_maps(ward_panel)

  candidate_points <- st_as_sf(
    candidate_coordinates,
    coords = c("centroid_x_crs_3435", "centroid_y_crs_3435"),
    crs = 3435,
    remove = FALSE
  )

  within_2003 <- st_within(candidate_points, ward_maps[["2003_2014"]])
  within_2015 <- st_within(candidate_points, ward_maps[["2015_2023"]])
  candidate_coordinates[, ward_2003 := vapply(within_2003, function(idx) {
    if (length(idx) == 1L) ward_maps[["2003_2014"]]$ward[idx] else NA_integer_
  }, integer(1))]
  candidate_coordinates[, ward_2015 := vapply(within_2015, function(idx) {
    if (length(idx) == 1L) ward_maps[["2015_2023"]]$ward[idx] else NA_integer_
  }, integer(1))]
}

parcel_candidate_counts <- parcels[
  pin10_norm %in% relevant_pin10 & valid_coordinate == TRUE,
  .(n_candidate_parcels = .N),
  by = pin10_norm
]
if (nrow(candidate_coordinates) == 0) {
  candidate_groups <- data.table(
    pin10_norm = character(),
    n_unique_coordinates = integer(),
    coordinate_span_ft = numeric(),
    n_current_wards = integer(),
    n_2003_wards = integer(),
    n_2015_wards = integer()
  )
} else {
  candidate_groups <- candidate_coordinates[, .(
    n_unique_coordinates = .N,
    coordinate_span_ft = sqrt(
      (max(centroid_x_crs_3435) - min(centroid_x_crs_3435))^2 +
        (max(centroid_y_crs_3435) - min(centroid_y_crs_3435))^2
    ),
    n_current_wards = uniqueN(ward_num[!is.na(ward_num)]),
    n_2003_wards = uniqueN(ward_2003[!is.na(ward_2003)]),
    n_2015_wards = uniqueN(ward_2015[!is.na(ward_2015)])
  ), by = pin10_norm]
}
candidate_groups <- merge(
  candidate_groups,
  parcel_candidate_counts,
  by = "pin10_norm",
  all = TRUE,
  sort = FALSE
)

sales_location <- merge(sales_location, candidate_groups, by = "pin10_norm", all.x = TRUE, sort = FALSE)
if (nrow(sales_location) != nrow(sales)) {
  stop("PIN10 candidate summary join changed the number of filtered sales.", call. = FALSE)
}

sales_location[, historical_ward_count := fifelse(
  year < 2015L,
  n_2003_wards,
  n_2015_wards
)]
sales_location[, location_match_status := fcase(
  exact_coordinate_found, "exact_full_pin",
  is.na(n_unique_coordinates), "no_pin10_coordinate_candidate",
  n_unique_coordinates == 1L, "pin10_unique_coordinate",
  historical_ward_count > 1L, "pin10_ambiguous_crosses_historical_ward",
  n_unique_coordinates > 1L, "pin10_ambiguous_multiple_coordinates",
  default = "unclassified"
)]

sales_location[, production_pin := fifelse(substr(pin_norm, 1L, 1L) == "0", substr(pin_norm, 2L, 14L), pin_norm)]
sales_location[, production_pin10_sales := substr(production_pin, 1L, 10L)]
production_pin10_values <- unique(fifelse(
  substr(parcels$pin10_norm, 1L, 1L) == "0",
  substr(parcels$pin10_norm, 2L, 10L),
  parcels$pin10_norm
))
sales_location[, production_fallback_key_found := production_pin10_sales %in% production_pin10_values]

coordinate_match_summary <- rbindlist(list(
  sales_location[, .(n = .N), by = location_match_status][
    , sample := "eligible_sales_1999_2025"
  ],
  sales_location[year >= 2006 & year <= 2022, .(n = .N), by = location_match_status][
    , sample := "eligible_sales_2006_2022"
  ]
), use.names = TRUE)
coordinate_match_summary[, share := n / sum(n), by = sample]
setcolorder(coordinate_match_summary, c("sample", "location_match_status", "n", "share"))
setorder(coordinate_match_summary, sample, location_match_status)

coordinate_fallback_cases <- sales_location[
  year >= 2006 & year <= 2022 & !exact_coordinate_found,
  .(
    row_id,
    sale_date,
    year,
    pin_raw = pin,
    pin_norm,
    pin10_norm,
    pin_length_raw,
    exact_pin_found,
    location_match_status,
    n_candidate_parcels,
    n_unique_coordinates,
    coordinate_span_ft,
    n_current_wards,
    n_2003_wards,
    n_2015_wards,
    production_fallback_key_found
  )
]
setorder(coordinate_fallback_cases, location_match_status, -coordinate_span_ft, year, pin_norm)

sales_final <- as.data.table(read_parquet("../input/sales_with_hedonics_amenities.parquet"))
sales_final[, `:=`(
  audit_sale_id = .I,
  pin_current = as.character(pin),
  pin_digits = gsub("[^0-9]", "", trimws(as.character(pin))),
  pin_length_current = nchar(gsub("[^0-9]", "", trimws(as.character(pin))))
)]
sales_final[, pin_norm := fcase(
  pin_length_current == 14L, pin_digits,
  pin_length_current == 13L, paste0("0", pin_digits),
  default = NA_character_
)]
sales_final[, sale_year_audit := as.integer(format(as.Date(sale_date), "%Y"))]
sales_final[, exact_coordinate_pin := pin_norm %in% parcels[valid_coordinate == TRUE, pin_norm]]

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbWriteTable(con, "audit_sales_pins", unique(sales_final[!is.na(pin_norm), .(pin_norm)]), overwrite = TRUE)
improvements <- as.data.table(dbGetQuery(con, "
  SELECT
    i.pin,
    i.tax_year,
    i.tax_year AS assessor_year,
    i.year_built,
    i.building_sqft,
    i.land_sqft,
    i.num_bedrooms,
    i.num_full_baths,
    i.num_half_baths,
    i.garage_size
  FROM read_parquet('../input/residential_improvements_panel.parquet') AS i
  INNER JOIN audit_sales_pins AS s
    ON i.pin = s.pin_norm
"))
if (anyDuplicated(improvements[, .(pin, tax_year)]) > 0) {
  stop("Residential improvements panel is not unique by PIN-tax year.", call. = FALSE)
}
setkey(improvements, pin, tax_year)

sales_join_keys <- sales_final[, .(
  audit_sale_id,
  pin_current,
  pin_norm,
  sale_year = sale_year_audit,
  pin_length_current
)]

current_format_match <- improvements[
  sales_join_keys,
  on = .(pin = pin_current, tax_year = sale_year),
  roll = TRUE,
  rollends = c(TRUE, FALSE),
  .(audit_sale_id, assessor_year)
]
current_format_match[, join_rule := "current_pin_format_current_rollends"]

normalized_current_match <- improvements[
  sales_join_keys,
  on = .(pin = pin_norm, tax_year = sale_year),
  roll = TRUE,
  rollends = c(TRUE, FALSE),
  .(
    audit_sale_id,
    assessor_year,
    audit_year_built = year_built,
    audit_building_sqft = building_sqft,
    audit_land_sqft = land_sqft,
    audit_num_bedrooms = num_bedrooms,
    audit_num_full_baths = num_full_baths,
    audit_num_half_baths = num_half_baths,
    audit_garage_size = garage_size
  )
]
normalized_current_match[, join_rule := "normalized_pin_current_rollends"]

normalized_prior_match <- improvements[
  sales_join_keys,
  on = .(pin = pin_norm, tax_year = sale_year),
  roll = TRUE,
  rollends = c(FALSE, TRUE),
  .(
    audit_sale_id,
    assessor_year,
    audit_year_built = year_built,
    audit_building_sqft = building_sqft,
    audit_land_sqft = land_sqft,
    audit_num_bedrooms = num_bedrooms,
    audit_num_full_baths = num_full_baths,
    audit_num_half_baths = num_half_baths,
    audit_garage_size = garage_size
  )
]
normalized_prior_match[, join_rule := "normalized_pin_latest_prior"]

normalized_exact_match <- improvements[
  sales_join_keys,
  on = .(pin = pin_norm, tax_year = sale_year),
  roll = FALSE,
  .(audit_sale_id, assessor_year)
]
normalized_exact_match[, join_rule := "normalized_pin_exact_year"]

assessor_matches <- rbindlist(list(
  current_format_match,
  normalized_current_match[, .(audit_sale_id, assessor_year, join_rule)],
  normalized_prior_match[, .(audit_sale_id, assessor_year, join_rule)],
  normalized_exact_match
), use.names = TRUE)
assessor_matches <- merge(
  assessor_matches,
  sales_final[, .(
    audit_sale_id,
    pin_current,
    pin_norm,
    pin_length_current,
    sale_year = sale_year_audit,
    signed_dist_m,
    segment_id,
    strictness_own,
    strictness_neighbor
  )],
  by = "audit_sale_id",
  all.x = TRUE,
  sort = FALSE
)
assessor_matches[, assessor_gap := sale_year - assessor_year]
assessor_matches[, gap_category := fcase(
  is.na(assessor_year), "no_match",
  assessor_gap < 0, "future_assessment",
  assessor_gap == 0, "same_year",
  assessor_gap == 1, "one_year_prior",
  assessor_gap >= 2 & assessor_gap <= 3, "two_to_three_years_prior",
  assessor_gap >= 4 & assessor_gap <= 5, "four_to_five_years_prior",
  assessor_gap > 5, "more_than_five_years_prior",
  default = "unclassified"
)]
assessor_matches[, rd_geometry_sample :=
  sale_year >= 2006 & sale_year <= 2022 &
    is.finite(signed_dist_m) & abs(signed_dist_m / 0.3048) <= 500 &
    !is.na(segment_id) & segment_id != "" &
    !is.na(strictness_own) & !is.na(strictness_neighbor)]

assessor_match_summary <- rbindlist(list(
  assessor_matches[sale_year >= 2006 & sale_year <= 2022, .(n = .N), by = .(join_rule, gap_category)][
    , sample := "located_sales_2006_2022"
  ],
  assessor_matches[rd_geometry_sample == TRUE, .(n = .N), by = .(join_rule, gap_category)][
    , sample := "rd_500ft_before_hedonic_complete_cases"
  ]
), use.names = TRUE)
assessor_match_summary[, share := n / sum(n), by = .(sample, join_rule)]
setcolorder(assessor_match_summary, c("sample", "join_rule", "gap_category", "n", "share"))
setorder(assessor_match_summary, sample, join_rule, gap_category)

assessor_gap_cases <- assessor_matches[
  sale_year >= 2006 & sale_year <= 2022 &
    (is.na(assessor_year) | assessor_gap != 0),
  .(
    audit_sale_id,
    pin_current,
    pin_norm,
    pin_length_current,
    sale_year,
    join_rule,
    assessor_year,
    assessor_gap,
    gap_category,
    rd_geometry_sample
  )
]
assessor_gap_cases[, abs_assessor_gap := abs(assessor_gap)]
setorder(assessor_gap_cases, join_rule, gap_category, -abs_assessor_gap, sale_year, pin_norm)
assessor_gap_cases[, abs_assessor_gap := NULL]

model_common <- sales_final[, .(
  audit_sale_id,
  sale_price,
  sale_year = sale_year_audit,
  year_quarter,
  segment_id,
  ward_pair_id,
  signed_dist = as.numeric(signed_dist_m) / 0.3048,
  strictness_own,
  strictness_neighbor,
  exact_coordinate_pin,
  nearest_school_dist_ft,
  nearest_park_dist_ft,
  nearest_major_road_dist_ft,
  lake_michigan_dist_ft
)]
model_common[, right := as.integer(signed_dist >= 0)]

current_model_data <- merge(
  model_common,
  sales_final[, .(
    audit_sale_id,
    model_log_sqft = log_sqft,
    model_log_land_sqft = log_land_sqft,
    model_log_building_age = log_building_age,
    model_log_bedrooms = log_bedrooms,
    model_log_baths = log_baths,
    model_has_garage = has_garage
  )],
  by = "audit_sale_id",
  all.x = TRUE,
  sort = FALSE
)
current_model_data[, `:=`(
  assessor_year = as.integer(NA),
  assessor_gap = as.integer(NA),
  specification = "current_production_fields"
)]

normalized_current_data <- merge(
  model_common,
  normalized_current_match[, .(
    audit_sale_id,
    assessor_year,
    audit_year_built,
    audit_building_sqft,
    audit_land_sqft,
    audit_num_bedrooms,
    audit_num_full_baths,
    audit_num_half_baths,
    audit_garage_size
  )],
  by = "audit_sale_id",
  all.x = TRUE,
  sort = FALSE
)
normalized_current_data[, `:=`(
  building_age = sale_year - audit_year_built,
  baths_total = audit_num_full_baths + 0.5 * fifelse(is.na(audit_num_half_baths), 0, audit_num_half_baths),
  model_has_garage = as.integer(audit_garage_size > 0 & !is.na(audit_garage_size)),
  assessor_gap = sale_year - assessor_year
)]
normalized_current_data[building_age < 0, building_age := NA_real_]
normalized_current_data[, `:=`(
  model_log_sqft = fifelse(!is.na(audit_building_sqft) & audit_building_sqft > 0, log(audit_building_sqft), NA_real_),
  model_log_land_sqft = fifelse(!is.na(audit_land_sqft) & audit_land_sqft > 0, log(audit_land_sqft), NA_real_),
  model_log_building_age = fifelse(!is.na(building_age) & building_age > 0, log(building_age), NA_real_),
  model_log_bedrooms = fifelse(!is.na(audit_num_bedrooms) & audit_num_bedrooms > 0, log(audit_num_bedrooms), NA_real_),
  model_log_baths = fifelse(!is.na(baths_total) & baths_total > 0, log(baths_total), NA_real_),
  specification = "normalized_pin_current_rollends"
)]

normalized_prior_data <- merge(
  model_common,
  normalized_prior_match[, .(
    audit_sale_id,
    assessor_year,
    audit_year_built,
    audit_building_sqft,
    audit_land_sqft,
    audit_num_bedrooms,
    audit_num_full_baths,
    audit_num_half_baths,
    audit_garage_size
  )],
  by = "audit_sale_id",
  all.x = TRUE,
  sort = FALSE
)
normalized_prior_data[, `:=`(
  building_age = sale_year - audit_year_built,
  baths_total = audit_num_full_baths + 0.5 * fifelse(is.na(audit_num_half_baths), 0, audit_num_half_baths),
  model_has_garage = as.integer(audit_garage_size > 0 & !is.na(audit_garage_size)),
  assessor_gap = sale_year - assessor_year
)]
normalized_prior_data[building_age < 0, building_age := NA_real_]
normalized_prior_data[, `:=`(
  model_log_sqft = fifelse(!is.na(audit_building_sqft) & audit_building_sqft > 0, log(audit_building_sqft), NA_real_),
  model_log_land_sqft = fifelse(!is.na(audit_land_sqft) & audit_land_sqft > 0, log(audit_land_sqft), NA_real_),
  model_log_building_age = fifelse(!is.na(building_age) & building_age > 0, log(building_age), NA_real_),
  model_log_bedrooms = fifelse(!is.na(audit_num_bedrooms) & audit_num_bedrooms > 0, log(audit_num_bedrooms), NA_real_),
  model_log_baths = fifelse(!is.na(baths_total) & baths_total > 0, log(baths_total), NA_real_),
  specification = "normalized_pin_latest_prior_any_gap"
)]

estimate_sales_rd <- function(data, specification) {
  model_vars <- c(
    "model_log_sqft", "model_log_land_sqft", "model_log_building_age",
    "model_log_bedrooms", "model_log_baths", "model_has_garage",
    "nearest_school_dist_ft", "nearest_park_dist_ft",
    "nearest_major_road_dist_ft", "lake_michigan_dist_ft"
  )
  model_sample <- data[
    sale_year >= 2006 & sale_year <= 2022 &
      !is.na(sale_price) & sale_price > 0 &
      !is.na(ward_pair_id) & !is.na(segment_id) & segment_id != "" &
      is.finite(signed_dist) & abs(signed_dist) <= 500 &
      !is.na(strictness_own) & !is.na(strictness_neighbor)
  ]
  model_sample <- model_sample[complete.cases(model_sample[, ..model_vars])]
  if (nrow(model_sample) == 0) {
    stop(sprintf("No observations remain for audit specification %s.", specification), call. = FALSE)
  }

  model <- feols(
    log(sale_price) ~ right + model_log_sqft + model_log_land_sqft +
      model_log_building_age + model_log_bedrooms + model_log_baths +
      model_has_garage + nearest_school_dist_ft + nearest_park_dist_ft +
      nearest_major_road_dist_ft + lake_michigan_dist_ft |
      segment_id^year_quarter,
    data = model_sample,
    cluster = ~segment_id
  )
  coefficient <- coeftable(model)["right", , drop = FALSE]
  data.table(
    specification = specification,
    estimate = unname(coefficient[1, "Estimate"]),
    std_error = unname(coefficient[1, "Std. Error"]),
    p_value = unname(coefficient[1, "Pr(>|t|)"]),
    n = nobs(model),
    n_segments = uniqueN(model_sample$segment_id),
    exact_coordinate_share = mean(model_sample$exact_coordinate_pin),
    assessor_same_year_share = if (all(is.na(model_sample$assessor_gap))) NA_real_ else mean(model_sample$assessor_gap == 0, na.rm = TRUE)
  )
}

sales_rd_sensitivity <- rbindlist(list(
  estimate_sales_rd(current_model_data, "current_production_fields"),
  estimate_sales_rd(normalized_current_data, "normalized_pin_current_rollends"),
  estimate_sales_rd(normalized_prior_data, "normalized_pin_latest_prior_any_gap"),
  estimate_sales_rd(normalized_prior_data[assessor_gap == 0], "normalized_pin_exact_assessment_year"),
  estimate_sales_rd(normalized_prior_data[assessor_gap >= 0 & assessor_gap <= 1], "normalized_pin_prior_gap_at_most_1"),
  estimate_sales_rd(normalized_prior_data[assessor_gap >= 0 & assessor_gap <= 3], "normalized_pin_prior_gap_at_most_3"),
  estimate_sales_rd(normalized_prior_data[assessor_gap >= 0 & assessor_gap <= 5], "normalized_pin_prior_gap_at_most_5")
), use.names = TRUE)

write_csv(as_tibble(coordinate_match_summary), "../output/sales_coordinate_match_summary.csv")
write_csv(as_tibble(coordinate_fallback_cases), "../output/sales_coordinate_fallback_cases.csv")
write_csv(as_tibble(assessor_match_summary), "../output/sales_assessor_match_summary.csv")
write_csv(as_tibble(assessor_gap_cases), "../output/sales_assessor_gap_cases.csv")
write_csv(as_tibble(sales_rd_sensitivity), "../output/sales_rd_match_sensitivity.csv")
