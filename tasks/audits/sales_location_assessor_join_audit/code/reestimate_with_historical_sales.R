# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/audits/sales_location_assessor_join_audit/code")

source("../../../setup_environment/code/packages.R")
source("../../../_lib/amenity_distance_helpers.R")

historical_cases <- fread(
  "../output/historical_parcel_match_cases.csv",
  colClasses = list(character = c("row_id", "pin_norm"))
)
historical_cases <- historical_cases[rd_geometry_complete == TRUE]
if (nrow(historical_cases) == 0) {
  stop("No historically recovered sales have complete 500-foot RD geometry.", call. = FALSE)
}

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
  pin_digits = gsub("[^0-9]", "", trimws(pin))
)]
sales_raw[, pin_norm := fcase(
  nchar(pin_digits) == 14L, pin_digits,
  nchar(pin_digits) == 13L, paste0("0", pin_digits),
  default = NA_character_
)]
sales_raw[, sale_date_use := as.Date(substr(sale_date, 1L, 10L))]
missing_date <- which(is.na(sales_raw$sale_date_use))
if (length(missing_date) > 0) {
  sales_raw[missing_date, sale_date_use := as.Date(sale_date, format = "%B %d, %Y")]
}

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
sales[, sale_date_for_price := fifelse(
  !is.na(sale_date_use),
  sale_date_use,
  as.Date(paste0(year, "-06-15"))
)]
sales[, sale_year_month := format(as.Date(sale_date_for_price), "%Y-%m")]

cpi_raw <- fread("../input/fred_cpi_cuura207sa0.csv", colClasses = "character")
cpi_lookup <- data.table(
  observation_date = as.Date(cpi_raw$observation_date),
  cpi_value = suppressWarnings(as.numeric(cpi_raw$CUURA207SA0))
)
cpi <- merge(
  data.table(
  observation_date = seq(
    as.Date(format(min(sales$sale_date_for_price, na.rm = TRUE), "%Y-%m-01")),
    as.Date(format(max(sales$sale_date_for_price, na.rm = TRUE), "%Y-%m-01")),
    by = "month"
  )
  ),
  cpi_lookup,
  by = "observation_date",
  all.x = TRUE,
  sort = TRUE
)
setorder(cpi, observation_date)
if (anyNA(cpi$cpi_value)) {
  known <- which(!is.na(cpi$cpi_value))
  cpi_interpolated <- approx(
    x = known,
    y = cpi$cpi_value[known],
    xout = seq_len(nrow(cpi)),
    method = "linear",
    rule = 1
  )$y
  cpi[is.na(cpi_value), cpi_value := cpi_interpolated[is.na(cpi$cpi_value)]]
}
if (anyNA(cpi$cpi_value)) {
  stop("CPI has unresolved gaps after interpolation.", call. = FALSE)
}
base_cpi <- mean(cpi[format(observation_date, "%Y") == "2022", cpi_value])
cpi[, `:=`(
  sale_year_month = format(observation_date, "%Y-%m"),
  sale_price_deflator_to_2022 = base_cpi / cpi_value
)]
sales <- merge(
  sales,
  cpi[, .(sale_year_month, sale_price_deflator_to_2022)],
  by = "sale_year_month",
  all.x = TRUE,
  sort = FALSE
)
if (anyNA(sales$sale_price_deflator_to_2022)) {
  stop("Recovered-sales price calculation has missing CPI deflators.", call. = FALSE)
}
sales[, sale_price_real_2022_raw := sale_price_nominal * sale_price_deflator_to_2022]
price_p01 <- quantile(sales$sale_price_real_2022_raw, 0.01, na.rm = TRUE)
price_p99 <- quantile(sales$sale_price_real_2022_raw, 0.99, na.rm = TRUE)
sales[, sale_price_model := pmin(pmax(sale_price_real_2022_raw, price_p01), price_p99)]

recovered <- merge(
  historical_cases,
  sales[, .(
    row_id,
    sale_date_model = sale_date_for_price,
    sale_year = year,
    pin_model = pin_norm,
    sale_price_model
  )],
  by = "row_id",
  all.x = TRUE,
  sort = FALSE
)
if (nrow(recovered) != nrow(historical_cases) || anyNA(recovered$sale_price_model)) {
  stop("Failed to recover one filtered raw sale row per historical location case.", call. = FALSE)
}
if (any(recovered$pin_norm != recovered$pin_model)) {
  stop("Historical location PINs disagree with normalized raw-sale PINs.", call. = FALSE)
}

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbWriteTable(
  con,
  "historical_sale_keys",
  unique(recovered[, .(pin = pin_norm, tax_year = sale_year)]),
  overwrite = TRUE
)
improvements <- as.data.table(dbGetQuery(con, "
  SELECT
    i.pin,
    i.tax_year,
    i.year_built,
    i.building_sqft,
    i.land_sqft,
    i.num_bedrooms,
    i.num_full_baths,
    i.num_half_baths,
    i.garage_size
  FROM read_parquet('../input/residential_improvements_panel.parquet') AS i
  INNER JOIN historical_sale_keys AS s
    ON i.pin = s.pin AND i.tax_year = s.tax_year
"))
if (anyDuplicated(improvements[, .(pin, tax_year)]) > 0) {
  stop("Recovered improvements are not unique by PIN-tax year.", call. = FALSE)
}
recovered <- merge(
  recovered,
  improvements,
  by.x = c("pin_norm", "sale_year"),
  by.y = c("pin", "tax_year"),
  all.x = TRUE,
  sort = FALSE
)
if (nrow(recovered) != nrow(historical_cases)) {
  stop("Exact-year improvements join changed the recovered sales row count.", call. = FALSE)
}

recovered[, `:=`(
  building_age = sale_year - year_built,
  baths_total = num_full_baths + 0.5 * fifelse(is.na(num_half_baths), 0, num_half_baths),
  model_has_garage = as.integer(garage_size > 0 & !is.na(garage_size))
)]
recovered[building_age < 0, building_age := NA_real_]
recovered[, `:=`(
  model_log_sqft = fifelse(!is.na(building_sqft) & building_sqft > 0, log(building_sqft), NA_real_),
  model_log_land_sqft = fifelse(!is.na(land_sqft) & land_sqft > 0, log(land_sqft), NA_real_),
  model_log_building_age = fifelse(!is.na(building_age) & building_age > 0, log(building_age), NA_real_),
  model_log_bedrooms = fifelse(!is.na(num_bedrooms) & num_bedrooms > 0, log(num_bedrooms), NA_real_),
  model_log_baths = fifelse(!is.na(baths_total) & baths_total > 0, log(baths_total), NA_real_)
)]

amenity_coordinates <- build_unique_coordinate_amenity_table(
  recovered,
  "longitude",
  "latitude",
  "../input/schools_2015.gpkg",
  "../input/parks.gpkg",
  "../input/major_streets.gpkg",
  "../input/gis_osm_water_a_free_1.shp",
  100000L
)
recovered <- append_amenity_distances(recovered, amenity_coordinates, "longitude", "latitude")
recovered[, `:=`(
  sale_price = sale_price_model,
  year_quarter = paste0(sale_year, "-Q", quarter(as.Date(sale_date_model))),
  signed_dist = dist_ft * fifelse(strictness_own > strictness_neighbor, 1, -1),
  source = "historical_exact_year"
)]
recovered[, right := as.integer(signed_dist >= 0)]

current <- as.data.table(read_parquet("../input/sales_with_hedonics_amenities.parquet"))
current_model_data <- current[, .(
  sale_price,
  sale_year = as.integer(format(as.Date(sale_date), "%Y")),
  year_quarter,
  segment_id,
  ward_pair_id,
  signed_dist = as.numeric(signed_dist_m) / 0.3048,
  right = as.integer(as.numeric(signed_dist_m) >= 0),
  strictness_own,
  strictness_neighbor,
  model_log_sqft = log_sqft,
  model_log_land_sqft = log_land_sqft,
  model_log_building_age = log_building_age,
  model_log_bedrooms = log_bedrooms,
  model_log_baths = log_baths,
  model_has_garage = has_garage,
  nearest_school_dist_ft,
  nearest_park_dist_ft,
  nearest_major_road_dist_ft,
  lake_michigan_dist_ft,
  source = "current_2025_universe",
  n_historical_coordinates = NA_integer_
)]
recovered_model_data <- recovered[, .(
  sale_price,
  sale_year,
  year_quarter,
  segment_id,
  ward_pair_id,
  signed_dist,
  right,
  strictness_own,
  strictness_neighbor,
  model_log_sqft,
  model_log_land_sqft,
  model_log_building_age,
  model_log_bedrooms,
  model_log_baths,
  model_has_garage,
  nearest_school_dist_ft,
  nearest_park_dist_ft,
  nearest_major_road_dist_ft,
  lake_michigan_dist_ft,
  source,
  n_historical_coordinates
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
    n_historical_recovered = model_sample[source == "historical_exact_year", .N],
    n_segments = uniqueN(model_sample$segment_id)
  )
}

historical_sales_rd_sensitivity <- rbindlist(list(
  estimate_sales_rd(current_model_data, "current_2025_parcel_universe"),
  estimate_sales_rd(
    rbindlist(list(current_model_data, recovered_model_data), use.names = TRUE),
    "historical_exact_sale_year_coordinates"
  ),
  estimate_sales_rd(
    rbindlist(list(
      current_model_data,
      recovered_model_data[n_historical_coordinates == 1L]
    ), use.names = TRUE),
    "historical_coordinates_stable_across_years_only"
  )
), use.names = TRUE)

fwrite(historical_sales_rd_sensitivity, "../output/historical_sales_rd_sensitivity.csv")
