# setwd("tasks/prep_sales_border_data/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

sales <- fread(
  "../input/sales_with_ward_distances.csv",
  colClasses = list(character = "pin")
)
if (!"segment_id" %in% names(sales)) {
  stop("Input sales_with_ward_distances.csv is missing segment_id. Rebuild merge_sales_scores after segment assignment.", call. = FALSE)
}
sales[, `:=`(
  pin = gsub("[^0-9]", "", trimws(pin)),
  sale_date = as.Date(sale_date),
  sale_year = year(sale_date)
)]
sales[nchar(pin) == 13L, pin := paste0("0", pin)]
if (any(nchar(sales$pin) != 14L)) {
  stop("Sales input contains an invalid full PIN.", call. = FALSE)
}

improvements <- read_parquet("../input/residential_improvements_panel.parquet")
setDT(improvements)
improvements[, pin := as.character(pin)]
improvements[, hedonic_tax_year := tax_year]
if (anyDuplicated(improvements[, .(pin, tax_year)]) > 0) {
  stop("Residential improvements must be unique by PIN-tax year.", call. = FALSE)
}

setkey(improvements, pin, tax_year)

sales_h <- improvements[
  sales,
  on = .(pin, tax_year = sale_year),
  roll = FALSE
]
sales_h[, sale_year := year(sale_date)]

if (any(
  !is.na(sales_h$hedonic_tax_year) &
    sales_h$hedonic_tax_year != sales_h$sale_year
)) {
  stop("Assessor characteristics must match the sale year exactly.", call. = FALSE)
}

sales_h[, `:=`(
  building_age = sale_year - year_built,
  baths_total = num_full_baths + 0.5 * fifelse(is.na(num_half_baths), 0, num_half_baths),
  has_garage = as.integer(garage_size > 0 & !is.na(garage_size)),
  improvement_class_mismatch = !is.na(improvement_class) & class != improvement_class,
  baseline_sale_eligible =
    !is.na(hedonic_tax_year) &
    num_buildings == 1 &
    !is_multibuilding &
    is.finite(tieback_proration_rate) &
    abs(tieback_proration_rate - 1) < 0.000001 &
    is.finite(building_sqft) & building_sqft > 0
)]
sales_h[building_age < 0, building_age := NA]

sales_h[, `:=`(
  log_sqft = fifelse(!is.na(building_sqft) & building_sqft > 0, log(building_sqft), NA_real_),
  log_land_sqft = fifelse(!is.na(land_sqft) & land_sqft > 0, log(land_sqft), NA_real_),
  log_building_age = fifelse(!is.na(building_age) & building_age > 0, log(building_age), NA_real_),
  log_bedrooms = fifelse(!is.na(num_bedrooms) & num_bedrooms > 0, log(num_bedrooms), NA_real_),
  log_baths = fifelse(!is.na(baths_total) & baths_total > 0, log(baths_total), NA_real_)
)]

sales_h[, `:=`(
  year = sale_year,
  year_quarter = paste0(sale_year, "-Q", quarter(sale_date)),
  year_month = format(sale_date, "%Y-%m")
)]

sales_out <- sales_h[sale_year >= 2006 & baseline_sale_eligible]
property_restricted <- nrow(sales_out)

# Property records with more bedrooms than rooms are recording errors. Missing room or bedroom counts remain eligible.
sales_out <- sales_out[is.na(num_rooms) | is.na(num_bedrooms) | num_bedrooms <= num_rooms]
consistent_rooms <- nrow(sales_out)

# As in school_closures_house_prices (tasks/clean_home_sales): more than $5,000 per building square foot is a
# recording error (an 893 sq ft house sold for $134.9 million), and sales outside the within-year citywide 1st-99th
# percentiles of nominal price are flagged, not trimmed.
sales_out[, price_per_building_sqft := sale_price_nominal / building_sqft]
sales_out <- sales_out[price_per_building_sqft <= 5000]
sales_out[, price_outside_p01_p99 := sale_price_nominal < quantile(sale_price_nominal, 0.01, type = 7) |
  sale_price_nominal > quantile(sale_price_nominal, 0.99, type = 7), by = sale_year]

if (anyDuplicated(sales_out$row_id) > 0) {
  stop("Final residential sales data must be unique by source row_id.", call. = FALSE)
}
if (any(
  is.na(sales_out$num_buildings) |
    sales_out$num_buildings != 1 |
    sales_out$is_multibuilding |
    !is.finite(sales_out$tieback_proration_rate) |
    abs(sales_out$tieback_proration_rate - 1) >= 0.000001 |
    !is.finite(sales_out$building_sqft) |
    sales_out$building_sqft <= 0
)) {
  stop("Final residential sales data violate the structural eligibility rules.", call. = FALSE)
}

# Sample flow quoted in the paper's data appendix.
sample_flow <- data.table(
  step = c("located_sales", "one_building_whole_property", "more_bedrooms_than_rooms", "above_5000_per_sqft",
    "sales_with_hedonics"),
  count = c(nrow(sales), property_restricted, property_restricted - consistent_rooms,
    consistent_rooms - nrow(sales_out), nrow(sales_out))
)

SaveData(sales_out, c("row_id"), "../output/sales_with_hedonics.parquet")
SaveData(sample_flow, "step", "../output/sales_hedonics_sample_flow.csv")
