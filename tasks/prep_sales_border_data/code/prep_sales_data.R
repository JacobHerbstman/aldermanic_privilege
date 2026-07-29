# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/prep_sales_border_data/code")

source("../../setup_environment/code/packages.R")

sales <- fread(
  "../input/sales_with_ward_distances.csv",
  colClasses = list(character = "pin")
)
if (!"segment_id" %in% names(sales)) {
  stop("Input sales_with_ward_distances.csv is missing segment_id. Rebuild merge_event_study_scores after segment assignment.", call. = FALSE)
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
  has_garage = as.integer(garage_size > 0 & !is.na(garage_size))
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

sales_out <- sales_h[sale_year >= 2006]

write_parquet(sales_out, "../output/sales_with_hedonics.parquet")
