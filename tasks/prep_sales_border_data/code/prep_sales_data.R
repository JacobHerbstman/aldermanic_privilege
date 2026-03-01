source("../../setup_environment/code/packages.R")

message("=== Prep Sales Border Data: Merge Hedonics via Rolling Join ===")

# ── Load data ──
sales <- fread("../input/sales_with_ward_distances.csv")
if (!"segment_id" %in% names(sales)) {
  stop("Input sales_with_ward_distances.csv is missing segment_id. Rebuild merge_event_study_scores after segment assignment.", call. = FALSE)
}
sales[, `:=`(
  pin = as.character(pin),
  sale_date = as.Date(sale_date),
  sale_year = year(sale_date)
)]

improvements <- read_parquet("../input/residential_improvements_panel.parquet")
setDT(improvements)
improvements[, pin := as.character(pin)]

message(sprintf("%s sales, %s improvement records (%s PINs)",
                format(nrow(sales), big.mark = ","),
                format(nrow(improvements), big.mark = ","),
                format(uniqueN(improvements$pin), big.mark = ",")))

# ── Rolling join: most recent assessment <= sale year ──
setkey(improvements, pin, tax_year)

sales_h <- improvements[
  sales,
  on = .(pin, tax_year = sale_year),
  roll = TRUE,
  rollends = c(TRUE, FALSE)
]
setnames(sales_h, "tax_year", "hedonic_tax_year")
sales_h[, sale_year := year(sale_date)]

n_matched <- sum(!is.na(sales_h$building_sqft))
message(sprintf("Match rate: %s / %s (%.1f%%)",
                format(n_matched, big.mark = ","),
                format(nrow(sales_h), big.mark = ","),
                100 * n_matched / nrow(sales_h)))

# ── Derived hedonic variables ──
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

# ── Time variables ──
sales_h[, `:=`(
  year = sale_year,
  year_quarter = paste0(sale_year, "-Q", quarter(sale_date)),
  year_month = format(sale_date, "%Y-%m")
)]

# ── Filter to 2006+ and save ──
sales_out <- sales_h[sale_year >= 2006]

message(sprintf("%s sales (2006-2025), %d ward pairs, median $%s",
                format(nrow(sales_out), big.mark = ","),
                uniqueN(sales_out$ward_pair_id),
                format(median(sales_out$sale_price, na.rm = TRUE), big.mark = ",")))

write_parquet(sales_out, "../output/sales_with_hedonics.parquet")
message(sprintf("Saved: ../output/sales_with_hedonics.parquet"))
