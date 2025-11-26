## this code cleans the parcel sales data from the cook county assessor

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# 1. Load Data
# -----------------------------------------------------------------------------
message("Loading data...")

# Sales Data
sales_raw <- fread("../input/Assessor_-_Parcel_Sales_20251123.csv")
message(sprintf("Loaded %s sales records", format(nrow(sales_raw), big.mark = ",")))

# Parcel Universe (for geolocation)
# Only need PIN, PIN10, and coordinates
parcels <- fread("../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv",
  select = c("pin", "pin10", "latitude", "longitude")
)
message(sprintf("Loaded %s parcel records", format(nrow(parcels), big.mark = ",")))

# Ward Panel (historical boundaries)
ward_panel <- st_read("../input/ward_panel.gpkg", quiet = TRUE)
message(sprintf("Loaded ward panel with %d features", nrow(ward_panel)))

# 2. Clean Sales Data
# -----------------------------------------------------------------------------
message("Cleaning sales data...")

sales <- sales_raw %>%
  # Filter for Single Family Homes (Class 203, 204)
  filter(class %in% c(203, 204, 299)) %>%
  # Clean Price
  mutate(
    sale_price = as.numeric(gsub("[$,]", "", sale_price)),
    year = as.numeric(year),
    pin = as.character(pin) # Ensure PIN matches parcel file type
  ) %>%
  # Filter valid prices and years
  filter(!is.na(sale_price), sale_price > 10000, !is.na(year)) %>%
  filter(sale_deed_type %in% c("Warranty", "Trustee")) %>%
  filter(sale_type != "LAND") %>%
  filter(!sale_seller_name %in% c("", "-", "UNKNOWN", "..")) %>%
  filter(sale_seller_name != sale_buyer_name) %>%
  filter(num_parcels_sale == 1)


message(sprintf("Filtered to %s single-family sales", format(nrow(sales), big.mark = ",")))

# 3. Geolocate Sales
# -----------------------------------------------------------------------------
message("Geolocating sales...")

# Ensure parcel PINs are character
parcels[, `:=`(pin = as.character(pin), pin10 = as.character(pin10))]

# Join on full PIN first
sales_geo <- sales %>%
  left_join(parcels, by = "pin")

# Identify missing coordinates
missing_coords <- sum(is.na(sales_geo$latitude))
message(sprintf("Initial match missing coordinates: %s", format(missing_coords, big.mark = ",")))

# If missing, try matching on PIN10 (first 10 digits of sales PIN)
if (missing_coords > 0) {
  # Create PIN10 for sales
  sales_geo <- sales_geo %>%
    mutate(pin10_sales = substr(pin, 1, 10))

  # Separate missing and found
  sales_found <- sales_geo %>% filter(!is.na(latitude))
  sales_missing <- sales_geo %>%
    filter(is.na(latitude)) %>%
    select(-latitude, -longitude, -pin10)

  # Join missing on PIN10
  # Note: Parcels might have multiple PINs for same PIN10, take first match
  parcels_pin10 <- parcels %>%
    select(pin10, latitude, longitude) %>%
    distinct(pin10, .keep_all = TRUE)

  sales_missing_fixed <- sales_missing %>%
    left_join(parcels_pin10, by = c("pin10_sales" = "pin10"))

  # Recombine
  sales_geo <- bind_rows(sales_found, sales_missing_fixed)
}

# Filter out any remaining missing coordinates
sales_final_geo <- sales_geo %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

message(sprintf("Final geolocated sales: %s", format(nrow(sales_final_geo), big.mark = ",")))

# 4. Spatial Join with Wards (Year-Specific)
# -----------------------------------------------------------------------------
message("Spatially joining with historical ward boundaries...")

# Get unique years in sales data
sales_years <- sort(unique(sales_final_geo$year))
results_list <- list()

# Ensure ward panel has year column
if (!"year" %in% names(ward_panel)) {
  stop("Ward panel missing 'year' column")
}

for (yr in sales_years) {
  # Filter sales for this year
  sales_yr <- sales_final_geo %>% filter(year == yr)

  # Filter ward boundaries for this year
  wards_yr <- ward_panel %>% filter(year == yr)

  if (nrow(sales_yr) > 0 && nrow(wards_yr) > 0) {
    # Ensure CRS match
    if (st_crs(sales_yr) != st_crs(wards_yr)) {
      sales_yr <- st_transform(sales_yr, st_crs(wards_yr))
    }

    # Spatial join
    joined <- st_join(sales_yr, wards_yr, join = st_within)

    # Store result (keep only necessary columns)
    results_list[[as.character(yr)]] <- joined %>%
      st_drop_geometry() %>%
      select(pin, year = year.x, sale_price, ward, class) %>%
      filter(!is.na(ward))
  }
}

sales_with_wards <- bind_rows(results_list)
message(sprintf("Sales matched to wards: %s", format(nrow(sales_with_wards), big.mark = ",")))

# 5. Aggregate with Conditional Logic
# -----------------------------------------------------------------------------
message("Aggregating by Ward and Year (Conditional Logic)...")

# Calculate Total SFH Sales (Class 203, 204) per Ward
ward_sfh_counts <- sales_with_wards %>%
  filter(class %in% c(203, 204)) %>%
  group_by(ward) %>%
  summarize(total_sfh_sales = n())

# Identify "Low SFH" Wards (< 20 total SFH sales)
low_sfh_wards <- ward_sfh_counts %>%
  filter(total_sfh_sales < 30) %>%
  pull(ward)

message(sprintf("Wards with < 20 SFH sales (using Condos): %s", paste(low_sfh_wards, collapse = ", ")))

# Filter Data for Aggregation
sales_for_agg <- sales_with_wards %>%
  mutate(
    is_low_sfh_ward = ward %in% low_sfh_wards,
    is_sfh = class %in% c(203, 204),
    is_condo = class == 299
  ) %>%
  filter(
    (is_low_sfh_ward & is_condo) | # Keep Condos for Low SFH Wards
      (!is_low_sfh_ward & is_sfh) # Keep SFH for Normal Wards
  )

ward_year_summary <- sales_for_agg %>%
  group_by(ward, year) %>%
  summarize(
    median_sale_price = median(sale_price, na.rm = TRUE),
    mean_sale_price = mean(sale_price, na.rm = TRUE),
    n_sales = n(),
    type_used = ifelse(first(is_low_sfh_ward), "Condo", "SFH"),
    .groups = "drop"
  ) %>%
  arrange(ward, year)

# 6. Export
# -----------------------------------------------------------------------------
output_path <- "../output/ward_year_sales.csv"
write_csv(ward_year_summary, output_path)
message(sprintf("Saved summary to %s", output_path))

# Preview
print(head(ward_year_summary))
