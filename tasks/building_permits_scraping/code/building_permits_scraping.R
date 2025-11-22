### this code cleans the public building permits data from https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu/about_data

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

building_permits <- read_csv("../input/Building_Permits_20251121.csv")

## remove contact columns and clean up names
building_permits_clean <- building_permits %>% 
  janitor::clean_names() 


# Identify rows that are missing lat/lon but have x/y coordinates
needs_conversion_mask <- is.na(building_permits_clean$latitude) & !is.na(building_permits_clean$xcoordinate)
permits_to_convert <- building_permits_clean[needs_conversion_mask, ]

converted_sf <- st_as_sf(
  permits_to_convert,
  coords = c("xcoordinate", "ycoordinate"),
  crs = 3435, # Source CRS: NAD83 Illinois State Plane East (US Feet)
  remove = FALSE
) %>%
  st_transform(crs = 4326)

# Extract the new coordinates and update the original dataframe
new_coords <- st_coordinates(converted_sf)
building_permits_clean$longitude[needs_conversion_mask] <- new_coords[, "X"]
building_permits_clean$latitude[needs_conversion_mask] <- new_coords[, "Y"]

## remove rows with NA location 
building_permits_clean <- building_permits_clean %>% 
  dplyr::filter(!is.na(latitude)) %>% 
  dplyr::filter(!is.na(longitude))

## use zoo to convert dates to that format
building_permits_clean <- building_permits_clean %>% 
  dplyr::mutate(issue_date = as.Date(issue_date, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(application_start_date = as.Date(application_start_date, format = "%m/%d/%Y")) %>% 
  dplyr::mutate(issue_date_ym = zoo::as.yearmon(issue_date)) %>% 
  dplyr::mutate(application_start_date_ym = zoo::as.yearmon(application_start_date)) %>% 
  dplyr::mutate(permit_year = as.numeric(format(issue_date, "%Y"))) %>% 
  arrange(application_start_date) %>% 
  rename(pin = pin_list) %>% 
  # --- FIX: Remove rows with NA PINs to prevent bad clustering ---
  dplyr::filter(!is.na(pin)) %>%
  ## reorder dates to the front
  dplyr::select(id, pin, ward, application_start_date_ym, issue_date_ym, everything())



building_permits_filtered <- building_permits_clean %>%
  filter(permit_type %in% c("PERMIT - NEW CONSTRUCTION", 
                            "PERMIT - RENOVATION/ALTERATION")) %>%
  # Create a flag so you can analyze them separately later
  mutate(is_new_construction = if_else(permit_type == "PERMIT - NEW CONSTRUCTION", 1, 0)) %>% 
  filter(processing_time >= 0) %>% 
  select(-contains("contact_"))

rm(building_permits, converted_sf, permits_to_convert)


## text extraction
library(data.table)

# Convert to data.table for performance
setDT(building_permits_filtered)

## text extraction
# Using data.table update in place (:=) is much faster and memory efficient
building_permits_filtered[, desc_clean := str_to_upper(work_description)]

# --- FIX: Remove Permit Numbers to prevent false matches ---
building_permits_filtered[, desc_clean := str_replace_all(desc_clean, "#\\s*\\d+", "")]
building_permits_filtered[, desc_clean := str_replace_all(desc_clean, "PERMIT\\s*\\d+", "")]

# --- FIX: Remove specific phrases that cause false positive unit counts ---
building_permits_filtered[, desc_clean := str_replace_all(desc_clean, "UNIT MASONRY", "")]
building_permits_filtered[, desc_clean := str_replace_all(desc_clean, "HVAC UNITS", "")]
building_permits_filtered[, desc_clean := str_replace_all(desc_clean, "HEATING UNITS", "")]
building_permits_filtered[, desc_clean := str_replace_all(desc_clean, "KITCHEN UNITS", "")]
building_permits_filtered[, desc_clean := str_replace_all(desc_clean, "SECTION 504", "")]

# 2. Extract Unit Counts
building_permits_filtered[, units_mentioned_str := str_extract(desc_clean, "\\b(\\d+(?:,\\d+)*)\\s*(?:DWELLING|DU|UNITS|APTS|FLATS|FAM|RESIDENCES|CONDOMINIUMS|CONDOS)\\b")]
building_permits_filtered[, units_count_extracted := as.numeric(str_remove_all(str_extract(units_mentioned_str, "^[\\d,]+"), ","))]

# --- FIX: Sanity Check for "Comically Large" numbers ---
building_permits_filtered[units_count_extracted > 1000, units_count_extracted := NA]

# 3. Extract Stories/Height
building_permits_filtered[, stories_mentioned_str := str_extract(desc_clean, "\\b(\\d+)\\s*(?:STORY|STORIES|STY|FLR)\\b")]
building_permits_filtered[, stories_count_extracted := as.numeric(str_extract(stories_mentioned_str, "\\d+"))]

# 4. Extract Square Footage
building_permits_filtered[, sqft_mentioned_str := str_extract(desc_clean, "\\b(\\d+(?:,\\d+)*)\\s*(?:SQ\\.?\\s*FT\\.?|SQUARE\\s*FEET)\\b")]
building_permits_filtered[, sqft_extracted := as.numeric(str_remove_all(str_extract(sqft_mentioned_str, "^[\\d,]+"), ","))]

# 5. Extract Parking Spots
building_permits_filtered[, parking_mentioned_str := str_extract(desc_clean, "\\b(\\d+)\\s*(?:PARKING|SPACES|STALLS|CAR\\s*GARAGE)\\b")]
building_permits_filtered[, parking_extracted := as.numeric(str_extract(parking_mentioned_str, "\\d+"))]

# 6. Create Flags for Project Types
building_permits_filtered[, is_new_construction := if_else(permit_type == "PERMIT - NEW CONSTRUCTION", 1, 0)]
building_permits_filtered[, is_deconversion := str_detect(desc_clean, "DECONVERT|REDUCE|COMBINE|CONVERT TO SINGLE")]
building_permits_filtered[, is_adu := str_detect(desc_clean, "COACH HOUSE|\\bADU\\b|ACCESSORY DWELLING|REAR HOUSE|NEW BASEMENT UNIT|ESTABLISH DWELLING UNIT|NEW GARDEN UNIT")]
building_permits_filtered[, is_addition := str_detect(desc_clean, "ADDITION|EXPANSION|EXTEND")]
building_permits_filtered[, is_reduction := str_detect(desc_clean, "REDUCE|DECREASE|FROM \\d+ TO \\d+")]

# NEW: Detect single-family homes (various phrasings)
building_permits_filtered[, is_single_family := str_detect(desc_clean, "SINGLE FAMILY|SINGLE-FAMILY|SFR|1-FAMILY|ONE FAMILY|ONE-FAMILY|DETACHED RESIDENCE")]

# 7. Calculate Net Units Added
# IMPORTANT: net_units is ONLY for renovation-type changes (ADUs, deconversions)
# For new construction, we use chronological extraction (units_final_new_construction) to avoid double-counting
building_permits_filtered[, net_units := fcase(
  # New construction: always 0 here (we use chronological aggregation instead)
  is_new_construction == 1, 0,
  
  # If it's a deconversion, assume -1 unit (conservative estimate)
  is_deconversion == TRUE, -1,
  
  # If it's an ADU, add 1 unit
  is_adu == TRUE, 1,
  
  # Standard renovations usually don't change unit counts unless specified
  default = 0
)]

# Calculate other fields
building_permits_filtered[, permit_year := year(issue_date)]
building_permits_filtered[, proc_time_days := as.numeric(issue_date - application_start_date)]
building_permits_filtered[, total_fee_clean := as.numeric(str_remove_all(as.character(total_fee), "[\\$,]"))]
building_permits_filtered[, desc_len := str_length(work_description)]

# --- DEDUPLICATION / CLUSTERING STRATEGY ---
# Sort by PIN and Date
setorder(building_permits_filtered, pin, issue_date)

# Calculate clusters
# data.table shift() is equivalent to lag()
building_permits_filtered[, days_since_prev := as.numeric(issue_date - shift(issue_date, fill = first(issue_date))), by = pin]
building_permits_filtered[, new_cluster_flag := if_else(days_since_prev > 730, 1, 0)]
building_permits_filtered[, cluster_id := cumsum(new_cluster_flag), by = pin]
building_permits_filtered[, project_id := paste0(pin, "_", cluster_id)]

# -------------------------------------------------------------------------
# 4. Aggregation by Project (Cluster) - OPTIMIZED
# -------------------------------------------------------------------------

# Sort by project_id and issue_date descending (for chronological priority)
setorder(building_permits_filtered, project_id, -issue_date)

# Helper function to get first non-NA
first_non_na <- function(x) {
  val <- x[!is.na(x)][1]
  if (is.na(val)) return(0) else return(val)
}

# Aggregation
project_metrics <- building_permits_filtered[, .(
  # Project Metadata
  pin = first(pin),
  ward = first(ward),
  
  # Date Ranges
  application_start_date = min(application_start_date, na.rm = TRUE),
  issue_date = max(issue_date, na.rm = TRUE),
  
  # Project Type Flags
  is_new_construction_project = max(is_new_construction, na.rm = TRUE),
  is_deconversion_project = max(is_deconversion, na.rm = TRUE),
  is_adu_project = max(is_adu, na.rm = TRUE),
  has_reduction = max(is_reduction, na.rm = TRUE),
  is_single_family_project = max(is_single_family, na.rm = TRUE),  # Track if any permit mentioned single-family
  
  # Unit Counts - CHRONOLOGICAL LOGIC
  # Since we ordered by -issue_date, the first element is the latest
  units_final_new_construction = first_non_na(units_count_extracted),
  units_net_renovation = sum(net_units, na.rm = TRUE),  # Only renovation changes (ADUs, deconversions)
  
  # NEW: Count single-family permits (for projects without explicit unit counts)
  single_family_permit_count = sum(is_single_family, na.rm = TRUE),
  
  # Stories & SqFt
  stories_final = max(stories_count_extracted, na.rm = TRUE),
  sqft_final = max(sqft_extracted, na.rm = TRUE),
  parking_final = max(parking_extracted, na.rm = TRUE),
  
  # Processing Time
  avg_processing_time = mean(proc_time_days, na.rm = TRUE),
  max_processing_time = max(proc_time_days, na.rm = TRUE),
  
  # Description
  project_description = paste(unique(desc_clean), collapse = " | "),
  
  # Fees
  permit_count = .N,
  total_fees = sum(total_fee_clean, na.rm = TRUE)
), by = project_id]

# Final Calculations and Cleanup
# For new construction:
#   - If we have explicit units, use those (chronological)
#   - If no explicit units but single-family detected, assign 1 unit (not permit count - single-family is 1 unit)
#   - Otherwise 0
# For renovations: use net_units sum
project_metrics[, units_added := fcase(
  is_new_construction_project == 1 & units_final_new_construction > 0, units_final_new_construction,
  is_new_construction_project == 1 & units_final_new_construction == 0 & is_single_family_project == 1, 1,
  is_new_construction_project == 1, 0,
  default = units_net_renovation
)]

# Clean up Inf values
cols_to_clean <- c("stories_final", "sqft_final", "parking_final", "units_added", "max_processing_time")
for (col in cols_to_clean) {
  set(project_metrics, i = which(is.infinite(project_metrics[[col]])), j = col, value = NA)
}

# 3. EXTRACT REPRESENTATIVE ROWS
# Primary IDs (Highest Fee)
setorder(building_permits_filtered, project_id, -total_fee_clean)
primary_ids <- building_permits_filtered[, .SD[1], by = project_id, .SDcols = c("ward", "latitude", "longitude")]

# Best Descriptions (Longest)
setorder(building_permits_filtered, project_id, -desc_len)
best_descriptions <- building_permits_filtered[, .SD[1], by = project_id, .SDcols = c("work_description")]
setnames(best_descriptions, "work_description", "project_description_orig")

# 4. JOIN EVERYTHING TOGETHER
# data.table merge is fast
projects_clean <- merge(project_metrics, primary_ids, by = "project_id", all.x = TRUE)
# We already have aggregated description, but let's keep the "best" original one too if needed, 
# or just rely on the aggregated one. The original script joined `best_descriptions`.
# Let's join it to be consistent.
projects_clean <- merge(projects_clean, best_descriptions, by = "project_id", all.x = TRUE)

# Filter
projects_clean <- projects_clean[
  (is_new_construction_project == 1) |
    (units_added != 0 & !is.na(units_added))
]

print("Project Summary:")
print(projects_clean[, .(
  count = .N,
  total_units_created = sum(units_added, na.rm = TRUE),
  avg_stories = mean(stories_final, na.rm = TRUE)
), by = is_new_construction_project])

## convert to sf for writing
building_permits_sf <- st_as_sf(
  as.data.frame(projects_clean), # st_as_sf needs data.frame usually
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
) %>% 
  select(-ward.y) %>% 
  rename(ward = ward.x)

## write clean data
st_write(
  building_permits_sf,
  "../output/building_permits_clean.gpkg",
  delete_layer = TRUE
)


