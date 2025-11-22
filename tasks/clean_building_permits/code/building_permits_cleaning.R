### this code cleans the public building permits data from https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu/about_data

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

building_permits <- read_csv("../input/Building_Permits_20251121.csv")

## remove contact columns and clean up names
building_permits <- building_permits %>% 
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
  arrange(application_start_date) %>% 
  rename(pin = pin_list) %>% 
  ## reorder dates to the front
  dplyr::select(id, pin, ward, application_start_date_ym, issue_date_ym, everything())



### keep certain permit types
high_discretion_permits <- c(
  "PERMIT - NEW CONSTRUCTION",
  "PERMIT - RENOVATION/ALTERATION",
  "PERMIT - WRECKING/DEMOLITION",
  "PERMIT - PORCH CONSTRUCTION",
  "PERMIT - REINSTATE REVOKED PMT"
)

building_permits_clean2 <- building_permits_clean %>%
  mutate(high_discretion = ifelse(permit_type %in% high_discretion_permits, 1, 0))


## remove negative processing times and NA processing times
building_permits_clean2 <- building_permits_clean2 %>% 
  dplyr::filter(processing_time >= 0) 

building_permits_final <- building_permits_clean2 %>%
  mutate(
    ## 1. Create the binary permit success variable
    permit_issued = case_when(
      permit_status %in% c("COMPLETE", "ACTIVE", "PHASED PERMITTING") ~ 1,
      permit_status %in% c("EXPIRED", "CANCELLED", "REVOKED", "SUSPENDED") ~ 0,
      TRUE ~ NA_integer_
    ),
    
    ## 2. Create a more robust corporate applicant dummy
    # This checks if ANY contact name column contains a corporate keyword.
    # coalesce(.x, "") handles potential NAs in contact name fields.
    corporate_applicant = as.integer(
      if_any(
        starts_with("contact_") & ends_with("_name"),
        ~ str_detect(
          str_to_upper(coalesce(.x, "")), 
          "IN|SERVICE|CO|LLC|INC|CORP|LTD|LLP|PC|ASSOCIATE|GROUP|COMPANY|CONSTRUCTION|DEVELOPMENT|PROPERTY|PROPERTIES"
        )
      )
    )
  ) %>% 
  # Clean up by selecting key columns and dropping the numerous contact fields
  dplyr::select(
    id, pin, ward, application_start_date_ym, issue_date_ym,
    processing_time, reported_cost, total_fee,
    high_discretion, permit_issued, corporate_applicant,
    everything(), -contains("contact_")
  )


# ## keep just 2010-2019 permits for current census block analysis 
# building_permits_final <- building_permits_final %>% 
#   dplyr::filter(application_start_date_ym >= as.yearmon("Jan 2010") & application_start_date_ym <= as.yearmon("Dec 2019"))

## convert to sf for writing
building_permits_sf <- st_as_sf(
  building_permits_final,
  coords = c("longitude", "latitude"),   # adjust to your column names
  crs    = 4326,              # or whatever CRS your coords use
  remove = FALSE              # keep the lon/lat columns if you still want them
) %>% 
  mutate(across(c(application_start_date_ym, issue_date_ym), 
        function(x) as.Date(x)))
        
## write clean data
st_write(
  building_permits_sf,
  "../output/building_permits_clean.gpkg",
  delete_layer = TRUE         # overwrite any existing files cleanly
)

######################
## basic summary stats
######################

permits <- st_read("../output/building_permits_clean.gpkg")


# 
# summary_stats <- building_permits_clean2 %>% 
#   group_by(ward) %>% 
#   summarise(zone_fees = mean(zoning_fee_paid, na.rm = T),
#             building_fees = mean(building_fee_paid, na.rm = T),
#             processing_time = mean(processing_time, na.rm = T), 
#             total_fees = mean(total_fee, na.rm = T), 
#             num_permits = n()) 
# 
# ##proportion of pins missing (54 percent)
# building_permits_clean2 %>% 
#   dplyr::mutate(pin_na = is.na(pin)) %>% 
#   dplyr::summarize(prop_na = mean(pin_na))
# 
# ## amount of na pins by permit type (mostly non-construction stuff)
# building_permits_clean2 %>% 
#   dplyr::mutate(pin_na = is.na(pin)) %>% 
#   dplyr::group_by(permit_type) %>% 
#   dplyr::summarize(prop_na = mean(pin_na)) %>% 
#   dplyr::arrange(prop_na)
# 
# 
# ## unique permit types
# building_permits_clean2 %>% 
#   dplyr::select(permit_type) %>% 
#   dplyr::distinct() %>% 
#   dplyr::arrange(permit_type)
# 
# 
# 
# 
# 
# 
# ##find proportion with non-NA latitude
# building_permits_clean2 %>% 
#   dplyr::mutate(lat_na = is.na(latitude)) %>% 
#   dplyr::summarize(prop_na = mean(lat_na))
# 
# 
# 
