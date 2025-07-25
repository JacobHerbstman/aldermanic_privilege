### this code cleans the public building permits data from https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu/about_data

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

building_permits <- read_csv("../input/Building_Permits_20250109.csv")

## remove contact columns and clean up names
building_permits_clean <- building_permits %>% 
  janitor::clean_names() %>% 
  dplyr::select(-contains("contact")) 

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

## remove negative processing times and NA processing times
building_permits_clean2 <- building_permits_clean %>% 
  dplyr::filter(processing_time >= 0) 


## remove rows with NA location 
building_permits_clean2 <- building_permits_clean2 %>% 
  dplyr::filter(!is.na(latitude)) %>% 
  dplyr::filter(!is.na(longitude)) 

##keep permit types renovation, easy permit process, elevator, wrecking, new construction

## keep housing related stuff and then keep those with pins I can merge to ptax data
building_permits_clean2 <- building_permits_clean2 %>% 
  dplyr::filter(permit_type %in% c("PERMIT - RENOVATION/ALTERATION", "PERMIT - EASY PERMIT PROCESS", "PERMIT - ELEVATOR EQUIPMENT", 
                                   "PERMIT - WRECKING/DEMOLITION ", "PERMIT - NEW CONSTRUCTION")) %>% 
  filter(!is.na(pin))

## keep just 2010-2019 permits for current census block analysis 
building_permits_clean2 <- building_permits_clean2 %>% 
  dplyr::filter(application_start_date_ym >= as.yearmon("Jan 2010") & application_start_date_ym <= as.yearmon("Dec 2019")) 

## convert to sf for writing
building_permits_sf <- st_as_sf(
  building_permits_clean2,
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
