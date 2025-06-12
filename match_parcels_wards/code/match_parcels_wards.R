source("/Users/jacobherbstman/Desktop/aldermanic_privilege/source_script.R")

parcel_data_current <- read_parquet(paste0(root, "match_parcels_wards/input/chicago_attom_2023.parquet")) %>% 
  select(attom_id, propertylatitude, propertylongitude)

parcel_data_historical <- read_parquet(paste0(root, "match_parcels_wards/input/chicago_attom_history.parquet")) %>% 
  filter(sa_mail_city == "CHICAGO") %>% 
  filter(!str_detect(sa_mail_street_name, "PO BOX")) %>% 
  slice_sample(n = 10000, replace = FALSE) 

?slice_sample

ward_panel <- st_read(paste0(root, "match_parcels_wards/input/census_tract_panel_wards.shp")) %>%
  janitor::clean_names() %>%
  dplyr::select(year, ward, geometry)

# parcel_data_historical <- read_parquet(paste0(root, "match_parcels_wards/input/chicago_attom_history.parquet")) %>% 
#   filter(sa_mail_street_name == "Dorchester") 
#   filter(sa_mail_house_nbr == "5227")
  


