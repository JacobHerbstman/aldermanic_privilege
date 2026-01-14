### this script merges the historical attom data with the current 2023 version, 
# since the match rate is high and gives me latitude/longitude for 8.8 million out of 13.5 million observations

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")


## parcel data historical 
parcel_data_historical <- read_parquet("../input/chicago_attom_history.parquet") %>% 
  # slice_sample(n = 1000000, replace = FALSE) %>% ## sample for testing
  filter(sa_mail_city == "CHICAGO") %>% ## chicago only
  filter(!str_detect(sa_mail_street_name, "PO BOX")) %>% ## no PO boxes
  # filter(sa_nbr_bath >= .25) %>% 
  # filter(sa_nbr_bedrms >= .25) %>% 
  # filter(sa_lotsize > 0) %>% 
  # filter(sa_sqft > 0) %>% 
  select(attom_id)

gc()

parcel_data_current <- read_parquet("../input/chicago_attom_2023.parquet") %>% 
  select(attom_id, propertylongitude, propertylatitude)

hist_geo <- parcel_data_historical %>% 
  inner_join(parcel_data_current, by = "attom_id") %>% 
  st_as_sf(coords = c("propertylongitude", "propertylatitude"), crs = 4326)

missing_address <- parcel_data_historical %>% 
  anti_join(parcel_data_current, by = "attom_id") 

sfarrow::st_write_parquet(hist_geo, "../output/attom_historical_with_address.shp")
write_parquet(missing_address,  "../output/attom_historical_missing_address.parquet")


