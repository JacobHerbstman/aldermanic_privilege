### this script preps the assessor history dataset for geocoding by the Uchicago RCC-GIS Geocoding Service

## run this line when editing code in Rstudio
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/geocode_assessor_history/code")
# source(here::here("setup_environment", "code", "packages.R"))
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



# 
# address_cols <- c("sa_mail_house_nbr", "sa_mail_dir",   "sa_mail_street_name",
#                   "sa_mail_suf",        "sa_mail_post_dir", "sa_mail_city",
#                   "sa_mail_state",      "sa_mail_zip",      "sa_mail_plus_4")
# 
# ## create address name
# parcel_data_historical_addresses <- parcel_data_historical %>% 
#   unite(
#     col    = "street_line",
#     c(
#       sa_mail_house_nbr,
#       sa_mail_dir,
#       sa_mail_street_name,
#       sa_mail_suf,
#       sa_mail_post_dir
#     ),
#     sep    = " ",
#     na.rm  = TRUE,   # drop any NA pieces
#     remove = FALSE   # keep originals around
#   ) %>% 
#   mutate(
#     across(c(sa_mail_zip, sa_mail_plus_4), as.character),
#     
#     zip4 = case_when(
#       !is.na(sa_mail_plus_4) & sa_mail_plus_4 != "" & sa_mail_plus_4 != "0" ~ 
#         str_c(sa_mail_zip, "-", sa_mail_plus_4),
#       TRUE ~ sa_mail_zip
#     )
#   ) %>% 
#   mutate(
#     address = str_c(
#       street_line,   ", ",
#       sa_mail_city,  ", ",
#       sa_mail_state, ", ",
#       zip4
#     )
#   )
# 
# parcel_data_historical_addresses <- parcel_data_historical_addresses %>% 
#   select(attom_id, address)
# 
# 
# write_csv(
#   parcel_data_historical_addresses,
#   "../output/chi_addresses_for_geocoding.csv"
# )
# 
# 
# 
# 
# 
# 
