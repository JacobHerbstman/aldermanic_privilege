### this script preps the assessor history dataset for geocoding by the Uchicago RCC-GIS Geocoding Service

source("/Users/jacobherbstman/Desktop/aldermanic_privilege/source_script.R")

## parcel data historical 
parcel_data_historical <- read_parquet(paste0(root, "geocode_assessor_history/input/chicago_attom_history.parquet")) %>% 
  slice_sample(n = 50000, replace = FALSE) %>% ## sample for testing
  filter(sa_mail_city == "CHICAGO") %>% ## chicago only
  filter(!str_detect(sa_mail_street_name, "PO BOX")) %>% ## no PO boxes
  # filter(sa_nbr_bath >= .25) %>% 
  # filter(sa_nbr_bedrms >= .25) %>% 
  filter(sa_lotsize > 0) %>% 
  filter(sa_sqft > 0)

address_cols <- c("sa_mail_house_nbr", "sa_mail_dir",   "sa_mail_street_name",
                  "sa_mail_suf",        "sa_mail_post_dir", "sa_mail_city",
                  "sa_mail_state",      "sa_mail_zip",      "sa_mail_plus_4")

## create address name
parcel_data_historical_addresses <- parcel_data_historical %>% 
  unite(
    col    = "street_line",
    c(
      sa_mail_house_nbr,
      sa_mail_dir,
      sa_mail_street_name,
      sa_mail_suf,
      sa_mail_post_dir
    ),
    sep    = " ",
    na.rm  = TRUE,   # drop any NA pieces
    remove = FALSE   # keep originals around
  ) %>% 
  mutate(
    across(c(sa_mail_zip, sa_mail_plus_4), as.character),
    
    zip4 = case_when(
      !is.na(sa_mail_plus_4) & sa_mail_plus_4 != "" & sa_mail_plus_4 != "0" ~ 
        str_c(sa_mail_zip, "-", sa_mail_plus_4),
      TRUE ~ sa_mail_zip
    )
  ) %>% 
  mutate(
    address = str_c(
      street_line,   ", ",
      sa_mail_city,  ", ",
      sa_mail_state, ", ",
      zip4
    )
  )

parcel_data_historical_addresses <- parcel_data_historical_addresses %>% 
  select(attom_id, address)






