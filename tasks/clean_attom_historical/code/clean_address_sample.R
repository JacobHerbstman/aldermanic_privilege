#This script cleans the processed attom historical files from link_assessor_history

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

data <- st_read_parquet("../input/attom_historical_with_address.shp")
  
# Much faster - just takes the first row per group
unique_spatial_data <- data %>%
  group_by(attom_id) %>%
  slice(1) %>%
  ungroup()

## historical parcel data 
parcel_data_historical <- read_parquet("../input/chicago_attom_history.parquet") %>% 
  filter(sa_yr_blt >= 2006) %>%
  filter(sa_mail_city == "CHICAGO")  ## chicago only 
  # slice_sample(n = 100000, replace = FALSE) 



## merge geometry and parcel info
merged_parcels <- unique_spatial_data %>% 
  inner_join(parcel_data_historical, by = "attom_id")

gc()


# Enhanced cleaning with field-specific logic
merged_parcels_clean <- merged_parcels %>%
  group_by(attom_id) %>%
  summarise(
    
    # Year built: majority vote
    sa_yr_blt = {
      year_counts <- table(sa_yr_blt[!is.na(sa_yr_blt)])
      if(length(year_counts) > 0) {
        as.numeric(names(year_counts)[which.max(year_counts)])
      } else {
        NA_real_
      }
    },
    
    # Address number: majority vote
    sa_mail_house_nbr = {
      house_counts <- table(sa_mail_house_nbr[!is.na(sa_mail_house_nbr) & sa_mail_house_nbr != ""])
      if(length(house_counts) > 0) {
        names(house_counts)[which.max(house_counts)]
      } else {
        NA_character_
      }
    },
    
    sa_mail_street_name = {
      street_counts <- table(sa_mail_street_name[!is.na(sa_mail_street_name) & sa_mail_street_name != ""])
      if(length(street_counts) > 0) {
        names(street_counts)[which.max(street_counts)]
      } else {
        NA_character_
      }
    },
    
    sa_mail_suf = {
      suf_counts <- table(sa_mail_suf[!is.na(sa_mail_suf) & sa_mail_suf != ""])
      if(length(suf_counts) > 0) {
        names(suf_counts)[which.max(suf_counts)]
      } else {
        NA_character_
      }
    },
    
    # Bedrooms: most common non-zero, with reasonableness check
    sa_nbr_bedrms = {
      non_zero_beds <- sa_nbr_bedrms[sa_nbr_bedrms > 0 & sa_nbr_bedrms <= 20 & !is.na(sa_nbr_bedrms)]
      if(length(non_zero_beds) > 0) {
        bed_counts <- table(non_zero_beds)
        as.numeric(names(bed_counts)[which.max(bed_counts)])
      } else {
        NA_real_
      }
    },
    
    # Bathrooms: most common non-zero, with reasonableness check
    sa_nbr_bath = {
      non_zero_baths <- sa_nbr_bath[sa_nbr_bath > 0 & sa_nbr_bath <= 10 & !is.na(sa_nbr_bath)]
      if(length(non_zero_baths) > 0) {
        bath_counts <- table(non_zero_baths)
        as.numeric(names(bath_counts)[which.max(bath_counts)])
      } else {
        NA_real_
      }
    },
    
    # Units: most common non-zero, with reasonableness check
    sa_nbr_units = {
      non_zero_units <- sa_nbr_units[sa_nbr_units > 0 & sa_nbr_units <= 1000 & !is.na(sa_nbr_units)]
      if(length(non_zero_units) > 0) {
        unit_counts <- table(non_zero_units)
        as.numeric(names(unit_counts)[which.max(unit_counts)])
      } else {
        NA_real_
      }
    },
    
    # Stories: most common non-zero, with reasonableness check
    sa_nbr_stories = {
      non_zero_stories <- sa_nbr_stories[sa_nbr_stories > 0 & sa_nbr_stories <= 50 & !is.na(sa_nbr_stories)]
      if(length(non_zero_stories) > 0) {
        story_counts <- table(non_zero_stories)
        as.numeric(names(story_counts)[which.max(story_counts)])
      } else {
        NA_real_
      }
    },
    
    # Square footage: maximum reasonable value from multiple sqft fields
    sa_sqft = {
      reasonable_sqft <- sa_sqft[sa_sqft > 0 & sa_sqft < 50000 & !is.na(sa_sqft)]
      if(length(reasonable_sqft) > 0) {
        max(reasonable_sqft)
      } else {
        NA_real_
      }
    },
    
    # Lot size: keep as is since you mentioned it's fine - take maximum
    sa_lotsize = {
      reasonable_lotsize <- sa_lotsize[sa_lotsize > 0 & !is.na(sa_lotsize)]
      if(length(reasonable_lotsize) > 0) {
        max(reasonable_lotsize)
      } else {
        NA_real_
      }
    },
    
    # Count observations for quality check
    n_observations = n(),
    .groups = 'drop'
  )

st_write(merged_parcels_clean, "../output/year_built_sample.gpkg", delete_dsn = TRUE)

