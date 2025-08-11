## this code takes chicago ward shapefiles merges them for a panel from 2003-2023

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

# -----------------------------------------------------------------------------
# 1. CREATE ANNUAL WARD PANEL
# -----------------------------------------------------------------------------

## bring in old ward boundary data
ward_bound2005 <- st_read("../input/Wards_2014.geojson") 

## make annual panel for the pre-2015 ward map
ward_bound2005 <- ward_bound2005 %>%
  filter(ward != "OUT") %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(2005:2014)) %>% # Switched to annual sequence
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435) ## change to chicago coordinate system


## bring in new ward boundary data
ward_bound2015 <-st_read("../input/Wards_2015.geojson") 

## make annual panel for the post-2015 ward map
ward_bound2015 <- ward_bound2015 %>%
  filter(ward != "OUT") %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(2015:2023)) %>% # Switched to annual sequence
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435) ## change to chicago coordinate system


## join to one large annual panel
ward_panel_annual <- rbind(ward_bound2005, ward_bound2015) %>%
  mutate(ward = as.numeric(ward)) %>% 
  arrange(ward, year)

# Save the annual ward panel
st_write(ward_panel_annual, "../output/ward_panel.gpkg", delete_layer = TRUE)
