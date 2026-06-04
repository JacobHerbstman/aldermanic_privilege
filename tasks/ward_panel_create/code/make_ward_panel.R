# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/ward_panel_create/code")

source("../../setup_environment/code/packages.R")

ward_bound1998 <- st_read("../input/ward1998.shp")

ward_bound1998 <- ward_bound1998 %>%
  janitor::clean_names() %>% 
  mutate(ward = as.numeric(ward)) %>%
  arrange(ward) %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(1998:2002)) %>%
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435)

ward_bound2003 <- st_read("../input/Wards_2014.geojson") 

ward_bound2003 <- ward_bound2003 %>%
  filter(ward != "OUT") %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(2003:2014)) %>%
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435)

ward_bound2015 <- st_read("../input/Wards_2015.geojson")

ward_bound2015 <- ward_bound2015 %>%
  filter(ward != "OUT") %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(2015:2023)) %>%
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435)

ward_bound2024 <- st_read("../input/Wards_2024.geojson")

ward_bound2024 <- ward_bound2024 %>%
  filter(ward != "OUT") %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(2024:2025)) %>%
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435)

ward_panel_annual <- rbind(ward_bound1998, ward_bound2003, ward_bound2015, ward_bound2024) %>%
  mutate(ward = as.numeric(ward)) %>% 
  arrange(ward, year)

st_write(ward_panel_annual, "../output/ward_panel.gpkg", delete_layer = TRUE)
