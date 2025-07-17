## this code takes chicago ward shapefiles from the uchicago justice project and merges them for a panel with census tracts from 2005-2023
## (https://img.shields.io/github/repo-size/uchicago-justice-project/data)

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

## bring in ward boundary data
ward_bound2005 <- st_read("../input/CHI_2005/CHI_2005.shp")

## make panel from 2005-2014
ward_bound2005 <- ward_bound2005 %>%
  mutate(start_year = 2005, end_year = 2014)
ward_bound2005 <- ward_bound2005 %>%
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>% 
  dplyr::rename(ward = ward2005) %>% 
  dplyr::select(year, ward, geometry)


ward_bound2015 <- st_read("../input/CHI_2015/CHI_2015.shp")
## make panel from 2015-2024
ward_bound2015 <- ward_bound2015 %>%
  mutate(start_year = 2015, end_year = 2024)
ward_bound2015 <- ward_bound2015 %>%
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>% 
  dplyr::rename(ward = ward2015) %>% 
  dplyr::select(year, ward, geometry)

##join to one large panel
ward_panel <- rbind(ward_bound2005, ward_bound2015)

st_write(ward_panel, "../output/ward_panel.shp", append = F, delete_dsn = T)




##bring in census tracts and harmonize with ward boundaries (requires cleaned census tract data)

census_blocks <- st_read("../input/census_blocks.shp") 
census_blocks <- st_transform(census_blocks, st_crs(ward_panel))

## make tract panel for merge 
# Add year ranges for each census dataset
census_blocks_2000 <- census_blocks %>%
  filter(year == 2000) %>% 
  mutate(start_year = 2003, end_year = 2009) %>% 
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>% 
  arrange(block_id, year) %>% 
  dplyr::select(year, block_id, geometry)

census_blocks_2010 <- census_blocks %>%
  filter(year == 2010) %>% 
  mutate(start_year = 2010, end_year = 2019) %>% 
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>% 
  arrange(year, block_id) %>% 
  dplyr::select(year, block_id, geometry)

## check number of blocks in each year
census_blocks_2020 <- census_blocks %>%
  filter(year == 2020) %>% 
  mutate(start_year = 2020, end_year = 2023) %>% 
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  unnest(year) %>% 
  arrange(year, block_id) %>% 
  dplyr::select(year, block_id, geometry)



# Combine and expand to yearly panel
census_blocks_panel <- bind_rows(census_blocks_2000, census_blocks_2010, census_blocks_2020) 

census_blocks_panel <- st_transform(census_blocks_panel, st_crs(ward_panel))


## join census blocks to ward
joined_panel <- st_join(census_blocks_panel, ward_panel, join = st_intersects)



joined_panel <- joined_panel %>%
  filter(year.x == year.y) %>%   # Ensure years align
  dplyr::select(-year.y) %>%     # Drop duplicate year column
  dplyr::rename(year = year.x) %>%   # Rename year column
  filter(!is.na(block_id))   # Drop rows with no block ID))

#see which blocks show up more than once
joined_panel %>% 
  group_by(year, block_id) %>% 
  count() %>% 
  filter(n > 1) ##its a lot

#count unique census block_ids per year using summarise n_distinct
joined_panel %>% 
  group_by(year) %>% 
  summarise(n_distinct(block_id))


##write to shapefile
st_write(joined_panel,"../output/census_block_panel_wards.shp", append = F, delete_dsn = TRUE)



## map 
# ggplot() +
#   # Add ward boundaries with dark black lines and blank interiors
#   geom_sf(data = filter(joined_panel, year == 2015), 
#           fill = NA, 
#           color = scales::alpha("blue", 0.4), # Lower alpha value for more transparency
#           size = 0.1) +
#   
#   geom_sf(data = filter(ward_panel, year == 2015), fill = NA, color = scales::alpha("black", 0.8), size = 1) +
#   
#   # Add census tract boundaries with blue lines and blank interiors
# 
#   # Customize the theme
#   theme_minimal() +
#   
#   # Add labels for clarity
#   labs(
#     title = "Chicago Wards and Census Tracts",
#     subtitle = "Wards outlined in black, Census Tracts in blue",
#     x = NULL, y = NULL
#   )
# 
# ggsave(paste0(root, "merge_tracts_wards/output/wards_tracts_overlay.pdf"))




