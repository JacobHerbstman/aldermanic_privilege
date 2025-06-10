### this code writes in census tracts from tidycensus, cleans them, and makes them compatible with ward data

source("/Users/jacobherbstman/Desktop/aldermanic_privilege/source_script.R")

## get tracts for each year 2000, 2010, and 2020 with purrr
years <- c(2000, 2010, 2020)

## get tracts for 3 decades
tracts <- map(years, ~{
  tracts(state = "IL", county = "Cook", cb = TRUE, year = .x)
})

##combine different years
tracts_all <- rbindlist(tracts, idcol = "year", fill = T)

##change 1,2,3 to 2000, 2010, 2020 in the year column
tracts_all <- tracts_all %>% 
  mutate(year = case_when(
    year == 1 ~ "2000",
    year == 2 ~ "2010",
    year == 3 ~ "2020"
)
)

tracts_all <- tracts_all %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(tract_id = ifelse(year == 2020, TRACTCE, TRACT)) %>% 
  dplyr::select(year, tract_id, geometry)



st_write(tracts_all, paste0(root, "clean_tract_data/output/census_tracts.shp"), append = F)






                         