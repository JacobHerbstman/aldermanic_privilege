### this code writes in census tracts from tidycensus, cleans them, and makes them compatible with ward data

## run this line when editing code in Rstudio (replace "task" with the name of this particular task)
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/"task"/code")

source("../../setup_environment/code/packages.R")

## set census api key
census_api_key(Sys.getenv("CENSUS_API_KEY"))

## get tracts for each year 2000, 2010, and 2020 with purrr
years <- c(2000, 2010, 2020)

## get census blocks for 3 decades
blocks <- map(years, ~{
  blocks(state = "IL", county = "Cook", year = .x)
})

##strip year numbers off variable names
blocks <- map2(blocks, years, ~{
  # .x is the data frame, .y is the year
  df <- .x
  suffix <- substr(as.character(.y), 3, 4)
  
  # Find the base names of columns that have a year suffix
  # e.g., for "STATEFP10", the base name is "STATEFP"
  base_names_to_drop <- df %>% 
    select(ends_with(suffix)) %>% 
    names() %>% 
    gsub(paste0(suffix, "$"), "", .)
  
  # First, drop the original base columns (e.g., "STATEFP")
  # Then, rename the suffixed columns (e.g., "STATEFP10" -> "STATEFP")
  df %>%
    select(-any_of(base_names_to_drop)) %>%
    rename_with(~gsub(paste0(suffix, "$"), "", .))
})

##combine different years
blocks_all <- rbindlist(blocks, idcol = "year", fill = T)

##change 1,2,3 to 2000, 2010, 2020 in the year column
blocks_all <- blocks_all %>% 
  mutate(year = case_when(
    year == 1 ~ "2000",
    year == 2 ~ "2010",
    year == 3 ~ "2020"
)
)


## harmonize years
blocks_all <- blocks_all %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(GEOID = if_else(year == 2000, BLKIDFP, GEOID)) %>%
  rename(block_id = GEOID) %>% 
  dplyr::select(year, block_id, geometry)


## write to .shp file
st_write(blocks_all, "../output/census_blocks.shp", delete_dsn = TRUE)






                         