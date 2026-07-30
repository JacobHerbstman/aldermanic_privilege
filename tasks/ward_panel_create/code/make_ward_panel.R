# --- Interactive Test Block ---
# setwd("/Users/jacobherbstman/Desktop/aldermanic_privilege/tasks/ward_panel_create/code")
# start_year <- 2003
# end_year <- 2022

source("../../setup_environment/code/packages.R")

cli_args <- commandArgs(trailingOnly = TRUE)
if (length(cli_args) == 0) {
  cli_args <- c(start_year, end_year)
}
if (length(cli_args) != 2) {
  stop("Script requires 2 arguments: <start_year> <end_year>.", call. = FALSE)
}
start_year <- suppressWarnings(as.integer(cli_args[1]))
end_year <- suppressWarnings(as.integer(cli_args[2]))
if (!is.finite(start_year) || !is.finite(end_year) || start_year > end_year) {
  stop("start_year and end_year must be valid integers with start_year <= end_year.", call. = FALSE)
}
if (start_year < 2003 || end_year > 2023) {
  stop("The paper's ward maps support years 2003 through 2023.", call. = FALSE)
}

ward_bound2003 <- st_read("../input/Wards_2014.geojson") 

ward_bound2003 <- ward_bound2003 %>%
  filter(ward != "OUT") %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(intersect(2003:2014, start_year:end_year))) %>%
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435)

ward_bound2015 <- st_read("../input/Wards_2015.geojson")

ward_bound2015 <- ward_bound2015 %>%
  filter(ward != "OUT") %>% 
  select(ward, geometry) %>%
  rowwise() %>%
  mutate(year = list(intersect(2015:2023, start_year:end_year))) %>%
  unnest(year) %>%
  select(year, ward, geometry) %>% 
  arrange(ward, year) %>% 
  st_transform(crs = 3435)

ward_panel_annual <- rbind(ward_bound2003, ward_bound2015) %>%
  mutate(ward = as.numeric(ward)) %>% 
  arrange(ward, year)

st_write(ward_panel_annual, "../output/ward_panel.gpkg", delete_layer = TRUE)
