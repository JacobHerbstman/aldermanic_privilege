# setwd("tasks/build_assessor_new_buildings/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")

new_gap <- 5  # a building whose units were built at most five years before they first appear is new; older is a conversion

# Condominium buildings (10-digit parcels): the first year their unit records appear, the median year built of those
# units, and the units then (not parking spaces or common areas). Years are written "2026" or "2026.0".
units <- fread("../input/condominium_characteristics.csv", colClasses = "character", select = c("pin10", "year", "char_yrblt", "is_parking_space", "is_common_area"))
units[, `:=`(year = as.integer(as.numeric(year)), year_built = as.integer(as.numeric(char_yrblt)), unit = is_parking_space != "true" & is_common_area != "true")]
stopifnot(!anyNA(units$year))
first <- units[, .(first_year = min(year)), by = pin10]
buildings <- units[first, on = .(pin10, year = first_year)][, .(first_year = year[1], year_built = as.integer(median(year_built, na.rm = TRUE)), units = sum(unit)), by = pin10]
buildings[, kind := fcase(first_year == 1999, "on the rolls in 1999", is.na(year_built), "no year built",
  first_year - year_built <= new_gap, "new", default = "conversion")]
SaveData(buildings[order(pin10)], "pin10", "../output/condominium_buildings.csv")
