# setwd("tasks/audits/assessor_new_building_checks/code")
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("building_sites.R")

lag_years <- 2  # buildings are first assessed about two years after their permits

# New buildings by year and type (apartment class by development) against the Census Bureau's permitted buildings two
# years before: houses against 1-unit buildings, apartment class and 5+ unit condominium buildings against 5+ unit buildings.
buildings <- read_buildings()[year %in% 2000:2025]
counts <- buildings[, .(houses = sum(type == "house"), flats_2_6 = sum(type == "2-6 flat"), condominium = sum(type == "condominium"),
  condominium_5_plus = sum(type == "condominium" & units >= 5, na.rm = TRUE), apartment_developments = uniqueN(development_id[type == "apartment class"])), by = year]
census <- fread("../input/chicago_building_permits_survey.csv")[, .(year = year + lag_years, census_1_unit = units_1_bldgs,
  census_2_4_units = units_2_bldgs + units_3_4_bldgs, census_5_plus_units = units_5_plus_bldgs)]
comparison <- merge(counts, census, by = "year")[order(year)]
comparison[, `:=`(houses_to_census = houses / census_1_unit, large_to_census = (apartment_developments + condominium_5_plus) / census_5_plus_units)]
print(comparison)
cat(sprintf("correlations 2002-2025: houses %.2f, large buildings %.2f, 2-6 flats and small condominiums %.2f\n",
  comparison[year >= 2002, cor(houses, census_1_unit)], comparison[year >= 2002, cor(apartment_developments + condominium_5_plus, census_5_plus_units)],
  comparison[year >= 2002, cor(flats_2_6 + condominium - condominium_5_plus, census_2_4_units)]))
SaveData(comparison, "year", "../output/census_comparison.csv")
