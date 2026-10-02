# setwd("tasks/audits/assessor_new_building_checks/code")
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("building_sites.R")

issue_years <- 2006:2018  # permits whose buildings could be assessed by 2024
window <- -1:5            # a building found on the site from one year before to five years after the permit

# Permit-linked buildings of tasks/prepare_permit_construction (residential cards, condominiums, commercial valuations):
# found if a new building is on the site in the window, and found with its type if that building has the matching type.
buildings <- read_buildings()
sites <- building_sites(buildings)
reference <- fread("../input/construction_buildings.csv", colClasses = "character")[route == "permit" & status == "measured"]
reference <- reference[, .(pin10 = substr(unlist(strsplit(record_ids, ";")), 1, 10)), by = .(building_id, source, issue_year = as.integer(issue_year))][pin10 != ""]
found <- merge(merge(reference, sites, by = "pin10", allow.cartesian = TRUE, suffixes = c("", "_new")), buildings[, .(building_id_new = building_id, year, type)],
  by = "building_id_new")[(year - issue_year) %in% window]
found[, type_ok := (source == "residential" & type %in% c("house", "2-6 flat")) | (source == "condominium" & type == "condominium") |
  (source == "commercial" & type == "apartment class")]
recall <- unique(reference[issue_year %in% issue_years, .(building_id, source)])
recall[, `:=`(found = building_id %in% found$building_id, found_with_type = building_id %in% found[type_ok == TRUE, building_id])]
recall <- recall[, .(buildings = .N, found = mean(found), found_with_type = mean(found_with_type)), by = source][order(source)]
print(recall)
SaveData(recall, "source", "../output/recall_against_permits.csv")
