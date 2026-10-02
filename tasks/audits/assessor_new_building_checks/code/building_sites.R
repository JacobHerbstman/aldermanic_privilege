# A building's site: its parcels, the old parcels under them and the parcels replacing them (at least ten percent of the
# new parcel's area), from the parcel lineage of tasks/build_assessor_new_buildings.
building_sites <- function(buildings) {
  lineage <- fread("../input/parcel_lineage.csv", colClasses = c(pin10 = "character", old_pin10 = "character"))[new_share >= 0.1, .(pin10, old_pin10)]
  own <- buildings[, .(pin10 = unlist(strsplit(pin10s, ";"))), by = building_id]
  unique(rbind(own, merge(own, lineage, by = "pin10")[, .(building_id, pin10 = old_pin10)],
    merge(own[, .(building_id, old_pin10 = pin10)], lineage, by = "old_pin10")[, .(building_id, pin10)]))
}
read_buildings <- function() fread("../input/new_buildings.csv", colClasses = c(building_id = "character", development_id = "character", pin10 = "character", pin10s = "character"))
