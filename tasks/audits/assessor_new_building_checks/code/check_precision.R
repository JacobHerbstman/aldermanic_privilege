# setwd("tasks/audits/assessor_new_building_checks/code")
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("building_sites.R")
sf_use_s2(FALSE)

years <- 2012:2022   # buildings whose six years before are covered by the permits (2006 on)
site_ft <- 100       # a permit within this distance of the site
permit_years <- 6    # issued in the six years before the building's year
per_route <- 250     # buildings sampled per type and route
per_type <- 400      # parcels sampled per type with no new building
set.seed(20261002)

# New buildings and, as a base rate, parcels of the same type with no new building, by type and route: the share with a
# new-building permit near the site in the years before.
buildings <- read_buildings()
sites <- building_sites(buildings)
sample_buildings <- buildings[year %in% years][, .SD[sample(.N, min(.N, per_route))], by = .(type, route)][, .(building_id, type, route, year)]
assessed <- as.data.table(read_parquet("../input/assessed_values.parquet", col_select = c("pin", "year", "class")))[year %in% years]
assessed[, type := fcase(class == "299", "condominium", substr(class, 1, 1) %in% c("3", "9") & !class %in% c("399", "390", "990"), "apartment class",
  class %in% c("211", "212"), "2-6 flat", substr(class, 1, 1) == "2" & !class %in% c("241", "290", "299"), "house", default = NA_character_)]
placebo <- assessed[!is.na(type) & !substr(pin, 1, 10) %in% sites$pin10][, .SD[sample(.N, per_type)], by = type][,
  .(building_id = paste0("placebo-", .I), type, route = "placebo: parcel with no new building", year, pin10 = substr(pin, 1, 10))]
checked <- rbind(sample_buildings, placebo[, .(building_id, type, route, year)])
zones <- st_read("../input/parcel_history.gpkg", quiet = TRUE) |> select(pin10) |>
  inner_join(as_tibble(rbind(sites[building_id %in% sample_buildings$building_id], placebo[, .(building_id, pin10)])), by = "pin10", relationship = "many-to-many") |>
  group_by(building_id) |> summarise(.groups = "drop") |> st_buffer(site_ft)
permits <- fread("../input/construction_permits.csv", colClasses = "character")[scope %in% c("new_residential", "not_residential", "building_use_not_stated") & !is.na(as.numeric(longitude)) & !is.na(as.numeric(latitude))]
permits <- st_as_sf(permits, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
near <- st_intersects(zones, permits)
year_of <- setNames(checked$year, checked$building_id)
zones$permit <- vapply(seq_len(nrow(zones)), \(i) { y <- as.integer(permits$issue_year[near[[i]]]); any(y >= year_of[[zones$building_id[i]]] - permit_years & y <= year_of[[zones$building_id[i]]]) }, TRUE)
checked <- merge(checked, as.data.table(st_drop_geometry(zones)), by = "building_id", all.x = TRUE)
precision <- checked[, .(sampled = .N, no_parcel_polygon = sum(is.na(permit)), share_with_permit = mean(permit, na.rm = TRUE)), by = .(type, route)][order(type, route)]
print(precision)
SaveData(precision, c("type", "route"), "../output/precision_against_permits.csv")
