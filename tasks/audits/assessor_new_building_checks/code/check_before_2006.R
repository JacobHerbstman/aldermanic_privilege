# setwd("tasks/audits/assessor_new_building_checks/code")
source("../../../setup_environment/code/packages.R")
source("../../../shared/code/save_data.R")
source("building_sites.R")
sf_use_s2(FALSE)

window <- -1:3    # a building found from one year before to three years after the year built recorded by the other source
match_ft <- 300   # a benchmarking building matched to a condominium or apartment-class building this close
chicago <- c("Hyde Park", "Jefferson", "Lake", "Lake View", "North Chicago", "Rogers Park", "South Chicago", "West Chicago")

# Two records of buildings built 1999-2020 that do not come from the assessed values: the Assessor's 2021+ commercial
# valuations of apartment buildings (7+ units) still standing, and City Energy Benchmarking multifamily buildings
# (50,000+ square feet, year built reported by owners). Found: a new building on the site, or a condominium or
# apartment-class building nearby, in the window around the reported year built.
buildings <- read_buildings()
sites <- building_sites(buildings)
valuations <- fread("../input/commercial_valuation_data.csv", colClasses = "character", select = c("keypin", "pins", "year", "township", "class_es", "tot_units", "yearbuilt"))
valuations <- valuations[township %in% chicago][, `:=`(year_built = as.integer(as.numeric(yearbuilt)), units = as.numeric(tot_units), major = substr(gsub("[^0-9]", "", class_es), 1, 1))]
valuations <- valuations[major %in% c("3", "9") & units >= 7][order(keypin, -as.integer(year))][, .SD[1], by = keypin][year_built %in% 1999:2020]
parcels <- valuations[, .(pin10 = unique(substr(gsub("[^0-9]", "", unlist(strsplit(pins, ","))), 1, 10))), by = .(keypin, year_built)]
hit <- merge(merge(parcels, sites, by = "pin10", allow.cartesian = TRUE), buildings[, .(building_id, year)], by = "building_id")[(year - year_built) %in% window]
valuations[, found := keypin %in% hit$keypin]

benchmarking <- fread("../input/energy_benchmarking.csv", colClasses = "character")[primary_property_type %in% c("Multifamily Housing", "Residential") & !is.na(as.numeric(latitude)) & !is.na(as.numeric(longitude))]
benchmarking <- benchmarking[, .(year_built = as.integer(median(as.numeric(year_built), na.rm = TRUE)), latitude = latitude[1], longitude = longitude[1]), by = id][year_built %in% 1999:2020]
benchmarking <- st_as_sf(benchmarking, coords = c("longitude", "latitude"), crs = 4326) |> st_transform(3435)
large <- buildings[type %in% c("condominium", "apartment class")]
shapes <- st_read("../input/parcel_history.gpkg", quiet = TRUE) |> select(pin10) |>
  inner_join(as_tibble(sites[building_id %in% large$building_id]), by = "pin10", relationship = "many-to-many") |> group_by(building_id) |> summarise(.groups = "drop")
shapes$year <- large$year[match(shapes$building_id, large$building_id)]
near <- st_is_within_distance(benchmarking, shapes, match_ft)
benchmarking$found <- vapply(seq_len(nrow(benchmarking)), \(i) any((shapes$year[near[[i]]] - benchmarking$year_built[i]) %in% window), TRUE)

period <- function(y) fifelse(y <= 2005, "built 1999-2005", "built 2006-2020")
checks <- rbind(valuations[, .(check = "surviving apartment buildings (valuations, 7+ units)", period = period(year_built), found)],
  as.data.table(st_drop_geometry(benchmarking))[, .(check = "benchmarking multifamily buildings (50,000+ sq ft)", period = period(year_built), found)])
checks <- checks[, .(buildings = .N, found = mean(found)), by = .(check, period)][order(check, period)]
print(checks)
SaveData(checks, c("check", "period"), "../output/checks_before_2006.csv")
