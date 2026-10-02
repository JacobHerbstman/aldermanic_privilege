# setwd("tasks/build_assessor_new_buildings/code")
source("../../setup_environment/code/packages.R")
source("../../shared/code/save_data.R")
sf_use_s2(FALSE)

sliver <- 0.01  # overlaps below this share of a new parcel are drawing slivers

# New parcels: 10-digit parcels first assessed in 2000 or later.
assessed <- as.data.table(read_parquet("../input/assessed_values.parquet", col_select = c("pin", "year")))[year <= 2025]
first_year <- assessed[, .(first_year = min(year)), by = .(pin10 = substr(pin, 1, 10))]

# One shape per parcel: the parcel history layer's polygon, else the earliest yearly layer's, its parts merged.
parcels <- st_read("../input/parcel_history.gpkg", quiet = TRUE) |> mutate(priority = if_else(layer == "history", "0", layer)) |>
  group_by(pin10) |> filter(priority == min(priority)) |> ungroup()
several <- parcels$pin10 %in% parcels$pin10[duplicated(parcels$pin10)]
parcels <- bind_rows(parcels[!several, "pin10"], parcels[several, ] |> group_by(pin10) |> summarise(.groups = "drop")) |> st_make_valid()
stopifnot(!anyDuplicated(parcels$pin10))
parcels$area <- as.numeric(st_area(parcels))
new_parcels <- parcels[parcels$pin10 %in% first_year[first_year >= 2000, pin10], ]
message(sprintf("new parcels: %d, with a polygon: %d", first_year[first_year >= 2000, .N], nrow(new_parcels)))

# Every other parcel a new parcel overlaps: new_share is the share of the new parcel it covers, old_share the share of
# the other parcel under the new one.
hits <- st_intersects(new_parcels, parcels)
lineage <- rbindlist(lapply(seq_len(nrow(new_parcels)), \(i) {
  j <- setdiff(hits[[i]], which(parcels$pin10 == new_parcels$pin10[i]))
  if (!length(j)) return(NULL)
  g <- st_intersection(new_parcels[i, "pin10"], parcels[j, c("pin10", "area")] |> rename(old_pin10 = pin10, old_area = area))
  if (!nrow(g)) return(NULL)
  data.table(pin10 = g$pin10, old_pin10 = g$old_pin10, a = as.numeric(st_area(g)), old_area = g$old_area)[,
    .(new_share = sum(a) / new_parcels$area[i], old_share = sum(a) / old_area[1]), by = .(pin10, old_pin10)]
}))[new_share >= sliver][order(pin10, old_pin10)]
SaveData(lineage, c("pin10", "old_pin10"), "../output/parcel_lineage.csv")
