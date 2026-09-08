# setwd("tasks/working_paper_release_audit/code")
library(sf)

# Review both source PINs before accepting the earlier component exclusion.
parcels <- st_read("../input/natchez_historical_parcels.gpkg", quiet = TRUE,
  query = "SELECT * FROM preferred_historical_parcels WHERE target_year = 2020 AND pin14 IN ('13312050180000','13315000020000','13312140010000')")
parcels <- st_transform(parcels, 3435)
stopifnot(nrow(parcels) == 3L, !anyDuplicated(parcels$pin14), all(st_is_valid(parcels)))
footprints <- st_transform(st_read("../input/natchez_footprints_2022.geojson", quiet = TRUE), 3435)
zoning <- st_transform(st_read("../input/natchez_zoning_2025.geojson", quiet = TRUE), 3435)
development <- st_union(zoning[zoning$pd_num == "1345", ])
strip <- st_geometry(parcels[parcels$pin14 == "13312050180000", ])
corridor <- st_geometry(parcels[parcels$pin14 == "13315000020000", ])
b_parcel <- st_geometry(parcels[parcels$pin14 == "13312140010000", ])
two_parcels <- st_union(c(strip, corridor))
clipped_site <- st_intersection(two_parcels, development)

# This spatial screen yields the western row of eight residences and its community
# center. It is evidence about parcel coverage, not a production membership rule.
buildings <- footprints[lengths(st_intersects(footprints, clipped_site)) > 0L, ]
stopifnot(nrow(buildings) == 9L, !anyDuplicated(buildings$OBJECTID))
buildings <- buildings[order(st_coordinates(st_centroid(st_geometry(buildings)))[, 2], decreasing = TRUE), ]
coverage <- data.frame(footprint_id = buildings$OBJECTID,
  north_to_south_order = seq_len(nrow(buildings)),
  footprint_sqft = as.numeric(st_area(buildings)),
  strip_fraction = NA_real_, corridor_fraction = NA_real_,
  b_parcel_fraction = NA_real_, clipped_site_fraction = NA_real_)
for (i in seq_len(nrow(buildings))) {
  building <- st_geometry(buildings[i, ])
  coverage$strip_fraction[i] <- sum(as.numeric(st_area(st_intersection(building, strip)))) / coverage$footprint_sqft[i]
  coverage$corridor_fraction[i] <- sum(as.numeric(st_area(st_intersection(building, corridor)))) / coverage$footprint_sqft[i]
  coverage$b_parcel_fraction[i] <- sum(as.numeric(st_area(st_intersection(building, b_parcel)))) / coverage$footprint_sqft[i]
  coverage$clipped_site_fraction[i] <- sum(as.numeric(st_area(st_intersection(building, clipped_site)))) / coverage$footprint_sqft[i]
}
stopifnot(all(coverage$strip_fraction < 0.3), all(coverage$corridor_fraction > 0.45),
  all(coverage$clipped_site_fraction <= 1.0001))
write.csv(coverage, "../output/natchez_building_land_coverage.csv", row.names = FALSE)
cat("Strip area:", as.numeric(st_area(strip)), "sq ft\n")
cat("Whole two-parcel union:", as.numeric(st_area(two_parcels)), "sq ft\n")
cat("Two parcels clipped to PD:", as.numeric(st_area(clipped_site)), "sq ft\n")
