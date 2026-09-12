# setwd("tasks/working_paper_release_audit/code")
library(sf)

parcel <- st_transform(st_read("../input/natchez_historical_parcels.gpkg", quiet = TRUE,
  query = "SELECT * FROM preferred_historical_parcels WHERE target_year = 2020 AND pin14 = '13312140010000'"), 3435)
stopifnot(nrow(parcel) == 1L, st_is_valid(parcel))
footprints <- st_transform(st_read("../input/natchez_footprints_2022.geojson", quiet = TRUE), 3435)
c_coverage <- read.csv("../output/natchez_building_land_coverage.csv")
stopifnot(!anyDuplicated(footprints$OBJECTID), !anyDuplicated(c_coverage$footprint_id))

# Keep every intersecting footprint, including C and the neighboring garage.
# The approved 2016 plan supplies the residential-versus-accessory interpretation.
buildings <- footprints[lengths(st_intersects(footprints, parcel)) > 0L, ]
buildings <- buildings[order(buildings$OBJECTID), ]
coverage <- data.frame(footprint_id = buildings$OBJECTID,
  footprint_sqft = as.numeric(st_area(buildings)), height_ft = buildings$Height,
  in_c_review = buildings$OBJECTID %in% c_coverage$footprint_id,
  overlap_sqft = NA_real_, fraction_in_b_parcel = NA_real_)
for (i in seq_len(nrow(buildings))) {
  coverage$overlap_sqft[i] <- sum(as.numeric(st_area(st_intersection(
    st_geometry(buildings[i, ]), st_geometry(parcel)))))
}
coverage$fraction_in_b_parcel <- coverage$overlap_sqft / coverage$footprint_sqft
stopifnot(nrow(coverage) == 18L, sum(coverage$in_c_review) == 2L,
  all(coverage$fraction_in_b_parcel > 0), all(coverage$fraction_in_b_parcel <= 1.0001))
write.csv(coverage, "../output/natchez_b_land_coverage.csv", row.names = FALSE)
cat("B parcel:", as.numeric(st_area(parcel)), "sq ft\n")
cat("C footprint overlap:", sum(coverage$overlap_sqft[coverage$in_c_review]), "sq ft\n")
