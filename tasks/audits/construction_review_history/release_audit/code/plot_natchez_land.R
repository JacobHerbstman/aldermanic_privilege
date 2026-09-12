# setwd("tasks/working_paper_release_audit/code")
library(sf)

parcels <- st_transform(st_read("../input/natchez_historical_parcels.gpkg", quiet = TRUE,
  query = "SELECT * FROM preferred_historical_parcels WHERE (target_year = 2020 AND pin14 IN ('13312050180000','13315000020000','13312140010000')) OR (target_year = 2017 AND pin14 = '13312050690000')"), 3435)
footprints <- st_transform(st_read("../input/natchez_footprints_2022.geojson", quiet = TRUE), 3435)
coverage <- read.csv("../output/natchez_building_land_coverage.csv")
west <- footprints[match(coverage$footprint_id, footprints$OBJECTID), ]
stopifnot(!anyNA(west$OBJECTID))
parcels <- parcels[match(c("13312050690000", "13312140010000", "13312050180000", "13315000020000"), parcels$pin14), ]

pdf("../output/natchez_parcel_coverage.pdf", width = 8, height = 10)
par(mar = c(1, 1, 4, 1))
plot(st_geometry(parcels), col = c("#c7e4f2", "#cce7cf", "#ef9a76", "#ddc8ef"),
  border = "#777777", xlim = c(1131850, 1132700), ylim = c(1913050, 1914070))
plot(st_geometry(footprints), add = TRUE, col = "#999999", border = "white")
plot(st_geometry(west), add = TRUE, col = "#353535", border = "white")
plot(st_geometry(parcels), add = TRUE, border = "#b0472d", lwd = 1)
title("Natchez C: tax parcels cross the buildings", line = 2)
mtext("2020 parcels and 2022 building footprints; EPSG:3435", side = 3, line = 0.7, cex = 0.8)
legend("topright", c("A parcel", "B parcel", "Retained strip: 16,032 sq ft", "Excluded corridor: 395,698 sq ft", "C residences and community center"),
  fill = c("#c7e4f2", "#cce7cf", "#ef9a76", "#ddc8ef", "#353535"), bg = "white", cex = 0.75)
dev.off()
