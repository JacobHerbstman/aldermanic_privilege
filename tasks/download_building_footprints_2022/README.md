# Building footprints, 2022

`output/building_footprints_2022_chicago.gpkg` has one polygon per building in Chicago from Cook County GIS
Building Footprints 2022 (EPSG 3435), with `footprint_sqft`, `height_ft` (highest roof point above ground, the
service's `Height` = `Max_Point` − `Ground_Z`), `ground_z`, `max_point` and `image_year`.

Source: Cook County GIS, `https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0`
(1,960,186 footprints countywide when queried on September 23, 2026). The layer is a fixed 2022 product. The
query covers every `TILE_FT` square tile holding a Chicago parcel centroid (`tasks/download_parcel_centroids`),
2,000 features per request ordered by `OBJECTID`; footprints crossing tile edges are kept once. Re-running the task
queries the service again, which is a source refresh: compare the new report with the committed one.

The only consumer is `tasks/audits/footprint_floor_area_check`.
