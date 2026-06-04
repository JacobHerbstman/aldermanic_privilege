# prepare_amenities_data

Purpose: Converts local raw amenities layers into cleaned EPSG:3435 GeoPackages.

Produces: `output/cta_stops.gpkg`, `output/major_streets.gpkg`, `output/parks.gpkg`, and `output/schools_2015.gpkg`.

Main amenity sources:

- Lake Michigan: OSM water geometry is read downstream and treated as static.
- Major streets: City of Chicago Major Streets shapefile, treated as a stable major-road-access proxy.
- Parks: official Chicago Park District park-boundary polygons from `download_park_boundaries_data`, not CPD facility points.
- Schools: CPS SY2014-15 school locations, treated as the current static baseline school-access proxy.
- CTA stops: CTA current station points, projected to EPSG:3435. The output carries explicit opening dates for stations that opened during or after the rental sample (`Cermak-McCormick Place`, `Washington/Wabash`, and `Damen Green Line`) so downstream rent controls can use stations open by listing month.

Approx. runtime: ~1 minute.
