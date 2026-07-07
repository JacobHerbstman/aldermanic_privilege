# Download OSM Illinois Data

Downloads the Geofabrik Illinois OpenStreetMap shapefile extract:

`https://download.geofabrik.de/north-america/us/illinois-latest-free.shp.zip`

The task extracts only the road, landuse, water, and waterways layers used by border and amenity tasks.

This task downloads the current Geofabrik extract and is not the active paper input. The active
paper pipeline uses the older `data_raw/illinois-250919-free` vintage supplied outside git with the
replication package. Use this task only to regenerate a current-vintage comparison or to replace
the old vintage deliberately after checking output changes.
