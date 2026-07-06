# Download OSM Illinois Data

Downloads the Geofabrik Illinois OpenStreetMap shapefile extract:

`https://download.geofabrik.de/north-america/us/illinois-latest-free.shp.zip`

The task extracts only the road, landuse, water, and waterways layers used by border and amenity tasks.

The Geofabrik source is the current Illinois extract. The prior local `illinois-250919-free`
folder was incomplete for required sidecar files, and the matching dated Geofabrik zip is not
available from the public download server. Treat changes in this source as possible input-vintage
changes when comparing regenerated paper outputs.
