#!/usr/bin/env bash
# Download Illinois OSM shapefile extracts from Geofabrik.

set -euo pipefail

tmp_dir=$(mktemp -d "../temp/.download_osm_illinois_data.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

zip_file="$tmp_dir/illinois-latest-free.shp.zip"

curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 3600 \
	-sL -o "$zip_file" \
	"https://download.geofabrik.de/north-america/us/illinois-latest-free.shp.zip"

unzip -q -j "$zip_file" \
	"gis_osm_roads_free_1.shp" \
	"gis_osm_roads_free_1.dbf" \
	"gis_osm_roads_free_1.prj" \
	"gis_osm_roads_free_1.shx" \
	"gis_osm_roads_free_1.cpg" \
	"gis_osm_landuse_a_free_1.shp" \
	"gis_osm_landuse_a_free_1.dbf" \
	"gis_osm_landuse_a_free_1.prj" \
	"gis_osm_landuse_a_free_1.shx" \
	"gis_osm_landuse_a_free_1.cpg" \
	"gis_osm_water_a_free_1.shp" \
	"gis_osm_water_a_free_1.dbf" \
	"gis_osm_water_a_free_1.prj" \
	"gis_osm_water_a_free_1.shx" \
	"gis_osm_water_a_free_1.cpg" \
	"gis_osm_waterways_free_1.shp" \
	"gis_osm_waterways_free_1.dbf" \
	"gis_osm_waterways_free_1.prj" \
	"gis_osm_waterways_free_1.shx" \
	"gis_osm_waterways_free_1.cpg" \
	-d "$tmp_dir"

for layer in gis_osm_roads_free_1 gis_osm_landuse_a_free_1 gis_osm_water_a_free_1 gis_osm_waterways_free_1; do
	for suffix in shp dbf prj shx cpg; do
		test -s "$tmp_dir/${layer}.${suffix}"
		mv "$tmp_dir/${layer}.${suffix}" "../output/${layer}.${suffix}"
		touch "../output/${layer}.${suffix}"
	done
done

echo "Downloaded Illinois OSM roads, landuse, water, and waterways layers"
