#!/usr/bin/env bash
# Download official Chicago Data Portal spatial inputs used by the paper pipeline.

set -euo pipefail

dataset="${1:?dataset key required}"

tmp_dir=$(mktemp -d "../temp/.download_chicago_spatial_data.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

validate_geojson() {
	local input_file="$1"
	local expected_types="$2"
	local min_features="$3"

	python3 - "$input_file" "$expected_types" "$min_features" <<'PY'
import json
import sys

input_file, expected_types_raw, min_features_raw = sys.argv[1:4]
expected_types = set(expected_types_raw.split(","))
min_features = int(min_features_raw)

with open(input_file, encoding="utf-8") as f:
    data = json.load(f)

if data.get("type") != "FeatureCollection":
    raise SystemExit("ERROR: downloaded file is not a GeoJSON FeatureCollection")

features = data.get("features")
if not isinstance(features, list):
    raise SystemExit("ERROR: downloaded GeoJSON has no feature list")
if len(features) < min_features:
    raise SystemExit(
        f"ERROR: downloaded GeoJSON has {len(features)} features; expected at least {min_features}"
    )

bad_features = []
for idx, feature in enumerate(features, start=1):
    geometry = feature.get("geometry")
    geometry_type = geometry.get("type") if isinstance(geometry, dict) else None
    if geometry_type not in expected_types:
        bad_features.append((idx, geometry_type))

if bad_features:
    first_bad = ", ".join(f"{idx}:{geometry_type}" for idx, geometry_type in bad_features[:5])
    raise SystemExit(
        f"ERROR: {len(bad_features)} features have unexpected geometry types: {first_bad}"
    )

print(len(features))
PY
}

validate_census_blocks_2010() {
	local input_file="$1"

	python3 - "$input_file" <<'PY'
import csv
import sys

required = {
    "the_geom",
    "STATEFP10",
    "COUNTYFP10",
    "TRACTCE10",
    "BLOCKCE10",
    "GEOID10",
    "NAME10",
    "TRACT_BLOC",
}

with open(sys.argv[1], newline="", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    missing = sorted(required.difference(reader.fieldnames or []))
    if missing:
        raise SystemExit(f"ERROR: missing census block columns: {', '.join(missing)}")

    rows = list(reader)

if len(rows) < 40000:
    raise SystemExit(f"ERROR: downloaded {len(rows)} census block rows; expected at least 40000")

bad_geoids = [
    row.get("GEOID10", "")
    for row in rows
    if len(row.get("GEOID10", "")) != 15
]
if bad_geoids:
    raise SystemExit(f"ERROR: {len(bad_geoids)} rows have invalid geoid10 length")

bad_geometry = [
    row.get("GEOID10", "")
    for row in rows
    if not row.get("the_geom", "").startswith("MULTIPOLYGON")
]
if bad_geometry:
    raise SystemExit(f"ERROR: {len(bad_geometry)} rows have invalid WKT geometry")

print(len(rows))
PY
}

download_geojson() {
	local output_file="$1"
	local url="$2"
	local expected_types="$3"
	local min_features="$4"
	local tmp_output="$tmp_dir/$(basename "$output_file")"

	curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 \
		-sL -o "$tmp_output" "$url"

	feature_count=$(validate_geojson "$tmp_output" "$expected_types" "$min_features")
	mv "$tmp_output" "$output_file"
	echo "Downloaded ${feature_count} features to ${output_file}"
}

download_census_blocks_2010() {
	local output_file="../output/census_blocks_2010.csv"
	local tmp_raw="$tmp_dir/census_blocks_2010_raw.csv"
	local tmp_output="$tmp_dir/census_blocks_2010.csv"

	curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 \
		-sG -o "$tmp_raw" "https://data.cityofchicago.org/resource/bt9m-d2mf.csv" \
		--data-urlencode "\$limit=100000"

	python3 - "$tmp_raw" "$tmp_output" <<'PY'
import csv
import sys

input_file, output_file = sys.argv[1:3]
name_map = {
    "the_geom": "the_geom",
    "statefp10": "STATEFP10",
    "countyfp10": "COUNTYFP10",
    "tractce10": "TRACTCE10",
    "blockce10": "BLOCKCE10",
    "geoid10": "GEOID10",
    "name10": "NAME10",
    "tract_bloc": "TRACT_BLOC",
}

with open(input_file, newline="", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    fieldnames = [name_map.get(name, name) for name in reader.fieldnames or []]
    rows = [{name_map.get(k, k): v for k, v in row.items()} for row in reader]

with open(output_file, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)
PY

	row_count=$(validate_census_blocks_2010 "$tmp_output")
	mv "$tmp_output" "$output_file"
	echo "Downloaded ${row_count} 2010 census block rows"
}

case "$dataset" in
	cta_stations)
		download_geojson \
			"../output/cta_stations.geojson" \
			"https://data.cityofchicago.org/api/geospatial/3tzw-cg4m?method=export&format=GeoJSON" \
			"Point" \
			100
		;;
	cps_school_locations_sy1415)
		download_geojson \
			"../output/cps_school_locations_sy1415.geojson" \
			"https://data.cityofchicago.org/api/geospatial/mntu-576c?method=export&format=GeoJSON" \
			"Point" \
			500
		;;
	community_areas)
		download_geojson \
			"../output/community_areas.geojson" \
			"https://data.cityofchicago.org/api/geospatial/igwz-8jzy?method=export&format=GeoJSON" \
			"Polygon,MultiPolygon" \
			77
		;;
	city_boundary)
		download_geojson \
			"../output/city_boundary.geojson" \
			"https://data.cityofchicago.org/api/geospatial/qqq8-j68g?method=export&format=GeoJSON" \
			"Polygon,MultiPolygon" \
			1
		;;
	census_blocks_2010)
		download_census_blocks_2010
		;;
	*)
		echo "ERROR: unknown dataset key: ${dataset}" >&2
		exit 1
		;;
esac
