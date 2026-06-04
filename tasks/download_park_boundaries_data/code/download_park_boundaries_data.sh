#!/usr/bin/env bash
# Download official Chicago Park District park-boundary polygons.

set -euo pipefail

output_file="../output/cpd_park_boundaries.geojson"
api_url="https://data.cityofchicago.org/resource/ejsh-fztr.geojson"
limit=50000

tmp_dir=$(mktemp -d "$(dirname "$output_file")/.cpd_park_boundaries.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

tmp_output="$tmp_dir/cpd_park_boundaries.geojson"

curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 \
	-sG -o "$tmp_output" "$api_url" \
	--data-urlencode "\$limit=${limit}"

feature_count=$(python3 - "$tmp_output" <<'PY'
import json
import sys

with open(sys.argv[1], encoding="utf-8") as f:
    data = json.load(f)

if data.get("type") != "FeatureCollection":
    raise SystemExit("ERROR: downloaded file is not a GeoJSON FeatureCollection")

features = data.get("features")
if not isinstance(features, list) or not features:
    raise SystemExit("ERROR: downloaded GeoJSON contains no features")

bad_geometries = [
    i for i, feature in enumerate(features, start=1)
    if feature.get("geometry", {}).get("type") not in {"Polygon", "MultiPolygon"}
]
if bad_geometries:
    raise SystemExit(
        f"ERROR: {len(bad_geometries)} downloaded features are not polygonal"
    )

print(len(features))
PY
)

mv "$tmp_output" "$output_file"

echo "Downloaded ${feature_count} CPD park-boundary polygons"
