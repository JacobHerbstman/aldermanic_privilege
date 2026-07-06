#!/usr/bin/env bash
# Download zoning district descriptions from Second City Zoning.

set -euo pipefail

tmp_dir=$(mktemp -d "../temp/.download_second_city_zoning_data.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

tmp_output="$tmp_dir/zoning-code-summary-district-types.csv"

curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 \
	-sL -o "$tmp_output" \
	"https://secondcityzoning.org/resources/import/zoning-code-summary-district-types.csv"

row_count=$(python3 - "$tmp_output" <<'PY'
import csv
import sys

required = [
    "district_type_code",
    "old_zoning_ordinance_code",
    "zone_type",
    "old_description",
    "juan_description",
    "district_title",
    "zoning_code_section",
    "floor_area_ratio",
    "maximum_building_height",
    "lot_area_per_unit",
    "front_yard_setback",
    "side_setback",
    "rear_yard_setback",
    "rear_yard_open_space",
    "on_site_open_space",
    "minimum_lot_area",
]

with open(sys.argv[1], newline="", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    if reader.fieldnames != required:
        raise SystemExit("ERROR: Second City Zoning CSV header changed")
    rows = list(reader)

if len(rows) != 67:
    raise SystemExit(f"ERROR: expected 67 zoning district rows, found {len(rows)}")

codes = {row["district_type_code"] for row in rows}
for code in ["RS-3", "B3-2", "DX-5", "M2-3", "T"]:
    if code not in codes:
        raise SystemExit(f"ERROR: expected zoning district code {code} is missing")

print(len(rows))
PY
)

mv "$tmp_output" "../output/zoning-code-summary-district-types.csv"

echo "Downloaded ${row_count} Second City Zoning district rows"
