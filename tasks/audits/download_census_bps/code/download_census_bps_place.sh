#!/usr/bin/env bash
# Census Building Permits Survey, place-level annual files for the Midwest region.
# Source: https://www2.census.gov/econ/bps/Place/Midwest%20Region/mw<year>a.txt
#
# Two published layouts exist: files through 2006 carry 38 fields and later files
# carry 41, because the later vintages split the place code and add population.
# The trailing 24 unit columns and the two fields before them are stable in both,
# so identifiers are read from the front and unit counts from the back.
set -euo pipefail

output_file="../output/census_bps_place_midwest.csv"
base_url="https://www2.census.gov/econ/bps/Place/Midwest%20Region"
first_year="$1"
last_year="$2"

tmp_dir=$(mktemp -d "../output/.census_bps.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

for year in $(seq "$first_year" "$last_year"); do
    curl --fail --show-error --silent --retry 5 --retry-delay 2 --retry-connrefused \
        --connect-timeout 60 --max-time 600 \
        -o "$tmp_dir/mw${year}a.txt" "$base_url/mw${year}a.txt"
done

python3 - "$tmp_dir" "$first_year" "$last_year" "$tmp_dir/census_bps_place_midwest.csv" <<'PY'
import csv, os, sys

tmp_dir, first_year, last_year, out_path = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), sys.argv[4]

UNIT_BLOCKS = ["units1", "units2", "units34", "units5plus"]
UNIT_COLUMNS = [
    f"{block}{suffix}_{field}"
    for suffix in ("", "_rep")
    for block in UNIT_BLOCKS
    for field in ("bldgs", "units", "value")
]
COLUMNS = [
    "survey_year", "state_code", "six_digit_id", "county_code",
    "months_reported", "place_name",
] + UNIT_COLUMNS

rows = []
for year in range(first_year, last_year + 1):
    path = os.path.join(tmp_dir, f"mw{year}a.txt")
    with open(path, newline="", encoding="latin-1") as f:
        reader = csv.reader(f)
        header = next(reader, None)
        next(reader, None)
        next(reader, None)
        if header is None or header[0].strip() != "Survey":
            raise SystemExit(f"ERROR: unexpected header in mw{year}a.txt")
        for row in reader:
            if not row or not row[0].strip():
                continue
            if len(row) < len(UNIT_COLUMNS) + 6:
                raise SystemExit(
                    f"ERROR: mw{year}a.txt row has {len(row)} fields; "
                    f"expected at least {len(UNIT_COLUMNS) + 6}"
                )
            units = [v.strip() for v in row[-len(UNIT_COLUMNS):]]
            place_name = row[-len(UNIT_COLUMNS) - 1].strip()
            months = row[-len(UNIT_COLUMNS) - 2].strip()
            rows.append(
                [row[0].strip(), row[1].strip(), row[2].strip(), row[3].strip(), months, place_name]
                + units
            )

if not rows:
    raise SystemExit("ERROR: no Building Permits Survey rows parsed")

years = sorted({r[0] for r in rows})
if len(years) != last_year - first_year + 1:
    raise SystemExit(f"ERROR: expected {last_year - first_year + 1} survey years; parsed {years}")

chicago = [r for r in rows if r[1] == "17" and r[2] == "147700" and r[3] == "031"]
if len(chicago) != len(years):
    raise SystemExit(
        f"ERROR: expected one City of Chicago row per survey year; found {len(chicago)}"
    )
if any(r[5] != "Chicago" for r in chicago):
    raise SystemExit("ERROR: six-digit id 147700 does not resolve to Chicago in every year")

with open(out_path, "w", newline="", encoding="utf-8") as f:
    writer = csv.writer(f)
    writer.writerow(COLUMNS)
    writer.writerows(rows)
print(f"parsed {len(rows)} place-year rows across {len(years)} survey years")
PY

mv "$tmp_dir/census_bps_place_midwest.csv" "$output_file"
