#!/usr/bin/env bash
# Download the Cook County Assessor parcel universe slice used for paper geocoding.
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Parcel-Universe/nj4t-kc8j

set -euo pipefail

parcel_year="${1:-2025}"
triad_name="${2:-City}"
native_csv="../temp/parcel_universe_2025_city_native.csv"
metadata_file="../output/parcel_universe_2025_city_metadata.csv"
api_csv="https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.csv"
api_json="https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"
where_clause="year=${parcel_year} and triad_name='${triad_name}'"
prefix_width=3
order_clause="pin"

tmp_dir=$(mktemp -d "../temp/.parcel_universe.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

read_socrata_count() {
    curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$api_json" \
        --data-urlencode "\$select=count(*)" \
        --data-urlencode "\$where=${where_clause}" |
        python3 -c 'import json, sys; print(json.load(sys.stdin)[0]["count"])'
}

download_slice() {
    local target_file="$1"
    local slice_where="$2"
    local row_limit="$3"

    curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$where=${slice_where}" \
        --data-urlencode "\$order=${order_clause}" \
        --data-urlencode "\$limit=${row_limit}"
}

count_csv_records() {
    local csv_file="$1"
    python3 - "$csv_file" <<'PY'
import csv
import sys

csv.field_size_limit(10**9)
with open(sys.argv[1], newline="", encoding="utf-8") as f:
    reader = csv.reader(f, strict=True)
    header = next(reader, None)
    if not header:
        raise SystemExit("ERROR: missing CSV header")
    ncols = len(header)
    rows = 0
    for line_number, row in enumerate(reader, start=2):
        if len(row) != ncols:
            raise SystemExit(
                f"ERROR: row {line_number} has {len(row)} fields; expected {ncols}"
            )
        rows += 1
print(rows)
PY
}

csv_header() {
    local csv_file="$1"
    python3 - "$csv_file" <<'PY'
import csv
import json
import sys

csv.field_size_limit(10**9)
with open(sys.argv[1], newline="", encoding="utf-8") as f:
    header = next(csv.reader(f, strict=True), None)
    if not header:
        raise SystemExit("ERROR: missing CSV header")
print(json.dumps(header, separators=(",", ":")))
PY
}

prefix_count_total() {
    local csv_file="$1"
    python3 - "$csv_file" <<'PY'
import csv
import sys

with open(sys.argv[1], newline="", encoding="utf-8") as f:
    total = 0
    for row in csv.DictReader(f):
        total += int(row["rows"])
print(total)
PY
}

expected_records=$(read_socrata_count)
if ! [[ "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "ERROR: Could not read Socrata row count" >&2
    exit 1
fi

tmp_output="$tmp_dir/parcel_universe_2025_city_native.csv"
tmp_metadata="$tmp_dir/parcel_universe_2025_city_metadata.csv"
tmp_prefix_counts="$tmp_dir/prefix_counts.csv"

echo "Downloading parcel universe data from Cook County Open Data..."
echo "Filter: ${where_clause}"
echo "Partition: first ${prefix_width} PIN digits"
echo "Expected records: ${expected_records}"

curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$tmp_prefix_counts" "$api_csv" \
    --data-urlencode "\$select=substring(pin,1,${prefix_width}) as pin_prefix, count(*) as rows" \
    --data-urlencode "\$where=${where_clause}" \
    --data-urlencode "\$group=pin_prefix" \
    --data-urlencode "\$order=pin_prefix"

prefix_total=$(prefix_count_total "$tmp_prefix_counts")
if (( prefix_total != expected_records )); then
    echo "ERROR: PIN-prefix counts sum to ${prefix_total}, expected ${expected_records}" >&2
    exit 1
fi

first_slice=1
expected_header=""
while IFS=, read -r pin_prefix slice_rows; do
    pin_prefix="${pin_prefix%$'\r'}"
    pin_prefix="${pin_prefix//\"/}"
    slice_rows="${slice_rows%$'\r'}"
    slice_rows="${slice_rows//\"/}"

    if [[ "$pin_prefix" == "pin_prefix" ]]; then
        continue
    fi

    if ! [[ "$slice_rows" =~ ^[0-9]+$ ]]; then
        echo "ERROR: Invalid row count for PIN prefix ${pin_prefix}: ${slice_rows}" >&2
        exit 1
    fi

    slice_where="${where_clause} and substring(pin,1,${prefix_width}) = '${pin_prefix}'"
    slice_file="$tmp_dir/prefix_${pin_prefix}.csv"
    download_slice "$slice_file" "$slice_where" "$slice_rows"

    slice_actual=$(count_csv_records "$slice_file")
    if (( slice_actual != slice_rows )); then
        echo "ERROR: PIN prefix ${pin_prefix} expected ${slice_rows} records but downloaded ${slice_actual}" >&2
        exit 1
    fi

    slice_header=$(csv_header "$slice_file")
    if (( first_slice == 1 )); then
        expected_header="$slice_header"
        cp "$slice_file" "$tmp_output"
        first_slice=0
    else
        if [[ "$slice_header" != "$expected_header" ]]; then
            echo "ERROR: CSV header changed for PIN prefix ${pin_prefix}" >&2
            exit 1
        fi
        tail -n +2 "$slice_file" >> "$tmp_output"
    fi

    echo "  Downloaded PIN prefix ${pin_prefix}: ${slice_rows} records"
done < "$tmp_prefix_counts"

actual_records=$(count_csv_records "$tmp_output")
if (( actual_records != expected_records )); then
    echo "ERROR: Downloaded ${actual_records} parcel universe rows; expected ${expected_records}" >&2
    exit 1
fi

ending_records=$(read_socrata_count)
if (( ending_records != expected_records )); then
    echo "ERROR: Socrata row count changed during download: started ${expected_records}, ended ${ending_records}" >&2
    exit 1
fi

{
    echo "source_url,downloaded_at_utc,filter,partition,order_clause,rows,start_rows,end_rows"
    echo "$api_csv,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${where_clause}\",\"pin_prefix_${prefix_width}\",\"${order_clause}\",$actual_records,$expected_records,$ending_records"
} > "$tmp_metadata"

mv "$tmp_metadata" "$metadata_file"
mv "$tmp_output" "$native_csv"

echo "Download complete: ${actual_records} records"
