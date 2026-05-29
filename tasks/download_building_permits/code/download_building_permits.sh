#!/usr/bin/env bash
set -euo pipefail

output_file="../output/building_permits.csv"
api_csv="https://data.cityofchicago.org/resource/ydr8-5enu.csv"
api_json="https://data.cityofchicago.org/resource/ydr8-5enu.json"
batch_size=50000
order_clause="id"

tmp_dir=$(mktemp -d "../output/.building_permits.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

read_socrata_count() {
    curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$api_json" \
        --data-urlencode "\$select=count(*)" |
        python3 -c 'import json, sys; print(json.load(sys.stdin)[0]["count"])'
}

download_batch() {
    local offset="$1"
    local target_file="$2"

    curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$limit=$batch_size" \
        --data-urlencode "\$offset=$offset" \
        --data-urlencode "\$order=$order_clause"
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

expected_records=$(read_socrata_count)

if [[ ! "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "ERROR: Could not parse expected building permit record count: $expected_records" >&2
    exit 1
fi

tmp_output="$tmp_dir/building_permits.csv"
offset=0
batch_index=0
expected_header=""

while (( offset < expected_records )); do
    batch_file="$tmp_dir/batch_${batch_index}.csv"
    download_batch "$offset" "$batch_file"

    records_in_batch=$(count_csv_records "$batch_file")
    batch_header=$(csv_header "$batch_file")
    if (( records_in_batch == 0 )); then
        echo "ERROR: Empty batch at offset $offset before expected row count $expected_records" >&2
        exit 1
    fi

    if (( batch_index == 0 )); then
        expected_header="$batch_header"
        cp "$batch_file" "$tmp_output"
    else
        if [[ "$batch_header" != "$expected_header" ]]; then
            echo "ERROR: CSV header changed at offset $offset" >&2
            exit 1
        fi
        tail -n +2 "$batch_file" >> "$tmp_output"
    fi

    offset=$(( offset + records_in_batch ))
    batch_index=$(( batch_index + 1 ))
done

actual_records=$(count_csv_records "$tmp_output")
if (( actual_records != expected_records )); then
    echo "ERROR: Downloaded $actual_records building permit rows; expected $expected_records" >&2
    exit 1
fi

ending_records=$(read_socrata_count)
if (( ending_records != expected_records )); then
    echo "ERROR: Socrata row count changed during download: started $expected_records, ended $ending_records" >&2
    exit 1
fi

mv "$tmp_output" "$output_file"
