#!/usr/bin/env bash
# Download full residential improvement characteristics from Cook County Assessor.
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds

set -euo pipefail

output_file="../output/residential_improvement_characteristics_full.csv"
metadata_file="../output/residential_improvement_characteristics_full_metadata.csv"
api_csv="https://datacatalog.cookcountyil.gov/resource/x54s-btds.csv"
api_json="https://datacatalog.cookcountyil.gov/resource/x54s-btds.json"
batch_size=500000
where_clause="township_code in('70','71','72','73','74','75','76','77')"
order_clause="pin,year,card,row_id"

tmp_dir=$(mktemp -d "../output/.residential_improvements.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

read_socrata_count() {
    curl --fail --show-error --retry 5 --retry-delay 5 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$api_json" \
        --data-urlencode "\$select=count(*)" \
        --data-urlencode "\$where=${where_clause}" |
        python3 -c 'import json, sys; print(json.load(sys.stdin)[0]["count"])'
}

download_batch() {
    local target_file="$1"
    local offset="$2"
    local attempt
    local rc=1

    for attempt in 1 2 3 4 5; do
        if curl --fail --show-error --retry 3 --retry-delay 5 --retry-connrefused --connect-timeout 60 --max-time 1200 -sG -o "$target_file" "$api_csv" \
            --data-urlencode "\$where=${where_clause}" \
            --data-urlencode "\$order=${order_clause}" \
            --data-urlencode "\$limit=${batch_size}" \
            --data-urlencode "\$offset=${offset}"; then
            return 0
        fi
        rc=$?
        echo "  Download attempt ${attempt} failed at offset ${offset} with curl exit ${rc}; retrying..." >&2
        sleep $(( attempt * 10 ))
    done

    return "$rc"
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
if ! [[ "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "ERROR: Could not read Socrata row count" >&2
    exit 1
fi

tmp_output="$tmp_dir/residential_improvement_characteristics_full.csv"
tmp_metadata="$tmp_dir/residential_improvement_characteristics_full_metadata.csv"
offset=0
batch_index=0
expected_header=""

echo "Downloading full residential improvement characteristics for Chicago townships..."
echo "Filter: ${where_clause}"
echo "Expected records: ${expected_records}"

while (( offset < expected_records )); do
    batch_file="$tmp_dir/batch_${batch_index}.csv"
    download_batch "$batch_file" "$offset"

    records_in_batch=$(count_csv_records "$batch_file")
    batch_header=$(csv_header "$batch_file")
    if (( records_in_batch == 0 )); then
        echo "ERROR: Empty batch at offset ${offset} before expected row count ${expected_records}" >&2
        exit 1
    fi

    if (( batch_index == 0 )); then
        expected_header="$batch_header"
        cp "$batch_file" "$tmp_output"
    else
        if [[ "$batch_header" != "$expected_header" ]]; then
            echo "ERROR: CSV header changed at offset ${offset}" >&2
            exit 1
        fi
        tail -n +2 "$batch_file" >> "$tmp_output"
    fi

    echo "  Downloaded offset ${offset} (${records_in_batch} records)"
    offset=$(( offset + records_in_batch ))
    batch_index=$(( batch_index + 1 ))
done

actual_records=$(count_csv_records "$tmp_output")
if (( actual_records != expected_records )); then
    echo "ERROR: Downloaded ${actual_records} residential improvement rows; expected ${expected_records}" >&2
    exit 1
fi

ending_records=$(read_socrata_count)
if (( ending_records != expected_records )); then
    echo "ERROR: Socrata row count changed during download: started ${expected_records}, ended ${ending_records}" >&2
    exit 1
fi

{
    echo "source_url,downloaded_at_utc,filter,order_clause,batch_size,rows,start_rows,end_rows"
    echo "$api_csv,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${where_clause}\",\"${order_clause}\",$batch_size,$actual_records,$expected_records,$ending_records"
} > "$tmp_metadata"

mv "$tmp_metadata" "$metadata_file"
mv "$tmp_output" "$output_file"

echo "Download complete: ${actual_records} records"
