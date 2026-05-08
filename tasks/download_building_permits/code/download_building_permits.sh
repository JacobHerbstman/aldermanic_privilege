#!/usr/bin/env bash
set -euo pipefail

output_file="../output/building_permits.csv"
metadata_file="../output/building_permits_metadata.csv"
api_csv="https://data.cityofchicago.org/resource/ydr8-5enu.csv"
api_json="https://data.cityofchicago.org/resource/ydr8-5enu.json"
batch_size=50000
order_clause="id"

tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/building_permits.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

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
    python3 -c 'import csv, sys; csv.field_size_limit(10**9); f = open(sys.argv[1], newline="", encoding="utf-8"); r = csv.reader(f); next(r, None); print(sum(1 for _ in r)); f.close()' "$csv_file"
}

expected_records=$(curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$api_json" \
    --data-urlencode "\$select=count(*)" |
    sed -E 's/.*"count":"?([0-9]+)"?.*/\1/')

if [[ ! "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "Could not parse expected building permit record count: $expected_records" >&2
    exit 1
fi

tmp_output="$tmp_dir/building_permits.csv"
offset=0
batch_index=0

while (( offset < expected_records )); do
    batch_file="$tmp_dir/batch_${batch_index}.csv"
    download_batch "$offset" "$batch_file"

    records_in_batch=$(count_csv_records "$batch_file")
    if (( records_in_batch == 0 )); then
        echo "Empty batch at offset $offset" >&2
        exit 1
    fi

    if (( batch_index == 0 )); then
        cp "$batch_file" "$tmp_output"
    else
        tail -n +2 "$batch_file" >> "$tmp_output"
    fi

    offset=$(( offset + records_in_batch ))
    batch_index=$(( batch_index + 1 ))
done

actual_records=$(count_csv_records "$tmp_output")
if (( actual_records != expected_records )); then
    echo "Downloaded $actual_records building permit rows; expected $expected_records" >&2
    exit 1
fi

tmp_metadata="$tmp_dir/building_permits_metadata.csv"
{
    echo "source_url,downloaded_at_utc,order_clause,batch_size,records"
    echo "$api_csv,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${order_clause}\",$batch_size,$actual_records"
} > "$tmp_metadata"

mv "$tmp_output" "$output_file"
mv "$tmp_metadata" "$metadata_file"
