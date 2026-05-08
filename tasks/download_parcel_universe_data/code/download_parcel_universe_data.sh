#!/bin/bash
# Download the Cook County Assessor parcel universe slice used for paper geocoding.
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Parcel-Universe/nj4t-kc8j

set -euo pipefail

parcel_year="${1:-2025}"
triad_name="${2:-City}"
output_file="${3:-../output/parcel_universe_2025_city.csv}"
metadata_file="${4:-${output_file%.csv}_metadata.csv}"
api_csv="https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.csv"
api_json="https://datacatalog.cookcountyil.gov/resource/nj4t-kc8j.json"
batch_size=50000
where_clause="year=${parcel_year} and triad_name='${triad_name}'"
order_clause="pin"
tmp_output=$(mktemp "${output_file}.tmp.XXXXXX")
tmp_metadata=$(mktemp "${metadata_file}.tmp.XXXXXX")
temp_file=""

cleanup() {
    rm -f "$tmp_output" "$tmp_metadata"
    if [ -n "$temp_file" ]; then
        rm -f "$temp_file"
    fi
}
trap cleanup EXIT

download_batch() {
    local target_file="$1"
    local offset="$2"

    curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$where=${where_clause}" \
        --data-urlencode "\$order=${order_clause}" \
        --data-urlencode "\$limit=${batch_size}" \
        --data-urlencode "\$offset=${offset}"
}

expected_records=$(curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$api_json" \
    --data-urlencode "\$select=count(*)" \
    --data-urlencode "\$where=${where_clause}" |
    sed -E 's/.*"count":"?([0-9]+)"?.*/\1/')

if ! [[ "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "ERROR: Could not read Socrata row count"
    exit 1
fi

offset=0
echo "Downloading parcel universe data from Cook County Open Data..."
echo "Filter: ${where_clause}"
echo "Order: ${order_clause}"
echo "Expected records: ${expected_records}"
download_batch "$tmp_output" "$offset"

if [ ! -s "$tmp_output" ]; then
    echo "ERROR: Initial download failed or returned empty file"
    exit 1
fi

echo "  Downloaded offset ${offset}"

while true; do
    offset=$((offset + batch_size))
    if [ "$offset" -ge "$expected_records" ]; then
        break
    fi

    temp_file=$(mktemp)
    download_batch "$temp_file" "$offset"

    if [ ! -s "$temp_file" ]; then
        rm "$temp_file"
        echo "ERROR: Download failed at offset ${offset}"
        exit 1
    fi

    tail -n +2 "$temp_file" >> "$tmp_output"
    rm "$temp_file"
    temp_file=""
    echo "  Downloaded offset ${offset}"
done

actual_records=$(($(wc -l < "$tmp_output") - 1))
if [ "$actual_records" -ne "$expected_records" ]; then
    echo "ERROR: Expected ${expected_records} records but downloaded ${actual_records}"
    exit 1
fi

{
    echo "source_url,downloaded_at_utc,filter,order,batch_size,rows"
    echo "$api_csv,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${where_clause}\",\"${order_clause}\",$batch_size,$actual_records"
} > "$tmp_metadata"

mv "$tmp_output" "$output_file"
mv "$tmp_metadata" "$metadata_file"
trap - EXIT

echo "Download complete: ${actual_records} records"
