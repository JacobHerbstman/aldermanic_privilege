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
batch_size=1000000
where_clause="year=${parcel_year} and triad_name='${triad_name}'"

download_batch() {
    local target_file="$1"
    local offset="$2"

    curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$where=${where_clause}" \
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
echo "Expected records: ${expected_records}"
download_batch "$output_file" "$offset"

if [ ! -s "$output_file" ]; then
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

    tail -n +2 "$temp_file" >> "$output_file"
    rm "$temp_file"
    echo "  Downloaded offset ${offset}"
done

{
    echo "source_url,downloaded_at_utc,filter,rows"
    echo "$api_csv,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${where_clause}\",$expected_records"
} > "$metadata_file"

echo "Download complete: ${expected_records} records"
