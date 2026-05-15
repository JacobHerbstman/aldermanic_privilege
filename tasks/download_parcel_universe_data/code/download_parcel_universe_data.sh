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
where_clause="year=${parcel_year} and triad_name='${triad_name}'"
prefix_width=3
tmp_output=$(mktemp "${output_file}.tmp.XXXXXX")
tmp_metadata=$(mktemp "${metadata_file}.tmp.XXXXXX")
tmp_prefix_counts=$(mktemp)
temp_file=""

cleanup() {
    rm -f "$tmp_output" "$tmp_metadata" "$tmp_prefix_counts"
    if [ -n "$temp_file" ]; then
        rm -f "$temp_file"
    fi
}
trap cleanup EXIT

download_slice() {
    local target_file="$1"
    local slice_where="$2"
    local row_limit="$3"

    curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$where=${slice_where}" \
        --data-urlencode "\$limit=${row_limit}"
}

expected_records=$(curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$api_json" \
    --data-urlencode "\$select=count(*)" \
    --data-urlencode "\$where=${where_clause}" |
    sed -E 's/.*"count":"?([0-9]+)"?.*/\1/')

if ! [[ "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "ERROR: Could not read Socrata row count"
    exit 1
fi

echo "Downloading parcel universe data from Cook County Open Data..."
echo "Filter: ${where_clause}"
echo "Partition: first ${prefix_width} PIN digits"
echo "Expected records: ${expected_records}"

curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$tmp_prefix_counts" "$api_csv" \
    --data-urlencode "\$select=substring(pin,1,${prefix_width}) as pin_prefix, count(*) as rows" \
    --data-urlencode "\$where=${where_clause}" \
    --data-urlencode "\$group=pin_prefix" \
    --data-urlencode "\$order=pin_prefix"

if [ ! -s "$tmp_prefix_counts" ]; then
    echo "ERROR: PIN-prefix count query failed or returned empty file"
    exit 1
fi

prefix_total=$(awk -F, 'NR > 1 { gsub(/"/, "", $2); sum += $2 } END { print sum + 0 }' "$tmp_prefix_counts")
if [ "$prefix_total" -ne "$expected_records" ]; then
    echo "ERROR: PIN-prefix counts sum to ${prefix_total}, expected ${expected_records}"
    exit 1
fi

first_slice=1
while IFS=, read -r pin_prefix slice_rows; do
    pin_prefix="${pin_prefix%$'\r'}"
    pin_prefix="${pin_prefix//\"/}"
    slice_rows="${slice_rows%$'\r'}"
    slice_rows="${slice_rows//\"/}"

    if [ "$pin_prefix" = "pin_prefix" ]; then
        continue
    fi

    if ! [[ "$slice_rows" =~ ^[0-9]+$ ]]; then
        echo "ERROR: Invalid row count for PIN prefix ${pin_prefix}: ${slice_rows}"
        exit 1
    fi

    slice_where="${where_clause} and substring(pin,1,${prefix_width}) = '${pin_prefix}'"

    temp_file=$(mktemp)
    download_slice "$temp_file" "$slice_where" "$slice_rows"

    if [ ! -s "$temp_file" ]; then
        echo "ERROR: Download failed for PIN prefix ${pin_prefix}"
        exit 1
    fi

    slice_actual=$(($(wc -l < "$temp_file") - 1))
    if [ "$slice_actual" -ne "$slice_rows" ]; then
        echo "ERROR: PIN prefix ${pin_prefix} expected ${slice_rows} records but downloaded ${slice_actual}"
        exit 1
    fi

    if [ "$first_slice" -eq 1 ]; then
        cp "$temp_file" "$tmp_output"
        first_slice=0
    else
        tail -n +2 "$temp_file" >> "$tmp_output"
    fi

    rm "$temp_file"
    temp_file=""
    echo "  Downloaded PIN prefix ${pin_prefix}: ${slice_rows} records"
done < "$tmp_prefix_counts"

actual_records=$(($(wc -l < "$tmp_output") - 1))
if [ "$actual_records" -ne "$expected_records" ]; then
    echo "ERROR: Expected ${expected_records} records but downloaded ${actual_records}"
    exit 1
fi

{
    echo "source_url,downloaded_at_utc,filter,partition,rows"
    echo "$api_csv,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${where_clause}\",\"pin_prefix_${prefix_width}\",$actual_records"
} > "$tmp_metadata"

mv "$tmp_output" "$output_file"
mv "$tmp_metadata" "$metadata_file"
trap - EXIT

echo "Download complete: ${actual_records} records"
