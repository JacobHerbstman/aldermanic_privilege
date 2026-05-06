#!/bin/bash
# Download the Cook County Assessor parcel sales data used for sales geocoding.
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Parcel-Sales/wvhk-k5uv

set -euo pipefail

output_file="${1:-../output/parcel_sales_city.csv}"
metadata_file="${2:-${output_file%.csv}_metadata.csv}"
api_csv="https://datacatalog.cookcountyil.gov/resource/wvhk-k5uv.csv"
api_json="https://datacatalog.cookcountyil.gov/resource/wvhk-k5uv.json"
batch_size=1000000
where_clause="township_code in('70','71','72','73','74','75','76','77')"
select_columns="pin,year,township_code,nbhd as neighborhood_code,class,sale_date,is_mydec_date,sale_price,doc_no as sale_document_num,deed_type as sale_deed_type,mydec_deed_type,seller_name as sale_seller_name,is_multisale,num_parcels_sale,buyer_name as sale_buyer_name,sale_type,sale_filter_same_sale_within_365,sale_filter_less_than_10k,sale_filter_deed_type,row_id"
temp_output=$(mktemp "$(dirname "$output_file")/.parcel_sales_city.XXXXXX.csv")
temp_metadata=$(mktemp "$(dirname "$metadata_file")/.parcel_sales_city_metadata.XXXXXX.csv")

cleanup() {
    rm -f "$temp_output" "$temp_metadata"
}
trap cleanup EXIT

download_batch() {
    local target_file="$1"
    local offset="$2"

    curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$select=${select_columns}" \
        --data-urlencode "\$where=${where_clause}" \
        --data-urlencode "\$order=row_id" \
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
echo "Downloading parcel sales data from Cook County Open Data..."
echo "Filter: ${where_clause}"
echo "Expected records: ${expected_records}"
download_batch "$temp_output" "$offset"

if [ ! -s "$temp_output" ]; then
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

    tail -n +2 "$temp_file" >> "$temp_output"
    rm "$temp_file"
    echo "  Downloaded offset ${offset}"
done

actual_records=$(($(wc -l < "$temp_output") - 1))
if [ "$actual_records" -ne "$expected_records" ]; then
    echo "ERROR: Expected ${expected_records} records but downloaded ${actual_records}"
    exit 1
fi

{
    echo "source_url,downloaded_at_utc,filter,rows,columns"
    echo "$api_csv,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${where_clause}\",$expected_records,\"${select_columns}\""
} > "$temp_metadata"

mv "$temp_output" "$output_file"
mv "$temp_metadata" "$metadata_file"
trap - EXIT

echo "Download complete: ${expected_records} records"
