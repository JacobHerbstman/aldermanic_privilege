#!/bin/bash
# Download Cook County Assessor commercial valuation data.
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Commercial-Valuation-Data/csik-bsws

set -e

OUTPUT_DIR="../output"
OUTPUT_FILE="$OUTPUT_DIR/commercial_valuation_data.csv"
METADATA_FILE="$OUTPUT_DIR/commercial_valuation_data_metadata.csv"
API_BASE="https://datacatalog.cookcountyil.gov/resource/csik-bsws.csv"
COUNT_URL="https://datacatalog.cookcountyil.gov/resource/csik-bsws.json?\$select=count(*)"
BATCH_SIZE=50000

expected_records=$(curl -s "$COUNT_URL" | sed -E 's/.*"count":"?([0-9]+)"?.*/\1/')
if ! [[ "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "ERROR: Could not read Socrata row count"
    exit 1
fi

offset=0
echo "Downloading commercial valuation data from Cook County Open Data..."
echo "Expected records: $expected_records"
curl -s -o "$OUTPUT_FILE" "$API_BASE?\$limit=$BATCH_SIZE&\$offset=$offset"

if [ ! -s "$OUTPUT_FILE" ]; then
    echo "ERROR: Initial download failed or returned empty file"
    exit 1
fi

echo "  Downloaded offset $offset"

while true; do
    offset=$((offset + BATCH_SIZE))
    if [ "$offset" -ge "$expected_records" ]; then
        break
    fi

    temp_file=$(mktemp)
    curl -s -o "$temp_file" "$API_BASE?\$limit=$BATCH_SIZE&\$offset=$offset"

    if [ ! -s "$temp_file" ]; then
        rm "$temp_file"
        break
    fi

    tail -n +2 "$temp_file" >> "$OUTPUT_FILE"
    rm "$temp_file"
    echo "  Downloaded offset $offset"
done

{
    echo "source_url,downloaded_at_utc,rows"
    echo "$API_BASE,$(date -u +%Y-%m-%dT%H:%M:%SZ),$expected_records"
} > "$METADATA_FILE"

echo "Download complete: $expected_records records"
