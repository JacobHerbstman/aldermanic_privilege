#!/bin/bash
# Download full residential improvement characteristics from Cook County Assessor
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds
# 
# This script downloads ALL properties in Chicago townships (70-77) without any year_built filter.
# The data is paginated due to Socrata API limits (~11.8 million records total).

set -euo pipefail

OUTPUT_DIR="../output"
OUTPUT_FILE="$OUTPUT_DIR/residential_improvement_characteristics_full.csv"
METADATA_FILE="$OUTPUT_DIR/residential_improvement_characteristics_full_metadata.csv"
API_BASE="https://datacatalog.cookcountyil.gov/resource/x54s-btds.csv"
API_JSON="https://datacatalog.cookcountyil.gov/resource/x54s-btds.json"
BATCH_SIZE=500000

# Chicago township codes
WHERE_CLAUSE="township_code in('70','71','72','73','74','75','76','77')"
ORDER_CLAUSE="pin,year,card,row_id"
TMP_OUTPUT=$(mktemp "$OUTPUT_FILE.tmp.XXXXXX")
TMP_METADATA=$(mktemp "$METADATA_FILE.tmp.XXXXXX")
TEMP_BATCH=""

cleanup() {
    rm -f "$TMP_OUTPUT" "$TMP_METADATA"
    if [ -n "$TEMP_BATCH" ]; then
        rm -f "$TEMP_BATCH"
    fi
}
trap cleanup EXIT

download_batch() {
    local target_file="$1"
    local offset="$2"
    local attempt
    local rc

    for attempt in 1 2 3 4 5; do
        if curl --fail --retry 3 --retry-delay 5 --retry-connrefused --connect-timeout 60 --max-time 1200 -sG -o "$target_file" "$API_BASE" \
            --data-urlencode "\$where=${WHERE_CLAUSE}" \
            --data-urlencode "\$order=${ORDER_CLAUSE}" \
            --data-urlencode "\$limit=${BATCH_SIZE}" \
            --data-urlencode "\$offset=${offset}"; then
            return 0
        fi
        rc=$?
        echo "  Download attempt ${attempt} failed at offset ${offset} with curl exit ${rc}; retrying..."
        sleep $((attempt * 10))
    done

    return "$rc"
}

expected_records=$(curl --fail --retry 5 --retry-delay 5 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$API_JSON" \
    --data-urlencode "\$select=count(*)" \
    --data-urlencode "\$where=${WHERE_CLAUSE}" |
    sed -E 's/.*"count":"?([0-9]+)"?.*/\1/')

if ! [[ "$expected_records" =~ ^[0-9]+$ ]]; then
    echo "ERROR: Could not read Socrata row count"
    exit 1
fi

echo "Downloading full residential improvement characteristics for Chicago townships..."
echo "Filter: ${WHERE_CLAUSE}"
echo "Expected records: ${expected_records}"
echo ""

# First batch - include header
offset=0
echo "Downloading offset $offset..."
download_batch "$TMP_OUTPUT" "$offset"

# Check if first download succeeded
if [ ! -s "$TMP_OUTPUT" ]; then
    echo "ERROR: Initial download failed or returned empty file"
    exit 1
fi

# Get count from first batch
first_batch_count=$(wc -l < "$TMP_OUTPUT")
echo "  Downloaded $((first_batch_count - 1)) records"

# Continue with subsequent batches
while [ "$offset" -lt "$expected_records" ]; do
    offset=$((offset + BATCH_SIZE))
    if [ "$offset" -ge "$expected_records" ]; then
        break
    fi
    echo "Downloading offset $offset..."
    
    # Download next batch without header
    TEMP_BATCH=$(mktemp)
    download_batch "$TEMP_BATCH" "$offset"
    
    # Check batch size (excluding header)
    batch_count=$(($(wc -l < "$TEMP_BATCH") - 1))
    
    if [ "$batch_count" -le 0 ]; then
        echo "ERROR: Empty batch at offset ${offset} before expected row count ${expected_records}"
        exit 1
    fi
    
    echo "  Downloaded $batch_count records"
    
    # Append without header
    tail -n +2 "$TEMP_BATCH" >> "$TMP_OUTPUT"
    rm "$TEMP_BATCH"
    TEMP_BATCH=""
    
    # Safety check - if we got less than batch size, we're done
    if [ "$batch_count" -lt "$BATCH_SIZE" ]; then
        echo "  Reached end of data"
        break
    fi
done

# Final count
total_records=$(($(wc -l < "$TMP_OUTPUT") - 1))
if [ "$total_records" -ne "$expected_records" ]; then
    echo "ERROR: Expected ${expected_records} records but downloaded ${total_records}"
    exit 1
fi

{
    echo "source_url,downloaded_at_utc,filter,order,batch_size,rows"
    echo "$API_BASE,$(date -u +%Y-%m-%dT%H:%M:%SZ),\"${WHERE_CLAUSE}\",\"${ORDER_CLAUSE}\",$BATCH_SIZE,$total_records"
} > "$TMP_METADATA"

mv "$TMP_OUTPUT" "$OUTPUT_FILE"
mv "$TMP_METADATA" "$METADATA_FILE"
trap - EXIT
echo ""
echo "Download complete!"
echo "Total records: $total_records"
echo "Output file: $OUTPUT_FILE"
