#!/bin/bash
# Download full residential improvement characteristics from Cook County Assessor
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Single-and-Multi-Family-Improvement-Chara/x54s-btds
# 
# This script downloads ALL properties in Chicago townships (70-77) without any year_built filter.
# The data is paginated due to Socrata API limits (~11.8 million records total).

set -e

OUTPUT_DIR="../output"
OUTPUT_FILE="$OUTPUT_DIR/residential_improvement_characteristics_full.csv"
API_BASE="https://datacatalog.cookcountyil.gov/resource/x54s-btds.csv"
BATCH_SIZE=1000000

# Chicago township codes
TOWNSHIPS="('70','71','72','73','74','75','76','77')"

echo "Downloading full residential improvement characteristics for Chicago townships..."
echo "This will download approximately 11.8 million records via paginated API calls."
echo ""

# First batch - include header
offset=0
echo "Downloading offset $offset..."
curl -s -o "$OUTPUT_FILE" "$API_BASE?\$where=township_code%20in%20$TOWNSHIPS&\$limit=$BATCH_SIZE&\$offset=$offset"

# Check if first download succeeded
if [ ! -s "$OUTPUT_FILE" ]; then
    echo "ERROR: Initial download failed or returned empty file"
    exit 1
fi

# Get count from first batch
first_batch_count=$(wc -l < "$OUTPUT_FILE")
echo "  Downloaded $((first_batch_count - 1)) records"

# Continue with subsequent batches
while true; do
    offset=$((offset + BATCH_SIZE))
    echo "Downloading offset $offset..."
    
    # Download next batch without header
    temp_file=$(mktemp)
    curl -s -o "$temp_file" "$API_BASE?\$where=township_code%20in%20$TOWNSHIPS&\$limit=$BATCH_SIZE&\$offset=$offset"
    
    # Check batch size (excluding header)
    batch_count=$(($(wc -l < "$temp_file") - 1))
    
    if [ "$batch_count" -le 0 ]; then
        echo "  No more records to download"
        rm "$temp_file"
        break
    fi
    
    echo "  Downloaded $batch_count records"
    
    # Append without header
    tail -n +2 "$temp_file" >> "$OUTPUT_FILE"
    rm "$temp_file"
    
    # Safety check - if we got less than batch size, we're done
    if [ "$batch_count" -lt "$BATCH_SIZE" ]; then
        echo "  Reached end of data"
        break
    fi
done

# Final count
total_records=$(($(wc -l < "$OUTPUT_FILE") - 1))
echo ""
echo "Download complete!"
echo "Total records: $total_records"
echo "Output file: $OUTPUT_FILE"
