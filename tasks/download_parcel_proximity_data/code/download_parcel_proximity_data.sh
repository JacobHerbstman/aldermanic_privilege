#!/bin/bash
# Download the Cook County Assessor parcel proximity slice used for paper geocoding.
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Parcel-Proximity/ydue-e5u3

set -euo pipefail

proximity_year="${1:-2023}"
output_file="${2:-../output/parcel_proximity_2023.csv}"
metadata_file="${output_file%.csv}_metadata.csv"
api_csv="https://datacatalog.cookcountyil.gov/resource/ydue-e5u3.csv"
api_json="https://datacatalog.cookcountyil.gov/resource/ydue-e5u3.json"
batch_size=1000000
where_clause="year=${proximity_year}"
select_columns="pin10,year,num_pin_in_half_mile,num_bus_stop_in_half_mile,num_bus_stop_data_year,num_foreclosure_in_half_mile_past_5_years,num_foreclosure_per_1000_pin_past_5_years,num_foreclosure_data_year,num_school_in_half_mile,num_school_data_year,airport_dnl_total,nearest_bike_trail_id,nearest_bike_trail_name,nearest_bike_trail_dist_ft,nearest_bike_trail_data_year,nearest_cemetery_gnis_code,nearest_cemetery_name,nearest_cemetery_dist_ft,nearest_cemetery_data_year,nearest_cta_route_id,nearest_cta_route_name,nearest_cta_route_dist_ft,nearest_cta_route_data_year,nearest_cta_stop_id,nearest_cta_stop_name,nearest_cta_stop_dist_ft,nearest_cta_stop_data_year,nearest_golf_course_id,nearest_golf_course_dist_ft,nearest_golf_course_data_year,nearest_hospital_gnis_code,nearest_hospital_name,nearest_hospital_dist_ft,nearest_hospital_data_year,lake_michigan_dist_ft,lake_michigan_data_year,nearest_major_road_osm_id,nearest_major_road_name,nearest_major_road_dist_ft,nearest_major_road_data_year,nearest_metra_route_id,nearest_metra_route_name,nearest_metra_route_dist_ft,nearest_metra_route_data_year,nearest_metra_stop_id,nearest_metra_stop_name,nearest_metra_stop_dist_ft,nearest_metra_stop_data_year,nearest_park_osm_id,nearest_park_name,nearest_park_dist_ft,nearest_park_data_year,nearest_railroad_id,nearest_railroad_name,nearest_railroad_dist_ft,nearest_railroad_data_year,nearest_secondary_road_osm_id,nearest_secondary_road_name,nearest_secondary_road_dist_ft,nearest_secondary_road_data_year,nearest_water_id,nearest_water_name,nearest_water_dist_ft,nearest_water_data_year,nearest_neighbor_1_pin10,nearest_neighbor_1_dist_ft,nearest_neighbor_2_pin10,nearest_neighbor_2_dist_ft,nearest_neighbor_3_pin10,nearest_neighbor_3_dist_ft,row_id"

download_batch() {
    local target_file="$1"
    local offset="$2"

    curl --fail --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$select=${select_columns}" \
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
echo "Downloading parcel proximity data from Cook County Open Data..."
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
