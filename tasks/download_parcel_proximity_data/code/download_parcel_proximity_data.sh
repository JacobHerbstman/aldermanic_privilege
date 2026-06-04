#!/usr/bin/env bash
# Download the Cook County Assessor parcel proximity slice used for paper geocoding.
# Data source: https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Parcel-Proximity/ydue-e5u3

set -euo pipefail

proximity_year="${1:-2023}"
output_file="../output/parcel_proximity_2023.csv"
api_csv="https://datacatalog.cookcountyil.gov/resource/ydue-e5u3.csv"
api_json="https://datacatalog.cookcountyil.gov/resource/ydue-e5u3.json"
batch_size=1000000
where_clause="year=${proximity_year}"
order_clause="row_id"
select_columns="pin10,year,num_pin_in_half_mile,num_bus_stop_in_half_mile,num_bus_stop_data_year,num_foreclosure_in_half_mile_past_5_years,num_foreclosure_per_1000_pin_past_5_years,num_foreclosure_data_year,num_school_in_half_mile,num_school_data_year,airport_dnl_total,nearest_bike_trail_id,nearest_bike_trail_name,nearest_bike_trail_dist_ft,nearest_bike_trail_data_year,nearest_cemetery_gnis_code,nearest_cemetery_name,nearest_cemetery_dist_ft,nearest_cemetery_data_year,nearest_cta_route_id,nearest_cta_route_name,nearest_cta_route_dist_ft,nearest_cta_route_data_year,nearest_cta_stop_id,nearest_cta_stop_name,nearest_cta_stop_dist_ft,nearest_cta_stop_data_year,nearest_golf_course_id,nearest_golf_course_dist_ft,nearest_golf_course_data_year,nearest_hospital_gnis_code,nearest_hospital_name,nearest_hospital_dist_ft,nearest_hospital_data_year,lake_michigan_dist_ft,lake_michigan_data_year,nearest_major_road_osm_id,nearest_major_road_name,nearest_major_road_dist_ft,nearest_major_road_data_year,nearest_metra_route_id,nearest_metra_route_name,nearest_metra_route_dist_ft,nearest_metra_route_data_year,nearest_metra_stop_id,nearest_metra_stop_name,nearest_metra_stop_dist_ft,nearest_metra_stop_data_year,nearest_park_osm_id,nearest_park_name,nearest_park_dist_ft,nearest_park_data_year,nearest_railroad_id,nearest_railroad_name,nearest_railroad_dist_ft,nearest_railroad_data_year,nearest_secondary_road_osm_id,nearest_secondary_road_name,nearest_secondary_road_dist_ft,nearest_secondary_road_data_year,nearest_water_id,nearest_water_name,nearest_water_dist_ft,nearest_water_data_year,nearest_neighbor_1_pin10,nearest_neighbor_1_dist_ft,nearest_neighbor_2_pin10,nearest_neighbor_2_dist_ft,nearest_neighbor_3_pin10,nearest_neighbor_3_dist_ft,row_id"

tmp_dir=$(mktemp -d "$(dirname "$output_file")/.parcel_proximity.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT

read_socrata_count() {
    curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG "$api_json" \
        --data-urlencode "\$select=count(*)" \
        --data-urlencode "\$where=${where_clause}" |
        python3 -c 'import json, sys; print(json.load(sys.stdin)[0]["count"])'
}

download_batch() {
    local target_file="$1"
    local offset="$2"

    curl --fail --show-error --retry 5 --retry-delay 2 --retry-connrefused --connect-timeout 60 --max-time 600 -sG -o "$target_file" "$api_csv" \
        --data-urlencode "\$select=${select_columns}" \
        --data-urlencode "\$where=${where_clause}" \
        --data-urlencode "\$order=${order_clause}" \
        --data-urlencode "\$limit=${batch_size}" \
        --data-urlencode "\$offset=${offset}"
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

tmp_output="$tmp_dir/parcel_proximity_2023.csv"
offset=0
batch_index=0
expected_header=""

echo "Downloading parcel proximity data from Cook County Open Data..."
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
    echo "ERROR: Downloaded ${actual_records} parcel proximity rows; expected ${expected_records}" >&2
    exit 1
fi

ending_records=$(read_socrata_count)
if (( ending_records != expected_records )); then
    echo "ERROR: Socrata row count changed during download: started ${expected_records}, ended ${ending_records}" >&2
    exit 1
fi

mv "$tmp_output" "$output_file"

echo "Download complete: ${actual_records} records"
