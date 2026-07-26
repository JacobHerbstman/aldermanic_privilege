#!/usr/bin/env bash

set -euo pipefail

printf '%s\n' 'source_name,source_role,path,upstream_target,bytes,modified_at,sha256' > ../output/source_snapshot_manifest.csv

fingerprint() {
    source_name="$1"
    source_role="$2"
    source_path="$3"
    upstream_target=$(readlink "$source_path" 2>/dev/null || printf '%s' "$source_path")
    bytes=$(stat -L -f '%z' "$source_path")
    modified_at=$(stat -L -f '%Sm' -t '%Y-%m-%dT%H:%M:%S%z' "$source_path")
    sha256=$(shasum -a 256 "$source_path" | awk '{print $1}')
    printf '%s,%s,%s,%s,%s,%s,%s\n' \
        "$source_name" "$source_role" "$source_path" "$upstream_target" \
        "$bytes" "$modified_at" "$sha256" >> ../output/source_snapshot_manifest.csv
}

fingerprint commercial_valuation raw ../input/commercial_value_raw.csv
fingerprint residential_improvements raw ../input/residential_improvement_characteristics_full.csv
fingerprint building_permits supporting ../input/building_permits_clean.gpkg
fingerprint parcel_universe_2025 supporting ../input/parcel_universe_2025_city.csv
fingerprint historical_parcel_records supporting ../input/density_historical_exact_parcel_records.csv
fingerprint historical_parcel_addresses supporting ../input/density_parcel_address_selected_history.csv
fingerprint frozen_historical_coordinates supporting ../input/density_historical_coordinates.csv
fingerprint commercial_cross_section current_production ../input/multifamily_data_cleaned.csv
fingerprint residential_cross_section current_production ../input/residential_cross_section.csv
fingerprint geocoded_construction current_production ../input/geocoded_residential_data.gpkg
