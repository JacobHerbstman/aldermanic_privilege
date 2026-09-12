include ../../shared/code/shell_functions.make

HISTORY_START_YEAR ?= 2006
HISTORY_END_YEAR ?= 2022
MIN_INITIAL_AREA_SQFT ?= 1
MAX_DISTANCE_FT ?= 1500
HISTORICAL_EXACT_MATCH_FT ?= 10
HISTORICAL_LOCAL_MATCH_FT ?= 150
HISTORICAL_MAX_BUILDING_GAP ?= 0.10

all: ../output/density_historical_coordinates.csv

../%/density_historical_building_universe.csv \
	../%/density_historical_coordinate_candidates.csv \
	../%/density_project_lineage.csv: match_historical_parcel_records.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../../setup_environment/code/packages.R \
	../input/residential_discovery_cross_section.csv \
	../input/multifamily_data_cleaned.csv \
	../input/parcel_universe_2025_city.csv \
	../../shared/code/canonical_geometry_helpers.R \
	../input/density_historical_parcel_records.csv \
	../input/historical_address_matches.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	Makefile | ../output ../report
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) $(MIN_INITIAL_AREA_SQFT) $(MAX_DISTANCE_FT) $(HISTORICAL_EXACT_MATCH_FT) $(HISTORICAL_LOCAL_MATCH_FT) $(HISTORICAL_MAX_BUILDING_GAP)

../%/density_parcel_address_selected_history.csv \
	../%/density_parcel_address_lineage_evidence.csv \
	../%/density_historical_coordinates.csv: select_historical_coordinates.R \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/historical_parcel_requests.csv \
	../input/density_historical_address_records.csv \
	../../shared/code/normalize_chicago_address.R \
	../input/historical_address_corrections.csv \
	../output/density_project_lineage.csv \
	../output/density_historical_building_universe.csv \
	../output/density_historical_coordinate_candidates.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/historical_coordinate_year_corrections.csv \
	Makefile | ../output ../report
	$(R) $< $(HISTORICAL_MAX_BUILDING_GAP)

../input/density_historical_address_records.csv: ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/historical_address_corrections.csv: ../../new_construction_cleaning/output/historical_address_corrections.csv | ../input
	ln -sf $< $@

../input/historical_address_matches.csv: ../../new_construction_cleaning/output/historical_address_matches.csv | ../input
	ln -sf $< $@

../input/historical_coordinate_year_corrections.csv: ../../new_construction_cleaning/output/historical_coordinate_year_corrections.csv | ../input
	ln -sf $< $@

../input/historical_parcel_requests.csv: ../../../data_raw/construction_review/historical_parcel_requests_2026-09-04.csv | ../input
	ln -sf $< $@

../input/multifamily_data_cleaned.csv: ../../construction_assessor_records/output/multifamily_data_cleaned.csv | ../input
	ln -sf $< $@

../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/residential_discovery_cross_section.csv: ../../construction_assessor_records/output/residential_discovery_cross_section.csv | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/density_historical_address_records.csv \
	../input/density_historical_parcel_records.csv \
	../input/historical_address_corrections.csv \
	../input/historical_address_matches.csv \
	../input/historical_coordinate_year_corrections.csv \
	../input/historical_parcel_requests.csv \
	../input/multifamily_data_cleaned.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/parcel_universe_2025_city.csv \
	../input/residential_discovery_cross_section.csv \
	../input/ward_pair_boundaries.gpkg \
	../input/ward_panel.gpkg

include ../../shared/code/generic.make
