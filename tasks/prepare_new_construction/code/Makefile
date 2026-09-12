include ../../shared/code/shell_functions.make

FIRST_CANDIDATE_YEAR := 1999
PREFERRED_ASSESSMENT_YEAR := 2022
FALLBACK_ASSESSMENT_YEAR := 2025
FIRST_CONSTRUCTION_YEAR := 2006
LAST_CONSTRUCTION_YEAR := 2022
EPISODE_YEAR_WINDOW := 2
MAXIMUM_BUILDING_GAP := 0.02
SUCCESSOR_POINT_TOLERANCE_FT := 1
SUCCESSOR_LAND_TOLERANCE := 0.005
EARLIER_COMMERCIAL_YEAR := 2021
PERMIT_APPLICATION_YEARS_BEFORE := 6
PERMIT_ISSUE_YEARS_BEFORE := 4
PERMIT_YEARS_AFTER := 2
PRORATION_TOLERANCE := 0.001
LOCATION_ROUNDING_FT := 0.001
BOUNDARY_WINDOW_FT := 1500
MAIN_BOUNDARY_WINDOW_FT := 500

all: ../output/assessor_buildings.csv ../output/assessor_measurement_records.csv \
	../output/building_permit_evidence.csv ../output/building_permits_for_verification.gpkg \
	../output/preferred_new_construction_project_ledger.csv \
	../output/preferred_new_construction_project_components.csv \
	../output/preferred_new_construction_boundary_scope.csv \
	../output/preferred_new_construction_project_centroids.gpkg \
	../output/preferred_project_year_geometry.gpkg

../%/preferred_new_construction_project_ledger.csv ../%/preferred_new_construction_project_components.csv ../%/preferred_new_construction_boundary_scope.csv ../%/preferred_new_construction_project_centroids.gpkg ../%/preferred_project_year_geometry.gpkg: build_construction_data.R Makefile \
	../output/assessor_buildings.csv ../output/assessor_measurement_records.csv \
	../output/building_permit_evidence.csv ../output/building_permits_for_verification.gpkg \
	../input/parcel_universe_2025_city.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/density_historical_address_records.csv \
	../input/density_historical_parcel_records.csv \
	../input/geocoding_parcel_history.csv \
	../input/predecessor_parcel_history.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/history_reference_parcels.gpkg \
	../input/address_geocodes_chicago.csv \
	../input/address_geocodes_census.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/construction_condominium_history.csv \
	../input/recorded_building_changes.csv ../../setup_environment/code/packages.R \
	../../shared/code/assessor_classification.R ../../shared/code/canonical_geometry_helpers.R \
	../../shared/code/normalize_chicago_address.R ../../shared/code/save_data.R \
	../../shared/code/report.py | ../output ../report
	$(R) $< $(FIRST_CONSTRUCTION_YEAR) $(LAST_CONSTRUCTION_YEAR) $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR) $(EPISODE_YEAR_WINDOW) $(MAXIMUM_BUILDING_GAP) $(SUCCESSOR_POINT_TOLERANCE_FT) $(SUCCESSOR_LAND_TOLERANCE) $(LOCATION_ROUNDING_FT) $(BOUNDARY_WINDOW_FT) $(MAIN_BOUNDARY_WINDOW_FT)

../%/assessor_buildings.csv ../%/assessor_measurement_records.csv ../%/building_permit_evidence.csv ../%/building_permits_for_verification.gpkg: select_assessor_buildings.R Makefile \
	../input/residential_assessor_history.parquet ../input/commercial_valuation_data.csv \
	../input/building_permits_full.csv ../input/historical_project_parcel_source.gpkg \
	../input/historical_predecessor_parcel_source.gpkg ../input/predecessor_parcel_history.csv \
	../../shared/code/permit_unit_patterns.R \
	../../setup_environment/code/packages.R ../../shared/code/assessor_classification.R \
	../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $(FIRST_CANDIDATE_YEAR) $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR) $(FIRST_CONSTRUCTION_YEAR) $(LAST_CONSTRUCTION_YEAR) $(EARLIER_COMMERCIAL_YEAR) $(PERMIT_APPLICATION_YEARS_BEFORE) $(PERMIT_ISSUE_YEARS_BEFORE) $(PERMIT_YEARS_AFTER) $(PRORATION_TOLERANCE)

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

../input/commercial_valuation_data.csv: ../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@

../input/building_permits_full.csv: ../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@

../input/recorded_building_changes.csv: ../../new_construction_cleaning/output/recorded_building_changes.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@

../input/density_historical_address_records.csv: ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	ln -sf $< $@

../input/predecessor_parcel_history.csv: ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/construction_condominium_history.csv: ../../download_construction_assessor_history/output/construction_condominium_history.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_project_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/historical_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/history_reference_parcels.gpkg: ../../download_construction_historical_parcels/output/history_reference_parcels.gpkg | ../input
	ln -sf $< $@

../input/address_geocodes_chicago.csv: ../../download_construction_address_geocodes/output/address_geocodes_chicago.csv | ../input
	ln -sf $< $@

../input/address_geocodes_census.csv: ../../download_construction_address_geocodes/output/address_geocodes_census.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/history_reference_parcels.gpkg \
	../input/address_geocodes_chicago.csv \
	../input/address_geocodes_census.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/historical_project_parcel_source.gpkg ../input/historical_predecessor_parcel_source.gpkg \
	../input/residential_assessor_history.parquet ../input/commercial_valuation_data.csv \
	../input/building_permits_full.csv ../input/recorded_building_changes.csv \
	../input/parcel_universe_2025_city.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/density_historical_address_records.csv \
	../input/density_historical_parcel_records.csv \
	../input/geocoding_parcel_history.csv \
	../input/predecessor_parcel_history.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/construction_condominium_history.csv

include ../../shared/code/generic.make
