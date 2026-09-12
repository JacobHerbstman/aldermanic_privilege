include ../../shared/code/shell_functions.make

all: ../output/preferred_project_year_geometry.gpkg

../%/preferred_address_geocode_requests.csv \
	../%/preferred_historical_address_geocodes.csv \
	../%/preferred_chicago_address_geocodes.csv \
	../%/preferred_predecessor_reference_points.csv \
	../%/preferred_historical_predecessor_selected.gpkg \
	../%/preferred_historical_predecessor_resolution.csv: match_project_addresses_and_predecessors.R \
	../input/preferred_historical_parcel_coverage.csv \
	../input/density_historical_coordinates.csv \
	../input/density_parcel_address_selected_history.csv \
	../input/parcel_universe_2025_city.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/address_geocodes_census.csv \
	../../shared/code/normalize_chicago_address.R \
	../input/address_geocodes_chicago.csv \
	../input/new_construction_exact_permit_matches.csv \
	../input/predecessor_parcel_history.csv \
	../input/geocoding_parcel_history.csv \
	../input/history_reference_parcels.gpkg \
	../input/history_reference_queries.csv \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/preferred_predecessor_source_queries.csv \
	Makefile | ../output ../report
	$(R) $<

../%/preferred_project_year_geometry.gpkg \
	../%/preferred_project_component_geometry.gpkg \
	../%/preferred_project_year_centroids.gpkg \
	../%/preferred_project_year_geometry_coverage.csv \
	../%/preferred_project_boundary_scope.csv \
	../%/preferred_adjudication_scope.csv: locate_construction_projects.R \
	../output/preferred_chicago_address_geocodes.csv \
	../input/construction_modifications.csv \
	../input/residential_assessor_history.parquet \
	../../shared/code/assessor_classification.R \
	../input/residential_reviewed_parcel_locations.csv \
	../input/parcel_universe_2025_city.csv \
	../input/residential_reviewed_permit_locations.csv \
	../input/building_permits_for_verification.gpkg \
	../input/preferred_historical_parcel_coverage.csv \
	../input/preferred_historical_parcels.gpkg \
	../output/preferred_historical_predecessor_selected.gpkg \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/density_historical_parcel_records.csv \
	../input/geocoding_parcel_history.csv \
	../input/predecessor_parcel_history.csv \
	../input/preferred_residential_project_candidates.csv \
	../input/ward_pair_boundaries.gpkg \
	../input/ward_panel.gpkg \
	../input/commercial_adjudication_queue.csv \
	../input/residential_adjudication_queue.csv \
	../../shared/code/canonical_geometry_helpers.R \
	Makefile | ../output ../report
	$(R) $<

../input/address_geocodes_census.csv: ../../download_construction_address_geocodes/output/address_geocodes_census.csv | ../input
	ln -sf $< $@

../input/address_geocodes_chicago.csv: ../../download_construction_address_geocodes/output/address_geocodes_chicago.csv | ../input
	ln -sf $< $@

../input/building_permits_for_verification.gpkg: ../../construction_project_permits/output/building_permits_for_verification.gpkg | ../input
	ln -sf $< $@

../input/commercial_adjudication_queue.csv: ../../construction_commercial_measurements/output/commercial_adjudication_queue.csv | ../input
	ln -sf $< $@

../input/construction_modifications.csv: ../../new_construction_cleaning/output/construction_modifications.csv | ../input
	ln -sf $< $@

../input/density_historical_coordinates.csv: ../../construction_historical_coordinates/output/density_historical_coordinates.csv | ../input
	ln -sf $< $@

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/density_parcel_address_selected_history.csv: ../../construction_historical_coordinates/output/density_parcel_address_selected_history.csv | ../input
	ln -sf $< $@

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	ln -sf $< $@

../input/history_reference_parcels.gpkg: ../../download_construction_historical_parcels/output/history_reference_parcels.gpkg | ../input
	ln -sf $< $@

../input/history_reference_queries.csv: ../../download_construction_historical_parcels/output/history_reference_queries.csv | ../input
	ln -sf $< $@

../input/new_construction_exact_permit_matches.csv: ../../construction_project_permits/output/new_construction_exact_permit_matches.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/predecessor_parcel_history.csv: ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_coverage.csv: ../../construction_historical_sites/output/preferred_historical_parcel_coverage.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcels.gpkg: ../../construction_historical_sites/output/preferred_historical_parcels.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_predecessor_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_residential_project_candidates.csv: ../../construction_residential_measurements/output/preferred_residential_project_candidates.csv | ../input
	ln -sf $< $@

../input/residential_adjudication_queue.csv: ../../construction_residential_measurements/output/residential_adjudication_queue.csv | ../input
	ln -sf $< $@

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

../input/residential_reviewed_parcel_locations.csv: ../../new_construction_cleaning/output/residential_reviewed_parcel_locations.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_permit_locations.csv: ../../new_construction_cleaning/output/residential_reviewed_permit_locations.csv | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/address_geocodes_census.csv \
	../input/address_geocodes_chicago.csv \
	../input/building_permits_for_verification.gpkg \
	../input/commercial_adjudication_queue.csv \
	../input/construction_modifications.csv \
	../input/density_historical_coordinates.csv \
	../input/density_historical_parcel_records.csv \
	../input/density_parcel_address_selected_history.csv \
	../input/geocoding_parcel_history.csv \
	../input/history_reference_parcels.gpkg \
	../input/history_reference_queries.csv \
	../input/new_construction_exact_permit_matches.csv \
	../input/parcel_universe_2025_city.csv \
	../input/predecessor_parcel_history.csv \
	../input/preferred_historical_parcel_coverage.csv \
	../input/preferred_historical_parcels.gpkg \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/preferred_predecessor_source_queries.csv \
	../input/preferred_residential_project_candidates.csv \
	../input/residential_adjudication_queue.csv \
	../input/residential_assessor_history.parquet \
	../input/residential_reviewed_parcel_locations.csv \
	../input/residential_reviewed_permit_locations.csv \
	../input/ward_pair_boundaries.gpkg \
	../input/ward_panel.gpkg

include ../../shared/code/generic.make
