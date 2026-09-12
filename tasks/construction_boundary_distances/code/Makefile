include ../../shared/code/shell_functions.make

all: ../output/preferred_new_construction_project_ledger.csv \
	../output/preferred_new_construction_project_components.csv \
	../output/preferred_new_construction_boundary_scope.csv \
	../output/preferred_new_construction_project_centroids.gpkg \
	../output/preferred_new_construction_zoning.csv

../%/preferred_residential_boundary_scope.csv \
	../%/preferred_residential_project_ledger.csv \
	../%/preferred_residential_project_components_final.csv \
	../%/residential_adjudicated_project_geometry.gpkg \
	../%/preferred_residential_project_centroids.gpkg \
	../%/preferred_commercial_project_ledger.csv \
	../%/preferred_commercial_project_component_locations.csv \
	../%/preferred_commercial_project_centroids.gpkg \
	../%/preferred_commercial_boundary_scope.csv: calculate_project_boundary_distances.R \
	../input/new_construction_measurements.csv \
	../input/residential_selected_buildings.csv \
	../input/preferred_project_year_centroids.gpkg \
	../input/preferred_project_year_geometry.gpkg \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../../shared/code/canonical_geometry_helpers.R \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/commercial_reviewed_locations.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_commercial_project_candidates.csv \
	../input/building_permits_for_verification.gpkg \
	../input/parcel_universe_2025_city.csv \
	../input/preferred_commercial_projects.csv \
	../input/preferred_historical_parcel_source_queries.csv \
	Makefile | ../output ../report
	$(R) $<

../%/preferred_new_construction_project_ledger.csv \
	../%/preferred_new_construction_project_components.csv \
	../%/preferred_new_construction_boundary_scope.csv \
	../%/preferred_new_construction_project_centroids.gpkg \
	../%/preferred_new_construction_zoning.csv: assemble_construction_data.R \
	../input/new_construction_measurements.csv \
	../input/residential_additional_candidate_decisions.csv \
	../output/preferred_commercial_boundary_scope.csv \
	../output/preferred_commercial_project_centroids.gpkg \
	../output/preferred_commercial_project_component_locations.csv \
	../output/preferred_commercial_project_ledger.csv \
	../output/preferred_residential_boundary_scope.csv \
	../output/preferred_residential_project_centroids.gpkg \
	../output/preferred_residential_project_components_final.csv \
	../output/preferred_residential_project_ledger.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/corrected_year_zoning_decisions.csv \
	../input/zoning_jan2016.zip \
	../input/zoning_nov2012.zip \
	../input/zoning_sep2014.zip \
	../input/zoning_sep2025.geojson \
	../input/historical_zoning_2006_candidate.gpkg \
	../input/historical_zoning_project_construction_year.csv \
	Makefile | ../output ../report
	$(R) $<

../input/building_permits_for_verification.gpkg: ../../construction_project_permits/output/building_permits_for_verification.gpkg | ../input
	ln -sf $< $@

../input/commercial_reviewed_locations.csv: ../../new_construction_cleaning/output/commercial_reviewed_locations.csv | ../input
	ln -sf $< $@

../input/corrected_year_zoning_decisions.csv: ../../new_construction_cleaning/output/corrected_year_zoning_decisions.csv | ../input
	ln -sf $< $@

../input/historical_zoning_2006_candidate.gpkg: ../../construction_zoning_history/output/historical_zoning_2006_candidate.gpkg | ../input
	ln -sf $< $@

../input/historical_zoning_project_construction_year.csv: ../../construction_zoning_history/output/historical_zoning_project_construction_year.csv | ../input
	ln -sf $< $@

../input/new_construction_measurements.csv: ../../construction_density/output/new_construction_measurements.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_project_candidates.csv: ../../construction_commercial_measurements/output/preferred_commercial_project_candidates.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_projects.csv: ../../construction_commercial_measurements/output/preferred_commercial_projects.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_project_year_centroids.gpkg: ../../construction_project_locations/output/preferred_project_year_centroids.gpkg | ../input
	ln -sf $< $@

../input/preferred_project_year_geometry.gpkg: ../../construction_project_locations/output/preferred_project_year_geometry.gpkg | ../input
	ln -sf $< $@

../input/residential_additional_candidate_decisions.csv: ../../new_construction_cleaning/output/residential_additional_candidate_decisions.csv | ../input
	ln -sf $< $@

../input/residential_selected_buildings.csv: ../../construction_residential_measurements/output/residential_selected_buildings.csv | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/zoning_jan2016.zip: ../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	ln -sf $< $@

../input/zoning_nov2012.zip: ../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	ln -sf $< $@

../input/zoning_sep2014.zip: ../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	ln -sf $< $@

../input/zoning_sep2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_for_verification.gpkg \
	../input/commercial_reviewed_locations.csv \
	../input/corrected_year_zoning_decisions.csv \
	../input/historical_zoning_2006_candidate.gpkg \
	../input/historical_zoning_project_construction_year.csv \
	../input/new_construction_measurements.csv \
	../input/parcel_universe_2025_city.csv \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_commercial_projects.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_historical_parcel_source_queries.csv \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/preferred_project_year_centroids.gpkg \
	../input/preferred_project_year_geometry.gpkg \
	../input/residential_additional_candidate_decisions.csv \
	../input/residential_selected_buildings.csv \
	../input/ward_pair_boundaries.gpkg \
	../input/ward_panel.gpkg \
	../input/zoning_jan2016.zip \
	../input/zoning_nov2012.zip \
	../input/zoning_sep2014.zip \
	../input/zoning_sep2025.geojson

include ../../shared/code/generic.make
