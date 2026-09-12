include ../../shared/code/shell_functions.make

EPISODE_YEAR_WINDOW := 2
AUTOMATIC_CARD_BUILDING_GAP := 0.02
SUCCESSOR_POINT_TOLERANCE_FT := 1
SUCCESSOR_LAND_TOLERANCE := 0.005
MAX_DISCOVERY_CONSTRUCTION_YEAR ?= 2026
MIN_CONSTRUCTION_YEAR ?= 1999
PREFERRED_ASSESSMENT_YEAR ?= 2022
FALLBACK_ASSESSMENT_YEAR ?= 2025
HISTORY_START_YEAR ?= 2006
HISTORY_END_YEAR ?= 2022
MIN_INITIAL_AREA_SQFT ?= 1
MAX_DISTANCE_FT ?= 1500
HISTORICAL_EXACT_MATCH_FT ?= 10
HISTORICAL_LOCAL_MATCH_FT ?= 150
HISTORICAL_MAX_BUILDING_GAP ?= 0.10

all: ../output/preferred_new_construction_project_ledger.csv \
	../output/preferred_new_construction_project_components.csv \
	../output/preferred_new_construction_boundary_scope.csv \
	../output/preferred_new_construction_project_centroids.gpkg \
	../output/preferred_new_construction_zoning.csv

../%/preferred_historical_address_geocodes.csv \
	../%/preferred_chicago_address_geocodes.csv: read_address_geocodes.R \
	../output/preferred_address_geocode_requests.csv ../input/address_geocodes_census.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/normalize_chicago_address.R \
	Makefile ../input/address_geocodes_chicago.csv | ../output ../report
	$(R) $<

../%/historical_project_parcel_requests.csv \
	../%/historical_project_parcel_coverage.csv \
	../%/missing_project_reference_points.csv \
	../%/historical_project_predecessor_resolution.csv \
	../%/historical_project_year_geometry.gpkg: build_historical_project_geography.R \
	../output/new_construction_project_components.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../input/historical_project_parcel_source.gpkg \
	../input/historical_project_parcel_source_queries.csv ../input/predecessor_parcel_history.csv ../input/predecessor_history_queries.csv ../input/historical_predecessor_parcel_source.gpkg \
	../input/historical_predecessor_source_queries.csv | ../output ../report
	$(R) $<

../%/residential_selected_buildings.csv \
	../%/residential_review_source_dispositions.csv: select_residential_buildings.R ../output/preferred_commercial_projects.csv ../input/residential_additional_candidate_decisions.csv ../output/preferred_residential_project_candidates.csv ../output/residential_tieback_episode_resolution.csv ../output/residential_class297_resolution.csv ../output/residential_overlap_resolution.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/preferred_residential_boundary_scope.csv \
	../%/preferred_residential_project_ledger.csv \
	../%/preferred_residential_project_components_final.csv \
	../%/residential_adjudicated_project_geometry.gpkg \
	../%/preferred_residential_project_centroids.gpkg: build_residential_final_geography.R ../output/new_construction_measurements.csv ../output/residential_selected_buildings.csv ../output/preferred_project_year_centroids.gpkg ../output/preferred_project_year_geometry.gpkg ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/canonical_geometry_helpers.R ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg Makefile | ../output ../report
	$(R) $<

../%/preferred_new_construction_project_ledger.csv \
	../%/preferred_new_construction_project_components.csv \
	../%/preferred_new_construction_boundary_scope.csv \
	../%/preferred_new_construction_project_centroids.gpkg: build_preferred_new_construction_ledger.R ../output/new_construction_measurements.csv ../input/residential_additional_candidate_decisions.csv ../output/preferred_commercial_boundary_scope.csv ../output/preferred_commercial_project_centroids.gpkg ../output/preferred_commercial_project_component_locations.csv ../output/preferred_commercial_project_ledger.csv ../output/preferred_residential_boundary_scope.csv ../output/preferred_residential_project_centroids.gpkg ../output/preferred_residential_project_components_final.csv ../output/preferred_residential_project_ledger.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/residential_class297_resolution.csv \
	../%/residential_class297_source_disposition.csv: build_residential_class297_resolution.R ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/residential_project_candidate_inventory.csv ../input/construction_condominium_history.csv ../output/preferred_residential_project_candidates.csv ../output/residential_successor_condo_requests.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/residential_tieback_episode_resolution.csv: build_residential_tieback_episode_resolution.R Makefile ../input/residential_assessor_history.parquet ../output/preferred_residential_project_candidates.csv ../output/preferred_project_year_geometry.gpkg ../output/preferred_project_year_centroids.gpkg ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $(EPISODE_YEAR_WINDOW) $(AUTOMATIC_CARD_BUILDING_GAP) $(SUCCESSOR_LAND_TOLERANCE)

../output/residential_overlap_resolution.csv: build_residential_overlap_resolution.R ../output/preferred_commercial_project_candidates.csv ../output/preferred_commercial_source_disposition.csv ../input/residential_overlap_decisions.csv ../output/preferred_commercial_projects.csv ../output/preferred_residential_project_candidates.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/preferred_new_construction_zoning.csv: build_preferred_construction_zoning.R Makefile ../input/corrected_year_zoning_decisions.csv ../input/zoning_jan2016.zip ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_sep2025.geojson ../input/historical_zoning_2006_candidate.gpkg ../input/historical_zoning_project_construction_year.csv ../output/preferred_new_construction_boundary_scope.csv ../output/preferred_new_construction_project_centroids.gpkg ../output/preferred_new_construction_project_components.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/new_construction_measurements.csv: calculate_construction_density.R ../output/residential_selected_buildings.csv ../output/preferred_commercial_projects.csv ../input/construction_building_types.csv ../../shared/code/assessor_classification.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/preferred_commercial_project_ledger.csv \
	../%/preferred_commercial_project_component_locations.csv \
	../%/preferred_commercial_project_centroids.gpkg \
	../%/preferred_commercial_boundary_scope.csv: build_preferred_commercial_final_geography.R ../output/new_construction_measurements.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/commercial_reviewed_locations.csv ../input/preferred_historical_parcel_source.gpkg ../output/preferred_commercial_project_candidates.csv ../output/preferred_project_year_centroids.gpkg ../output/building_permits_for_verification.gpkg ../input/parcel_universe_2025_city.csv ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/preferred_commercial_projects.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/canonical_geometry_helpers.R ../input/preferred_historical_parcel_source_queries.csv Makefile | ../output ../report
	$(R) $<

../output/building_permits_for_verification.gpkg: build_construction_permit_history.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../input/building_permits_full.csv Makefile | ../output ../report
	$(R) $<

../%/residential_project_candidate_inventory.csv \
	../%/residential_project_history_summary.csv \
	../%/residential_fractional_base_groups.csv \
	../%/residential_tieback_groups_full.csv \
	../%/residential_tieback_members_full.csv \
	../%/residential_multicard_cards.csv: build_residential_project_candidates.R ../input/residential_reviewed_card_selections.csv ../input/residential_assessor_history.parquet ../output/geocoded_residential_data.gpkg ../output/multifamily_data_cleaned.csv ../output/construction_parcel_boundary_distances.csv ../output/residential_cross_section.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $< $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR)

../%/project_permit_chain_links.csv \
	../%/project_permit_chain_unit_mentions.csv \
	../%/project_permit_chain_summary.csv: build_permit_revision_evidence.R ../output/building_permits_for_verification.gpkg ../output/new_construction_exact_permit_matches.csv ../output/new_construction_spatial_permit_matches.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/permit_unit_patterns.R Makefile | ../output ../report
	$(R) $<

../output/geocoded_residential_data.gpkg: geocode_residential_data.R ../output/residential_cross_section.csv ../output/multifamily_data_cleaned.csv ../output/density_historical_coordinates.csv ../input/parcel_universe_2025_city.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/construction_discovery_ward_maps.gpkg \
	../%/construction_discovery_boundaries.gpkg: build_construction_discovery_geography.R ../../shared/code/canonical_geometry_helpers.R ../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/construction_parcel_boundary_distances.csv: build_construction_parcel_distances.R ../../shared/code/save_data.R ../../shared/code/report.py ../output/construction_discovery_ward_maps.gpkg ../output/construction_discovery_boundaries.gpkg ../output/geocoded_residential_data.gpkg ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R Makefile | ../output ../report
	$(R) $< $(MIN_CONSTRUCTION_YEAR) $(MAX_DISCOVERY_CONSTRUCTION_YEAR)

../output/commercial_post_evidence_resolution.csv: select_commercial_evidence_rules.R ../input/commercial_manual_decisions.csv ../output/commercial_family_vintage_summary.csv ../output/project_permit_chain_links.csv ../output/commercial_address_permit_history.csv ../output/new_construction_spatial_permit_matches.csv ../output/preferred_project_boundary_scope.csv ../output/preferred_adjudication_scope.csv ../output/preferred_commercial_project_candidates.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/preferred_commercial_projects.csv \
	../%/preferred_commercial_source_disposition.csv: build_preferred_commercial_ledger.R ../input/commercial_reported_land_decisions.csv ../input/commercial_component_overrides.csv ../input/commercial_cross_family_decisions.csv ../input/construction_modifications.csv ../input/commercial_manual_decisions.csv ../output/commercial_entity_version_candidates.csv ../output/commercial_post_evidence_resolution.csv ../output/preferred_commercial_project_candidates.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/commercial_address_permit_history.csv: build_commercial_address_permit_history.R ../output/building_permits_for_verification.gpkg ../output/commercial_ground_up_evidence.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/new_construction_spatial_permit_matches.csv: build_spatial_permit_evidence.R ../output/building_permits_for_verification.gpkg ../output/historical_project_year_geometry.gpkg ../output/new_construction_exact_permit_matches.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/residential_tieback_temporal_lineage_evidence.csv \
	../%/residential_tieback_temporal_snapshots.csv: build_residential_tieback_temporal_evidence.R ../input/residential_assessor_history.parquet ../output/residential_fractional_base_groups.csv ../output/residential_project_candidate_inventory.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/residential_assessor_project_candidates.csv: build_residential_assessor_projects.R ../input/construction_modifications.csv ../input/residential_reviewed_source_duplicates.csv ../input/residential_reviewed_building_components.csv ../input/residential_reviewed_source_exclusions.csv ../input/residential_assessor_history.parquet ../input/residential_reviewed_home_replacements.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/assessor_classification.R ../output/residential_project_candidate_inventory.csv ../output/residential_tieback_temporal_lineage_evidence.csv ../output/residential_multicard_cards.csv Makefile ../input/parcel_universe_2025_city.csv ../input/preferred_historical_parcel_source.gpkg ../input/preferred_predecessor_parcel_source.gpkg ../input/density_historical_address_records.csv ../input/density_historical_parcel_records.csv ../input/geocoding_parcel_history.csv ../input/parcel_addresses_2025_chicago.csv ../input/predecessor_parcel_history.csv | ../output ../report
	$(R) $< $(AUTOMATIC_CARD_BUILDING_GAP) $(EPISODE_YEAR_WINDOW) $(SUCCESSOR_POINT_TOLERANCE_FT) $(SUCCESSOR_LAND_TOLERANCE)

../output/residential_assessor_project_candidates.csv: ../input/geocoding_parcel_history.csv ../input/predecessor_parcel_history.csv ../input/density_historical_parcel_records.csv ../input/density_historical_address_records.csv ../input/parcel_addresses_2025_chicago.csv


../output/residential_selected_assessments.csv: select_residential_assessments.R ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/residential_assessor_project_candidates.csv ../output/residential_project_candidate_inventory.csv ../output/residential_tieback_temporal_lineage_evidence.csv ../../shared/code/assessor_classification.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/preferred_residential_project_candidates.csv \
	../%/preferred_residential_project_components.csv \
	../%/residential_adjudication_queue.csv: apply_residential_building_corrections.R ../output/residential_selected_assessments.csv ../input/construction_modifications.csv ../input/residential_source_decisions.csv ../output/building_permits_for_verification.gpkg ../input/residential_unresolved_source_dispositions.csv ../input/construction_condominium_history.csv ../input/residential_class297_component_overrides.csv ../input/residential_overlap_decisions.csv ../input/eligibility_manual_exceptions.csv ../output/project_permit_chain_links.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/commercial_entity_version_candidates.csv \
	../%/commercial_address_family_candidates.csv \
	../%/commercial_entity_component_candidates.csv \
	../%/commercial_family_vintage_summary.csv \
	../%/commercial_production_family_members.csv \
	../%/commercial_project_family_review.csv: build_commercial_project_candidates.R ../input/commercial_value_raw.csv ../output/construction_parcel_boundary_distances.csv ../output/multifamily_data_cleaned.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/new_construction_exact_permit_matches.csv \
	../%/new_construction_permit_unit_mentions.csv \
	../%/new_construction_project_components.csv: build_new_construction_permit_evidence.R ../output/building_permits_for_verification.gpkg ../output/commercial_entity_component_candidates.csv ../output/commercial_production_family_members.csv ../output/residential_project_candidate_inventory.csv ../output/residential_tieback_groups_full.csv ../output/residential_tieback_members_full.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/permit_unit_patterns.R Makefile | ../output ../report
	$(R) $<

../%/preferred_commercial_project_candidates.csv \
	../%/preferred_commercial_project_components.csv \
	../%/commercial_adjudication_queue.csv: build_preferred_commercial_candidates.R ../input/commercial_verified_case_review.csv ../output/commercial_address_family_candidates.csv ../output/commercial_entity_version_candidates.csv ../output/commercial_family_vintage_summary.csv ../output/commercial_project_family_review.csv ../output/project_permit_chain_links.csv ../output/project_permit_chain_summary.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/preferred_project_year_geometry.gpkg \
	../%/preferred_project_component_geometry.gpkg \
	../%/preferred_project_year_centroids.gpkg \
	../%/preferred_project_year_geometry_coverage.csv: build_preferred_project_geography.R ../output/preferred_chicago_address_geocodes.csv ../input/construction_modifications.csv ../input/residential_assessor_history.parquet ../../shared/code/assessor_classification.R ../input/residential_reviewed_parcel_locations.csv ../input/parcel_universe_2025_city.csv ../input/residential_reviewed_permit_locations.csv ../output/building_permits_for_verification.gpkg ../output/preferred_historical_parcel_coverage.csv ../output/preferred_historical_parcels.gpkg ../output/preferred_historical_predecessor_selected.gpkg ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../input/density_historical_parcel_records.csv ../input/geocoding_parcel_history.csv ../input/predecessor_parcel_history.csv | ../output ../report
	$(R) $<

../%/preferred_project_year_geometry.gpkg \
	../%/preferred_project_component_geometry.gpkg \
	../%/preferred_project_year_centroids.gpkg \
	../%/preferred_project_year_geometry_coverage.csv: ../output/preferred_residential_project_candidates.csv ../input/predecessor_parcel_history.csv ../input/geocoding_parcel_history.csv ../input/density_historical_parcel_records.csv


../%/preferred_project_boundary_scope.csv \
	../%/preferred_adjudication_scope.csv: build_preferred_boundary_scope.R ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/commercial_adjudication_queue.csv ../output/preferred_project_year_centroids.gpkg ../output/preferred_project_year_geometry_coverage.csv ../output/residential_adjudication_queue.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/canonical_geometry_helpers.R Makefile | ../output ../report
	$(R) $<

../output/preferred_project_geography_requests.csv: build_preferred_geography_requests.R ../output/preferred_commercial_project_candidates.csv ../output/preferred_residential_project_candidates.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/preferred_predecessor_reference_points.csv: build_preferred_predecessor_reference_points.R ../input/parcel_universe_2025_city.csv ../output/density_historical_coordinates.csv ../output/new_construction_exact_permit_matches.csv ../output/preferred_chicago_address_geocodes.csv ../output/preferred_historical_address_geocodes.csv ../output/preferred_historical_parcel_coverage.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/preferred_address_geocode_requests.csv: build_preferred_address_geocode_requests.R ../output/preferred_historical_parcel_coverage.csv ../output/density_historical_coordinates.csv ../output/density_parcel_address_selected_history.csv ../input/parcel_universe_2025_city.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/preferred_historical_parcels.gpkg \
	../%/preferred_historical_parcel_coverage.csv: build_preferred_historical_parcels.R ../input/preferred_historical_parcel_source.gpkg ../input/preferred_historical_parcel_source_queries.csv ../output/preferred_project_geography_requests.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../%/preferred_historical_predecessor_selected.gpkg \
	../%/preferred_historical_predecessor_resolution.csv: recover_preferred_historical_predecessors.R ../input/predecessor_parcel_history.csv ../input/geocoding_parcel_history.csv ../input/history_reference_parcels.gpkg ../input/history_reference_queries.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_source_queries.csv ../output/preferred_predecessor_reference_points.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/commercial_city_building_footprints.gpkg: build_commercial_city_building_footprints.R ../input/chicago_building_footprints_2015.zip ../output/preferred_project_year_geometry.gpkg ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/commercial_ground_up_evidence.csv: build_commercial_ground_up_evidence.R ../output/building_permits_for_verification.gpkg ../output/commercial_city_building_footprints.gpkg ../output/new_construction_exact_permit_matches.csv ../output/new_construction_spatial_permit_matches.csv ../output/preferred_commercial_project_candidates.csv ../output/preferred_project_year_geometry.gpkg ../output/project_permit_chain_links.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile | ../output ../report
	$(R) $<

../output/density_historical_coordinates.csv: select_historical_coordinates.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../input/historical_coordinate_year_corrections.csv ../output/density_project_lineage.csv ../output/density_parcel_address_lineage_evidence.csv ../output/density_historical_coordinate_candidates.csv Makefile | ../output ../report
	$(R) $<

../output/density_project_lineage.csv: build_historical_project_screen.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../output/density_historical_building_universe.csv ../input/density_historical_parcel_records.csv Makefile | ../output ../report
	$(R) $< $(HISTORICAL_EXACT_MATCH_FT) $(HISTORICAL_LOCAL_MATCH_FT) $(HISTORICAL_MAX_BUILDING_GAP)

../output/density_historical_building_universe.csv: build_historical_coordinate_requests.R ../../shared/code/save_data.R ../../shared/code/report.py ../../setup_environment/code/packages.R ../output/residential_discovery_cross_section.csv ../output/multifamily_data_cleaned.csv ../input/parcel_universe_2025_city.csv Makefile | ../output ../report
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) $(MIN_INITIAL_AREA_SQFT)

../output/multifamily_data_cleaned.csv: build_commercial_cross_section.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../input/commercial_value_raw.csv Makefile | ../output ../report
	$(R) $<

../output/density_parcel_address_selected_history.csv: build_historical_address_history.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../input/historical_parcel_requests.csv ../input/density_historical_address_records.csv ../../shared/code/normalize_chicago_address.R ../input/historical_address_corrections.csv Makefile | ../output ../report
	$(R) $<

../output/density_parcel_address_lineage_evidence.csv: build_historical_address_screen.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../output/density_project_lineage.csv ../output/density_historical_building_universe.csv ../output/density_historical_coordinate_candidates.csv ../output/density_parcel_address_selected_history.csv ../input/parcel_addresses_2025_chicago.csv Makefile | ../output ../report
	$(R) $< $(HISTORICAL_MAX_BUILDING_GAP)

../output/density_historical_coordinate_candidates.csv: build_historical_coordinates.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../../shared/code/canonical_geometry_helpers.R ../output/density_historical_building_universe.csv ../input/density_historical_parcel_records.csv ../input/historical_address_matches.csv ../input/parcel_universe_2025_city.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg Makefile | ../output ../report
	$(R) $< $(MAX_DISTANCE_FT)

../%/residential_cross_section.csv \
	../%/residential_discovery_cross_section.csv: build_residential_cross_section.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../input/residential_assessor_history.parquet Makefile | ../output ../report
	$(R) $< $(MIN_CONSTRUCTION_YEAR) $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR)

../%/residential_successor_condo_requests.csv \
	../%/residential_review_current_parcel_links.csv: build_residential_successor_condo_requests.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py ../input/parcel_universe_2025_city.csv ../output/preferred_project_year_geometry.gpkg ../output/preferred_residential_project_candidates.csv Makefile | ../output ../report
	$(R) $<

../input/historical_project_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_project_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_queries.csv: ../../download_construction_historical_parcels/output/historical_project_parcel_source_queries.csv | ../input
	ln -sf $< $@

../input/predecessor_parcel_history.csv: ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv | ../input
	ln -sf $< $@

../input/predecessor_history_queries.csv: ../../download_construction_historical_parcels/output/predecessor_history_queries.csv | ../input
	ln -sf $< $@

../input/historical_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/historical_predecessor_source_queries.csv: ../../download_construction_historical_parcels/output/historical_predecessor_source_queries.csv | ../input
	ln -sf $< $@

../input/commercial_value_raw.csv: ../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

../input/building_permits_full.csv: ../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/density_historical_address_records.csv: ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/chicago_building_footprints_2015.zip: ../../download_city_building_footprints/output/chicago_building_footprints_2015.zip | ../input
	ln -sf $< $@

../input/historical_parcel_requests.csv: ../../../data_raw/construction_review/historical_parcel_requests_2026-09-04.csv | ../input
	ln -sf $< $@

../input/Chicago_Wards_1998.geojson: ../../../data_raw/Chicago_Wards_1998.geojson | ../input
	ln -sf $< $@

../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@

../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	ln -sf $< $@

../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/address_geocodes_census.csv: ../../download_construction_address_geocodes/output/address_geocodes_census.csv | ../input
	ln -sf $< $@

../input/address_geocodes_chicago.csv: ../../download_construction_address_geocodes/output/address_geocodes_chicago.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_predecessor_source_queries.csv | ../input
	ln -sf $< $@

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	ln -sf $< $@

../input/history_reference_parcels.gpkg: ../../download_construction_historical_parcels/output/history_reference_parcels.gpkg | ../input
	ln -sf $< $@

../input/history_reference_queries.csv: ../../download_construction_historical_parcels/output/history_reference_queries.csv | ../input
	ln -sf $< $@

../input/construction_condominium_history.csv: ../../download_construction_assessor_history/output/construction_condominium_history.csv | ../input
	ln -sf $< $@

../input/zoning_nov2012.zip: ../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	ln -sf $< $@

../input/zoning_sep2014.zip: ../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	ln -sf $< $@

../input/zoning_jan2016.zip: ../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	ln -sf $< $@

../input/zoning_sep2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	ln -sf $< $@

../input/commercial_component_overrides.csv: ../../new_construction_corrections/output/commercial_component_overrides.csv | ../input
	ln -sf $< $@

../input/commercial_cross_family_decisions.csv: ../../new_construction_corrections/output/commercial_cross_family_decisions.csv | ../input
	ln -sf $< $@

../input/commercial_manual_decisions.csv: ../../new_construction_corrections/output/commercial_manual_decisions.csv | ../input
	ln -sf $< $@

../input/commercial_reported_land_decisions.csv: ../../new_construction_corrections/output/commercial_reported_land_decisions.csv | ../input
	ln -sf $< $@

../input/commercial_reviewed_locations.csv: ../../new_construction_corrections/output/commercial_reviewed_locations.csv | ../input
	ln -sf $< $@

../input/commercial_verified_case_review.csv: ../../new_construction_corrections/output/commercial_verified_case_review.csv | ../input
	ln -sf $< $@

../input/construction_building_types.csv: ../../new_construction_corrections/output/construction_building_types.csv | ../input
	ln -sf $< $@

../input/construction_modifications.csv: ../../new_construction_corrections/output/construction_modifications.csv | ../input
	ln -sf $< $@

../input/corrected_year_zoning_decisions.csv: ../../new_construction_corrections/output/corrected_year_zoning_decisions.csv | ../input
	ln -sf $< $@

../input/eligibility_manual_exceptions.csv: ../../new_construction_corrections/output/eligibility_manual_exceptions.csv | ../input
	ln -sf $< $@

../input/historical_address_corrections.csv: ../../new_construction_corrections/output/historical_address_corrections.csv | ../input
	ln -sf $< $@

../input/historical_address_matches.csv: ../../new_construction_corrections/output/historical_address_matches.csv | ../input
	ln -sf $< $@

../input/historical_coordinate_year_corrections.csv: ../../new_construction_corrections/output/historical_coordinate_year_corrections.csv | ../input
	ln -sf $< $@

../input/residential_additional_candidate_decisions.csv: ../../new_construction_corrections/output/residential_additional_candidate_decisions.csv | ../input
	ln -sf $< $@

../input/residential_class297_component_overrides.csv: ../../new_construction_corrections/output/residential_class297_component_overrides.csv | ../input
	ln -sf $< $@

../input/residential_overlap_decisions.csv: ../../new_construction_corrections/output/residential_overlap_decisions.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_building_components.csv: ../../new_construction_corrections/output/residential_reviewed_building_components.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_card_selections.csv: ../../new_construction_corrections/output/residential_reviewed_card_selections.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_home_replacements.csv: ../../new_construction_corrections/output/residential_reviewed_home_replacements.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_parcel_locations.csv: ../../new_construction_corrections/output/residential_reviewed_parcel_locations.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_permit_locations.csv: ../../new_construction_corrections/output/residential_reviewed_permit_locations.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_source_duplicates.csv: ../../new_construction_corrections/output/residential_reviewed_source_duplicates.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_source_exclusions.csv: ../../new_construction_corrections/output/residential_reviewed_source_exclusions.csv | ../input
	ln -sf $< $@

../input/residential_source_decisions.csv: ../../new_construction_corrections/output/residential_source_decisions.csv | ../input
	ln -sf $< $@

../input/residential_unresolved_source_dispositions.csv: ../../new_construction_corrections/output/residential_unresolved_source_dispositions.csv | ../input
	ln -sf $< $@

../input/historical_zoning_2006_candidate.gpkg: ../../construction_zoning_history/output/historical_zoning_2006_candidate.gpkg | ../input
	ln -sf $< $@

../input/historical_zoning_project_construction_year.csv: ../../construction_zoning_history/output/historical_zoning_project_construction_year.csv | ../input
	ln -sf $< $@

link-inputs: ../input/commercial_component_overrides.csv \
	../input/commercial_cross_family_decisions.csv \
	../input/commercial_manual_decisions.csv \
	../input/commercial_reported_land_decisions.csv \
	../input/commercial_reviewed_locations.csv \
	../input/commercial_verified_case_review.csv \
	../input/construction_building_types.csv \
	../input/construction_modifications.csv \
	../input/corrected_year_zoning_decisions.csv \
	../input/eligibility_manual_exceptions.csv \
	../input/historical_address_corrections.csv \
	../input/historical_address_matches.csv \
	../input/historical_coordinate_year_corrections.csv \
	../input/residential_additional_candidate_decisions.csv \
	../input/residential_class297_component_overrides.csv \
	../input/residential_overlap_decisions.csv \
	../input/residential_reviewed_building_components.csv \
	../input/residential_reviewed_card_selections.csv \
	../input/residential_reviewed_home_replacements.csv \
	../input/residential_reviewed_parcel_locations.csv \
	../input/residential_reviewed_permit_locations.csv \
	../input/residential_reviewed_source_duplicates.csv \
	../input/residential_reviewed_source_exclusions.csv \
	../input/residential_source_decisions.csv \
	../input/residential_unresolved_source_dispositions.csv \
	../input/historical_zoning_2006_candidate.gpkg \
	../input/historical_zoning_project_construction_year.csv \
	../input/historical_project_parcel_source.gpkg \
	../input/historical_project_parcel_source_queries.csv \
	../input/predecessor_parcel_history.csv \
	../input/predecessor_history_queries.csv \
	../input/historical_predecessor_parcel_source.gpkg \
	../input/historical_predecessor_source_queries.csv \
	../input/residential_improvement_characteristics_full.csv \
	../input/commercial_value_raw.csv \
	../input/parcel_universe_2025_city.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/building_permits_full.csv \
	../input/residential_assessor_history.parquet \
	../input/parcel_addresses_2025_chicago.csv \
	../input/density_historical_parcel_records.csv \
	../input/density_historical_address_records.csv \
	../input/chicago_building_footprints_2015.zip \
	../input/historical_parcel_requests.csv \
	../input/Chicago_Wards_1998.geojson \
	../input/Wards_2014.geojson \
	../input/Wards_2015.geojson \
	../input/Wards_2024.geojson \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_historical_parcel_source_queries.csv \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/address_geocodes_census.csv \
	../input/address_geocodes_chicago.csv \
	../input/preferred_predecessor_source_queries.csv \
	../input/geocoding_parcel_history.csv \
	../input/history_reference_parcels.gpkg \
	../input/history_reference_queries.csv \
	../input/construction_condominium_history.csv \
	../input/zoning_nov2012.zip \
	../input/zoning_sep2014.zip \
	../input/zoning_jan2016.zip \
	../input/zoning_sep2025.geojson

include ../../shared/code/generic.make
