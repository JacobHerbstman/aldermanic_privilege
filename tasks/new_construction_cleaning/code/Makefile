include ../../shared/code/shell_functions.make
include construction_settings.make

all: ../report/building_permits_for_verification.gpkg.log \
	../report/commercial_address_family_candidates.csv.log \
	../report/commercial_address_permit_history.csv.log \
	../report/commercial_adjudication_queue.csv.log \
	../report/commercial_city_building_footprints.gpkg.log \
	../report/commercial_completion_evidence.csv.log \
	../report/commercial_entity_component_candidates.csv.log \
	../report/commercial_entity_version_candidates.csv.log \
	../report/commercial_family_vintage_summary.csv.log \
	../report/commercial_ground_up_evidence.csv.log \
	../report/commercial_land_adjudication_evidence.csv.log \
	../report/commercial_permit_address_evidence.csv.log \
	../report/commercial_permit_chain_evidence.csv.log \
	../report/commercial_permit_project_evidence.csv.log \
	../report/commercial_post_evidence_resolution.csv.log \
	../report/commercial_production_family_members.csv.log \
	../report/commercial_project_family_review.csv.log \
	../report/commercial_residential_history_evidence.csv.log \
	../report/commercial_unit_adjudication_evidence.csv.log \
	../report/construction_discovery_boundaries.gpkg.log \
	../report/construction_discovery_ward_maps.gpkg.log \
	../report/construction_parcel_boundary_distances.csv.log \
	../report/density_historical_building_universe.csv.log \
	../report/density_historical_coordinate_candidates.csv.log \
	../report/density_historical_coordinates.csv.log \
	../report/density_parcel_address_lineage_evidence.csv.log \
	../report/density_parcel_address_selected_history.csv.log \
	../report/density_project_lineage.csv.log \
	../report/geocoded_residential_data.gpkg.log \
	../report/historical_project_parcel_coverage.csv.log \
	../report/historical_project_parcel_requests.csv.log \
	../report/historical_project_predecessor_resolution.csv.log \
	../report/historical_project_year_geometry.gpkg.log \
	../report/missing_project_reference_points.csv.log \
	../report/multifamily_data_cleaned.csv.log \
	../report/new_construction_exact_permit_matches.csv.log \
	../report/new_construction_permit_unit_mentions.csv.log \
	../report/new_construction_project_components.csv.log \
	../report/new_construction_spatial_permit_matches.csv.log \
	../report/preferred_address_geocode_requests.csv.log \
	../report/preferred_adjudication_scope.csv.log \
	../report/preferred_chicago_address_geocodes.csv.log \
	../report/preferred_commercial_project_candidates.csv.log \
	../report/preferred_commercial_project_components.csv.log \
	../report/preferred_commercial_projects.csv.log \
	../report/preferred_commercial_source_disposition.csv.log \
	../report/preferred_historical_address_geocodes.csv.log \
	../report/preferred_historical_parcel_coverage.csv.log \
	../report/preferred_historical_parcels.gpkg.log \
	../report/preferred_historical_predecessor_resolution.csv.log \
	../report/preferred_historical_predecessor_selected.gpkg.log \
	../report/preferred_predecessor_reference_points.csv.log \
	../report/preferred_project_boundary_scope.csv.log \
	../report/preferred_project_component_geometry.gpkg.log \
	../report/preferred_project_geography_requests.csv.log \
	../report/preferred_project_year_centroids.gpkg.log \
	../report/preferred_project_year_geometry.gpkg.log \
	../report/preferred_project_year_geometry_coverage.csv.log \
	../report/preferred_residential_project_candidates.csv.log \
	../report/preferred_residential_project_components.csv.log \
	../report/project_permit_chain_links.csv.log \
	../report/project_permit_chain_summary.csv.log \
	../report/project_permit_chain_unit_mentions.csv.log \
	../report/residential_adjudication_queue.csv.log \
	../report/residential_assessor_project_candidates.csv.log \
	../report/residential_cross_section.csv.log \
	../report/residential_discovery_cross_section.csv.log \
	../report/residential_fractional_base_groups.csv.log \
	../report/residential_multicard_cards.csv.log \
	../report/residential_project_candidate_inventory.csv.log \
	../report/residential_project_history_summary.csv.log \
	../report/residential_tieback_groups_full.csv.log \
	../report/residential_tieback_members_full.csv.log \
	../report/residential_tieback_temporal_lineage_evidence.csv.log \
	../report/residential_tieback_temporal_snapshots.csv.log

all: ../report/preferred_commercial_project_ledger.csv.log ../report/preferred_commercial_project_component_locations.csv.log ../report/preferred_commercial_project_centroids.gpkg.log ../report/preferred_commercial_boundary_scope.csv.log

ifneq ($(filter-out $(wildcard ../output/preferred_commercial_project_ledger.csv ../output/preferred_commercial_project_component_locations.csv ../output/preferred_commercial_project_centroids.gpkg ../output/preferred_commercial_boundary_scope.csv),../output/preferred_commercial_project_ledger.csv ../output/preferred_commercial_project_component_locations.csv ../output/preferred_commercial_project_centroids.gpkg ../output/preferred_commercial_boundary_scope.csv),)
.PHONY: ../output/preferred_commercial_project_ledger.csv
endif

../output/preferred_commercial_project_ledger.csv: build_preferred_commercial_final_geography.R ../adjudication/commercial_reviewed_locations.csv ../input/preferred_historical_parcel_source.gpkg ../output/preferred_commercial_project_candidates.csv ../output/preferred_project_year_centroids.gpkg ../output/building_permits_for_verification.gpkg ../input/parcel_universe_2025_city.csv ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/preferred_commercial_projects.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/preferred_commercial_project_component_locations.csv: ../output/preferred_commercial_project_ledger.csv
	@test -f "$@"

../output/preferred_commercial_project_centroids.gpkg: ../output/preferred_commercial_project_ledger.csv
	@test -f "$@"

../output/preferred_commercial_boundary_scope.csv: ../output/preferred_commercial_project_ledger.csv
	@test -f "$@"

../report/preferred_commercial_project_ledger.csv.log: ../../shared/code/report.py ../output/preferred_commercial_project_ledger.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_project_ledger.csv $@ project_id

../report/preferred_commercial_project_component_locations.csv.log: ../../shared/code/report.py ../output/preferred_commercial_project_component_locations.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_project_component_locations.csv $@ project_id component_pin

../report/preferred_commercial_project_centroids.gpkg.log: ../../shared/code/report.py ../output/preferred_commercial_project_centroids.gpkg | ../report
	$(PYTHON) $< ../output/preferred_commercial_project_centroids.gpkg $@ project_id

../report/preferred_commercial_boundary_scope.csv.log: ../../shared/code/report.py ../output/preferred_commercial_boundary_scope.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_boundary_scope.csv $@ project_id

../report/new_construction_project_components.csv.log: ../../shared/code/report.py ../output/new_construction_project_components.csv | ../report
	$(PYTHON) $< ../output/new_construction_project_components.csv $@ project_id component_pin

../report/project_permit_chain_links.csv.log: ../../shared/code/report.py ../output/project_permit_chain_links.csv | ../report
	$(PYTHON) $< ../output/project_permit_chain_links.csv $@ project_id permit_number

../report/project_permit_chain_unit_mentions.csv.log: ../../shared/code/report.py ../output/project_permit_chain_unit_mentions.csv | ../report
	$(PYTHON) $< ../output/project_permit_chain_unit_mentions.csv $@ project_id permit_number mention_order

../report/project_permit_chain_summary.csv.log: ../../shared/code/report.py ../output/project_permit_chain_summary.csv | ../report
	$(PYTHON) $< ../output/project_permit_chain_summary.csv $@ project_id

../report/new_construction_exact_permit_matches.csv.log: ../../shared/code/report.py ../output/new_construction_exact_permit_matches.csv | ../report
	$(PYTHON) $< ../output/new_construction_exact_permit_matches.csv $@ project_id component_pin permit_id

../report/new_construction_permit_unit_mentions.csv.log: ../../shared/code/report.py ../output/new_construction_permit_unit_mentions.csv | ../report
	$(PYTHON) $< ../output/new_construction_permit_unit_mentions.csv $@ project_id component_pin permit_id mention_order

../report/residential_adjudication_queue.csv.log: ../../shared/code/report.py ../output/residential_adjudication_queue.csv | ../report
	$(PYTHON) $< ../output/residential_adjudication_queue.csv $@ project_id

../report/preferred_residential_project_components.csv.log: ../../shared/code/report.py ../output/preferred_residential_project_components.csv | ../report
	$(PYTHON) $< ../output/preferred_residential_project_components.csv $@ project_id component_pin

../report/preferred_residential_project_candidates.csv.log: ../../shared/code/report.py ../output/preferred_residential_project_candidates.csv | ../report
	$(PYTHON) $< ../output/preferred_residential_project_candidates.csv $@ project_id

../report/preferred_project_year_geometry.gpkg.log: ../../shared/code/report.py ../output/preferred_project_year_geometry.gpkg | ../report
	$(PYTHON) $< ../output/preferred_project_year_geometry.gpkg $@ source_family project_id target_year

../report/preferred_project_component_geometry.gpkg.log: ../../shared/code/report.py ../output/preferred_project_component_geometry.gpkg | ../report
	$(PYTHON) $< ../output/preferred_project_component_geometry.gpkg $@ request_id

../report/preferred_project_year_centroids.gpkg.log: ../../shared/code/report.py ../output/preferred_project_year_centroids.gpkg | ../report
	$(PYTHON) $< ../output/preferred_project_year_centroids.gpkg $@ source_family project_id target_year

../report/preferred_project_year_geometry_coverage.csv.log: ../../shared/code/report.py ../output/preferred_project_year_geometry_coverage.csv | ../report
	$(PYTHON) $< ../output/preferred_project_year_geometry_coverage.csv $@ source_family project_id target_year

../report/preferred_project_boundary_scope.csv.log: ../../shared/code/report.py ../output/preferred_project_boundary_scope.csv | ../report
	$(PYTHON) $< ../output/preferred_project_boundary_scope.csv $@ source_family project_id target_year

../report/preferred_adjudication_scope.csv.log: ../../shared/code/report.py ../output/preferred_adjudication_scope.csv | ../report
	$(PYTHON) $< ../output/preferred_adjudication_scope.csv $@ source_family project_id

../output/building_permits_for_verification.gpkg: build_construction_permit_history.R ../../setup_environment/code/packages.R ../input/building_permits_full.csv | ../output
	$(R) $<

../output/commercial_completion_evidence.csv: build_commercial_completion_evidence.R ../output/commercial_address_permit_history.csv ../output/commercial_ground_up_evidence.csv ../output/commercial_residential_history_evidence.csv ../output/new_construction_spatial_permit_matches.csv ../output/preferred_commercial_project_candidates.csv ../output/project_permit_chain_links.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/residential_project_candidate_inventory.csv ../output/residential_project_history_summary.csv ../output/residential_fractional_base_groups.csv ../output/residential_tieback_groups_full.csv ../output/residential_tieback_members_full.csv ../output/residential_multicard_cards.csv),../output/residential_project_candidate_inventory.csv ../output/residential_project_history_summary.csv ../output/residential_fractional_base_groups.csv ../output/residential_tieback_groups_full.csv ../output/residential_tieback_members_full.csv ../output/residential_multicard_cards.csv),)
.PHONY: ../output/residential_project_candidate_inventory.csv
endif

../output/residential_project_candidate_inventory.csv: build_residential_project_candidates.R ../adjudication/residential_reviewed_card_selections.csv ../input/residential_assessor_history.parquet ../output/geocoded_residential_data.gpkg ../output/multifamily_data_cleaned.csv ../output/construction_parcel_boundary_distances.csv ../output/residential_cross_section.csv ../../setup_environment/code/packages.R construction_settings.make | ../output
	$(R) $< $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR)

../output/residential_project_history_summary.csv: ../output/residential_project_candidate_inventory.csv
	@test -f "$@"

../output/residential_fractional_base_groups.csv: ../output/residential_project_candidate_inventory.csv
	@test -f "$@"

../output/residential_tieback_groups_full.csv: ../output/residential_project_candidate_inventory.csv
	@test -f "$@"

../output/residential_tieback_members_full.csv: ../output/residential_project_candidate_inventory.csv
	@test -f "$@"

../output/residential_multicard_cards.csv: ../output/residential_project_candidate_inventory.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/project_permit_chain_summary.csv),../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/project_permit_chain_summary.csv),)
.PHONY: ../output/project_permit_chain_links.csv
endif

../output/project_permit_chain_links.csv: build_permit_revision_evidence.R ../output/building_permits_for_verification.gpkg ../output/new_construction_exact_permit_matches.csv ../output/new_construction_spatial_permit_matches.csv ../../setup_environment/code/packages.R ../../shared/code/permit_unit_patterns.R | ../output
	$(R) $<

../output/project_permit_chain_unit_mentions.csv: ../output/project_permit_chain_links.csv
	@test -f "$@"

../output/project_permit_chain_summary.csv: ../output/project_permit_chain_links.csv
	@test -f "$@"

../output/geocoded_residential_data.gpkg: geocode_residential_data.R ../output/residential_cross_section.csv ../output/multifamily_data_cleaned.csv ../output/density_historical_coordinates.csv ../input/parcel_universe_2025_city.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/construction_discovery_ward_maps.gpkg: build_construction_discovery_ward_maps.R ../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/construction_discovery_boundaries.gpkg: build_construction_discovery_boundaries.R ../output/construction_discovery_ward_maps.gpkg ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/construction_parcel_boundary_distances.csv: build_construction_parcel_distances.R ../output/construction_discovery_ward_maps.gpkg ../output/construction_discovery_boundaries.gpkg ../output/geocoded_residential_data.gpkg ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R construction_settings.make | ../output
	$(R) $< $(MIN_CONSTRUCTION_YEAR) $(MAX_DISCOVERY_CONSTRUCTION_YEAR)

../output/commercial_post_evidence_resolution.csv: build_commercial_post_evidence_queue.R ../adjudication/commercial_manual_decisions.csv ../output/commercial_completion_evidence.csv ../output/commercial_land_adjudication_evidence.csv ../output/commercial_unit_adjudication_evidence.csv ../output/preferred_adjudication_scope.csv ../output/preferred_commercial_project_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/preferred_commercial_projects.csv ../output/preferred_commercial_source_disposition.csv),../output/preferred_commercial_projects.csv ../output/preferred_commercial_source_disposition.csv),)
.PHONY: ../output/preferred_commercial_projects.csv
endif

../output/preferred_commercial_projects.csv: build_preferred_commercial_ledger.R ../adjudication/commercial_reported_land_decisions.csv ../adjudication/commercial_component_overrides.csv ../adjudication/commercial_cross_family_decisions.csv ../adjudication/commercial_field_decisions.csv ../adjudication/commercial_manual_decisions.csv ../adjudication/commercial_semantic_decisions.csv ../output/commercial_entity_version_candidates.csv ../output/commercial_post_evidence_resolution.csv ../output/preferred_commercial_project_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/preferred_commercial_projects.csv.log: ../../shared/code/report.py ../output/preferred_commercial_projects.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_projects.csv $@ project_id

../report/preferred_commercial_source_disposition.csv.log: ../../shared/code/report.py ../output/preferred_commercial_source_disposition.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_source_disposition.csv $@ source_project_id

../output/preferred_commercial_source_disposition.csv: ../output/preferred_commercial_projects.csv
	@test -f "$@"

../output/commercial_land_adjudication_evidence.csv: build_commercial_land_adjudication_evidence.R ../output/preferred_commercial_project_candidates.csv ../output/preferred_project_boundary_scope.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/commercial_unit_adjudication_evidence.csv: build_commercial_unit_adjudication_evidence.R ../output/commercial_family_vintage_summary.csv ../output/commercial_permit_address_evidence.csv ../output/commercial_permit_chain_evidence.csv ../output/commercial_permit_project_evidence.csv ../output/preferred_commercial_project_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/commercial_address_permit_history.csv: build_commercial_address_permit_history.R ../output/building_permits_for_verification.gpkg ../output/commercial_ground_up_evidence.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/commercial_residential_history_evidence.csv: build_commercial_residential_history_evidence.R ../input/residential_improvement_characteristics_full.csv ../output/preferred_commercial_project_candidates.csv ../output/preferred_commercial_project_components.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/new_construction_spatial_permit_matches.csv: build_spatial_permit_evidence.R ../output/building_permits_for_verification.gpkg ../output/historical_project_year_geometry.gpkg ../output/new_construction_exact_permit_matches.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/residential_tieback_temporal_lineage_evidence.csv ../output/residential_tieback_temporal_snapshots.csv),../output/residential_tieback_temporal_lineage_evidence.csv ../output/residential_tieback_temporal_snapshots.csv),)
.PHONY: ../output/residential_tieback_temporal_lineage_evidence.csv
endif

../output/residential_tieback_temporal_lineage_evidence.csv: build_residential_tieback_temporal_evidence.R ../input/residential_assessor_history.parquet ../output/residential_fractional_base_groups.csv ../output/residential_project_candidate_inventory.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_tieback_temporal_snapshots.csv: ../output/residential_tieback_temporal_lineage_evidence.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/preferred_residential_project_candidates.csv ../output/preferred_residential_project_components.csv ../output/residential_adjudication_queue.csv),../output/preferred_residential_project_candidates.csv ../output/preferred_residential_project_components.csv ../output/residential_adjudication_queue.csv),)
.PHONY: ../output/preferred_residential_project_candidates.csv
endif

../output/residential_assessor_project_candidates.csv: build_residential_assessor_projects.R ../adjudication/residential_reviewed_construction_years.csv ../adjudication/residential_reviewed_land_areas.csv ../adjudication/residential_reviewed_source_duplicates.csv ../adjudication/residential_reviewed_building_components.csv ../adjudication/residential_reviewed_source_exclusions.csv ../input/residential_assessor_history.parquet ../adjudication/residential_reviewed_home_replacements.csv ../../setup_environment/code/packages.R ../../shared/code/assessor_classification.R ../output/residential_project_candidate_inventory.csv ../output/residential_tieback_temporal_lineage_evidence.csv ../output/residential_multicard_cards.csv construction_settings.make ../input/parcel_universe_2025_city.csv ../input/preferred_historical_parcel_source.gpkg ../input/preferred_predecessor_parcel_source.gpkg | ../output
	$(R) $< $(AUTOMATIC_CARD_BUILDING_GAP) $(EPISODE_YEAR_WINDOW) $(SUCCESSOR_POINT_TOLERANCE_FT) $(SUCCESSOR_LAND_TOLERANCE)

../output/residential_assessor_project_candidates.csv: ../input/geocoding_parcel_history.csv ../input/predecessor_parcel_history.csv ../input/density_historical_parcel_records.csv ../input/density_historical_address_records.csv ../input/parcel_addresses_2025_chicago.csv

../report/residential_assessor_project_candidates.csv.log: ../../shared/code/report.py ../output/residential_assessor_project_candidates.csv | ../report
	$(PYTHON) $< ../output/residential_assessor_project_candidates.csv $@ project_id

../output/preferred_residential_project_candidates.csv: build_preferred_residential_candidates.R ../../shared/code/assessor_classification.R ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/residential_assessor_project_candidates.csv ../output/residential_project_candidate_inventory.csv ../output/residential_tieback_temporal_lineage_evidence.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_residential_project_components.csv: ../output/preferred_residential_project_candidates.csv
	@test -f "$@"

../output/residential_adjudication_queue.csv: ../output/preferred_residential_project_candidates.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/commercial_entity_version_candidates.csv ../output/commercial_address_family_candidates.csv ../output/commercial_entity_component_candidates.csv ../output/commercial_family_vintage_summary.csv ../output/commercial_production_family_members.csv ../output/commercial_project_family_review.csv),../output/commercial_entity_version_candidates.csv ../output/commercial_address_family_candidates.csv ../output/commercial_entity_component_candidates.csv ../output/commercial_family_vintage_summary.csv ../output/commercial_production_family_members.csv ../output/commercial_project_family_review.csv),)
.PHONY: ../output/commercial_entity_version_candidates.csv
endif

../output/commercial_entity_version_candidates.csv: build_commercial_project_candidates.R ../input/commercial_value_raw.csv ../output/construction_parcel_boundary_distances.csv ../output/multifamily_data_cleaned.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/commercial_address_family_candidates.csv: ../output/commercial_entity_version_candidates.csv
	@test -f "$@"

../output/commercial_entity_component_candidates.csv: ../output/commercial_entity_version_candidates.csv
	@test -f "$@"

../output/commercial_family_vintage_summary.csv: ../output/commercial_entity_version_candidates.csv
	@test -f "$@"

../output/commercial_production_family_members.csv: ../output/commercial_entity_version_candidates.csv
	@test -f "$@"

../output/commercial_project_family_review.csv: ../output/commercial_entity_version_candidates.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/new_construction_exact_permit_matches.csv ../output/new_construction_permit_unit_mentions.csv ../output/new_construction_project_components.csv),../output/new_construction_exact_permit_matches.csv ../output/new_construction_permit_unit_mentions.csv ../output/new_construction_project_components.csv),)
.PHONY: ../output/new_construction_exact_permit_matches.csv
endif

../output/new_construction_exact_permit_matches.csv: build_new_construction_permit_evidence.R ../output/building_permits_for_verification.gpkg ../output/commercial_entity_component_candidates.csv ../output/commercial_production_family_members.csv ../output/residential_project_candidate_inventory.csv ../output/residential_tieback_groups_full.csv ../output/residential_tieback_members_full.csv ../../setup_environment/code/packages.R ../../shared/code/permit_unit_patterns.R | ../output
	$(R) $<

../output/new_construction_permit_unit_mentions.csv: ../output/new_construction_exact_permit_matches.csv
	@test -f "$@"

../output/new_construction_project_components.csv: ../output/new_construction_exact_permit_matches.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/preferred_commercial_project_candidates.csv ../output/preferred_commercial_project_components.csv ../output/commercial_adjudication_queue.csv),../output/preferred_commercial_project_candidates.csv ../output/preferred_commercial_project_components.csv ../output/commercial_adjudication_queue.csv),)
.PHONY: ../output/preferred_commercial_project_candidates.csv
endif

../output/preferred_commercial_project_candidates.csv: build_preferred_commercial_candidates.R ../adjudication/commercial_verified_case_review.csv ../output/commercial_address_family_candidates.csv ../output/commercial_entity_version_candidates.csv ../output/commercial_family_vintage_summary.csv ../output/commercial_project_family_review.csv ../output/project_permit_chain_links.csv ../output/project_permit_chain_summary.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_commercial_project_components.csv: ../output/preferred_commercial_project_candidates.csv
	@test -f "$@"

../output/commercial_adjudication_queue.csv: ../output/preferred_commercial_project_candidates.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/preferred_project_year_geometry.gpkg ../output/preferred_project_component_geometry.gpkg ../output/preferred_project_year_centroids.gpkg ../output/preferred_project_year_geometry_coverage.csv),../output/preferred_project_year_geometry.gpkg ../output/preferred_project_component_geometry.gpkg ../output/preferred_project_year_centroids.gpkg ../output/preferred_project_year_geometry_coverage.csv),)
.PHONY: ../output/preferred_project_year_geometry.gpkg
endif

../output/preferred_project_year_geometry.gpkg: build_preferred_project_geography.R ../adjudication/residential_reviewed_parcel_locations.csv ../input/parcel_universe_2025_city.csv ../adjudication/residential_reviewed_permit_locations.csv ../output/building_permits_for_verification.gpkg ../output/preferred_historical_parcel_coverage.csv ../output/preferred_historical_parcels.gpkg ../output/preferred_historical_predecessor_selected.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_project_year_geometry.gpkg: ../output/preferred_residential_project_candidates.csv ../input/predecessor_parcel_history.csv ../input/geocoding_parcel_history.csv ../input/density_historical_parcel_records.csv

../output/preferred_project_component_geometry.gpkg: ../output/preferred_project_year_geometry.gpkg
	@test -f "$@"

../output/preferred_project_year_centroids.gpkg: ../output/preferred_project_year_geometry.gpkg
	@test -f "$@"

../output/preferred_project_year_geometry_coverage.csv: ../output/preferred_project_year_geometry.gpkg
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/preferred_project_boundary_scope.csv ../output/preferred_adjudication_scope.csv),../output/preferred_project_boundary_scope.csv ../output/preferred_adjudication_scope.csv),)
.PHONY: ../output/preferred_project_boundary_scope.csv
endif

../output/preferred_project_boundary_scope.csv: build_preferred_boundary_scope.R ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/commercial_adjudication_queue.csv ../output/preferred_project_year_centroids.gpkg ../output/preferred_project_year_geometry_coverage.csv ../output/residential_adjudication_queue.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/preferred_adjudication_scope.csv: ../output/preferred_project_boundary_scope.csv
	@test -f "$@"

../output/preferred_project_geography_requests.csv: build_preferred_geography_requests.R ../output/preferred_commercial_project_candidates.csv ../output/preferred_residential_project_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/preferred_predecessor_reference_points.csv.log: ../../shared/code/report.py ../output/preferred_predecessor_reference_points.csv | ../report
	$(PYTHON) $< ../output/preferred_predecessor_reference_points.csv $@ request_id

../report/preferred_historical_predecessor_resolution.csv.log: ../../shared/code/report.py ../output/preferred_historical_predecessor_resolution.csv | ../report
	$(PYTHON) $< ../output/preferred_historical_predecessor_resolution.csv $@

../report/preferred_historical_predecessor_selected.gpkg.log: ../../shared/code/report.py ../output/preferred_historical_predecessor_selected.gpkg | ../report
	$(PYTHON) $< ../output/preferred_historical_predecessor_selected.gpkg $@ request_id

../output/preferred_predecessor_reference_points.csv: build_preferred_predecessor_reference_points.R ../input/parcel_universe_2025_city.csv ../output/density_historical_coordinates.csv ../output/new_construction_exact_permit_matches.csv ../output/preferred_chicago_address_geocodes.csv ../output/preferred_historical_address_geocodes.csv ../output/preferred_historical_parcel_coverage.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_address_geocode_requests.csv: build_preferred_address_geocode_requests.R ../output/preferred_historical_parcel_coverage.csv ../output/density_historical_coordinates.csv ../output/density_parcel_address_selected_history.csv ../input/parcel_universe_2025_city.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_historical_address_geocodes.csv: build_preferred_census_geocodes.R ../output/preferred_address_geocode_requests.csv ../input/address_geocodes_census.csv ../../setup_environment/code/packages.R ../../shared/code/normalize_chicago_address.R | ../output
	$(R) $<

../output/preferred_chicago_address_geocodes.csv: build_preferred_chicago_geocodes.R ../output/preferred_address_geocode_requests.csv ../input/address_geocodes_chicago.csv ../../setup_environment/code/packages.R ../../shared/code/normalize_chicago_address.R | ../output
	$(R) $<

../report/preferred_address_geocode_requests.csv.log: ../../shared/code/report.py ../output/preferred_address_geocode_requests.csv | ../report
	$(PYTHON) $< ../output/preferred_address_geocode_requests.csv $@ request_id

../report/preferred_historical_address_geocodes.csv.log: ../../shared/code/report.py ../output/preferred_historical_address_geocodes.csv | ../report
	$(PYTHON) $< ../output/preferred_historical_address_geocodes.csv $@ request_id

../report/preferred_chicago_address_geocodes.csv.log: ../../shared/code/report.py ../output/preferred_chicago_address_geocodes.csv | ../report
	$(PYTHON) $< ../output/preferred_chicago_address_geocodes.csv $@ request_id

../report/preferred_historical_parcels.gpkg.log: ../../shared/code/report.py ../output/preferred_historical_parcels.gpkg | ../report
	$(PYTHON) $< ../output/preferred_historical_parcels.gpkg $@ target_year object_id

../report/preferred_historical_parcel_coverage.csv.log: ../../shared/code/report.py ../output/preferred_historical_parcel_coverage.csv | ../report
	$(PYTHON) $< ../output/preferred_historical_parcel_coverage.csv $@ project_id component_pin target_year

ifneq ($(filter-out $(wildcard ../output/preferred_historical_parcels.gpkg ../output/preferred_historical_parcel_coverage.csv),../output/preferred_historical_parcels.gpkg ../output/preferred_historical_parcel_coverage.csv),)
.PHONY: ../output/preferred_historical_parcels.gpkg
endif

../output/preferred_historical_parcels.gpkg: build_preferred_historical_parcels.R ../input/preferred_historical_parcel_source.gpkg ../input/preferred_historical_parcel_source_queries.csv ../output/preferred_project_geography_requests.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_historical_parcel_coverage.csv: ../output/preferred_historical_parcels.gpkg
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/preferred_historical_predecessor_selected.gpkg ../output/preferred_historical_predecessor_resolution.csv),../output/preferred_historical_predecessor_selected.gpkg ../output/preferred_historical_predecessor_resolution.csv),)
.PHONY: ../output/preferred_historical_predecessor_selected.gpkg
endif

../output/preferred_historical_predecessor_selected.gpkg: recover_preferred_historical_predecessors.R ../input/predecessor_parcel_history.csv ../input/geocoding_parcel_history.csv ../input/history_reference_parcels.gpkg ../input/history_reference_queries.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_source_queries.csv ../output/preferred_predecessor_reference_points.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_historical_predecessor_resolution.csv: ../output/preferred_historical_predecessor_selected.gpkg
	@test -f "$@"

../output/commercial_city_building_footprints.gpkg: build_commercial_city_building_footprints.R ../input/chicago_building_footprints_2015.zip ../output/preferred_project_year_geometry.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/commercial_city_building_footprints.gpkg.log: ../../shared/code/report.py ../output/commercial_city_building_footprints.gpkg | ../report
	$(PYTHON) $< ../output/commercial_city_building_footprints.gpkg $@ footprint_id

../output/commercial_ground_up_evidence.csv: build_commercial_ground_up_evidence.R ../output/building_permits_for_verification.gpkg ../output/commercial_city_building_footprints.gpkg ../output/new_construction_exact_permit_matches.csv ../output/new_construction_spatial_permit_matches.csv ../output/preferred_commercial_project_candidates.csv ../output/preferred_project_year_geometry.gpkg ../output/project_permit_chain_links.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/commercial_permit_project_evidence.csv ../output/commercial_permit_chain_evidence.csv ../output/commercial_permit_address_evidence.csv),../output/commercial_permit_project_evidence.csv ../output/commercial_permit_chain_evidence.csv ../output/commercial_permit_address_evidence.csv),)
.PHONY: ../output/commercial_permit_project_evidence.csv
endif

../output/commercial_permit_project_evidence.csv: build_commercial_permit_adjudication_evidence.R ../output/commercial_adjudication_queue.csv ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/commercial_permit_chain_evidence.csv: ../output/commercial_permit_project_evidence.csv
	@test -f "$@"

../output/commercial_permit_address_evidence.csv: ../output/commercial_permit_project_evidence.csv
	@test -f "$@"

../output/historical_project_parcel_requests.csv: build_historical_project_parcel_requests.R ../output/new_construction_project_components.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/historical_project_parcel_coverage.csv: build_historical_project_parcel_coverage.R ../output/historical_project_parcel_requests.csv ../input/historical_project_parcel_source.gpkg ../input/historical_project_parcel_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/historical_project_parcel_requests.csv.log: ../../shared/code/report.py ../output/historical_project_parcel_requests.csv | ../report
	$(PYTHON) $< ../output/historical_project_parcel_requests.csv $@ source_family project_id component_pin target_year

../report/historical_project_parcel_coverage.csv.log: ../../shared/code/report.py ../output/historical_project_parcel_coverage.csv | ../report
	$(PYTHON) $< ../output/historical_project_parcel_coverage.csv $@ source_family project_id component_pin target_year

../output/missing_project_reference_points.csv: build_missing_project_reference_points.R ../output/historical_project_parcel_coverage.csv ../input/predecessor_parcel_history.csv ../input/predecessor_history_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/historical_project_predecessor_resolution.csv: build_historical_project_predecessor_resolution.R ../output/missing_project_reference_points.csv ../input/historical_predecessor_parcel_source.gpkg ../input/historical_predecessor_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/missing_project_reference_points.csv.log: ../../shared/code/report.py ../output/missing_project_reference_points.csv | ../report
	$(PYTHON) $< ../output/missing_project_reference_points.csv $@ source_family project_id component_pin target_year

../report/historical_project_predecessor_resolution.csv.log: ../../shared/code/report.py ../output/historical_project_predecessor_resolution.csv | ../report
	$(PYTHON) $< ../output/historical_project_predecessor_resolution.csv $@

../output/historical_project_year_geometry.gpkg: build_historical_project_geography.R ../output/historical_project_parcel_coverage.csv ../input/historical_project_parcel_source.gpkg ../input/historical_predecessor_parcel_source.gpkg ../output/historical_project_predecessor_resolution.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/historical_project_year_geometry.gpkg.log: ../../shared/code/report.py ../output/historical_project_year_geometry.gpkg | ../report
	$(PYTHON) $< ../output/historical_project_year_geometry.gpkg $@ source_family project_id target_year

../output/density_historical_coordinates.csv: select_historical_coordinates.R ../../setup_environment/code/packages.R ../adjudication/historical_coordinate_year_corrections.csv ../output/density_project_lineage.csv ../output/density_parcel_address_lineage_evidence.csv ../output/density_historical_coordinate_candidates.csv | ../output
	$(R) $<

../output/density_project_lineage.csv: build_historical_project_screen.R ../../setup_environment/code/packages.R ../output/density_historical_building_universe.csv ../input/density_historical_parcel_records.csv construction_settings.make | ../output
	$(R) $< $(HISTORICAL_EXACT_MATCH_FT) $(HISTORICAL_LOCAL_MATCH_FT) $(HISTORICAL_MAX_BUILDING_GAP)

../output/density_historical_building_universe.csv: build_historical_coordinate_requests.R ../../setup_environment/code/packages.R ../output/residential_discovery_cross_section.csv ../output/multifamily_data_cleaned.csv ../input/parcel_universe_2025_city.csv construction_settings.make | ../output
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) $(MIN_INITIAL_AREA_SQFT)

../output/residential_discovery_cross_section.csv: build_residential_discovery_cross_section.R ../../setup_environment/code/packages.R ../input/residential_assessor_history.parquet construction_settings.make | ../output
	$(R) $< $(MIN_CONSTRUCTION_YEAR)

../output/multifamily_data_cleaned.csv: build_commercial_cross_section.R ../../setup_environment/code/packages.R ../input/commercial_value_raw.csv ../adjudication/initial_commercial_unit_overrides.csv | ../output
	$(R) $<

../report/density_parcel_address_selected_history.csv.log: ../../shared/code/report.py ../output/density_parcel_address_selected_history.csv | ../report
	$(PYTHON) $< ../output/density_parcel_address_selected_history.csv $@ pin

../output/density_parcel_address_selected_history.csv: build_historical_address_history.R ../../setup_environment/code/packages.R ../input/historical_parcel_requests.csv ../input/density_historical_address_records.csv ../../shared/code/normalize_chicago_address.R ../adjudication/historical_address_corrections.csv | ../output
	$(R) $<

../output/density_parcel_address_lineage_evidence.csv: build_historical_address_screen.R ../../setup_environment/code/packages.R ../output/density_project_lineage.csv ../output/density_historical_building_universe.csv ../output/density_historical_coordinate_candidates.csv ../output/density_parcel_address_selected_history.csv ../input/parcel_addresses_2025_chicago.csv construction_settings.make | ../output
	$(R) $< $(HISTORICAL_MAX_BUILDING_GAP)

../output/density_historical_coordinate_candidates.csv: build_historical_coordinates.R ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R ../output/density_historical_building_universe.csv ../input/density_historical_parcel_records.csv ../adjudication/historical_address_matches.csv ../input/parcel_universe_2025_city.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg construction_settings.make | ../output
	$(R) $< $(MAX_DISTANCE_FT)

../output/residential_cross_section.csv: build_residential_cross_section.R ../../setup_environment/code/packages.R ../input/residential_assessor_history.parquet construction_settings.make | ../output
	$(R) $< $(MIN_CONSTRUCTION_YEAR) $(PREFERRED_ASSESSMENT_YEAR) $(FALLBACK_ASSESSMENT_YEAR)

../report/residential_cross_section.csv.log: ../output/residential_cross_section.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ pin

../report/residential_discovery_cross_section.csv.log: ../output/residential_discovery_cross_section.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ pin

../report/multifamily_data_cleaned.csv.log: ../output/multifamily_data_cleaned.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ pin

../report/density_historical_coordinates.csv.log: ../output/density_historical_coordinates.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ pin

../report/residential_project_history_summary.csv.log: ../../shared/code/report.py ../output/residential_project_history_summary.csv | ../report
	$(PYTHON) $< ../output/residential_project_history_summary.csv $@ pin

../report/residential_fractional_base_groups.csv.log: ../../shared/code/report.py ../output/residential_fractional_base_groups.csv | ../report
	$(PYTHON) $< ../output/residential_fractional_base_groups.csv $@ base_pin

../report/residential_tieback_groups_full.csv.log: ../../shared/code/report.py ../output/residential_tieback_groups_full.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_groups_full.csv $@ tieback_group

../report/residential_tieback_members_full.csv.log: ../../shared/code/report.py ../output/residential_tieback_members_full.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_members_full.csv $@ tieback_group pin

../report/residential_multicard_cards.csv.log: ../../shared/code/report.py ../output/residential_multicard_cards.csv | ../report
	$(PYTHON) $< ../output/residential_multicard_cards.csv $@ pin card_num

../report/residential_project_candidate_inventory.csv.log: ../output/residential_project_candidate_inventory.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ pin

../report/residential_tieback_temporal_lineage_evidence.csv.log: ../output/residential_tieback_temporal_lineage_evidence.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ tieback_lineage_id

../report/residential_tieback_temporal_snapshots.csv.log: ../output/residential_tieback_temporal_snapshots.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ tieback_lineage_id tax_year

../report/commercial_ground_up_evidence.csv.log: ../../shared/code/report.py ../output/commercial_ground_up_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_ground_up_evidence.csv $@ project_id

../report/commercial_permit_project_evidence.csv.log: ../../shared/code/report.py ../output/commercial_permit_project_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_permit_project_evidence.csv $@ project_id

../report/commercial_permit_chain_evidence.csv.log: ../../shared/code/report.py ../output/commercial_permit_chain_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_permit_chain_evidence.csv $@ project_id permit_chain_id

../report/commercial_permit_address_evidence.csv.log: ../../shared/code/report.py ../output/commercial_permit_address_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_permit_address_evidence.csv $@ project_id address_key

../report/building_permits_for_verification.gpkg.log: ../../shared/code/report.py ../output/building_permits_for_verification.gpkg | ../report
	$(PYTHON) $< ../output/building_permits_for_verification.gpkg $@

../report/commercial_address_family_candidates.csv.log: ../../shared/code/report.py ../output/commercial_address_family_candidates.csv | ../report
	$(PYTHON) $< ../output/commercial_address_family_candidates.csv $@

../report/commercial_address_permit_history.csv.log: ../../shared/code/report.py ../output/commercial_address_permit_history.csv | ../report
	$(PYTHON) $< ../output/commercial_address_permit_history.csv $@

../report/commercial_adjudication_queue.csv.log: ../../shared/code/report.py ../output/commercial_adjudication_queue.csv | ../report
	$(PYTHON) $< ../output/commercial_adjudication_queue.csv $@

../report/commercial_completion_evidence.csv.log: ../../shared/code/report.py ../output/commercial_completion_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_completion_evidence.csv $@

../report/commercial_entity_component_candidates.csv.log: ../../shared/code/report.py ../output/commercial_entity_component_candidates.csv | ../report
	$(PYTHON) $< ../output/commercial_entity_component_candidates.csv $@

../report/commercial_entity_version_candidates.csv.log: ../../shared/code/report.py ../output/commercial_entity_version_candidates.csv | ../report
	$(PYTHON) $< ../output/commercial_entity_version_candidates.csv $@

../report/commercial_family_vintage_summary.csv.log: ../../shared/code/report.py ../output/commercial_family_vintage_summary.csv | ../report
	$(PYTHON) $< ../output/commercial_family_vintage_summary.csv $@

../report/commercial_land_adjudication_evidence.csv.log: ../../shared/code/report.py ../output/commercial_land_adjudication_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_land_adjudication_evidence.csv $@

../report/commercial_post_evidence_resolution.csv.log: ../../shared/code/report.py ../output/commercial_post_evidence_resolution.csv | ../report
	$(PYTHON) $< ../output/commercial_post_evidence_resolution.csv $@

../report/commercial_production_family_members.csv.log: ../../shared/code/report.py ../output/commercial_production_family_members.csv | ../report
	$(PYTHON) $< ../output/commercial_production_family_members.csv $@

../report/commercial_project_family_review.csv.log: ../../shared/code/report.py ../output/commercial_project_family_review.csv | ../report
	$(PYTHON) $< ../output/commercial_project_family_review.csv $@

../report/commercial_residential_history_evidence.csv.log: ../../shared/code/report.py ../output/commercial_residential_history_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_residential_history_evidence.csv $@

../report/commercial_unit_adjudication_evidence.csv.log: ../../shared/code/report.py ../output/commercial_unit_adjudication_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_unit_adjudication_evidence.csv $@

../report/construction_discovery_boundaries.gpkg.log: ../../shared/code/report.py ../output/construction_discovery_boundaries.gpkg | ../report
	$(PYTHON) $< ../output/construction_discovery_boundaries.gpkg $@

../report/construction_discovery_ward_maps.gpkg.log: ../../shared/code/report.py ../output/construction_discovery_ward_maps.gpkg | ../report
	$(PYTHON) $< ../output/construction_discovery_ward_maps.gpkg $@

../report/construction_parcel_boundary_distances.csv.log: ../../shared/code/report.py ../output/construction_parcel_boundary_distances.csv | ../report
	$(PYTHON) $< ../output/construction_parcel_boundary_distances.csv $@

../report/density_historical_building_universe.csv.log: ../../shared/code/report.py ../output/density_historical_building_universe.csv | ../report
	$(PYTHON) $< ../output/density_historical_building_universe.csv $@

../report/density_historical_coordinate_candidates.csv.log: ../../shared/code/report.py ../output/density_historical_coordinate_candidates.csv | ../report
	$(PYTHON) $< ../output/density_historical_coordinate_candidates.csv $@

../report/density_parcel_address_lineage_evidence.csv.log: ../../shared/code/report.py ../output/density_parcel_address_lineage_evidence.csv | ../report
	$(PYTHON) $< ../output/density_parcel_address_lineage_evidence.csv $@

../report/density_project_lineage.csv.log: ../../shared/code/report.py ../output/density_project_lineage.csv | ../report
	$(PYTHON) $< ../output/density_project_lineage.csv $@

../report/geocoded_residential_data.gpkg.log: ../../shared/code/report.py ../output/geocoded_residential_data.gpkg | ../report
	$(PYTHON) $< ../output/geocoded_residential_data.gpkg $@

../report/new_construction_spatial_permit_matches.csv.log: ../../shared/code/report.py ../output/new_construction_spatial_permit_matches.csv | ../report
	$(PYTHON) $< ../output/new_construction_spatial_permit_matches.csv $@

../report/preferred_commercial_project_candidates.csv.log: ../../shared/code/report.py ../output/preferred_commercial_project_candidates.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_project_candidates.csv $@

../report/preferred_commercial_project_components.csv.log: ../../shared/code/report.py ../output/preferred_commercial_project_components.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_project_components.csv $@

../report/preferred_project_geography_requests.csv.log: ../../shared/code/report.py ../output/preferred_project_geography_requests.csv | ../report
	$(PYTHON) $< ../output/preferred_project_geography_requests.csv $@

../input/historical_project_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_project_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_source_queries.csv: ../../download_construction_historical_parcels/output/historical_project_parcel_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/predecessor_parcel_history.csv: ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/predecessor_history_queries.csv: ../../download_construction_historical_parcels/output/predecessor_history_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_predecessor_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_predecessor_source_queries.csv: ../../download_construction_historical_parcels/output/historical_predecessor_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/residential_improvement_characteristics_full.csv: ../../../data_raw/construction_review/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/commercial_value_raw.csv: ../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/building_permits_full.csv: ../../../data_raw/construction_review/building_permits_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/density_historical_address_records.csv: ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_building_footprints_2015.zip: ../../download_city_building_footprints/output/chicago_building_footprints_2015.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_parcel_requests.csv: ../../../data_raw/construction_review/historical_parcel_requests_2026-09-04.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/Chicago_Wards_1998.geojson: ../../../data_raw/Chicago_Wards_1998.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_historical_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_historical_parcel_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_predecessor_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/address_geocodes_census.csv: ../../download_construction_address_geocodes/output/address_geocodes_census.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/address_geocodes_chicago.csv: ../../download_construction_address_geocodes/output/address_geocodes_chicago.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_predecessor_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_predecessor_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/history_reference_parcels.gpkg: ../../download_construction_historical_parcels/output/history_reference_parcels.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/history_reference_queries.csv: ../../download_construction_historical_parcels/output/history_reference_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/historical_project_parcel_source.gpkg \
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
	../input/history_reference_queries.csv

# Earlier review and final-assembly targets remain available explicitly.
include legacy_construction.make
include ../../shared/code/generic.make
