include ../../shared/code/shell_functions.make

EPISODE_YEAR_WINDOW := 2
AUTOMATIC_CARD_BUILDING_GAP := 0.02
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

all: ../report/density_parcel_address_selected_history.csv.log \
	../report/commercial_city_building_footprints.gpkg.log \
	../report/preferred_predecessor_reference_points.csv.log \
	../report/preferred_historical_predecessor_resolution.csv.log \
	../report/preferred_historical_predecessor_selected.gpkg.log \
	../report/preferred_historical_parcels.gpkg.log \
	../report/preferred_historical_parcel_coverage.csv.log \
	../report/preferred_address_geocode_requests.csv.log \
	../report/preferred_historical_address_geocodes.csv.log \
	../report/preferred_chicago_address_geocodes.csv.log \
	../report/historical_project_parcel_requests.csv.log \
	../report/historical_project_parcel_coverage.csv.log \
	../report/historical_project_year_geometry.gpkg.log \
	../report/missing_project_reference_points.csv.log \
	../report/historical_project_predecessor_resolution.csv.log \
	../report/preferred_commercial_projects.csv.log \
	../report/preferred_commercial_source_disposition.csv.log \
	../report/residential_project_history_summary.csv.log \
	../report/residential_fractional_base_groups.csv.log \
	../report/residential_tieback_groups_full.csv.log \
	../report/residential_tieback_members_full.csv.log \
	../report/residential_multicard_cards.csv.log \
	../report/residential_assessor_project_candidates.csv.log \
	../report/provisional_validated_density_input.csv.log \
	../report/final_density_model_input.csv.log \
	../report/preferred_density_model_production_card_input.csv.log \
	../report/multifamily_classification_decisions.csv.log \
	../report/new_construction_project_components.csv.log \
	../report/project_permit_chain_links.csv.log \
	../report/project_permit_chain_unit_mentions.csv.log \
	../report/project_permit_chain_summary.csv.log \
	../report/preferred_residential_boundary_scope.csv.log \
	../report/new_construction_exact_permit_matches.csv.log \
	../report/new_construction_permit_unit_mentions.csv.log \
	../report/multicard_permit_adjudication_links.csv.log \
	../report/multicard_final_adjudication.csv.log \
	../report/multicard_adjudicated_density_model_input.csv.log \
	../report/multicard_external_review_queue.csv.log \
	../report/multicard_external_reviewed_model_input.csv.log \
	../report/project_evidence_inventory.csv.log \
	../report/preferred_new_construction_project_ledger.csv.log \
	../report/preferred_residential_project_ledger.csv.log \
	../report/residential_adjudication_queue.csv.log \
	../report/preferred_residential_project_components.csv.log \
	../report/preferred_residential_project_candidates.csv.log \
	../report/multicard_adjudication_evidence.csv.log \
	../report/multicard_card_snapshot.csv.log \
	../report/multicard_project_evidence_base.csv.log \
	../report/multicard_successor_match_summary.csv.log \
	../report/multicard_successor_building_candidates.csv.log \
	../report/multicard_final_review_bundle.csv.log \
	../report/multicard_component_successor_matches.csv.log \
	../report/multicard_episode_component_summary.csv.log \
	../report/multicard_episode_component_nodes.csv.log \
	../report/multicard_same_episode_edges.csv.log \
	../report/historical_zoning_project_construction_year.csv.log \
	../report/residential_successor_condo_requests.csv.log \
	../report/residential_review_current_parcel_links.csv.log \
	../report/residential_shared_site_location_review.csv.log \
	../report/project_overlap_evidence.csv.log \
	../report/residential_remaining_case_evidence.csv.log \
	../report/residential_class297_resolution.csv.log \
	../report/residential_class297_condo_cohort_evidence.csv.log \
	../report/residential_class297_source_disposition.csv.log \
	../report/residential_review_assessor_history.csv.log \
	../report/residential_review_city_building_evidence.csv.log \
	../report/residential_manual_review_bundle.csv.log \
	../report/residential_tieback_card_project_evidence.csv.log \
	../report/residential_tieback_construction_episode_evidence.csv.log \
	../output/final_verified_density_input.csv \
	../report/residential_cross_section.csv.log \
	../report/residential_discovery_cross_section.csv.log \
	../report/multifamily_data_cleaned.csv.log \
	../report/density_historical_coordinates.csv.log \
	../report/residential_project_candidate_inventory.csv.log \
	../report/residential_tieback_temporal_lineage_evidence.csv.log \
	../report/residential_tieback_temporal_snapshots.csv.log \
	../report/final_verified_density_input.csv.log \
	../report/commercial_ground_up_evidence.csv.log \
	../report/commercial_permit_project_evidence.csv.log \
	../report/commercial_permit_chain_evidence.csv.log \
	../report/commercial_permit_address_evidence.csv.log \
	../report/preferred_new_construction_zoning.csv.log \
	../report/residential_tieback_episode_candidates.csv.log \
	../report/residential_tieback_no_snapshot_review.csv.log \
	../report/final_new_construction_zoning.csv.log

../report/preferred_density_model_production_card_input.csv.log: ../../shared/code/report.py ../output/preferred_density_model_production_card_input.csv | ../report
	$(PYTHON) $< ../output/preferred_density_model_production_card_input.csv $@ project_id

../report/final_density_model_input.csv.log: ../../shared/code/report.py ../output/final_density_model_input.csv | ../report
	$(PYTHON) $< ../output/final_density_model_input.csv $@ project_id

../report/provisional_validated_density_input.csv.log: ../../shared/code/report.py ../output/provisional_validated_density_input.csv | ../report
	$(PYTHON) $< ../output/provisional_validated_density_input.csv $@ project_id

../report/multifamily_classification_decisions.csv.log: ../../shared/code/report.py ../output/multifamily_classification_decisions.csv | ../report
	$(PYTHON) $< ../output/multifamily_classification_decisions.csv $@ project_id

../report/new_construction_project_components.csv.log: ../../shared/code/report.py ../output/new_construction_project_components.csv | ../report
	$(PYTHON) $< ../output/new_construction_project_components.csv $@ project_id component_pin

../report/project_permit_chain_links.csv.log: ../../shared/code/report.py ../output/project_permit_chain_links.csv | ../report
	$(PYTHON) $< ../output/project_permit_chain_links.csv $@ project_id permit_number

../report/project_permit_chain_unit_mentions.csv.log: ../../shared/code/report.py ../output/project_permit_chain_unit_mentions.csv | ../report
	$(PYTHON) $< ../output/project_permit_chain_unit_mentions.csv $@ project_id permit_number mention_order

../report/project_permit_chain_summary.csv.log: ../../shared/code/report.py ../output/project_permit_chain_summary.csv | ../report
	$(PYTHON) $< ../output/project_permit_chain_summary.csv $@ project_id

../report/preferred_residential_boundary_scope.csv.log: ../../shared/code/report.py ../output/preferred_residential_boundary_scope.csv | ../report
	$(PYTHON) $< ../output/preferred_residential_boundary_scope.csv $@ project_id

../report/new_construction_exact_permit_matches.csv.log: ../../shared/code/report.py ../output/new_construction_exact_permit_matches.csv | ../report
	$(PYTHON) $< ../output/new_construction_exact_permit_matches.csv $@ project_id component_pin permit_id

../report/new_construction_permit_unit_mentions.csv.log: ../../shared/code/report.py ../output/new_construction_permit_unit_mentions.csv | ../report
	$(PYTHON) $< ../output/new_construction_permit_unit_mentions.csv $@ project_id component_pin permit_id mention_order

../report/multicard_permit_adjudication_links.csv.log: ../../shared/code/report.py ../output/multicard_permit_adjudication_links.csv | ../report
	$(PYTHON) $< ../output/multicard_permit_adjudication_links.csv $@ project_id permit_id

../report/multicard_final_adjudication.csv.log: ../../shared/code/report.py ../output/multicard_final_adjudication.csv | ../report
	$(PYTHON) $< ../output/multicard_final_adjudication.csv $@ project_id

../report/multicard_adjudicated_density_model_input.csv.log: ../../shared/code/report.py ../output/multicard_adjudicated_density_model_input.csv | ../report
	$(PYTHON) $< ../output/multicard_adjudicated_density_model_input.csv $@ project_id

../report/multicard_external_review_queue.csv.log: ../../shared/code/report.py ../output/multicard_external_review_queue.csv | ../report
	$(PYTHON) $< ../output/multicard_external_review_queue.csv $@ project_id

../report/multicard_external_reviewed_model_input.csv.log: ../../shared/code/report.py ../output/multicard_external_reviewed_model_input.csv | ../report
	$(PYTHON) $< ../output/multicard_external_reviewed_model_input.csv $@ project_id

../report/project_evidence_inventory.csv.log: ../../shared/code/report.py ../output/project_evidence_inventory.csv | ../report
	$(PYTHON) $< ../output/project_evidence_inventory.csv $@ project_id

../report/preferred_new_construction_project_ledger.csv.log: ../../shared/code/report.py ../output/preferred_new_construction_project_ledger.csv | ../report
	$(PYTHON) $< ../output/preferred_new_construction_project_ledger.csv $@ project_id


../report/preferred_residential_project_ledger.csv.log: ../../shared/code/report.py ../output/preferred_residential_project_ledger.csv | ../report
	$(PYTHON) $< ../output/preferred_residential_project_ledger.csv $@ project_id


../report/residential_adjudication_queue.csv.log: ../../shared/code/report.py ../output/residential_adjudication_queue.csv | ../report
	$(PYTHON) $< ../output/residential_adjudication_queue.csv $@ project_id


../report/preferred_residential_project_components.csv.log: ../../shared/code/report.py ../output/preferred_residential_project_components.csv | ../report
	$(PYTHON) $< ../output/preferred_residential_project_components.csv $@ project_id component_pin


../report/preferred_residential_project_candidates.csv.log: ../../shared/code/report.py ../output/preferred_residential_project_candidates.csv | ../report
	$(PYTHON) $< ../output/preferred_residential_project_candidates.csv $@ project_id


../report/multicard_adjudication_evidence.csv.log: ../../shared/code/report.py ../output/multicard_adjudication_evidence.csv | ../report
	$(PYTHON) $< ../output/multicard_adjudication_evidence.csv $@ project_id


../report/multicard_card_snapshot.csv.log: ../../shared/code/report.py ../output/multicard_card_snapshot.csv | ../report
	$(PYTHON) $< ../output/multicard_card_snapshot.csv $@ pin card_num


../report/multicard_project_evidence_base.csv.log: ../../shared/code/report.py ../output/multicard_project_evidence_base.csv | ../report
	$(PYTHON) $< ../output/multicard_project_evidence_base.csv $@ project_id


../report/multicard_successor_match_summary.csv.log: ../../shared/code/report.py ../output/multicard_successor_match_summary.csv | ../report
	$(PYTHON) $< ../output/multicard_successor_match_summary.csv $@ project_id


../report/multicard_same_episode_edges.csv.log: ../../shared/code/report.py ../output/multicard_same_episode_edges.csv | ../report
	$(PYTHON) $< ../output/multicard_same_episode_edges.csv $@ project_id child_project_id

../report/multicard_episode_component_nodes.csv.log: ../../shared/code/report.py ../output/multicard_episode_component_nodes.csv | ../report
	$(PYTHON) $< ../output/multicard_episode_component_nodes.csv $@ project_id

../report/multicard_episode_component_summary.csv.log: ../../shared/code/report.py ../output/multicard_episode_component_summary.csv | ../report
	$(PYTHON) $< ../output/multicard_episode_component_summary.csv $@ component_id

../report/multicard_component_successor_matches.csv.log: ../../shared/code/report.py ../output/multicard_component_successor_matches.csv | ../report
	$(PYTHON) $< ../output/multicard_component_successor_matches.csv $@ card_id

../report/multicard_final_review_bundle.csv.log: ../../shared/code/report.py ../output/multicard_final_review_bundle.csv | ../report
	$(PYTHON) $< ../output/multicard_final_review_bundle.csv $@ project_id

../report/multicard_successor_building_candidates.csv.log: ../../shared/code/report.py ../output/multicard_successor_building_candidates.csv | ../report
	$(PYTHON) $< ../output/multicard_successor_building_candidates.csv $@ project_id successor_id

../output/historical_zoning_project_construction_year.csv: build_project_construction_zoning.py ../adjudication/historical_zoning_reviewed_events.csv ../adjudication/historical_zoning_exact_preconstruction_support.csv ../adjudication/historical_zoning_recovered_ordinance_dates.csv ../output/historical_zoning_paper_sample_history_seeds.csv ../output/historical_zoning_project_comparison.csv ../output/historical_zoning_event_links.csv ../output/historical_zoning_2006_repeat_event_review_paper_sample.csv ../output/historical_zoning_matter_parsing.csv ../output/historical_zoning_ordinances_2006_2012.csv ../input/O2017-7056.pdf ../input/O2020-2959.pdf ../input/O2022-2464.pdf ../input/O2023-1331.pdf ../input/O2024-0008982.pdf ../input/O2024-0010153.pdf ../input/SO2017-7051.pdf ../input/SO2018-4452.pdf ../input/SO2018-7749.pdf ../input/SO2018-863.pdf ../input/SO2019-4107.pdf ../input/SO2023-43.pdf ../input/matters_20101101_20260212.csv | ../output
	$(PYTHON) $<

../report/historical_zoning_project_construction_year.csv.log: ../../shared/code/report.py ../output/historical_zoning_project_construction_year.csv | ../report
	$(PYTHON) $< ../output/historical_zoning_project_construction_year.csv $@ pin

../output/historical_zoning_2006_candidate.gpkg: build_candidate_2006_zoning_map.R ../../setup_environment/code/packages.R ../input/zoning_nov2012.zip ../input/zoning_map_index.geojson ../output/historical_zoning_ordinances_2006_2012.csv ../output/historical_zoning_matter_parsing.csv | ../output
	$(R) $<

all: ../report/preferred_project_year_geometry.gpkg.log ../report/preferred_project_component_geometry.gpkg.log ../report/preferred_project_year_centroids.gpkg.log ../report/preferred_project_year_geometry_coverage.csv.log ../report/preferred_project_boundary_scope.csv.log ../report/preferred_adjudication_scope.csv.log

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

ifneq ($(filter-out $(wildcard ../output/residential_review_current_parcel_links.csv ../output/residential_successor_condo_requests.csv),../output/residential_review_current_parcel_links.csv ../output/residential_successor_condo_requests.csv),)
.PHONY: ../output/residential_successor_condo_requests.csv
endif

../output/residential_successor_condo_requests.csv: build_residential_successor_condo_requests.R ../../setup_environment/code/packages.R ../adjudication/residential_successor_condo_overrides.csv ../input/parcel_universe_2025_city.csv ../output/preferred_project_year_geometry.gpkg ../output/residential_manual_review_bundle.csv | ../output
	$(R) $<

../output/residential_review_current_parcel_links.csv: ../output/residential_successor_condo_requests.csv
	@test -f "$@"

../report/residential_successor_condo_requests.csv.log: ../../shared/code/report.py ../output/residential_successor_condo_requests.csv | ../report
	$(PYTHON) $< ../output/residential_successor_condo_requests.csv $@ project_id pin10

../report/residential_review_current_parcel_links.csv.log: ../../shared/code/report.py ../output/residential_review_current_parcel_links.csv | ../report
	$(PYTHON) $< ../output/residential_review_current_parcel_links.csv $@ project_id pin

../output/residential_shared_site_location_review.csv: build_residential_shared_site_location_review.R ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R ../adjudication/residential_candidate_suppressions.csv ../adjudication/residential_additional_candidate_decisions.csv ../output/preferred_residential_project_candidates.csv ../output/preferred_residential_project_components.csv ../output/preferred_project_component_geometry.gpkg ../output/preferred_predecessor_reference_points.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	$(R) $<

../report/residential_shared_site_location_review.csv.log: ../../shared/code/report.py ../output/residential_shared_site_location_review.csv | ../report
	$(PYTHON) $< ../output/residential_shared_site_location_review.csv $@ project_id

ifneq ($(filter-out $(wildcard ../output/residential_tieback_card_project_evidence.csv ../output/residential_tieback_construction_episode_evidence.csv),../output/residential_tieback_card_project_evidence.csv ../output/residential_tieback_construction_episode_evidence.csv),)
.PHONY: ../output/residential_tieback_card_project_evidence.csv
endif

../output/residential_tieback_card_project_evidence.csv: build_residential_tieback_card_evidence.R ../../setup_environment/code/packages.R ../output/residential_manual_review_bundle.csv ../output/residential_review_assessor_history.csv | ../output
	$(R) $<

../output/residential_tieback_construction_episode_evidence.csv: ../output/residential_tieback_card_project_evidence.csv
	@test -f "$@"

../report/residential_tieback_card_project_evidence.csv.log: ../../shared/code/report.py ../output/residential_tieback_card_project_evidence.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_card_project_evidence.csv $@ project_id

../report/residential_tieback_construction_episode_evidence.csv.log: ../../shared/code/report.py ../output/residential_tieback_construction_episode_evidence.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_construction_episode_evidence.csv $@ project_id tax_year construction_year

../output/residential_manual_review_bundle.csv: build_residential_manual_review_bundle.R ../../setup_environment/code/packages.R ../output/density_parcel_address_selected_history.csv ../output/preferred_adjudication_scope.csv ../output/preferred_commercial_project_candidates.csv ../output/preferred_commercial_project_components.csv ../output/preferred_historical_address_geocodes.csv ../output/preferred_residential_project_components.csv ../output/project_overlap_evidence.csv ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/residential_adjudication_queue.csv ../output/residential_multicard_cards.csv ../output/residential_project_history_summary.csv ../output/residential_review_city_building_evidence.csv ../output/residential_tieback_temporal_lineage_evidence.csv ../output/residential_tieback_temporal_snapshots.csv | ../output
	$(R) $<

../report/residential_manual_review_bundle.csv.log: ../../shared/code/report.py ../output/residential_manual_review_bundle.csv | ../report
	$(PYTHON) $< ../output/residential_manual_review_bundle.csv $@ project_id

../output/residential_review_assessor_history.csv: build_residential_review_assessor_history.R ../../setup_environment/code/packages.R ../output/residential_manual_review_bundle.csv ../input/residential_improvement_characteristics_full.csv | ../output
	$(R) $<

../report/residential_review_assessor_history.csv.log: ../../shared/code/report.py ../output/residential_review_assessor_history.csv | ../report
	$(PYTHON) $< ../output/residential_review_assessor_history.csv $@ project_id pin tax_year card_num

../output/residential_review_city_building_evidence.csv: build_residential_city_building_evidence.R ../../setup_environment/code/packages.R ../output/preferred_adjudication_scope.csv ../output/preferred_project_year_geometry.gpkg ../output/residential_review_city_building_footprints.gpkg | ../output
	$(R) $<

../report/residential_review_city_building_evidence.csv.log: ../../shared/code/report.py ../output/residential_review_city_building_evidence.csv | ../report
	$(PYTHON) $< ../output/residential_review_city_building_evidence.csv $@ project_id

../output/residential_unresolved_predecessor_selected.gpkg: build_residential_unresolved_predecessor_selections.R ../../setup_environment/code/packages.R ../adjudication/residential_unresolved_predecessor_selections.csv ../output/residential_unresolved_predecessor_candidates.gpkg | ../output
	$(R) $<

../report/residential_class297_resolution.csv.log: ../../shared/code/report.py ../output/residential_class297_resolution.csv | ../report
	$(PYTHON) $< ../output/residential_class297_resolution.csv $@

../output/residential_class297_condo_cohort_evidence.csv: build_residential_class297_condo_evidence.R ../../setup_environment/code/packages.R ../output/residential_manual_review_bundle.csv ../output/residential_successor_condo_base_year_summary.csv ../output/residential_successor_condo_requests.csv | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/residential_class297_resolution.csv ../output/residential_class297_source_disposition.csv),../output/residential_class297_resolution.csv ../output/residential_class297_source_disposition.csv),)
.PHONY: ../output/residential_class297_resolution.csv
endif

../output/residential_class297_source_disposition.csv: ../output/residential_class297_resolution.csv
	@test -f "$@"

../report/residential_class297_condo_cohort_evidence.csv.log: ../../shared/code/report.py ../output/residential_class297_condo_cohort_evidence.csv | ../report
	$(PYTHON) $< ../output/residential_class297_condo_cohort_evidence.csv $@ project_id pin10

../report/residential_class297_source_disposition.csv.log: ../../shared/code/report.py ../output/residential_class297_source_disposition.csv | ../report
	$(PYTHON) $< ../output/residential_class297_source_disposition.csv $@ source_project_id

../output/project_overlap_evidence.csv: build_project_overlap_evidence.R ../../setup_environment/code/packages.R ../output/preferred_adjudication_scope.csv ../output/preferred_commercial_project_candidates.csv ../output/preferred_project_year_geometry.gpkg ../output/preferred_residential_project_candidates.csv | ../output
	$(R) $<

../report/project_overlap_evidence.csv.log: ../../shared/code/report.py ../output/project_overlap_evidence.csv | ../report
	$(PYTHON) $< ../output/project_overlap_evidence.csv $@ source_family project_id

../output/residential_remaining_case_evidence.csv: build_residential_remaining_case_evidence.R ../../setup_environment/code/packages.R ../input/building_permits_clean.gpkg ../input/parcel_addresses_2025_chicago.csv ../output/new_construction_spatial_permit_matches.csv ../output/preferred_project_year_geometry.gpkg ../output/residential_class297_source_disposition.csv ../output/residential_manual_review_bundle.csv ../output/residential_overlap_resolution.csv ../output/residential_review_current_parcel_links.csv ../output/residential_successor_condo_evidence.csv ../output/residential_tieback_episode_resolution.csv ../output/residential_tieback_no_snapshot_resolution.csv ../input/residential_improvement_characteristics_full.csv | ../output
	$(R) $<

../report/residential_remaining_case_evidence.csv.log: ../../shared/code/report.py ../output/residential_remaining_case_evidence.csv | ../report
	$(PYTHON) $< ../output/residential_remaining_case_evidence.csv $@ source_project_id

../output/final_verified_density_input.csv: build_final_verified_density_input.R ../adjudication/density_denominator_decisions.csv ../../shared/code/canonical_geometry_helpers.R ../../setup_environment/code/packages.R ../adjudication/corrected_year_zoning_decisions.csv ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_terms.csv ../input/ward_controls_2000_2023.csv ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/final_new_construction_boundary_scope.csv ../output/final_new_construction_zoning.csv ../output/final_project_verification_ledger.csv ../output/provisional_validated_density_input.csv | ../output
	$(R) $<

../output/final_project_verification_ledger.csv: build_final_project_verification_ledger.R ../../setup_environment/code/packages.R ../adjudication/assessor_default_project_exceptions.csv ../adjudication/assessor_default_site_reviews.csv ../adjudication/final_project_overrides.csv ../output/assessor_only_exact_permit_summary.csv ../output/assessor_only_validation.csv ../output/reviewed_project_ledger.csv | ../output
	$(R) $<

../output/assessor_only_exact_permit_summary.csv: build_assessor_only_exact_permit_review.R ../../setup_environment/code/packages.R ../output/building_permits_for_verification.gpkg ../output/reviewed_project_ledger.csv | ../output
	$(R) $<

../output/building_permits_for_verification.gpkg: build_construction_permit_history.R ../../setup_environment/code/packages.R ../input/building_permits_full.csv Makefile | ../output
	$(R) $<

../output/reviewed_project_ledger.csv: build_reviewed_project_ledger.R ../../setup_environment/code/packages.R ../adjudication/project_manual_reviews.csv ../output/extended_permit_candidate_summary.csv ../output/project_permit_history.csv ../output/project_verification_ledger.csv ../output/provisional_validated_density_input.csv | ../output
	$(R) $<

../output/extended_permit_candidate_summary.csv: build_extended_permit_candidates.R ../../setup_environment/code/packages.R ../output/building_permits_for_verification.gpkg ../output/project_verification_ledger.csv | ../output
	$(R) $<

../output/project_verification_ledger.csv: build_project_verification_ledger.R ../../setup_environment/code/packages.R ../output/building_permits_for_verification.gpkg ../input/official_building_footprints_2008.gpkg ../input/official_building_footprints_2022.gpkg ../input/parcel_addresses_2025_chicago.csv ../input/density_historical_address_records.csv ../output/eligibility_uncorroborated_retained.csv ../output/multicard_external_review_queue.csv ../output/multicard_permit_adjudication_links.csv ../output/official_building_footprints_2015.gpkg ../output/preferred_project_year_geometry.gpkg | ../output
	$(R) $<

../output/eligibility_uncorroborated_retained.csv: ../output/eligibility_rule_validation.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/eligibility_rule_validation.csv ../output/eligibility_uncorroborated_retained.csv),../output/eligibility_rule_validation.csv ../output/eligibility_uncorroborated_retained.csv),)
.PHONY: ../output/eligibility_rule_validation.csv
endif

../output/eligibility_rule_validation.csv: validate_new_construction_eligibility.R ../adjudication/eligibility_manual_exceptions.csv ../output/commercial_completion_evidence.csv ../output/multicard_external_reviewed_model_input.csv ../output/permit_rule_coverage.csv ../output/predecessor_assessor_evidence.csv ../output/presample_assessor_structure_evidence.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/commercial_completion_evidence.csv: build_commercial_completion_evidence.R ../output/commercial_address_permit_history.csv ../output/commercial_ground_up_evidence.csv ../output/commercial_residential_history_evidence.csv ../output/new_construction_spatial_permit_matches.csv ../output/preferred_commercial_project_candidates.csv ../output/project_permit_chain_links.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_external_reviewed_model_input.csv: apply_multicard_external_reviews.R ../output/multicard_adjudicated_density_model_input.csv ../output/multicard_external_review_queue.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_adjudicated_density_model_input.csv: ../output/multicard_final_adjudication.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/multicard_final_adjudication.csv ../output/multicard_adjudicated_density_model_input.csv),../output/multicard_final_adjudication.csv ../output/multicard_adjudicated_density_model_input.csv),)
.PHONY: ../output/multicard_final_adjudication.csv
endif

../output/multicard_final_adjudication.csv: build_final_multicard_adjudication.R ../adjudication/multicard_cross_project_suppressions.csv ../adjudication/multicard_manual_episode_decisions.csv ../adjudication/multicard_manual_overrides.csv ../adjudication/multicard_parent_pair_decisions.csv ../adjudication/multicard_year_overrides.csv ../input/boundary_segments_1320ft.gpkg ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/final_density_model_input.csv ../output/final_new_construction_boundary_scope.csv ../output/multicard_component_successor_matches.csv ../output/multicard_final_review_bundle.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/multicard_final_review_bundle.csv: build_multicard_final_review_bundle.R ../output/multicard_successor_match_summary.csv ../adjudication/early_multicard_manual_review.csv ../output/multicard_adjudication_evidence.csv ../output/multicard_episode_component_nodes.csv ../output/multicard_episode_component_summary.csv ../output/multicard_successor_building_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_episode_component_nodes.csv: ../output/multicard_episode_component_summary.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/multicard_episode_component_summary.csv ../output/multicard_episode_component_nodes.csv),../output/multicard_episode_component_summary.csv ../output/multicard_episode_component_nodes.csv),)
.PHONY: ../output/multicard_episode_component_summary.csv
endif

../output/multicard_episode_component_summary.csv: build_multicard_episode_components.R ../output/multicard_project_evidence_base.csv ../output/multicard_same_episode_edges.csv ../output/preferred_new_construction_project_ledger.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_project_evidence_base.csv: ../output/multicard_card_snapshot.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/multicard_card_snapshot.csv ../output/multicard_project_evidence_base.csv),../output/multicard_card_snapshot.csv ../output/multicard_project_evidence_base.csv),)
.PHONY: ../output/multicard_card_snapshot.csv
endif

../output/multicard_card_snapshot.csv: build_multicard_evidence_inventory.R ../adjudication/early_multicard_manual_review.csv ../input/parcel_addresses_2025_chicago.csv ../input/residential_assessor_history.parquet ../output/final_density_model_input.csv ../output/preferred_residential_project_ledger.csv ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/residential_multicard_cards.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_external_review_queue.csv: build_multicard_external_review_queue.R ../adjudication/multicard_external_web_reviews.csv ../output/multicard_card_snapshot.csv ../output/multicard_current_successor_project_summary.csv ../output/multicard_final_adjudication.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/multicard_current_successor_project_summary.csv ../output/multicard_current_successor_links.csv),../output/multicard_current_successor_project_summary.csv ../output/multicard_current_successor_links.csv),)
.PHONY: ../output/multicard_current_successor_project_summary.csv
endif

../output/multicard_current_successor_project_summary.csv: build_multicard_current_successor_evidence.R ../input/parcel_addresses_2025_chicago.csv ../input/parcel_universe_2025_city.csv ../input/residential_improvement_characteristics_full.csv ../output/multicard_project_evidence_base.csv ../output/preferred_new_construction_project_centroids.gpkg ../output/preferred_new_construction_project_components.csv ../output/preferred_project_year_geometry.gpkg ../output/residential_unresolved_adjacent_year_parcels.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/permit_rule_coverage.csv: audit_permit_rule_coverage.R ../output/building_permits_for_verification.gpkg ../output/project_evidence_inventory.csv ../output/project_permit_chain_links.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/project_evidence_inventory.csv: build_project_evidence_inventory.R ../output/final_density_model_input.csv ../output/final_new_construction_audit_ledger.csv ../output/historical_permit_project_evidence.csv ../output/multicard_external_review_queue.csv ../output/new_construction_exact_permit_matches.csv ../output/new_construction_permit_unit_mentions.csv ../output/new_construction_spatial_permit_matches.csv ../output/preferred_project_duplicate_dispositions.csv ../output/preferred_residential_project_ledger.csv ../output/residential_project_candidate_inventory.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/historical_permit_project_evidence.csv: build_historical_permit_project_evidence.R ../output/building_permits_for_verification.gpkg ../output/final_new_construction_audit_ledger.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/predecessor_assessor_evidence.csv: build_predecessor_assessor_evidence.R ../output/permit_rule_coverage.csv ../output/preferred_historical_predecessor_resolution.csv ../input/residential_improvement_characteristics_full.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/presample_assessor_structure_evidence.csv: build_presample_assessor_structure_evidence.R ../input/residential_improvement_characteristics_full.csv ../output/permit_rule_coverage.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_permit_adjudication_links.csv: build_multicard_permit_adjudication_evidence.R ../output/building_permits_for_verification.gpkg ../output/final_new_construction_audit_ledger.csv ../output/multicard_episode_component_nodes.csv ../output/multicard_final_review_bundle.csv ../output/preferred_project_year_geometry.gpkg ../../setup_environment/code/packages.R ../../shared/code/permit_unit_patterns.R | ../output
	$(R) $<

../output/official_building_footprints_2015.gpkg: build_2015_footprint_snapshot.R ../../setup_environment/code/packages.R ../output/eligibility_uncorroborated_retained.csv ../output/preferred_project_year_geometry.gpkg ../input/chicago_building_footprints_2015.zip | ../output
	$(R) $<

../output/project_permit_history.csv: build_project_permit_history.R ../../setup_environment/code/packages.R ../output/building_permits_for_verification.gpkg ../output/project_verification_ledger.csv | ../output
	$(R) $<

../output/multicard_adjudication_evidence.csv: build_multicard_adjudication_queue.R ../output/density_parcel_address_selected_history.csv ../output/multicard_footprint_evidence.csv ../output/multicard_project_evidence_base.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_successor_building_candidates.csv: build_multicard_successor_building_candidates.R ../input/parcel_addresses_2025_chicago.csv ../input/parcel_universe_2025_city.csv ../output/multicard_current_successor_links.csv ../output/multicard_project_evidence_base.csv ../output/multicard_successor_condo_evidence.csv ../output/multicard_successor_condo_links.csv ../output/multicard_successor_condo_requests.csv ../../setup_environment/code/packages.R Makefile | ../output
	$(R) $< $(EPISODE_YEAR_WINDOW)

../output/multicard_successor_match_summary.csv: ../output/multicard_component_successor_matches.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/multicard_component_successor_matches.csv ../output/multicard_successor_match_summary.csv),../output/multicard_component_successor_matches.csv ../output/multicard_successor_match_summary.csv),)
.PHONY: ../output/multicard_component_successor_matches.csv
endif

../output/multicard_component_successor_matches.csv: match_multicard_components_to_successors.R Makefile ../output/multicard_card_snapshot.csv ../output/multicard_episode_component_nodes.csv ../output/multicard_successor_building_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $< $(AUTOMATIC_CARD_BUILDING_GAP)

../output/multicard_same_episode_edges.csv: build_multicard_episode_overlap.R ../output/multicard_current_successor_links.csv ../output/multicard_project_evidence_base.csv ../output/preferred_new_construction_project_ledger.csv ../../setup_environment/code/packages.R Makefile | ../output
	$(R) $< $(EPISODE_YEAR_WINDOW)

ifneq ($(filter-out $(wildcard ../output/residential_project_candidate_inventory.csv ../output/residential_project_history_summary.csv ../output/residential_fractional_base_groups.csv ../output/residential_tieback_groups_full.csv ../output/residential_tieback_members_full.csv ../output/residential_multicard_cards.csv),../output/residential_project_candidate_inventory.csv ../output/residential_project_history_summary.csv ../output/residential_fractional_base_groups.csv ../output/residential_tieback_groups_full.csv ../output/residential_tieback_members_full.csv ../output/residential_multicard_cards.csv),)
.PHONY: ../output/residential_project_candidate_inventory.csv
endif

../output/residential_project_candidate_inventory.csv: build_residential_project_candidates.R ../adjudication/residential_reviewed_card_selections.csv ../input/residential_assessor_history.parquet ../output/geocoded_residential_data.gpkg ../output/multifamily_data_cleaned.csv ../output/construction_parcel_boundary_distances.csv ../output/residential_cross_section.csv ../../setup_environment/code/packages.R Makefile | ../output
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

../output/multicard_footprint_evidence.csv: build_multicard_footprint_evidence.R ../output/cook_building_footprints_2006_2008.gpkg ../output/multicard_city_building_footprints.gpkg ../output/multicard_project_query_geometries.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/project_permit_chain_summary.csv),../output/project_permit_chain_links.csv ../output/project_permit_chain_unit_mentions.csv ../output/project_permit_chain_summary.csv),)
.PHONY: ../output/project_permit_chain_links.csv
endif

../output/project_permit_chain_links.csv: build_permit_revision_evidence.R ../output/building_permits_for_verification.gpkg ../output/new_construction_exact_permit_matches.csv ../output/new_construction_spatial_permit_matches.csv ../../setup_environment/code/packages.R ../../shared/code/permit_unit_patterns.R | ../output
	$(R) $<

../output/project_permit_chain_unit_mentions.csv: ../output/project_permit_chain_links.csv
	@test -f "$@"

../output/project_permit_chain_summary.csv: ../output/project_permit_chain_links.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/final_new_construction_audit_ledger.csv ../output/final_recovered_missing_project_dedupe_screen.csv ../output/final_recovered_missing_project_pair_screen.csv ../output/final_residual_permit_chain_dispositions.csv),../output/final_new_construction_audit_ledger.csv ../output/final_recovered_missing_project_dedupe_screen.csv ../output/final_recovered_missing_project_pair_screen.csv ../output/final_residual_permit_chain_dispositions.csv),)
.PHONY: ../output/final_new_construction_audit_ledger.csv
endif

../output/final_new_construction_audit_ledger.csv: build_final_new_construction_audit_ledger.R ../adjudication/residual_historical_candidate_overrides.csv ../input/parcel_universe_2025_city.csv ../output/preferred_new_construction_project_ledger.csv ../output/preferred_project_duplicate_dispositions.csv ../output/residual_footprint_field_transitions.csv ../output/residual_footprint_multi_parcel_evidence.csv ../output/residual_permit_footprint_2022_assessor_evidence.csv ../output/residual_permit_historical_chain_evidence.csv ../output/residual_permit_historical_parcels.gpkg ../output/residual_permit_historical_pin_reconciliation.csv ../output/residual_permit_nearest_historical_parcels.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/final_recovered_missing_project_dedupe_screen.csv: ../output/final_new_construction_audit_ledger.csv
	@test -f "$@"

../output/final_recovered_missing_project_pair_screen.csv: ../output/final_new_construction_audit_ledger.csv
	@test -f "$@"

../output/final_residual_permit_chain_dispositions.csv: ../output/final_new_construction_audit_ledger.csv
	@test -f "$@"

../output/final_new_construction_boundary_scope.csv: validate_final_new_construction_ledger.R ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/final_adversarial_duplicate_summary.csv ../output/final_new_construction_audit_ledger.csv ../output/final_recovered_missing_project_dedupe_screen.csv ../output/final_recovered_missing_project_pair_screen.csv ../output/final_residual_permit_chain_dispositions.csv ../output/preferred_project_duplicate_dispositions.csv ../output/preferred_project_duplicate_pair_dispositions.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/multicard_successor_condo_requests.csv ../output/multicard_successor_condo_links.csv),../output/multicard_successor_condo_requests.csv ../output/multicard_successor_condo_links.csv),)
.PHONY: ../output/multicard_successor_condo_requests.csv
endif

../output/multicard_successor_condo_requests.csv: build_multicard_successor_condo_requests.R ../input/parcel_universe_2025_city.csv ../output/multicard_project_evidence_base.csv ../output/preferred_new_construction_project_centroids.gpkg ../output/preferred_project_year_geometry.gpkg ../output/residential_unresolved_adjacent_year_parcels.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multicard_successor_condo_links.csv: ../output/multicard_successor_condo_requests.csv
	@test -f "$@"

../output/geocoded_residential_data.gpkg: geocode_residential_data.R ../output/residential_cross_section.csv ../output/multifamily_data_cleaned.csv ../output/density_historical_coordinates.csv ../input/parcel_universe_2025_city.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/construction_discovery_ward_maps.gpkg: build_construction_discovery_ward_maps.R ../input/Chicago_Wards_1998.geojson ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/construction_discovery_boundaries.gpkg: build_construction_discovery_boundaries.R ../output/construction_discovery_ward_maps.gpkg ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/construction_parcel_boundary_distances.csv: build_construction_parcel_distances.R ../output/construction_discovery_ward_maps.gpkg ../output/construction_discovery_boundaries.gpkg ../output/geocoded_residential_data.gpkg ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R Makefile | ../output
	$(R) $< $(MIN_CONSTRUCTION_YEAR) $(MAX_DISCOVERY_CONSTRUCTION_YEAR)

../output/final_new_construction_zoning.csv: build_final_construction_zoning.R ../../setup_environment/code/packages.R ../output/historical_zoning_2006_candidate.gpkg ../adjudication/recovered_project_zoning_overrides.csv ../input/zoning_jan2016.zip ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_sep2025.geojson ../output/final_new_construction_audit_ledger.csv ../output/final_new_construction_boundary_scope.csv ../output/historical_zoning_project_construction_year.csv ../output/preferred_new_construction_zoning.csv | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/preferred_residential_project_ledger.csv ../output/preferred_residential_project_components_final.csv ../output/residential_adjudicated_project_geometry.gpkg ../output/preferred_residential_project_centroids.gpkg),../output/preferred_residential_project_ledger.csv ../output/preferred_residential_project_components_final.csv ../output/residential_adjudicated_project_geometry.gpkg ../output/preferred_residential_project_centroids.gpkg),)
.PHONY: ../output/preferred_residential_project_ledger.csv
endif

../output/preferred_residential_project_ledger.csv: build_residential_unresolved_final_ledger.R ../adjudication/residential_additional_candidate_decisions.csv ../adjudication/residential_candidate_suppressions.csv ../adjudication/residential_unresolved_final_projects.csv ../adjudication/residential_unresolved_source_dispositions.csv ../output/building_permits_for_verification.gpkg ../output/preferred_commercial_projects.csv ../output/preferred_predecessor_reference_points.csv ../output/preferred_project_component_geometry.gpkg ../output/preferred_project_year_centroids.gpkg ../output/preferred_residential_project_candidates.csv ../output/preferred_residential_project_components.csv ../output/residential_review_resolution_components.csv ../output/residential_review_resolution_projects.csv ../output/residential_review_source_dispositions.csv ../output/residential_shared_site_location_review.csv ../output/residential_unresolved_accepted_episode_geometry.gpkg ../output/residential_unresolved_adjacent_year_parcels.gpkg ../output/residential_unresolved_adjudication_evidence.csv ../output/residential_unresolved_episode_component_geometry.gpkg ../output/residential_unresolved_predecessor_reference_points.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_residential_project_components_final.csv: ../output/preferred_residential_project_ledger.csv
	@test -f "$@"

../output/residential_adjudicated_project_geometry.gpkg: ../output/preferred_residential_project_ledger.csv
	@test -f "$@"

../output/preferred_residential_project_centroids.gpkg: ../output/preferred_residential_project_ledger.csv
	@test -f "$@"

../output/preferred_residential_boundary_scope.csv: validate_preferred_residential_ledger.R ../adjudication/residential_additional_candidate_decisions.csv ../adjudication/residential_candidate_suppressions.csv ../adjudication/residential_class297_component_overrides.csv ../adjudication/residential_unresolved_final_projects.csv ../adjudication/residential_unresolved_source_dispositions.csv ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/preferred_adjudication_scope.csv ../output/preferred_commercial_projects.csv ../output/preferred_project_year_geometry.gpkg ../output/preferred_residential_project_candidates.csv ../output/preferred_residential_project_centroids.gpkg ../output/preferred_residential_project_components_final.csv ../output/preferred_residential_project_ledger.csv ../output/residential_adjudicated_project_geometry.gpkg ../output/residential_review_resolution_projects.csv ../output/residential_review_source_dispositions.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/preferred_new_construction_project_ledger.csv ../output/preferred_new_construction_project_components.csv ../output/preferred_new_construction_boundary_scope.csv ../output/preferred_new_construction_project_centroids.gpkg),../output/preferred_new_construction_project_ledger.csv ../output/preferred_new_construction_project_components.csv ../output/preferred_new_construction_boundary_scope.csv ../output/preferred_new_construction_project_centroids.gpkg),)
.PHONY: ../output/preferred_new_construction_project_ledger.csv
endif

../output/preferred_new_construction_project_ledger.csv: build_preferred_new_construction_ledger.R ../adjudication/residential_additional_candidate_decisions.csv ../output/preferred_commercial_boundary_scope.csv ../output/preferred_commercial_project_centroids.gpkg ../output/preferred_commercial_project_component_locations.csv ../output/preferred_commercial_project_ledger.csv ../output/preferred_residential_boundary_scope.csv ../output/preferred_residential_project_centroids.gpkg ../output/preferred_residential_project_components_final.csv ../output/preferred_residential_project_ledger.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/preferred_new_construction_project_components.csv: ../output/preferred_new_construction_project_ledger.csv
	@test -f "$@"

../output/preferred_new_construction_boundary_scope.csv: ../output/preferred_new_construction_project_ledger.csv
	@test -f "$@"

../output/preferred_new_construction_project_centroids.gpkg: ../output/preferred_new_construction_project_ledger.csv
	@test -f "$@"

../output/preferred_density_model_production_card_input.csv: build_preferred_density_input.R ../input/boundary_segments_1320ft.gpkg ../output/preferred_new_construction_boundary_scope.csv ../output/preferred_new_construction_project_centroids.gpkg ../output/preferred_new_construction_project_ledger.csv ../output/preferred_new_construction_zoning.csv ../output/residential_cross_section.csv ../output/residential_multicard_cards.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/final_density_model_input.csv: build_final_density_input.R ../input/boundary_segments_1320ft.gpkg ../output/final_new_construction_audit_ledger.csv ../output/final_new_construction_boundary_scope.csv ../output/final_new_construction_zoning.csv ../output/preferred_density_model_production_card_input.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/commercial_post_evidence_resolution.csv: build_commercial_post_evidence_queue.R ../adjudication/commercial_manual_decisions.csv ../output/commercial_completion_evidence.csv ../output/commercial_land_adjudication_evidence.csv ../output/commercial_unit_adjudication_evidence.csv ../output/preferred_adjudication_scope.csv ../output/preferred_commercial_project_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/preferred_commercial_projects.csv ../output/preferred_commercial_source_disposition.csv),../output/preferred_commercial_projects.csv ../output/preferred_commercial_source_disposition.csv),)
.PHONY: ../output/preferred_commercial_projects.csv
endif

../output/preferred_commercial_projects.csv: build_preferred_commercial_ledger.R ../adjudication/commercial_component_overrides.csv ../adjudication/commercial_cross_family_decisions.csv ../adjudication/commercial_field_decisions.csv ../adjudication/commercial_manual_decisions.csv ../adjudication/commercial_semantic_decisions.csv ../output/commercial_entity_version_candidates.csv ../output/commercial_post_evidence_resolution.csv ../output/preferred_commercial_project_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/preferred_commercial_projects.csv.log: ../../shared/code/report.py ../output/preferred_commercial_projects.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_projects.csv $@ project_id

../report/preferred_commercial_source_disposition.csv.log: ../../shared/code/report.py ../output/preferred_commercial_source_disposition.csv | ../report
	$(PYTHON) $< ../output/preferred_commercial_source_disposition.csv $@ source_project_id

../output/preferred_commercial_source_disposition.csv: ../output/preferred_commercial_projects.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/preferred_commercial_project_ledger.csv ../output/preferred_commercial_project_component_locations.csv ../output/preferred_commercial_project_centroids.gpkg ../output/preferred_commercial_boundary_scope.csv),../output/preferred_commercial_project_ledger.csv ../output/preferred_commercial_project_component_locations.csv ../output/preferred_commercial_project_centroids.gpkg ../output/preferred_commercial_boundary_scope.csv),)
.PHONY: ../output/preferred_commercial_project_ledger.csv
endif

../output/preferred_commercial_project_ledger.csv: build_preferred_commercial_final_geography.R ../input/parcel_universe_2025_city.csv ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/preferred_commercial_final_historical_parcels.gpkg ../output/preferred_commercial_final_parcel_coverage.csv ../output/preferred_commercial_projects.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../output/preferred_commercial_project_component_locations.csv: ../output/preferred_commercial_project_ledger.csv
	@test -f "$@"

../output/preferred_commercial_project_centroids.gpkg: ../output/preferred_commercial_project_ledger.csv
	@test -f "$@"

../output/preferred_commercial_boundary_scope.csv: ../output/preferred_commercial_project_ledger.csv
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

ifneq ($(filter-out $(wildcard ../output/residential_unresolved_accepted_episode_geometry.gpkg ../output/residential_unresolved_accepted_episode_geometry_coverage.csv),../output/residential_unresolved_accepted_episode_geometry.gpkg ../output/residential_unresolved_accepted_episode_geometry_coverage.csv),)
.PHONY: ../output/residential_unresolved_accepted_episode_geometry.gpkg
endif

../output/residential_unresolved_accepted_episode_geometry.gpkg: build_residential_unresolved_accepted_episode_geometry.R ../output/residential_unresolved_episode_component_geometry.gpkg ../output/residential_unresolved_episode_inventory.csv ../output/residential_unresolved_predecessor_selected.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_unresolved_accepted_episode_geometry_coverage.csv: ../output/residential_unresolved_accepted_episode_geometry.gpkg
	@test -f "$@"

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

../output/residential_assessor_project_candidates.csv: build_residential_assessor_projects.R ../adjudication/residential_reviewed_home_replacements.csv ../../setup_environment/code/packages.R ../../shared/code/assessor_classification.R ../output/residential_project_candidate_inventory.csv ../output/residential_tieback_temporal_lineage_evidence.csv ../output/residential_multicard_cards.csv Makefile ../input/parcel_universe_2025_city.csv ../input/preferred_historical_parcel_source.gpkg ../input/preferred_predecessor_parcel_source.gpkg | ../output
	$(R) $< $(AUTOMATIC_CARD_BUILDING_GAP) $(EPISODE_YEAR_WINDOW)

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

ifneq ($(filter-out $(wildcard ../output/residential_review_resolution_projects.csv ../output/residential_review_resolution_components.csv ../output/residential_review_source_dispositions.csv),../output/residential_review_resolution_projects.csv ../output/residential_review_resolution_components.csv ../output/residential_review_source_dispositions.csv),)
.PHONY: ../output/residential_review_resolution_projects.csv
endif

../output/residential_review_resolution_projects.csv: build_residential_review_resolution_ledger.R ../adjudication/residential_class297_component_overrides.csv ../output/preferred_adjudication_scope.csv ../output/preferred_residential_project_candidates.csv ../output/residential_class297_resolution.csv ../output/residential_overlap_resolution.csv ../output/residential_remaining_case_resolution.csv ../output/residential_tieback_episode_resolution.csv ../output/residential_tieback_no_snapshot_resolution.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_review_resolution_components.csv: ../output/residential_review_resolution_projects.csv
	@test -f "$@"

../output/residential_review_source_dispositions.csv: ../output/residential_review_resolution_projects.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/preferred_project_year_geometry.gpkg ../output/preferred_project_component_geometry.gpkg ../output/preferred_project_year_centroids.gpkg ../output/preferred_project_year_geometry_coverage.csv),../output/preferred_project_year_geometry.gpkg ../output/preferred_project_component_geometry.gpkg ../output/preferred_project_year_centroids.gpkg ../output/preferred_project_year_geometry_coverage.csv),)
.PHONY: ../output/preferred_project_year_geometry.gpkg
endif

../output/preferred_project_year_geometry.gpkg: build_preferred_project_geography.R ../output/preferred_historical_parcel_coverage.csv ../output/preferred_historical_parcels.gpkg ../output/preferred_historical_predecessor_selected.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

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

../output/residential_class297_resolution.csv: build_residential_class297_resolution.R ../adjudication/residential_class297_exceptions.csv ../output/residential_class297_condo_cohort_evidence.csv ../output/residential_manual_review_bundle.csv ../output/residential_successor_condo_evidence.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_tieback_episode_resolution.csv: build_residential_tieback_episode_resolution.R ../adjudication/residential_tieback_episode_exceptions.csv ../output/residential_tieback_episode_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_tieback_no_snapshot_resolution.csv: build_residential_tieback_no_snapshot_resolution.R ../adjudication/residential_tieback_no_snapshot_decisions.csv ../output/residential_tieback_no_snapshot_review.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_remaining_case_resolution.csv: build_residential_remaining_case_resolution.R ../adjudication/residential_remaining_case_decisions.csv ../output/residential_remaining_case_evidence.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_overlap_resolution.csv: build_residential_overlap_resolution.R ../adjudication/residential_overlap_decisions.csv ../output/preferred_commercial_projects.csv ../output/preferred_residential_project_candidates.csv ../output/residential_manual_review_bundle.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

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

../output/preferred_new_construction_zoning.csv: build_preferred_construction_zoning.R ../input/zoning_jan2016.zip ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_sep2025.geojson ../output/historical_zoning_2006_candidate.gpkg ../output/historical_zoning_project_construction_year.csv ../output/preferred_new_construction_boundary_scope.csv ../output/preferred_new_construction_project_centroids.gpkg ../output/preferred_new_construction_project_components.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/residential_tieback_episode_candidates.csv ../output/residential_tieback_no_snapshot_review.csv),../output/residential_tieback_episode_candidates.csv ../output/residential_tieback_no_snapshot_review.csv),)
.PHONY: ../output/residential_tieback_episode_candidates.csv
endif

../output/residential_tieback_episode_candidates.csv: build_residential_tieback_resolution_candidates.R ../output/residential_class297_source_disposition.csv ../output/residential_manual_review_bundle.csv ../output/residential_overlap_resolution.csv ../output/residential_tieback_card_project_evidence.csv ../output/residential_tieback_construction_episode_evidence.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_tieback_no_snapshot_review.csv: ../output/residential_tieback_episode_candidates.csv
	@test -f "$@"

../output/provisional_validated_density_input.csv: build_provisional_validated_sample.R ../output/eligibility_rule_validation.csv ../output/multicard_external_reviewed_model_input.csv ../output/multifamily_classification_decisions.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/multifamily_classification_decisions.csv: validate_multifamily_classification_rules.R ../../shared/code/assessor_classification.R ../output/eligibility_rule_validation.csv ../output/project_evidence_inventory.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/assessor_only_validation.csv: build_assessor_only_validation.R ../../setup_environment/code/packages.R ../input/parcel_sales_city.csv ../output/reviewed_project_ledger.csv | ../output
	$(R) $<

../output/multicard_current_successor_links.csv: ../output/multicard_current_successor_project_summary.csv
	@test -f "$@"

../output/density_historical_coordinates.csv: select_historical_coordinates.R ../../setup_environment/code/packages.R ../adjudication/historical_coordinate_year_corrections.csv ../output/density_project_lineage.csv ../output/density_parcel_address_lineage_evidence.csv ../output/density_historical_coordinate_candidates.csv | ../output
	$(R) $<

../output/density_project_lineage.csv: build_historical_project_screen.R ../../setup_environment/code/packages.R ../output/density_historical_building_universe.csv ../input/density_historical_parcel_records.csv Makefile | ../output
	$(R) $< $(HISTORICAL_EXACT_MATCH_FT) $(HISTORICAL_LOCAL_MATCH_FT) $(HISTORICAL_MAX_BUILDING_GAP)

../output/density_historical_building_universe.csv: build_historical_coordinate_requests.R ../../setup_environment/code/packages.R ../output/residential_discovery_cross_section.csv ../output/multifamily_data_cleaned.csv ../input/parcel_universe_2025_city.csv Makefile | ../output
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) $(MIN_INITIAL_AREA_SQFT)

../output/residential_discovery_cross_section.csv: build_residential_discovery_cross_section.R ../../setup_environment/code/packages.R ../input/residential_assessor_history.parquet Makefile | ../output
	$(R) $< $(MIN_CONSTRUCTION_YEAR)

../output/multifamily_data_cleaned.csv: build_commercial_cross_section.R ../../setup_environment/code/packages.R ../input/commercial_value_raw.csv ../adjudication/initial_commercial_unit_overrides.csv | ../output
	$(R) $<

../report/density_parcel_address_selected_history.csv.log: ../../shared/code/report.py ../output/density_parcel_address_selected_history.csv | ../report
	$(PYTHON) $< ../output/density_parcel_address_selected_history.csv $@ pin

../output/density_parcel_address_selected_history.csv: build_historical_address_history.R ../../setup_environment/code/packages.R ../input/historical_parcel_requests.csv ../input/density_historical_address_records.csv ../../shared/code/normalize_chicago_address.R ../adjudication/historical_address_corrections.csv | ../output
	$(R) $<

../output/density_parcel_address_lineage_evidence.csv: build_historical_address_screen.R ../../setup_environment/code/packages.R ../output/density_project_lineage.csv ../output/density_historical_building_universe.csv ../output/density_historical_coordinate_candidates.csv ../output/density_parcel_address_selected_history.csv ../input/parcel_addresses_2025_chicago.csv Makefile | ../output
	$(R) $< $(HISTORICAL_MAX_BUILDING_GAP)

../output/density_historical_coordinate_candidates.csv: build_historical_coordinates.R ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R ../output/density_historical_building_universe.csv ../input/density_historical_parcel_records.csv ../adjudication/historical_address_matches.csv ../input/parcel_universe_2025_city.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg Makefile | ../output
	$(R) $< $(MAX_DISTANCE_FT)

../output/residential_cross_section.csv: build_residential_cross_section.R ../../setup_environment/code/packages.R ../input/residential_assessor_history.parquet Makefile | ../output
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

../report/final_verified_density_input.csv.log: ../output/final_verified_density_input.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ project_id


../report/commercial_ground_up_evidence.csv.log: ../../shared/code/report.py ../output/commercial_ground_up_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_ground_up_evidence.csv $@ project_id

../report/commercial_permit_project_evidence.csv.log: ../../shared/code/report.py ../output/commercial_permit_project_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_permit_project_evidence.csv $@ project_id

../report/commercial_permit_chain_evidence.csv.log: ../../shared/code/report.py ../output/commercial_permit_chain_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_permit_chain_evidence.csv $@ project_id permit_chain_id

../report/commercial_permit_address_evidence.csv.log: ../../shared/code/report.py ../output/commercial_permit_address_evidence.csv | ../report
	$(PYTHON) $< ../output/commercial_permit_address_evidence.csv $@ project_id address_key


../report/preferred_new_construction_zoning.csv.log: ../../shared/code/report.py ../output/preferred_new_construction_zoning.csv | ../report
	$(PYTHON) $< ../output/preferred_new_construction_zoning.csv $@ project_id

../report/residential_tieback_episode_candidates.csv.log: ../../shared/code/report.py ../output/residential_tieback_episode_candidates.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_episode_candidates.csv $@ proposed_project_id

../report/residential_tieback_no_snapshot_review.csv.log: ../../shared/code/report.py ../output/residential_tieback_no_snapshot_review.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_no_snapshot_review.csv $@ source_project_id


../report/final_new_construction_zoning.csv.log: ../../shared/code/report.py ../output/final_new_construction_zoning.csv | ../report
	$(PYTHON) $< ../output/final_new_construction_zoning.csv $@ project_id

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

link-inputs: ../input/historical_project_parcel_source.gpkg ../input/historical_project_parcel_source_queries.csv ../input/predecessor_parcel_history.csv ../input/predecessor_history_queries.csv ../input/historical_predecessor_parcel_source.gpkg ../input/historical_predecessor_source_queries.csv

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

../input/parcel_sales_city.csv: ../../../data_raw/construction_review/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_controls_2000_2023.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/O2017-7056.pdf: ../../../data_raw/construction_review/O2017-7056.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/O2020-2959.pdf: ../../../data_raw/construction_review/O2020-2959.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/O2022-2464.pdf: ../../../data_raw/construction_review/O2022-2464.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/O2023-1331.pdf: ../../../data_raw/construction_review/O2023-1331.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/O2024-0008982.pdf: ../../../data_raw/construction_review/O2024-0008982.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/O2024-0010153.pdf: ../../../data_raw/construction_review/O2024-0010153.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/SO2017-7051.pdf: ../../../data_raw/construction_review/SO2017-7051.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/SO2018-4452.pdf: ../../../data_raw/construction_review/SO2018-4452.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/SO2018-7749.pdf: ../../../data_raw/construction_review/SO2018-7749.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/SO2018-863.pdf: ../../../data_raw/construction_review/SO2018-863.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/SO2019-4107.pdf: ../../../data_raw/construction_review/SO2019-4107.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/SO2023-43.pdf: ../../../data_raw/construction_review/SO2023-43.pdf | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/matters_20101101_20260212.csv: ../../../data_raw/construction_review/matters_20101101_20260212.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_map_index.geojson: ../../../data_raw/construction_review/zoning_map_index.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/density_historical_address_records.csv: ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_building_footprints_2015.zip: ../../download_city_building_footprints/output/chicago_building_footprints_2015.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/official_building_footprints_2008.gpkg: ../../../data_raw/construction_review/official_building_footprints_2008.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/official_building_footprints_2022.gpkg: ../../../data_raw/construction_review/official_building_footprints_2022.gpkg | ../input
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

../input/zoning_nov2012.zip: ../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_sep2014.zip: ../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_jan2016.zip: ../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_sep2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
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

link-inputs: ../input/address_geocodes_census.csv ../input/address_geocodes_chicago.csv

../input/preferred_predecessor_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_predecessor_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/residential_assessor_history.parquet ../input/O2017-7056.pdf ../input/O2020-2959.pdf ../input/O2022-2464.pdf ../input/O2023-1331.pdf ../input/O2024-0008982.pdf ../input/O2024-0010153.pdf ../input/SO2017-7051.pdf ../input/SO2018-4452.pdf ../input/SO2018-7749.pdf ../input/SO2018-863.pdf ../input/SO2019-4107.pdf ../input/SO2023-43.pdf ../input/matters_20101101_20260212.csv ../input/zoning_map_index.geojson ../input/building_permits_clean.gpkg ../input/preferred_predecessor_source_queries.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_historical_parcel_source_queries.csv ../input/preferred_historical_parcel_source.gpkg ../input/zoning_sep2025.geojson ../input/zoning_jan2016.zip ../input/zoning_sep2014.zip ../input/zoning_nov2012.zip ../input/Wards_2024.geojson ../input/Wards_2015.geojson ../input/Wards_2014.geojson ../input/Chicago_Wards_1998.geojson ../input/historical_parcel_requests.csv ../input/official_building_footprints_2022.gpkg ../input/official_building_footprints_2008.gpkg ../input/chicago_building_footprints_2015.zip ../input/density_historical_address_records.csv ../input/density_historical_parcel_records.csv ../input/building_permits_full.csv ../input/parcel_sales_city.csv ../input/chicago_alderman_terms.csv ../input/ward_controls_2000_2023.csv ../input/boundary_segments_1320ft.gpkg ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../input/parcel_addresses_2025_chicago.csv  ../input/residential_improvement_characteristics_full.csv ../input/commercial_value_raw.csv ../input/parcel_universe_2025_city.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/history_reference_parcels.gpkg: ../../download_construction_historical_parcels/output/history_reference_parcels.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/history_reference_queries.csv: ../../download_construction_historical_parcels/output/history_reference_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/geocoding_parcel_history.csv ../input/history_reference_parcels.gpkg ../input/history_reference_queries.csv

include ../../shared/code/generic.make
