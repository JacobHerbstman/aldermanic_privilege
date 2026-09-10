# Earlier review and final-assembly rules; not part of the current default build.
# These targets retain their existing names and still fail on missing sources.

../report/preferred_density_model_production_card_input.csv.log: ../../shared/code/report.py ../output/preferred_density_model_production_card_input.csv | ../report
	$(PYTHON) $< ../output/preferred_density_model_production_card_input.csv $@ project_id

../report/final_density_model_input.csv.log: ../../shared/code/report.py ../output/final_density_model_input.csv | ../report
	$(PYTHON) $< ../output/final_density_model_input.csv $@ project_id

../report/provisional_validated_density_input.csv.log: ../../shared/code/report.py ../output/provisional_validated_density_input.csv | ../report
	$(PYTHON) $< ../output/provisional_validated_density_input.csv $@ project_id

../report/multifamily_classification_decisions.csv.log: ../../shared/code/report.py ../output/multifamily_classification_decisions.csv | ../report
	$(PYTHON) $< ../output/multifamily_classification_decisions.csv $@ project_id

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

ifneq ($(filter-out $(wildcard ../output/residential_review_current_parcel_links.csv ../output/residential_successor_condo_requests.csv),../output/residential_review_current_parcel_links.csv ../output/residential_successor_condo_requests.csv),)
.PHONY: ../output/residential_successor_condo_requests.csv
endif

../output/residential_successor_condo_requests.csv: build_residential_successor_condo_requests.R ../../setup_environment/code/packages.R ../input/parcel_universe_2025_city.csv ../output/preferred_project_year_geometry.gpkg ../output/preferred_residential_project_candidates.csv | ../output
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

../output/residential_class297_condo_cohort_evidence.csv: build_residential_class297_condo_evidence.R ../../setup_environment/code/packages.R ../output/residential_manual_review_bundle.csv ../output/residential_successor_condo_base_year_summary.csv ../output/residential_successor_condo_requests.csv | ../output
	$(R) $<

../report/residential_class297_condo_cohort_evidence.csv.log: ../../shared/code/report.py ../output/residential_class297_condo_cohort_evidence.csv | ../report
	$(PYTHON) $< ../output/residential_class297_condo_cohort_evidence.csv $@ project_id pin10

../output/project_overlap_evidence.csv: build_project_overlap_evidence.R ../../setup_environment/code/packages.R ../output/preferred_adjudication_scope.csv ../output/preferred_commercial_project_candidates.csv ../output/preferred_project_year_geometry.gpkg ../output/preferred_residential_project_candidates.csv | ../output
	$(R) $<

../report/project_overlap_evidence.csv.log: ../../shared/code/report.py ../output/project_overlap_evidence.csv | ../report
	$(PYTHON) $< ../output/project_overlap_evidence.csv $@ source_family project_id

../report/residential_remaining_case_evidence.csv.log: ../../shared/code/report.py ../output/residential_remaining_case_evidence.csv | ../report
	$(PYTHON) $< ../output/residential_remaining_case_evidence.csv $@ source_project_id

../output/final_verified_density_input.csv: build_final_verified_density_input.R ../adjudication/density_denominator_decisions.csv ../../shared/code/canonical_geometry_helpers.R ../../setup_environment/code/packages.R ../adjudication/corrected_year_zoning_decisions.csv ../input/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv ../input/boundary_segments_1320ft.gpkg ../input/chicago_alderman_terms.csv ../input/ward_controls_2000_2023.csv ../input/ward_pair_boundaries.gpkg ../input/ward_panel.gpkg ../output/final_new_construction_boundary_scope.csv ../output/final_new_construction_zoning.csv ../output/final_project_verification_ledger.csv ../output/provisional_validated_density_input.csv | ../output
	$(R) $<

../output/final_project_verification_ledger.csv: build_final_project_verification_ledger.R ../../setup_environment/code/packages.R ../adjudication/assessor_default_project_exceptions.csv ../adjudication/assessor_default_site_reviews.csv ../adjudication/final_project_overrides.csv ../output/assessor_only_exact_permit_summary.csv ../output/assessor_only_validation.csv ../output/reviewed_project_ledger.csv | ../output
	$(R) $<

../output/assessor_only_exact_permit_summary.csv: build_assessor_only_exact_permit_review.R ../../setup_environment/code/packages.R ../output/building_permits_for_verification.gpkg ../output/reviewed_project_ledger.csv | ../output
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

../output/multicard_project_evidence_base.csv: ../output/multicard_card_snapshot.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/multicard_card_snapshot.csv ../output/multicard_project_evidence_base.csv),../output/multicard_card_snapshot.csv ../output/multicard_project_evidence_base.csv),)
.PHONY: ../output/multicard_card_snapshot.csv
endif

../output/multicard_external_review_queue.csv: build_multicard_external_review_queue.R ../adjudication/multicard_external_web_reviews.csv ../output/multicard_card_snapshot.csv ../output/multicard_current_successor_project_summary.csv ../output/multicard_final_adjudication.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/multicard_current_successor_project_summary.csv ../output/multicard_current_successor_links.csv),../output/multicard_current_successor_project_summary.csv ../output/multicard_current_successor_links.csv),)
.PHONY: ../output/multicard_current_successor_project_summary.csv
endif

../output/permit_rule_coverage.csv: audit_permit_rule_coverage.R ../output/building_permits_for_verification.gpkg ../output/project_evidence_inventory.csv ../output/project_permit_chain_links.csv ../../setup_environment/code/packages.R | ../output
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

../output/multicard_successor_building_candidates.csv: build_multicard_successor_building_candidates.R ../input/parcel_addresses_2025_chicago.csv ../input/parcel_universe_2025_city.csv ../output/multicard_current_successor_links.csv ../output/multicard_project_evidence_base.csv ../output/multicard_successor_condo_evidence.csv ../output/multicard_successor_condo_links.csv ../output/multicard_successor_condo_requests.csv ../../setup_environment/code/packages.R construction_settings.make | ../output
	$(R) $< $(EPISODE_YEAR_WINDOW)

../output/multicard_successor_match_summary.csv: ../output/multicard_component_successor_matches.csv
	@test -f "$@"

ifneq ($(filter-out $(wildcard ../output/multicard_component_successor_matches.csv ../output/multicard_successor_match_summary.csv),../output/multicard_component_successor_matches.csv ../output/multicard_successor_match_summary.csv),)
.PHONY: ../output/multicard_component_successor_matches.csv
endif

../output/multicard_component_successor_matches.csv: match_multicard_components_to_successors.R construction_settings.make ../output/multicard_card_snapshot.csv ../output/multicard_episode_component_nodes.csv ../output/multicard_successor_building_candidates.csv ../../setup_environment/code/packages.R | ../output
	$(R) $< $(AUTOMATIC_CARD_BUILDING_GAP)

../output/multicard_footprint_evidence.csv: build_multicard_footprint_evidence.R ../output/cook_building_footprints_2006_2008.gpkg ../output/multicard_city_building_footprints.gpkg ../output/multicard_project_query_geometries.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/final_new_construction_audit_ledger.csv ../output/final_recovered_missing_project_dedupe_screen.csv ../output/final_recovered_missing_project_pair_screen.csv ../output/final_residual_permit_chain_dispositions.csv),../output/final_new_construction_audit_ledger.csv ../output/final_recovered_missing_project_dedupe_screen.csv ../output/final_recovered_missing_project_pair_screen.csv ../output/final_residual_permit_chain_dispositions.csv),)
.PHONY: ../output/final_new_construction_audit_ledger.csv
endif

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

../output/multicard_successor_condo_links.csv: ../output/multicard_successor_condo_requests.csv
	@test -f "$@"

../output/final_new_construction_zoning.csv: build_final_construction_zoning.R ../../setup_environment/code/packages.R ../output/historical_zoning_2006_candidate.gpkg ../adjudication/recovered_project_zoning_overrides.csv ../input/zoning_jan2016.zip ../input/zoning_nov2012.zip ../input/zoning_sep2014.zip ../input/zoning_sep2025.geojson ../output/final_new_construction_audit_ledger.csv ../output/final_new_construction_boundary_scope.csv ../output/historical_zoning_project_construction_year.csv ../output/preferred_new_construction_zoning.csv | ../output
	$(R) $<

../output/final_density_model_input.csv: build_final_density_input.R ../input/boundary_segments_1320ft.gpkg ../output/final_new_construction_audit_ledger.csv ../output/final_new_construction_boundary_scope.csv ../output/final_new_construction_zoning.csv ../output/preferred_density_model_production_card_input.csv ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/residential_unresolved_accepted_episode_geometry.gpkg ../output/residential_unresolved_accepted_episode_geometry_coverage.csv),../output/residential_unresolved_accepted_episode_geometry.gpkg ../output/residential_unresolved_accepted_episode_geometry_coverage.csv),)
.PHONY: ../output/residential_unresolved_accepted_episode_geometry.gpkg
endif

../output/residential_unresolved_accepted_episode_geometry.gpkg: build_residential_unresolved_accepted_episode_geometry.R ../output/residential_unresolved_episode_component_geometry.gpkg ../output/residential_unresolved_episode_inventory.csv ../output/residential_unresolved_predecessor_selected.gpkg ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_unresolved_accepted_episode_geometry_coverage.csv: ../output/residential_unresolved_accepted_episode_geometry.gpkg
	@test -f "$@"

../output/residential_tieback_no_snapshot_resolution.csv: build_residential_tieback_no_snapshot_resolution.R ../adjudication/residential_tieback_no_snapshot_decisions.csv ../output/residential_tieback_no_snapshot_review.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../output/residential_remaining_case_resolution.csv: build_residential_remaining_case_resolution.R ../adjudication/residential_remaining_case_decisions.csv ../output/residential_remaining_case_evidence.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/residential_tieback_episode_candidates.csv ../output/residential_tieback_no_snapshot_review.csv),../output/residential_tieback_episode_candidates.csv ../output/residential_tieback_no_snapshot_review.csv),)
.PHONY: ../output/residential_tieback_episode_candidates.csv
endif

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

../report/final_verified_density_input.csv.log: ../output/final_verified_density_input.csv ../../shared/code/report.py | ../report
	$(PYTHON) ../../shared/code/report.py $< $@ project_id

../report/preferred_new_construction_zoning.csv.log: ../../shared/code/report.py ../output/preferred_new_construction_zoning.csv | ../report
	$(PYTHON) $< ../output/preferred_new_construction_zoning.csv $@ project_id

../report/residential_tieback_episode_candidates.csv.log: ../../shared/code/report.py ../output/residential_tieback_episode_candidates.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_episode_candidates.csv $@ proposed_project_id

../report/residential_tieback_no_snapshot_review.csv.log: ../../shared/code/report.py ../output/residential_tieback_no_snapshot_review.csv | ../report
	$(PYTHON) $< ../output/residential_tieback_no_snapshot_review.csv $@ source_project_id

../report/final_new_construction_zoning.csv.log: ../../shared/code/report.py ../output/final_new_construction_zoning.csv | ../report
	$(PYTHON) $< ../output/final_new_construction_zoning.csv $@ project_id

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

../input/official_building_footprints_2008.gpkg: ../../../data_raw/construction_review/official_building_footprints_2008.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/official_building_footprints_2022.gpkg: ../../../data_raw/construction_review/official_building_footprints_2022.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_nov2012.zip: ../../../data_raw/construction_review/zoning_nov2012.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_sep2014.zip: ../../../data_raw/construction_review/zoning_sep2014.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_jan2016.zip: ../../../data_raw/construction_review/zoning_jan2016.zip | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/zoning_sep2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"