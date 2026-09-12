include ../../shared/code/shell_functions.make

all: ../report/added_zoning_eighteen_review.csv.log

../output/added_zoning_eighteen_review.csv: review_added_zoning.py ../reference/added_zoning_eighteen.csv ../input/preferred_new_construction_zoning.csv ../input/historical_zoning_project_construction_year.csv ../input/zoning_review_matters.csv | ../output
	$(PYTHON) $<

../report/added_zoning_eighteen_review.csv.log: ../../shared/code/report.py ../output/added_zoning_eighteen_review.csv | ../report
	$(PYTHON) $< ../output/added_zoning_eighteen_review.csv $@ project_id

all: ../report/construction_reconciliation_progress.csv.log

../output/construction_reconciliation_progress.csv: review_construction_reconciliation.py ../input/residential_tieback_temporal_snapshots.csv ../input/frozen_construction_analysis.csv ../input/residential_successor_condo_requests.csv ../input/construction_condominium_history.csv ../reference/construction_reconciliation_cohort.csv ../output/current_construction_questions.csv ../input/preferred_residential_project_candidates.csv ../input/preferred_commercial_project_ledger.csv ../input/residential_review_source_dispositions.csv ../input/preferred_residential_project_ledger.csv | ../output
	$(PYTHON) $<

../report/construction_reconciliation_progress.csv.log: ../../shared/code/report.py ../output/construction_reconciliation_progress.csv | ../report
	$(PYTHON) $< ../output/construction_reconciliation_progress.csv $@ project_id

all: ../report/score_covariate_comparison.csv.log ../report/phased_project_permits.csv.log ../report/restored_stage_checks.csv.log ../output/manual_episode_increment.csv ../report/manual_episode_increment.csv.log ../output/manual_review_increment.csv ../report/manual_review_increment.csv.log ../output/multicard_followup_permits.csv ../report/multicard_followup_permits.csv.log ../output/multicard_parent_coverage.csv ../report/multicard_parent_coverage.csv.log ../output/density_exclusion_comparison.csv ../report/density_exclusion_comparison.csv.log ../output/denominator_permit_evidence.csv ../report/denominator_permit_evidence.csv.log ../output/model_checks.csv ../output/score_checks.csv ../output/data_checks.csv ../output/multicard_land_scope.csv ../report/multicard_land_scope.csv.log ../output/denominator_case_review.csv ../output/denominator_case_members.csv ../output/denominator_case_maps.pdf ../report/denominator_case_review.csv.log ../report/denominator_case_members.csv.log

all: ../report/natchez_building_land_coverage.csv.log ../output/natchez_parcel_coverage.pdf

all: ../report/commercial_reported_land_comparison.csv.log

all: ../report/remaining_individual_location_findings.csv.log

../output/remaining_individual_location_findings.csv: review_remaining_individual_locations.py ../input/density_historical_address_records.csv ../reference/individual_location_remaining_cases.csv ../input/preferred_residential_project_candidates.csv ../input/residential_reviewed_construction_years.csv ../input/residential_assessor_history.parquet ../input/parcel_addresses_2025_chicago.csv ../input/parcel_universe_2025_city.csv ../input/predecessor_parcel_history.csv ../input/geocoding_parcel_history.csv ../input/density_historical_parcel_records.csv ../input/preferred_project_boundary_scope.csv | ../output
	$(PYTHON) $<

../report/remaining_individual_location_findings.csv.log: ../../shared/code/report.py ../output/remaining_individual_location_findings.csv | ../report
	$(PYTHON) $< ../output/remaining_individual_location_findings.csv $@ project_id

all: ../report/individual_home_location_comparison.csv.log

../output/individual_home_location_comparison.csv: review_individual_home_locations.R ../input/preferred_project_boundary_scope.csv ../input/preferred_residential_project_candidates.csv ../input/parcel_universe_2025_city.csv ../input/residential_assessor_history.parquet ../input/preferred_project_year_geometry.gpkg ../input/preferred_project_year_centroids.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/frozen_construction_analysis.csv ../../setup_environment/code/packages.R ../../shared/code/assessor_classification.R ../../shared/code/canonical_geometry_helpers.R | ../output
	$(R) $<

../report/individual_home_location_comparison.csv.log: ../../shared/code/report.py ../output/individual_home_location_comparison.csv | ../report
	$(PYTHON) $< ../output/individual_home_location_comparison.csv $@ project_id


all: ../report/current_construction_questions.csv.log

../output/current_construction_questions.csv: reconcile_construction_decisions.py ../input/eligibility_manual_exceptions.csv ../input/residential_successor_condo_overrides.csv ../reference/remaining_building_decision_questions.csv ../input/preferred_residential_project_candidates.csv ../input/preferred_commercial_projects.csv ../input/preferred_commercial_project_ledger.csv ../input/preferred_project_boundary_scope.csv ../input/parcel_addresses_2025_chicago.csv ../input/residential_tieback_no_snapshot_decisions.csv ../input/residential_class297_exceptions.csv ../input/residential_overlap_decisions.csv ../input/residential_remaining_case_decisions.csv ../input/residential_unresolved_source_dispositions.csv ../input/project_manual_reviews.csv ../input/residential_review_source_dispositions.csv ../input/preferred_residential_project_ledger.csv ../input/density_parcel_address_selected_history.csv | ../output
	$(PYTHON) $<

../report/current_construction_questions.csv.log: ../../shared/code/report.py ../output/current_construction_questions.csv | ../report
	$(PYTHON) $< ../output/current_construction_questions.csv $@ project_id


../output/commercial_reported_land_comparison.csv: review_commercial_reported_land.py ../reference/commercial_map_land_baseline.csv ../input/preferred_commercial_projects.csv ../input/preferred_commercial_project_candidates.csv ../input/commercial_entity_version_candidates.csv | ../output
	$(PYTHON) $<

../report/commercial_reported_land_comparison.csv.log: ../../shared/code/report.py ../output/commercial_reported_land_comparison.csv | ../report
	$(PYTHON) $< ../output/commercial_reported_land_comparison.csv $@ project_id

all: ../report/natchez_b_land_coverage.csv.log

all: ../report/geocoder_mismatch_review.csv.log ../report/geocoding_parcel_history_review.csv.log

all: ../report/candidate_geography_checks.csv.log ../report/candidate_geography_review_queue.csv.log

all: ../report/remaining_project_location_checks.csv.log

all: ../report/prior_construction_decision_review.csv.log ../output/prior_construction_case_maps.pdf

../output/prior_construction_case_maps.pdf: map_prior_construction_cases.R ../reference/prior_construction_cases.csv ../input/preferred_residential_project_candidates.csv ../input/parcel_universe_2025_city.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_historical_parcel_source.gpkg ../input/lincoln_footprints_2022.geojson ../input/campbell_footprints_2022.geojson ../input/seeley_footprints_2022.geojson ../input/street38_footprints_2022.geojson ../input/calumet_footprints_2022.geojson ../input/marquette_footprints_2022.geojson | ../output
	$(R) $<

../output/prior_construction_decision_review.csv: review_prior_construction_decisions.R ../reference/prior_construction_cases.csv ../input/preferred_residential_project_candidates.csv | ../output
	$(R) $<

../report/prior_construction_decision_review.csv.log: ../../shared/code/report.py ../output/prior_construction_decision_review.csv | ../report
	$(PYTHON) $< ../output/prior_construction_decision_review.csv $@ project_id

all: ../report/remaining_project_successor_evidence.csv.log

../output/remaining_project_successor_evidence.csv: review_remaining_successor_locations.R ../output/candidate_geography_review_queue.csv ../input/preferred_residential_project_candidates.csv ../input/parcel_universe_2025_city.csv | ../output
	$(R) $<

../report/remaining_project_successor_evidence.csv.log: ../../shared/code/report.py ../output/remaining_project_successor_evidence.csv | ../report
	$(PYTHON) $< ../output/remaining_project_successor_evidence.csv $@ case_id

../output/remaining_project_location_checks.csv: review_remaining_project_locations.R ../output/candidate_geography_review_queue.csv ../input/preferred_residential_project_candidates.csv ../input/preferred_commercial_project_candidates.csv ../input/preferred_project_boundary_scope.csv ../input/parcel_universe_2025_city.csv ../input/parcel_addresses_2025_chicago.csv ../input/preferred_project_year_geometry.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	$(R) $<

../report/remaining_project_location_checks.csv.log: ../../shared/code/report.py ../output/remaining_project_location_checks.csv | ../report
	$(PYTHON) $< ../output/remaining_project_location_checks.csv $@ project_id

../output/candidate_geography_checks.csv: review_candidate_geography.R ../input/preferred_project_boundary_scope.csv ../input/preferred_project_year_geometry.gpkg ../input/preferred_project_year_centroids.gpkg ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	$(R) $<

../output/candidate_geography_review_queue.csv: build_candidate_geography_review_queue.R ../reference/prior_construction_cases.csv ../input/history_reference_parcels.gpkg ../input/history_reference_queries.csv ../input/residential_multicard_cards.csv ../input/residential_tieback_temporal_lineage_evidence.csv ../input/current_predecessor_parcel_source.gpkg ../input/preferred_residential_project_candidates.csv ../input/preferred_commercial_project_candidates.csv ../input/historical_project_predecessor_resolution.csv ../input/preferred_historical_predecessor_resolution.csv ../input/preferred_adjudication_scope.csv ../input/preferred_project_boundary_scope.csv ../output/candidate_geography_checks.csv ../output/geocoding_parcel_history_review.csv | ../output
	$(R) $<

../report/candidate_geography_checks.csv.log: ../../shared/code/report.py ../output/candidate_geography_checks.csv | ../report
	$(PYTHON) $< ../output/candidate_geography_checks.csv $@ project_id

../report/candidate_geography_review_queue.csv.log: ../../shared/code/report.py ../output/candidate_geography_review_queue.csv | ../report
	$(PYTHON) $< ../output/candidate_geography_review_queue.csv $@ case_id

../output/geocoder_mismatch_review.csv: review_geocoder_mismatches.R ../input/preferred_historical_address_geocodes.csv ../input/preferred_chicago_address_geocodes.csv ../input/building_permits_full.csv ../../shared/code/normalize_chicago_address.R | ../output
	$(R) $<

../output/geocoding_parcel_history_review.csv: review_geocoding_parcel_history.R ../input/preferred_address_geocode_requests.csv ../input/geocoding_parcel_history.csv ../input/geocoding_history_queries.csv | ../output
	$(R) $<

../report/geocoder_mismatch_review.csv.log: ../../shared/code/report.py ../output/geocoder_mismatch_review.csv | ../report
	$(PYTHON) $< ../output/geocoder_mismatch_review.csv $@ request_id

../report/geocoding_parcel_history_review.csv.log: ../../shared/code/report.py ../output/geocoding_parcel_history_review.csv | ../report
	$(PYTHON) $< ../output/geocoding_parcel_history_review.csv $@ request_id

../output/natchez_b_land_coverage.csv: review_natchez_b_land.R ../input/natchez_historical_parcels.gpkg ../input/natchez_footprints_2022.geojson ../output/natchez_building_land_coverage.csv | ../output
	$(R) $<

../report/natchez_b_land_coverage.csv.log: ../../shared/code/report.py ../output/natchez_b_land_coverage.csv | ../report
	$(PYTHON) $< ../output/natchez_b_land_coverage.csv $@ footprint_id

../output/natchez_building_land_coverage.csv: review_natchez_land.R ../input/natchez_footprints_2022.geojson ../input/natchez_historical_parcels.gpkg ../input/natchez_zoning_2025.geojson | ../output
	$(R) $<

../report/natchez_building_land_coverage.csv.log: ../../shared/code/report.py ../output/natchez_building_land_coverage.csv | ../report
	$(PYTHON) $< ../output/natchez_building_land_coverage.csv $@ footprint_id

../output/natchez_parcel_coverage.pdf: plot_natchez_land.R ../output/natchez_building_land_coverage.csv ../input/natchez_footprints_2022.geojson ../input/natchez_historical_parcels.gpkg | ../output
	$(R) $<

../output/phased_project_permits.csv: review_phased_project_permits.py ../../setup_environment/code/report_requirements.txt ../input/building_permits_full.csv | ../output
	$(PYTHON) $<

../report/phased_project_permits.csv.log: ../../shared/code/report.py ../output/phased_project_permits.csv | ../report
	$(PYTHON) $< ../output/phased_project_permits.csv $@ id

../report/restored_stage_checks.csv.log: ../../shared/code/report.py ../restored_stage_checks.csv | ../report
	$(PYTHON) $< ../restored_stage_checks.csv $@

../output/manual_episode_increment.csv: review_manual_episode_increment.py ../../setup_environment/code/report_requirements.txt ../reference/multicard_manual_episode_decisions.csv ../input/multicard_final_adjudication.csv | ../output
	$(PYTHON) $<

../report/manual_episode_increment.csv.log: ../../shared/code/report.py ../output/manual_episode_increment.csv | ../report
	$(PYTHON) $< ../output/manual_episode_increment.csv $@ project_id


../output/manual_review_increment.csv: review_manual_review_increment.py ../../setup_environment/code/report_requirements.txt ../reference/multicard_external_review_queue.csv ../reference/multicard_adjudicated_density_model_input.csv | ../output
	$(PYTHON) $<

../report/manual_review_increment.csv.log: ../../shared/code/report.py ../output/manual_review_increment.csv | ../report
	$(PYTHON) $< ../output/manual_review_increment.csv $@ project_id



../output/multicard_followup_permits.csv: review_multicard_followup_permits.py ../../setup_environment/code/report_requirements.txt ../input/building_permits_full.csv | ../output
	$(PYTHON) $<

../report/multicard_followup_permits.csv.log: ../../shared/code/report.py ../output/multicard_followup_permits.csv | ../report
	$(PYTHON) $< ../output/multicard_followup_permits.csv $@ id

../output/multicard_parent_coverage.csv: review_multicard_parent_coverage.R ../../setup_environment/code/packages.R ../output/multicard_land_scope.csv ../input/projects.csv ../input/multicard_component_successor_matches.csv ../input/parcel_universe_2025_city.csv ../reference/multicard_project_query_geometries.gpkg | ../output
	$(R) $<

../report/multicard_parent_coverage.csv.log: ../../shared/code/report.py ../output/multicard_parent_coverage.csv | ../report
	$(PYTHON) $< ../output/multicard_parent_coverage.csv $@ project_id

../output/density_exclusion_comparison.csv: compare_density_exclusions.R ../../setup_environment/code/packages.R ../../density_main_results/code/build_density_main_results.R ../input/projects.csv ../input/scores.csv ../input/density_denominator_decisions.csv | ../output
	$(R) $<

../report/density_exclusion_comparison.csv.log: ../../shared/code/report.py ../output/density_exclusion_comparison.csv | ../report
	$(PYTHON) $< ../output/density_exclusion_comparison.csv $@ scenario sample outcome term

../output/denominator_permit_evidence.csv: review_denominator_permits.py ../../setup_environment/code/report_requirements.txt ../input/building_permits_full.csv | ../output
	$(PYTHON) $<

../report/denominator_permit_evidence.csv.log: ../../shared/code/report.py ../output/denominator_permit_evidence.csv | ../report
	$(PYTHON) $< ../output/denominator_permit_evidence.csv $@ id

ifneq ($(filter-out $(wildcard ../output/denominator_case_review.csv ../output/denominator_case_members.csv ../output/denominator_case_maps.pdf),../output/denominator_case_review.csv ../output/denominator_case_members.csv ../output/denominator_case_maps.pdf),)
.PHONY: ../output/denominator_case_review.csv
endif

../output/denominator_case_review.csv: review_denominator_cases.R ../../setup_environment/code/packages.R ../reference/denominator_cases.csv ../reference/denominator_current_parcel_evidence.csv ../reference/multicard_component_successor_matches.csv ../input/projects.csv ../input/residential_improvement_characteristics_full.csv ../input/preferred_predecessor_parcel_source.gpkg | ../output
	$(R) $<

../output/denominator_case_members.csv ../output/denominator_case_maps.pdf: ../output/denominator_case_review.csv
	@test -f "$@"

../report/denominator_case_review.csv.log: ../../shared/code/report.py ../output/denominator_case_review.csv | ../report
	$(PYTHON) $< ../output/denominator_case_review.csv $@ project_id

../report/denominator_case_members.csv.log: ../../shared/code/report.py ../output/denominator_case_members.csv | ../report
	$(PYTHON) $< ../output/denominator_case_members.csv $@ project_id current_pin

../output/multicard_land_scope.csv: review_multicard_land_scope.R ../../setup_environment/code/packages.R ../input/projects.csv ../input/multicard_component_successor_matches.csv ../input/multicard_final_adjudication.csv ../input/multicard_external_web_reviews.csv | ../output
	$(R) $<

../report/multicard_land_scope.csv.log: ../../shared/code/report.py ../output/multicard_land_scope.csv | ../report
	$(PYTHON) $< ../output/multicard_land_scope.csv $@ project_id

../output/model_checks.csv: review_models.R ../input/projects.csv ../input/terms.csv ../input/scores.csv ../input/rents.parquet ../input/sales.parquet ../input/permits.parquet | ../output
	$(R) $<

ifneq ($(filter-out $(wildcard ../output/score_checks.csv ../output/score_comparison_by_alderman.csv ../output/density_joint_adjustment.csv ../output/score_covariate_comparison.csv),../output/score_checks.csv ../output/score_comparison_by_alderman.csv ../output/density_joint_adjustment.csv ../output/score_covariate_comparison.csv),)
.PHONY: ../output/score_checks.csv
endif

../output/score_comparison_by_alderman.csv ../output/density_joint_adjustment.csv ../output/score_covariate_comparison.csv: ../output/score_checks.csv

../report/score_covariate_comparison.csv.log: ../../shared/code/report.py ../output/score_covariate_comparison.csv | ../report
	$(PYTHON) $< ../output/score_covariate_comparison.csv $@ term

../output/score_checks.csv: review_scores.R ../../shared/code/alderman_uncertainty_helpers.R ../input/score_permits.csv ../input/scores.csv ../input/projects.csv | ../output
	$(R) $<

../output/data_checks.csv: review_data.R ../input/rents.parquet ../input/sales.parquet ../input/projects.csv | ../output
	$(R) $<


../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/density_denominator_decisions.csv: ../../audits/construction_review_history/records/density_denominator_decisions.csv | ../input
	ln -sf $< $@

../input/building_permits_full.csv: ../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@

../input/residential_improvement_characteristics_full.csv: ../../../data_raw/construction_review/residential_improvement_characteristics_full.csv | ../input
	ln -sf $< $@
../input/preferred_predecessor_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/multicard_component_successor_matches.csv: ../reference/multicard_component_successor_matches.csv | ../input
	ln -sf $< $@
../input/multicard_final_adjudication.csv: ../reference/multicard_final_adjudication.csv | ../input
	ln -sf $< $@
../input/multicard_external_web_reviews.csv: ../../audits/construction_review_history/records/multicard_external_web_reviews.csv | ../input
	ln -sf $< $@

../input/projects.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@
../input/scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@
../input/rents.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@
../input/sales.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@
../input/permits.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	ln -sf $< $@
../input/score_permits.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@

link-inputs: ../input/parcel_universe_2025_city.csv ../input/density_denominator_decisions.csv ../input/building_permits_full.csv ../input/residential_improvement_characteristics_full.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/multicard_component_successor_matches.csv ../input/multicard_final_adjudication.csv ../input/multicard_external_web_reviews.csv ../input/projects.csv ../input/terms.csv ../input/scores.csv ../input/rents.parquet ../input/sales.parquet ../input/permits.parquet ../input/score_permits.csv

../input/multicard_manual_episode_decisions.csv: ../../audits/construction_review_history/records/multicard_manual_episode_decisions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../audits/construction_review_history/records/multicard_manual_episode_decisions.csv" ]; then ln -sf ../../audits/construction_review_history/records/multicard_manual_episode_decisions.csv "$@"; fi

../input/natchez_footprints_2022.geojson: ../../download_construction_development_records/output/natchez_footprints_2022.geojson | ../input
	ln -sf $< $@

../input/natchez_historical_parcels.gpkg: ../../../data_raw/construction_review/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/natchez_zoning_2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	ln -sf $< $@

../input/preferred_address_geocode_requests.csv: ../../new_construction_cleaning/output/preferred_address_geocode_requests.csv | ../input
	ln -sf $< $@

../input/preferred_historical_address_geocodes.csv: ../../new_construction_cleaning/output/preferred_historical_address_geocodes.csv | ../input
	ln -sf $< $@

../input/preferred_chicago_address_geocodes.csv: ../../new_construction_cleaning/output/preferred_chicago_address_geocodes.csv | ../input
	ln -sf $< $@

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	ln -sf $< $@

../input/geocoding_history_queries.csv: ../../download_construction_historical_parcels/output/geocoding_history_queries.csv | ../input
	ln -sf $< $@

link-inputs: ../input/preferred_address_geocode_requests.csv ../input/preferred_historical_address_geocodes.csv ../input/preferred_chicago_address_geocodes.csv ../input/geocoding_parcel_history.csv ../input/geocoding_history_queries.csv

../input/preferred_project_boundary_scope.csv: ../../new_construction_cleaning/output/preferred_project_boundary_scope.csv | ../input
	ln -sf $< $@

../input/preferred_project_year_geometry.gpkg: ../../new_construction_cleaning/output/preferred_project_year_geometry.gpkg | ../input
	ln -sf $< $@

../input/preferred_project_year_centroids.gpkg: ../../new_construction_cleaning/output/preferred_project_year_centroids.gpkg | ../input
	ln -sf $< $@

../input/preferred_adjudication_scope.csv: ../../new_construction_cleaning/output/preferred_adjudication_scope.csv | ../input
	ln -sf $< $@

../input/historical_project_predecessor_resolution.csv: ../../new_construction_cleaning/output/historical_project_predecessor_resolution.csv | ../input
	ln -sf $< $@

../input/preferred_historical_predecessor_resolution.csv: ../../new_construction_cleaning/output/preferred_historical_predecessor_resolution.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/preferred_project_boundary_scope.csv ../input/preferred_project_year_geometry.gpkg ../input/preferred_project_year_centroids.gpkg ../input/preferred_adjudication_scope.csv ../input/historical_project_predecessor_resolution.csv ../input/preferred_historical_predecessor_resolution.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg

../input/current_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_residential_project_candidates.csv: ../../new_construction_cleaning/output/preferred_residential_project_candidates.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_project_candidates.csv: ../../new_construction_cleaning/output/preferred_commercial_project_candidates.csv | ../input
	ln -sf $< $@

link-inputs: ../input/current_predecessor_parcel_source.gpkg ../input/preferred_residential_project_candidates.csv ../input/preferred_commercial_project_candidates.csv

../input/history_reference_parcels.gpkg: ../../download_construction_historical_parcels/output/history_reference_parcels.gpkg | ../input
	ln -sf $< $@

../input/history_reference_queries.csv: ../../download_construction_historical_parcels/output/history_reference_queries.csv | ../input
	ln -sf $< $@

../input/residential_multicard_cards.csv: ../../new_construction_cleaning/output/residential_multicard_cards.csv | ../input
	ln -sf $< $@

../input/residential_tieback_temporal_lineage_evidence.csv: ../../new_construction_cleaning/output/residential_tieback_temporal_lineage_evidence.csv | ../input
	ln -sf $< $@

link-inputs: ../input/history_reference_parcels.gpkg ../input/history_reference_queries.csv ../input/residential_multicard_cards.csv ../input/residential_tieback_temporal_lineage_evidence.csv

../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	@if [ "$$(readlink "$@")" != "../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv" ]; then ln -sf ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv "$@"; fi

link-inputs: ../input/parcel_addresses_2025_chicago.csv

../input/lincoln_footprints_2022.geojson: ../../download_construction_historical_parcels/output/lincoln_footprints_2022.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/lincoln_footprints_2022.geojson

../input/campbell_footprints_2022.geojson: ../../download_construction_historical_parcels/output/campbell_footprints_2022.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/campbell_footprints_2022.geojson

../input/seeley_footprints_2022.geojson: ../../download_construction_historical_parcels/output/seeley_footprints_2022.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/seeley_footprints_2022.geojson

../input/street38_footprints_2022.geojson: ../../download_construction_historical_parcels/output/street38_footprints_2022.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/street38_footprints_2022.geojson

../input/calumet_footprints_2022.geojson: ../../download_construction_historical_parcels/output/calumet_footprints_2022.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/calumet_footprints_2022.geojson

../input/marquette_footprints_2022.geojson: ../../download_construction_historical_parcels/output/marquette_footprints_2022.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/marquette_footprints_2022.geojson

../input/preferred_historical_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/preferred_historical_parcel_source.gpkg

../input/preferred_commercial_projects.csv: ../../new_construction_cleaning/output/preferred_commercial_projects.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/preferred_commercial_projects.csv" ]; then ln -sf ../../new_construction_cleaning/output/preferred_commercial_projects.csv "$@"; fi

../input/commercial_entity_version_candidates.csv: ../../new_construction_cleaning/output/commercial_entity_version_candidates.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/commercial_entity_version_candidates.csv" ]; then ln -sf ../../new_construction_cleaning/output/commercial_entity_version_candidates.csv "$@"; fi


../input/preferred_commercial_project_ledger.csv: ../../new_construction_cleaning/output/preferred_commercial_project_ledger.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/preferred_commercial_project_ledger.csv" ]; then ln -sf ../../new_construction_cleaning/output/preferred_commercial_project_ledger.csv "$@"; fi

../input/residential_tieback_no_snapshot_decisions.csv: ../../audits/construction_review_history/records/residential_tieback_no_snapshot_decisions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../audits/construction_review_history/records/residential_tieback_no_snapshot_decisions.csv" ]; then ln -sf ../../audits/construction_review_history/records/residential_tieback_no_snapshot_decisions.csv "$@"; fi

../input/residential_class297_exceptions.csv: ../../audits/construction_review_history/records/residential_class297_exceptions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../audits/construction_review_history/records/residential_class297_exceptions.csv" ]; then ln -sf ../../audits/construction_review_history/records/residential_class297_exceptions.csv "$@"; fi

../input/residential_overlap_decisions.csv: ../../new_construction_corrections/output/residential_overlap_decisions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_corrections/output/residential_overlap_decisions.csv" ]; then ln -sf ../../new_construction_corrections/output/residential_overlap_decisions.csv "$@"; fi

../input/residential_remaining_case_decisions.csv: ../../audits/construction_review_history/records/residential_remaining_case_decisions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../audits/construction_review_history/records/residential_remaining_case_decisions.csv" ]; then ln -sf ../../audits/construction_review_history/records/residential_remaining_case_decisions.csv "$@"; fi

../input/residential_unresolved_source_dispositions.csv: ../../new_construction_corrections/output/residential_unresolved_source_dispositions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_corrections/output/residential_unresolved_source_dispositions.csv" ]; then ln -sf ../../new_construction_corrections/output/residential_unresolved_source_dispositions.csv "$@"; fi

../input/project_manual_reviews.csv: ../../audits/construction_review_history/records/project_manual_reviews.csv | ../input
	@if [ "$$(readlink "$@")" != "../../audits/construction_review_history/records/project_manual_reviews.csv" ]; then ln -sf ../../audits/construction_review_history/records/project_manual_reviews.csv "$@"; fi


../input/eligibility_manual_exceptions.csv: ../../new_construction_corrections/output/eligibility_manual_exceptions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_corrections/output/eligibility_manual_exceptions.csv" ]; then ln -sf ../../new_construction_corrections/output/eligibility_manual_exceptions.csv "$@"; fi

../input/residential_successor_condo_overrides.csv: ../../audits/construction_review_history/records/residential_successor_condo_overrides.csv | ../input
	@if [ "$$(readlink "$@")" != "../../audits/construction_review_history/records/residential_successor_condo_overrides.csv" ]; then ln -sf ../../audits/construction_review_history/records/residential_successor_condo_overrides.csv "$@"; fi


../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	@if [ "$$(readlink "$@")" != "../../prepare_construction_assessor_history/output/residential_assessor_history.parquet" ]; then ln -sf ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet "$@"; fi


../input/frozen_construction_analysis.csv: ../reference/new_construction_analysis_data_before_reconnection.csv Makefile | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "../reference/new_construction_analysis_data_before_reconnection.csv" || ln -sf ../reference/new_construction_analysis_data_before_reconnection.csv "$@"

../input/residential_reviewed_construction_years.csv: ../../audits/construction_review_history/records/residential_reviewed_construction_years.csv | ../input
	@if [ "$$(readlink "$@")" != "../../audits/construction_review_history/records/residential_reviewed_construction_years.csv" ]; then ln -sf ../../audits/construction_review_history/records/residential_reviewed_construction_years.csv "$@"; fi

../input/predecessor_parcel_history.csv: ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv | ../input
	@if [ "$$(readlink "$@")" != "../../download_construction_historical_parcels/output/predecessor_parcel_history.csv" ]; then ln -sf ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv "$@"; fi

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	@if [ "$$(readlink "$@")" != "../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv" ]; then ln -sf ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv "$@"; fi

../input/density_historical_address_records.csv: ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv | ../input
	@if [ "$$(readlink "$@")" != "../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv" ]; then ln -sf ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv "$@"; fi

../input/residential_successor_condo_requests.csv: ../../new_construction_cleaning/output/residential_successor_condo_requests.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/residential_successor_condo_requests.csv" ]; then ln -sf ../../new_construction_cleaning/output/residential_successor_condo_requests.csv "$@"; fi

../input/construction_condominium_history.csv: ../../download_construction_assessor_history/output/construction_condominium_history.csv | ../input
	@if [ "$$(readlink "$@")" != "../../download_construction_assessor_history/output/construction_condominium_history.csv" ]; then ln -sf ../../download_construction_assessor_history/output/construction_condominium_history.csv "$@"; fi

../input/residential_tieback_temporal_snapshots.csv: ../../new_construction_cleaning/output/residential_tieback_temporal_snapshots.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/residential_tieback_temporal_snapshots.csv" ]; then ln -sf ../../new_construction_cleaning/output/residential_tieback_temporal_snapshots.csv "$@"; fi

../input/residential_review_source_dispositions.csv: ../../new_construction_cleaning/output/residential_review_source_dispositions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/residential_review_source_dispositions.csv" ]; then ln -sf ../../new_construction_cleaning/output/residential_review_source_dispositions.csv "$@"; fi

../input/preferred_residential_project_ledger.csv: ../../new_construction_cleaning/output/preferred_residential_project_ledger.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/preferred_residential_project_ledger.csv" ]; then ln -sf ../../new_construction_cleaning/output/preferred_residential_project_ledger.csv "$@"; fi

../input/density_parcel_address_selected_history.csv: ../../new_construction_cleaning/output/density_parcel_address_selected_history.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/output/density_parcel_address_selected_history.csv" ]; then ln -sf ../../new_construction_cleaning/output/density_parcel_address_selected_history.csv "$@"; fi

# Compare the preserved paper input with the newly connected construction data.
ifneq ($(filter-out $(wildcard ../output/construction_estimation_project_changes.csv ../output/construction_estimation_sample_comparison.csv),../output/construction_estimation_project_changes.csv ../output/construction_estimation_sample_comparison.csv),)
.PHONY: ../output/construction_estimation_project_changes.csv
endif

../output/construction_estimation_project_changes.csv: compare_construction_estimation_samples.R ../../setup_environment/code/packages.R ../reference/new_construction_analysis_data_before_reconnection.csv ../input/current_construction_analysis.csv ../input/preferred_new_construction_project_ledger.csv | ../output
	$(R) $<

../output/construction_estimation_sample_comparison.csv: ../output/construction_estimation_project_changes.csv

../report/construction_estimation_project_changes.csv.log: ../../shared/code/report.py ../output/construction_estimation_project_changes.csv | ../report
	$(PYTHON) $< ../output/construction_estimation_project_changes.csv $@ project_id

../report/construction_estimation_sample_comparison.csv.log: ../../shared/code/report.py ../output/construction_estimation_sample_comparison.csv | ../report
	$(PYTHON) $< ../output/construction_estimation_sample_comparison.csv $@ sample outcome

../input/preferred_new_construction_project_ledger.csv: ../../new_construction_cleaning/output/preferred_new_construction_project_ledger.csv | ../input
	ln -sf $< $@

../input/current_construction_analysis.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../output/density_change_decomposition.csv: decompose_density_changes.R ../../setup_environment/code/packages.R ../input/scores.csv ../reference/new_construction_analysis_data_before_reconnection.csv ../input/current_construction_analysis.csv ../output/construction_estimation_project_changes.csv | ../output
	$(R) $<

../report/density_change_decomposition.csv.log: ../../shared/code/report.py ../output/density_change_decomposition.csv | ../report
	$(PYTHON) $< ../output/density_change_decomposition.csv $@ sample outcome stage

../input/preferred_new_construction_zoning.csv: ../../new_construction_cleaning/output/preferred_new_construction_zoning.csv | ../input
	ln -sf $< $@

../input/historical_zoning_project_construction_year.csv: ../../construction_zoning_history/output/historical_zoning_project_construction_year.csv | ../input
	ln -sf $< $@

../input/zoning_review_matters.csv: ../../../data_raw/construction_review/matters_20101101_20260212.csv | ../input
	ln -sf $< $@

all: ../report/entering_home_assignments.csv.log

../output/entering_home_assignments.csv: review_entering_home_assignments.py ../output/construction_estimation_project_changes.csv ../input/current_construction_analysis.csv ../input/terms.csv ../input/scores.csv | ../output
	$(PYTHON) $<

../report/entering_home_assignments.csv.log: ../../shared/code/report.py ../output/entering_home_assignments.csv | ../report
	$(PYTHON) $< ../output/entering_home_assignments.csv $@ project_id

all: ../report/ward_two_year_review.csv.log

../output/ward_two_year_review.csv: review_ward_two_years.py ../output/entering_home_assignments.csv ../input/current_construction_analysis.csv ../input/residential_assessor_history.parquet ../input/parcel_addresses_2025_chicago.csv | ../output
	$(PYTHON) $<

../report/ward_two_year_review.csv.log: ../../shared/code/report.py ../output/ward_two_year_review.csv | ../report
	$(PYTHON) $< ../output/ward_two_year_review.csv $@ project_id

all: ../report/population_chronology_review.csv.log

../output/population_chronology_review.csv: review_population_chronology.py ../input/residential_assessor_history.parquet ../input/commercial_value_raw.csv ../input/construction_condominium_history.csv ../reference/residential_assessments_before_chronology.csv ../input/residential_selected_assessments.csv ../input/preferred_new_construction_project_ledger.csv ../input/current_construction_analysis.csv | ../output
	$(PYTHON) $<

../report/population_chronology_review.csv.log: ../../shared/code/report.py ../output/population_chronology_review.csv | ../report
	$(PYTHON) $< ../output/population_chronology_review.csv $@ project_id

../input/residential_selected_assessments.csv: ../../new_construction_cleaning/output/residential_selected_assessments.csv | ../input
	ln -sf $< $@

../input/commercial_value_raw.csv: ../../download_construction_assessor_history/output/commercial_valuation_data.csv | ../input
	ln -sf $< $@

all: ../report/large_year_change_review.csv.log

../output/large_year_change_review.csv: review_large_year_changes.py ../reference/large_year_change_recommendations.csv ../output/population_chronology_review.csv ../input/parcel_addresses_2025_chicago.csv | ../output
	$(PYTHON) $<

../report/large_year_change_review.csv.log: ../../shared/code/report.py ../output/large_year_change_review.csv | ../report
	$(PYTHON) $< ../output/large_year_change_review.csv $@ project_id

include ../../shared/code/generic.make
