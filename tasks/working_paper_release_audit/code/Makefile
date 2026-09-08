include ../../shared/code/shell_functions.make

all: ../report/phased_project_permits.csv.log ../report/restored_stage_checks.csv.log ../output/manual_episode_increment.csv ../report/manual_episode_increment.csv.log ../output/manual_review_increment.csv ../report/manual_review_increment.csv.log ../output/multicard_followup_permits.csv ../report/multicard_followup_permits.csv.log ../output/multicard_parent_coverage.csv ../report/multicard_parent_coverage.csv.log ../output/density_exclusion_comparison.csv ../report/density_exclusion_comparison.csv.log ../output/denominator_permit_evidence.csv ../report/denominator_permit_evidence.csv.log ../output/model_checks.csv ../output/score_checks.csv ../output/data_checks.csv ../output/multicard_land_scope.csv ../report/multicard_land_scope.csv.log ../output/denominator_case_review.csv ../output/denominator_case_members.csv ../output/denominator_case_maps.pdf ../report/denominator_case_review.csv.log ../report/denominator_case_members.csv.log

all: ../report/natchez_building_land_coverage.csv.log ../output/natchez_parcel_coverage.pdf

all: ../report/natchez_b_land_coverage.csv.log

all: ../report/geocoder_mismatch_review.csv.log ../report/geocoding_parcel_history_review.csv.log

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

../output/score_checks.csv: review_scores.R ../../_lib/alderman_uncertainty_helpers.R ../input/score_permits.csv ../input/scores.csv ../input/projects.csv | ../output
	$(R) $<

../output/data_checks.csv: review_data.R ../input/rents.parquet ../input/sales.parquet ../input/projects.csv | ../output
	$(R) $<


../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/density_denominator_decisions.csv: ../../new_construction_cleaning/adjudication/density_denominator_decisions.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/building_permits_full.csv: ../../../data_raw/construction_review/building_permits_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/residential_improvement_characteristics_full.csv: ../../../data_raw/construction_review/residential_improvement_characteristics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/preferred_predecessor_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/multicard_component_successor_matches.csv: ../reference/multicard_component_successor_matches.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/multicard_final_adjudication.csv: ../reference/multicard_final_adjudication.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/multicard_external_web_reviews.csv: ../../new_construction_cleaning/adjudication/multicard_external_web_reviews.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/projects.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/scores.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/rents.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/sales.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/permits.parquet: ../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"
../input/score_permits.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcel_universe_2025_city.csv ../input/density_denominator_decisions.csv ../input/building_permits_full.csv ../input/residential_improvement_characteristics_full.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/multicard_component_successor_matches.csv ../input/multicard_final_adjudication.csv ../input/multicard_external_web_reviews.csv ../input/projects.csv ../input/terms.csv ../input/scores.csv ../input/rents.parquet ../input/sales.parquet ../input/permits.parquet ../input/score_permits.csv

../input/multicard_manual_episode_decisions.csv: ../../new_construction_cleaning/adjudication/multicard_manual_episode_decisions.csv | ../input
	@if [ "$$(readlink "$@")" != "../../new_construction_cleaning/adjudication/multicard_manual_episode_decisions.csv" ]; then ln -sf ../../new_construction_cleaning/adjudication/multicard_manual_episode_decisions.csv "$@"; fi

../input/natchez_footprints_2022.geojson: ../../download_construction_development_records/output/natchez_footprints_2022.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/natchez_historical_parcels.gpkg: ../../../data_raw/construction_review/preferred_historical_parcel_source.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/natchez_zoning_2025.geojson: ../../../data_raw/construction_review/zoning_sep2025.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_address_geocode_requests.csv: ../../new_construction_cleaning/output/preferred_address_geocode_requests.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_historical_address_geocodes.csv: ../../new_construction_cleaning/output/preferred_historical_address_geocodes.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_chicago_address_geocodes.csv: ../../new_construction_cleaning/output/preferred_chicago_address_geocodes.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/geocoding_history_queries.csv: ../../download_construction_historical_parcels/output/geocoding_history_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_address_geocode_requests.csv ../input/preferred_historical_address_geocodes.csv ../input/preferred_chicago_address_geocodes.csv ../input/geocoding_parcel_history.csv ../input/geocoding_history_queries.csv

include ../../shared/code/generic.make
