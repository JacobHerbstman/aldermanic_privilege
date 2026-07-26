all: ../output/project_evidence_inventory.csv \
	../output/historical_permit_project_evidence.csv \
	../output/full_ledger_duplicate_candidates.csv \
	../output/full_ledger_duplicate_summary.csv \
	../output/high_priority_duplicate_candidates.csv \
	../output/unresolved_duplicate_candidates.csv \
	../output/parent_successor_cluster_rule_validation.csv \
	../output/parent_successor_cluster_review_queue.csv \
	../output/parent_successor_project_actions.csv \
	../output/multifamily_classification_rule_validation.csv \
	../output/multifamily_classification_review_summary.csv \
	../output/multifamily_classification_error_modes.csv \
	../output/multifamily_classification_mode_b_review_sample.csv \
	../output/multifamily_classification_new_review_queue.csv \
	../output/multifamily_classification_known_exceptions.csv \
	../output/multifamily_classification_decisions.csv \
	../output/permit_rule_coverage.csv \
	../output/permit_rule_gap_review.csv \
	../output/permit_rule_coverage_summary.csv \
	../output/eligibility_rule_validation.csv \
	../output/eligibility_rule_summary.csv \
	../output/eligibility_manual_review_queue.csv \
	../output/eligibility_manual_exception_ledger.csv \
	../output/eligibility_uncorroborated_retained.csv \
	../output/permit_pin_geography_conflicts.csv \
	../output/permit_pin_geography_conflict_summary.csv \
	../output/commercial_manual_rule_coverage.csv \
	../output/commercial_manual_rule_summary.csv \
	../output/commercial_minimal_exception_ledger.csv \
	../output/presample_assessor_structure_evidence.csv \
	../output/presample_assessor_structure_summary.csv \
	../output/predecessor_assessor_evidence.csv \
	../output/predecessor_assessor_evidence_summary.csv \
	../output/city_2015_footprint_evidence.csv \
	../output/city_2015_footprint_evidence_summary.csv \
	../output/residual_completed_density_attrition.csv \
	../output/residual_completed_density_attrition_summary.csv \
	../output/density_attrition_balance_sample.csv \
	../output/density_attrition_balance_results.csv \
	../output/density_attrition_treatment_counts.csv \
	../output/minimal_manual_exception_ledger.csv \
	../output/minimal_manual_exception_summary.csv \
	../output/analysis_scope_omissions.csv \
	../output/analysis_scope_reconciliation.csv \
	../output/provisional_validated_density_input.csv \
	../output/provisional_validated_sample_changes.csv \
	../output/provisional_validated_density_results.csv \
	../output/audited_density_rd_current_no_income.pdf \
	../output/audited_density_rd_current_no_income.png \
	../output/audited_density_rd_education_added_back.pdf \
	../output/audited_density_rd_education_added_back.png \
	../output/audited_density_rd_income_added_back.pdf \
	../output/audited_density_rd_income_added_back.png \
	../output/audited_density_rd_all_covariates.pdf \
	../output/audited_density_rd_all_covariates.png \
	../output/project_evidence_summary.csv \
	../output/initial_review_queue.csv \
	../output/known_case_validation.csv

../output/project_evidence_inventory.csv: build_project_evidence_inventory.R \
	../output/historical_permit_project_evidence.csv \
	../input/final_new_construction_audit_ledger.csv \
	../input/final_density_model_input.csv \
	../input/preferred_residential_project_ledger.csv \
	../input/preferred_project_duplicate_dispositions.csv \
	../input/residential_project_candidate_inventory.csv \
	../input/new_construction_exact_permit_matches.csv \
	../input/new_construction_spatial_permit_matches.csv \
	../input/new_construction_permit_unit_mentions.csv \
	../input/multicard_external_review_queue.csv
	$(R) $<

../output/historical_permit_project_evidence.csv: build_historical_permit_project_evidence.R \
	../input/final_new_construction_audit_ledger.csv \
	../input/building_permits_clean.gpkg
	$(R) $<

../output/full_ledger_duplicate_candidates.csv: build_full_ledger_duplicate_candidates.R \
	../output/project_evidence_inventory.csv \
	../input/multicard_external_reviewed_model_input.csv \
	../input/new_construction_exact_permit_matches.csv \
	../input/new_construction_spatial_permit_matches.csv \
	../input/preferred_project_duplicate_pair_dispositions.csv
	$(R) $<

../output/full_ledger_duplicate_summary.csv: \
	../output/full_ledger_duplicate_candidates.csv

../output/high_priority_duplicate_candidates.csv: \
	../output/full_ledger_duplicate_candidates.csv

../output/unresolved_duplicate_candidates.csv: \
	../output/full_ledger_duplicate_candidates.csv

../output/parent_successor_cluster_rule_validation.csv: \
	resolve_parent_successor_clusters.R \
	../input/multicard_final_adjudication.csv \
	../input/multicard_component_successor_matches.csv \
	../input/multicard_successor_suppressions.csv \
	../input/multicard_manual_episode_decisions.csv \
	../input/final_density_model_input.csv
	$(R) $<

../output/parent_successor_cluster_review_queue.csv: \
	../output/parent_successor_cluster_rule_validation.csv

../output/parent_successor_project_actions.csv: \
	../output/parent_successor_cluster_rule_validation.csv

../output/multifamily_classification_rule_validation.csv: \
	validate_multifamily_classification_rules.R \
	../output/project_evidence_inventory.csv \
	../output/eligibility_rule_validation.csv
	$(R) $<

../output/multifamily_classification_review_summary.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/multifamily_classification_error_modes.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/multifamily_classification_mode_b_review_sample.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/multifamily_classification_new_review_queue.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/multifamily_classification_known_exceptions.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/multifamily_classification_decisions.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/permit_rule_gap_review.csv: audit_permit_rule_coverage.R \
	../output/project_evidence_inventory.csv \
	../input/project_permit_chain_links.csv \
	../input/building_permits_clean.gpkg
	$(R) $<

../output/permit_rule_coverage_summary.csv: \
	../output/permit_rule_gap_review.csv

../output/permit_rule_coverage.csv: \
	../output/permit_rule_gap_review.csv

../output/eligibility_rule_validation.csv: \
	validate_new_construction_eligibility.R \
	../adjudication/eligibility_manual_exceptions.csv \
	../output/permit_rule_coverage.csv \
	../output/presample_assessor_structure_evidence.csv \
	../output/predecessor_assessor_evidence.csv \
	../input/multicard_external_reviewed_model_input.csv \
	../input/commercial_completion_evidence.csv
	$(R) $<

../output/eligibility_rule_summary.csv: \
	../output/eligibility_rule_validation.csv

../output/eligibility_manual_review_queue.csv: \
	../output/eligibility_rule_validation.csv

../output/eligibility_manual_exception_ledger.csv: \
	../output/eligibility_rule_validation.csv

../output/eligibility_uncorroborated_retained.csv: \
	../output/eligibility_rule_validation.csv

../output/permit_pin_geography_conflicts.csv: \
	audit_permit_pin_geography_conflicts.R \
	../output/project_evidence_inventory.csv \
	../input/multicard_external_reviewed_model_input.csv \
	../input/new_construction_exact_permit_matches.csv \
	../input/new_construction_spatial_permit_matches.csv
	$(R) $<

../output/permit_pin_geography_conflict_summary.csv: \
	../output/permit_pin_geography_conflicts.csv

../output/commercial_manual_rule_coverage.csv: \
	audit_commercial_manual_rule_coverage.R \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_commercial_project_ledger.csv \
	../input/final_density_model_input.csv \
	../input/commercial_unit_adjudication_evidence.csv \
	../input/commercial_completion_evidence.csv \
	../input/commercial_land_adjudication_evidence.csv \
	../input/new_construction_permit_unit_mentions.csv \
	../input/project_permit_chain_links.csv \
	../input/building_permits_clean.gpkg
	$(R) $<

../output/commercial_manual_rule_summary.csv: \
	../output/commercial_manual_rule_coverage.csv

../output/commercial_minimal_exception_ledger.csv: \
	../output/commercial_manual_rule_coverage.csv

../output/presample_assessor_structure_evidence.csv: \
	build_presample_assessor_structure_evidence.R \
	../output/permit_rule_coverage.csv \
	../input/residential_improvement_characteristics_full.csv
	$(R) $<

../output/presample_assessor_structure_summary.csv: \
	../output/presample_assessor_structure_evidence.csv

../output/predecessor_assessor_evidence.csv: \
	build_predecessor_assessor_evidence.R \
	../output/permit_rule_coverage.csv \
	../input/preferred_historical_predecessor_resolution.csv \
	../input/residential_improvement_characteristics_full.csv
	$(R) $<

../output/predecessor_assessor_evidence_summary.csv: \
	../output/predecessor_assessor_evidence.csv

../output/city_2015_footprint_evidence.csv: \
	build_2015_city_footprint_evidence.R \
	../output/eligibility_rule_validation.csv \
	../output/multifamily_classification_decisions.csv \
	../output/project_evidence_inventory.csv \
	../input/preferred_project_year_geometry.gpkg \
	../input/chicago_building_footprints_2015.zip
	$(R) $<

../output/city_2015_footprint_evidence_summary.csv: \
	../output/city_2015_footprint_evidence.csv

../output/residual_completed_density_attrition.csv: \
	audit_residual_completed_density_attrition.R \
	../input/final_residual_permit_chain_dispositions.csv \
	../input/residual_permit_footprint_2022_assessor_evidence.csv \
	../input/residual_footprint_field_transitions.csv \
	../input/residual_permit_historical_pin_reconciliation.csv \
	../input/residual_permit_historical_residential_history.csv \
	../input/residual_permit_historical_commercial_history.csv
	$(R) $<

../output/residual_completed_density_attrition_summary.csv: \
	../output/residual_completed_density_attrition.csv

../output/density_attrition_balance_results.csv: \
	test_residual_density_attrition_balance.R \
	../output/residual_completed_density_attrition.csv \
	../output/provisional_validated_density_input.csv \
	../input/permit_first_chain_inventory.csv \
	../input/chicago_alderman_terms.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/score_control_variants.csv
	$(R) $<

../output/density_attrition_balance_sample.csv: \
	../output/density_attrition_balance_results.csv

../output/density_attrition_treatment_counts.csv: \
	../output/density_attrition_balance_results.csv

../output/minimal_manual_exception_ledger.csv: \
	build_minimal_exception_ledger.R \
	../output/commercial_minimal_exception_ledger.csv \
	../output/parent_successor_cluster_review_queue.csv \
	../output/project_evidence_inventory.csv \
	../output/multifamily_classification_known_exceptions.csv \
	../output/eligibility_manual_exception_ledger.csv \
	../output/unresolved_duplicate_candidates.csv
	$(R) $<

../output/minimal_manual_exception_summary.csv: \
	../output/minimal_manual_exception_ledger.csv

../output/analysis_scope_omissions.csv: \
	validate_analysis_scope.R \
	../output/project_evidence_inventory.csv \
	../output/parent_successor_project_actions.csv \
	../input/multicard_external_reviewed_model_input.csv \
	../input/multicard_external_review_queue.csv
	$(R) $<

../output/analysis_scope_reconciliation.csv: \
	../output/analysis_scope_omissions.csv

../output/provisional_validated_density_input.csv: \
	build_provisional_validated_sample.R \
	../input/multicard_external_reviewed_model_input.csv \
	../output/eligibility_rule_validation.csv \
	../output/multifamily_classification_decisions.csv
	$(R) $<

../output/provisional_validated_sample_changes.csv: \
	../output/provisional_validated_density_input.csv

../output/provisional_validated_density_results.csv: \
	estimate_provisional_validated_density_models.R \
	../output/provisional_validated_density_input.csv \
	../input/score_control_variants.csv
	$(R) $<

../output/audited_density_rd_%.pdf: \
	plot_provisional_validated_density_rd.R \
	../output/provisional_validated_density_input.csv \
	../output/provisional_validated_density_results.csv \
	../input/score_control_variants.csv
	$(R) $< $*

../output/audited_density_rd_%.png: \
	../output/audited_density_rd_%.pdf
	pdftoppm -png -singlefile -r 160 $< ../output/audited_density_rd_$*

../output/audited_density_rd_%_estimates.csv: \
	../output/audited_density_rd_%.pdf

../output/project_evidence_summary.csv: ../output/project_evidence_inventory.csv

../output/initial_review_queue.csv: ../output/project_evidence_inventory.csv

../output/known_case_validation.csv: ../output/project_evidence_inventory.csv

../input/final_new_construction_audit_ledger.csv:
	@source="../../commercial_new_construction_sample_audit/output/final_new_construction_audit_ledger.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/final_density_model_input.csv:
	@source="../../commercial_new_construction_sample_audit/output/final_density_model_input.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_residential_project_ledger.csv:
	@source="../../commercial_new_construction_sample_audit/output/preferred_residential_project_ledger.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_project_duplicate_dispositions.csv:
	@source="../../commercial_new_construction_sample_audit/output/preferred_project_duplicate_dispositions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residential_project_candidate_inventory.csv:
	@source="../../commercial_new_construction_sample_audit/output/residential_project_candidate_inventory.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/new_construction_exact_permit_matches.csv:
	@source="../../commercial_new_construction_sample_audit/output/new_construction_exact_permit_matches.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/new_construction_spatial_permit_matches.csv:
	@source="../../commercial_new_construction_sample_audit/output/new_construction_spatial_permit_matches.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/new_construction_permit_unit_mentions.csv:
	@source="../../commercial_new_construction_sample_audit/output/new_construction_permit_unit_mentions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_external_review_queue.csv:
	@source="../../commercial_new_construction_sample_audit/output/multicard_external_review_queue.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_project_duplicate_pair_dispositions.csv:
	@source="../../commercial_new_construction_sample_audit/output/preferred_project_duplicate_pair_dispositions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/building_permits_clean.gpkg:
	@source="../../../clean_building_permits/output/building_permits_clean.gpkg"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/project_permit_chain_links.csv:
	@source="../../commercial_new_construction_sample_audit/output/project_permit_chain_links.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_external_reviewed_model_input.csv:
	@source="../../commercial_new_construction_sample_audit/output/multicard_external_reviewed_model_input.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_final_adjudication.csv:
	@source="../../commercial_new_construction_sample_audit/output/multicard_final_adjudication.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_component_successor_matches.csv:
	@source="../../commercial_new_construction_sample_audit/output/multicard_component_successor_matches.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_successor_suppressions.csv:
	@source="../../commercial_new_construction_sample_audit/output/multicard_successor_suppressions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_manual_episode_decisions.csv:
	@source="../../commercial_new_construction_sample_audit/adjudication/multicard_manual_episode_decisions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/commercial_completion_evidence.csv:
	@source="../../commercial_new_construction_sample_audit/output/commercial_completion_evidence.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_commercial_project_candidates.csv:
	@source="../../commercial_new_construction_sample_audit/output/preferred_commercial_project_candidates.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_commercial_project_ledger.csv:
	@source="../../commercial_new_construction_sample_audit/output/preferred_commercial_project_ledger.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/commercial_unit_adjudication_evidence.csv:
	@source="../../commercial_new_construction_sample_audit/output/commercial_unit_adjudication_evidence.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/commercial_land_adjudication_evidence.csv:
	@source="../../commercial_new_construction_sample_audit/output/commercial_land_adjudication_evidence.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_historical_predecessor_resolution.csv:
	@source="../../commercial_new_construction_sample_audit/output/preferred_historical_predecessor_resolution.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residential_improvement_characteristics_full.csv:
	@source="../../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_project_year_geometry.gpkg:
	@source="../../commercial_new_construction_sample_audit/output/preferred_project_year_geometry.gpkg"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/chicago_building_footprints_2015.zip:
	curl --fail --location \
		"https://data.cityofchicago.org/api/views/syp8-uezg/files/Wir2BTHPb7-BTOWMkr8XcKCfKCt8U6y8wK20cV4Tjhw?download=true&filename=buildings.zip" \
		--output $@

../input/score_control_variants.csv:
	@source="../../commercial_new_construction_sample_audit/input/score_control_variants.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/final_residual_permit_chain_dispositions.csv:
	@source="../../commercial_new_construction_sample_audit/output/final_residual_permit_chain_dispositions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/permit_first_chain_inventory.csv:
	@source="../../commercial_new_construction_sample_audit/output/permit_first_chain_inventory.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/chicago_alderman_terms.csv:
	@source="../../../create_alderman_data/output/chicago_alderman_terms.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/boundary_segments_1320ft.gpkg:
	@source="../../../border_segment_creation/output/boundary_segments_1320ft.gpkg"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residual_permit_footprint_2022_assessor_evidence.csv:
	@source="../../commercial_new_construction_sample_audit/output/residual_permit_footprint_2022_assessor_evidence.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residual_footprint_field_transitions.csv:
	@source="../../commercial_new_construction_sample_audit/output/residual_footprint_field_transitions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residual_permit_historical_pin_reconciliation.csv:
	@source="../../commercial_new_construction_sample_audit/output/residual_permit_historical_pin_reconciliation.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residual_permit_historical_residential_history.csv:
	@source="../../commercial_new_construction_sample_audit/output/residual_permit_historical_residential_history.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residual_permit_historical_commercial_history.csv:
	@source="../../commercial_new_construction_sample_audit/output/residual_permit_historical_commercial_history.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/final_new_construction_audit_ledger.csv \
	../input/final_density_model_input.csv \
	../input/preferred_residential_project_ledger.csv \
	../input/preferred_project_duplicate_dispositions.csv \
	../input/residential_project_candidate_inventory.csv \
	../input/new_construction_exact_permit_matches.csv \
	../input/new_construction_spatial_permit_matches.csv \
	../input/new_construction_permit_unit_mentions.csv \
	../input/multicard_external_review_queue.csv \
	../input/preferred_project_duplicate_pair_dispositions.csv \
	../input/building_permits_clean.gpkg \
	../input/project_permit_chain_links.csv \
	../input/multicard_external_reviewed_model_input.csv \
	../input/multicard_final_adjudication.csv \
	../input/multicard_component_successor_matches.csv \
	../input/multicard_successor_suppressions.csv \
	../input/multicard_manual_episode_decisions.csv \
	../input/commercial_completion_evidence.csv \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_commercial_project_ledger.csv \
	../input/commercial_unit_adjudication_evidence.csv \
	../input/commercial_land_adjudication_evidence.csv \
	../input/preferred_historical_predecessor_resolution.csv \
	../input/residential_improvement_characteristics_full.csv \
	../input/preferred_project_year_geometry.gpkg \
	../input/chicago_building_footprints_2015.zip \
	../input/score_control_variants.csv \
	../input/final_residual_permit_chain_dispositions.csv \
	../input/permit_first_chain_inventory.csv \
	../input/chicago_alderman_terms.csv \
	../input/boundary_segments_1320ft.gpkg \
	../input/residual_permit_footprint_2022_assessor_evidence.csv \
	../input/residual_footprint_field_transitions.csv \
	../input/residual_permit_historical_pin_reconciliation.csv \
	../input/residual_permit_historical_residential_history.csv \
	../input/residual_permit_historical_commercial_history.csv

include ../../../shell_functions.make
include ../../../generic.make
