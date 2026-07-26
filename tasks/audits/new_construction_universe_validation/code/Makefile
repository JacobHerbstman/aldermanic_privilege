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
	../output/multifamily_classification_new_review_queue.csv \
	../output/multifamily_classification_known_exceptions.csv \
	../output/permit_rule_coverage.csv \
	../output/permit_rule_gap_review.csv \
	../output/permit_rule_coverage_summary.csv \
	../output/eligibility_rule_validation.csv \
	../output/eligibility_rule_summary.csv \
	../output/eligibility_manual_review_queue.csv \
	../output/permit_pin_geography_conflicts.csv \
	../output/permit_pin_geography_conflict_summary.csv \
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

../output/multifamily_classification_new_review_queue.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/multifamily_classification_known_exceptions.csv: \
	../output/multifamily_classification_rule_validation.csv

../output/permit_rule_gap_review.csv: audit_permit_rule_coverage.R \
	../output/project_evidence_inventory.csv \
	../input/project_permit_chain_links.csv
	$(R) $<

../output/permit_rule_coverage_summary.csv: \
	../output/permit_rule_gap_review.csv

../output/permit_rule_coverage.csv: \
	../output/permit_rule_gap_review.csv

../output/eligibility_rule_validation.csv: \
	validate_new_construction_eligibility.R \
	../output/permit_rule_coverage.csv \
	../input/multicard_external_reviewed_model_input.csv \
	../input/commercial_completion_evidence.csv
	$(R) $<

../output/eligibility_rule_summary.csv: \
	../output/eligibility_rule_validation.csv

../output/eligibility_manual_review_queue.csv: \
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
	../input/commercial_completion_evidence.csv

include ../../../shell_functions.make
include ../../../generic.make
