all: ../output/project_verification_ledger.csv \
	../output/project_verification_summary.csv \
	../output/project_verification_review_queue.csv \
	../output/snapshot_rule_sensitivity.csv \
	../output/extended_permit_candidates.csv \
	../output/extended_permit_candidate_summary.csv \
	../output/assessor_history_review.csv \
	../output/assessor_history_review_summary.csv \
	../output/project_review_bundle.csv \
	../output/project_permit_history.csv \
	../output/reviewed_project_ledger.csv \
	../output/reviewed_project_summary.csv \
	../output/remaining_multifamily_review_queue.csv \
	../output/nonmultifamily_validation_sample.csv \
	../output/street_range_permit_candidates.csv \
	../output/duplicate_project_candidates.csv \
	../output/duplicate_project_summary.csv \
	../output/assessor_only_validation.csv \
	../output/assessor_only_cluster_summary.csv \
	../output/assessor_only_validation_summary.csv \
	../output/assessor_only_exact_permit_history.csv \
	../output/assessor_only_exact_permit_summary.csv \
	../output/final_project_verification_ledger.csv \
	../output/final_project_verification_summary.csv \
	../output/final_project_verification_checks.csv \
	../output/final_project_verification_unresolved.csv \
	../output/final_verified_density_input.csv \
	../output/final_verified_density_input_summary.csv \
	../output/final_verified_density_geography_changes.csv \
	../output/final_verified_density_results.csv \
	../output/final_verified_density_checks.csv \
	../output/final_review_decomposition.csv \
	../output/final_verified_density_rd.pdf \
	../output/final_verified_density_rd.png \
	../output/final_verified_density_rd_estimates.csv \
	../output/assessor_only_retention_sensitivity.csv \
	../output/official_building_footprints_2008.gpkg \
	../output/official_building_footprints_2008_manifest.csv \
	../output/official_building_footprints_2015.gpkg \
	../output/official_building_footprints_2015_manifest.csv \
	../output/official_building_footprints_2022.gpkg \
	../output/official_building_footprints_2022_manifest.csv

../output/project_verification_ledger.csv: \
	build_project_verification_ledger.R \
	../input/eligibility_uncorroborated_retained.csv \
	../input/preferred_project_year_geometry.gpkg \
	../input/chicago_building_footprints_2015.zip \
	../input/multicard_permit_adjudication_links.csv \
	../input/multicard_external_review_queue.csv \
	../input/density_historical_address_records.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/building_permits_clean.gpkg \
	../output/official_building_footprints_2008.gpkg \
	../output/official_building_footprints_2015.gpkg \
	../output/official_building_footprints_2022.gpkg
	$(R) $<

../output/project_verification_summary.csv: \
	../output/project_verification_ledger.csv

../output/project_verification_review_queue.csv: \
	../output/project_verification_ledger.csv

../output/snapshot_rule_sensitivity.csv: \
	../output/project_verification_ledger.csv

../output/extended_permit_candidates.csv: \
	build_extended_permit_candidates.R \
	../output/project_verification_ledger.csv \
	../input/building_permits_clean.gpkg
	$(R) $<

../output/extended_permit_candidate_summary.csv: \
	../output/extended_permit_candidates.csv

../output/assessor_history_review.csv: \
	build_assessor_history_review.R \
	../output/project_verification_ledger.csv \
	../input/residential_improvement_characteristics_full.csv
	$(R) $<

../output/assessor_history_review_summary.csv: \
	../output/assessor_history_review.csv

../output/project_review_bundle.csv: \
	build_project_review_bundle.R \
	../output/project_verification_review_queue.csv \
	../output/assessor_history_review_summary.csv \
	../output/extended_permit_candidate_summary.csv
	$(R) $<

../output/project_permit_history.csv: \
	build_project_permit_history.R \
	../output/project_verification_ledger.csv \
	../input/building_permits_clean.gpkg
	$(R) $<

../output/reviewed_project_ledger.csv: \
	build_reviewed_project_ledger.R \
	../output/project_verification_ledger.csv \
	../output/project_permit_history.csv \
	../output/extended_permit_candidate_summary.csv \
	../input/provisional_validated_density_input.csv \
	../adjudication/project_manual_reviews.csv
	$(R) $<

../output/reviewed_project_summary.csv: \
	../output/reviewed_project_ledger.csv

../output/remaining_multifamily_review_queue.csv: \
	build_remaining_review_queues.R \
	../output/reviewed_project_ledger.csv
	$(R) $<

../output/nonmultifamily_validation_sample.csv: \
	../output/remaining_multifamily_review_queue.csv

../output/street_range_permit_candidates.csv: \
	build_street_range_permit_candidates.R \
	../output/reviewed_project_ledger.csv \
	../input/building_permits_clean.gpkg
	$(R) $<

../output/duplicate_project_candidates.csv: \
	build_duplicate_project_check.R \
	../output/reviewed_project_ledger.csv \
	../input/preferred_project_year_geometry.gpkg
	$(R) $<

../output/duplicate_project_summary.csv: \
	../output/duplicate_project_candidates.csv

../output/assessor_only_validation.csv: \
	build_assessor_only_validation.R \
	../output/reviewed_project_ledger.csv \
	../input/parcel_sales_city.csv
	$(R) $<

../output/assessor_only_cluster_summary.csv: \
	../output/assessor_only_validation.csv

../output/assessor_only_validation_summary.csv: \
	../output/assessor_only_validation.csv

../output/assessor_only_exact_permit_history.csv: \
	build_assessor_only_exact_permit_review.R \
	../output/reviewed_project_ledger.csv \
	../input/building_permits_clean.gpkg
	$(R) $<

../output/assessor_only_exact_permit_summary.csv: \
	../output/assessor_only_exact_permit_history.csv

../output/final_project_verification_ledger.csv: \
	build_final_project_verification_ledger.R \
	../output/reviewed_project_ledger.csv \
	../output/assessor_only_validation.csv \
	../output/assessor_only_exact_permit_summary.csv \
	../adjudication/assessor_default_site_reviews.csv \
	../adjudication/assessor_default_project_exceptions.csv \
	../adjudication/final_project_overrides.csv
	$(R) $<

../output/final_project_verification_summary.csv: \
	../output/final_project_verification_ledger.csv

../output/final_project_verification_checks.csv: \
	check_final_project_verification.R \
	../output/final_project_verification_ledger.csv \
	../../new_construction_universe_validation/output/unresolved_duplicate_candidates.csv \
	../adjudication/final_project_overrides.csv
	$(R) $<

../output/final_project_verification_unresolved.csv: \
	../output/final_project_verification_checks.csv

../output/final_verified_density_input.csv: \
	build_final_verified_density_input.R \
	../../../_lib/canonical_geometry_helpers.R \
	../output/final_project_verification_ledger.csv \
	../input/provisional_validated_density_input.csv \
	../input/final_new_construction_boundary_scope.csv \
	../input/final_new_construction_zoning.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/ward_controls_2000_2023.csv \
	../input/alderman_uncertainty_index.csv \
	../adjudication/corrected_year_zoning_decisions.csv
	$(R) $<

../output/final_verified_density_input_summary.csv: \
	../output/final_verified_density_input.csv

../output/final_verified_density_geography_changes.csv: \
	../output/final_verified_density_input.csv

../output/final_verified_density_results.csv: \
	estimate_final_verified_density_models.R \
	../output/final_verified_density_input.csv
	$(R) $<

../output/final_verified_density_checks.csv: \
	check_final_verified_density_input.R \
	../output/final_verified_density_input.csv \
	../output/final_verified_density_geography_changes.csv \
	../output/final_verified_density_results.csv \
	../output/final_project_verification_ledger.csv \
	../input/provisional_validated_density_input.csv
	$(R) $<

../output/final_review_decomposition.csv: \
	estimate_final_review_decomposition.R \
	../output/final_verified_density_input.csv \
	../output/final_project_verification_ledger.csv \
	../input/provisional_validated_density_input.csv \
	../input/alderman_uncertainty_index.csv
	$(R) $<

../output/final_verified_density_rd.pdf: \
	plot_final_verified_density_rd.R \
	../output/final_verified_density_input.csv \
	../output/final_verified_density_results.csv
	$(R) $<

../output/final_verified_density_rd.png: \
	../output/final_verified_density_rd.pdf

../output/final_verified_density_rd_estimates.csv: \
	../output/final_verified_density_rd.pdf

../output/assessor_only_retention_sensitivity.csv: \
	estimate_assessor_only_retention_sensitivity.R \
	../output/reviewed_project_ledger.csv \
	../input/provisional_validated_density_input.csv \
	../input/score_control_variants.csv
	$(R) $<

../output/official_building_footprints_%.gpkg: \
	download_official_footprint_snapshot.R \
	../input/eligibility_uncorroborated_retained.csv \
	../input/preferred_project_year_geometry.gpkg
	$(R) $< $*

../output/official_building_footprints_%_manifest.csv: \
	../output/official_building_footprints_%.gpkg

../output/official_building_footprints_2015.gpkg: \
	build_2015_footprint_snapshot.R \
	../input/eligibility_uncorroborated_retained.csv \
	../input/preferred_project_year_geometry.gpkg \
	../input/chicago_building_footprints_2015.zip
	$(R) $<

../input/eligibility_uncorroborated_retained.csv: \
	../../new_construction_universe_validation/output/eligibility_uncorroborated_retained.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/preferred_project_year_geometry.gpkg: \
	../../new_construction_universe_validation/input/preferred_project_year_geometry.gpkg
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/chicago_building_footprints_2015.zip: \
	../../new_construction_universe_validation/input/chicago_building_footprints_2015.zip
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_permit_adjudication_links.csv: \
	../../commercial_new_construction_sample_audit/output/multicard_permit_adjudication_links.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/multicard_external_review_queue.csv: \
	../../commercial_new_construction_sample_audit/output/multicard_external_review_queue.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/density_historical_address_records.csv: \
	../../density_project_lineage/output/density_historical_address_records.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/parcel_addresses_2025_chicago.csv: \
	../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/building_permits_clean.gpkg: \
	../../../clean_building_permits/output/building_permits_clean.gpkg
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residential_improvement_characteristics_full.csv: \
	../../../download_residential_improvements_full/output/residential_improvement_characteristics_full.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/parcel_sales_city.csv: \
	../../../download_parcel_sales_data/output/parcel_sales_city.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/provisional_validated_density_input.csv: \
	../../new_construction_universe_validation/output/provisional_validated_density_input.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/score_control_variants.csv: \
	../../income_score_current_results/output/current_income_scores.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/final_new_construction_boundary_scope.csv: \
	../../commercial_new_construction_sample_audit/output/final_new_construction_boundary_scope.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/final_new_construction_zoning.csv: \
	../../commercial_new_construction_sample_audit/output/final_new_construction_zoning.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/ward_panel.gpkg: \
	../../../ward_panel_create/output/ward_panel.gpkg
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/ward_pair_boundaries.gpkg: \
	../../../border_segment_creation/output/ward_pair_boundaries.gpkg
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/boundary_segments_1320ft.gpkg: \
	../../../border_segment_creation/output/boundary_segments_1320ft.gpkg
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/chicago_alderman_panel.csv: \
	../../../create_alderman_data/output/chicago_alderman_panel.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/ward_controls_2000_2023.csv: \
	../../../create_ward_controls/output/ward_controls_2000_2023.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/alderman_uncertainty_index.csv: \
	../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/eligibility_uncorroborated_retained.csv \
	../input/preferred_project_year_geometry.gpkg \
	../input/chicago_building_footprints_2015.zip \
	../input/multicard_permit_adjudication_links.csv \
	../input/multicard_external_review_queue.csv \
	../input/density_historical_address_records.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/building_permits_clean.gpkg \
	../input/residential_improvement_characteristics_full.csv \
	../input/parcel_sales_city.csv \
	../input/provisional_validated_density_input.csv \
	../input/score_control_variants.csv \
	../input/final_new_construction_boundary_scope.csv \
	../input/final_new_construction_zoning.csv \
	../input/ward_panel.gpkg \
	../input/ward_pair_boundaries.gpkg \
	../input/boundary_segments_1320ft.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/ward_controls_2000_2023.csv \
	../input/alderman_uncertainty_index.csv

include ../../../shell_functions.make
include ../../../generic.make
