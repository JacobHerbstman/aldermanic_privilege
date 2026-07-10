KHAN_START_DATE ?= 2010-11-01
KHAN_END_DATE ?= 2016-08-31
DATE_TAG ?= $(subst -,,$(KHAN_START_DATE))_$(subst -,,$(KHAN_END_DATE))
PD_FAR_REVIEW_START_YEAR ?= 2011
PD_FAR_REVIEW_END_YEAR ?= 2020
PD_FAR_REVIEW_TAG := $(PD_FAR_REVIEW_START_YEAR)_$(PD_FAR_REVIEW_END_YEAR)

all: \
	../output/councilmatic_missing_ordinance_text.csv \
	../output/khan_zoning_matters_journal_codes_$(DATE_TAG).csv \
	../output/khan_zoning_matters_far_$(DATE_TAG).csv \
	../output/khan_comparison_summary_$(DATE_TAG).csv \
	../output/khan_comparison_year_counts_$(DATE_TAG).csv \
	../output/khan_comparison_pd_transitions_$(DATE_TAG).csv \
	../output/khan_comparison_sample_$(DATE_TAG).csv \
	../output/khan_family_counts_$(DATE_TAG).csv \
	../output/khan_far_rule_variants_$(DATE_TAG).csv \
	../output/khan_pd_direction_decomposition_$(DATE_TAG).csv \
	../output/khan_reconciled_far_summary.csv \
	../output/khan_reconciled_sample_corrections.csv \
	../output/khan_reconciled_family_summary.csv \
	../output/khan_pd_preexisting_far_audit.csv \
	../output/khan_pd_transition_corrections_audit.csv \
	../output/khan_destination_code_corrections_audit.csv \
	../output/khan_reconciled_sample.csv \
	../output/khan_allowed_use_family_variants.csv \
	../output/khan_mixed_pd_allowed_use_review.csv

../output/councilmatic_missing_ordinance_text.csv: fetch_councilmatic_missing_ordinances.py ../output/khan_zoning_matters_far_$(DATE_TAG).csv ../input/khan_sample_corrections.csv ../input/khan_pd_transition_corrections.csv | ../output run.sbatch
	$(PYTHON) $<

../output/khan_reconciled_far_summary.csv: analyze_reconciled_sample.py ../output/khan_zoning_matters_far_$(DATE_TAG).csv ../output/councilmatic_missing_ordinance_text.csv ../input/khan_sample_corrections.csv ../input/khan_pd_underlying_evidence.csv ../input/khan_pd_transition_corrections.csv ../input/khan_destination_code_corrections.csv | ../output run.sbatch
	$(PYTHON) $<

../output/khan_reconciled_sample_corrections.csv: ../output/khan_reconciled_far_summary.csv

../output/khan_reconciled_family_summary.csv: ../output/khan_reconciled_far_summary.csv

../output/khan_pd_preexisting_far_audit.csv: ../output/khan_reconciled_far_summary.csv

../output/khan_pd_transition_corrections_audit.csv: ../output/khan_reconciled_far_summary.csv

../output/khan_destination_code_corrections_audit.csv: ../output/khan_reconciled_far_summary.csv

../output/khan_reconciled_sample.csv: ../output/khan_reconciled_far_summary.csv

../output/khan_allowed_use_family_variants.csv: compare_allowed_use_family_rules.py ../output/khan_reconciled_sample.csv ../input/khan_pd_subtype_overrides.csv ../input/khan_mixed_pd_allowed_use_decisions.csv ../input/pdf_text_$(DATE_TAG).csv | ../output run.sbatch
	$(PYTHON) $<

../output/khan_mixed_pd_allowed_use_review.csv: ../output/khan_allowed_use_family_variants.csv

../output/khan_zoning_matters_journal_codes_$(DATE_TAG).csv: apply_journal_code_fills.py ../input/zoning_matters_$(DATE_TAG).csv ../input/khan_journal_2010_code_fills.csv | ../output run.sbatch
	$(PYTHON) $< --date-tag $(DATE_TAG)

../output/khan_zoning_matters_far_$(DATE_TAG).csv: ../../../rezoning_far_pre_geocode/code/assign_far_pre_geocode.py ../output/khan_zoning_matters_journal_codes_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv | ../output run.sbatch
	$(PYTHON) ../../../rezoning_far_pre_geocode/code/assign_far_pre_geocode.py \
		--in-master-csv ../output/khan_zoning_matters_journal_codes_$(DATE_TAG).csv \
		--in-zoning-lookup-csv ../input/zoning_far_lookup_clean.csv \
		--out-far-csv ../output/khan_zoning_matters_far_$(DATE_TAG).csv

../output/khan_comparison_summary_$(DATE_TAG).csv: compare_khan_rezonings.py ../output/khan_zoning_matters_far_$(DATE_TAG).csv ../input/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv | ../output run.sbatch
	$(PYTHON) compare_khan_rezonings.py \
		--in-rezoning-csv ../output/khan_zoning_matters_far_$(DATE_TAG).csv \
		--in-pd-far-updates-csv ../input/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv \
		--in-pd-to-pd-manual-csv ../output/khan_pd_to_pd_plausible_far_manual_decisions.csv \
		--in-review-queue-csv ../output/khan_pd_missing_far_manual_review_queue.csv \
		--start-date $(KHAN_START_DATE) \
		--end-date $(KHAN_END_DATE) \
		--out-summary-csv ../output/khan_comparison_summary_$(DATE_TAG).csv \
		--out-year-counts-csv ../output/khan_comparison_year_counts_$(DATE_TAG).csv \
		--out-pd-transitions-csv ../output/khan_comparison_pd_transitions_$(DATE_TAG).csv \
		--out-sample-csv ../output/khan_comparison_sample_$(DATE_TAG).csv \
		--out-family-counts-csv ../output/khan_family_counts_$(DATE_TAG).csv \
		--out-far-rule-variants-csv ../output/khan_far_rule_variants_$(DATE_TAG).csv \
		--out-pd-direction-csv ../output/khan_pd_direction_decomposition_$(DATE_TAG).csv

../output/khan_comparison_year_counts_$(DATE_TAG).csv: ../output/khan_comparison_summary_$(DATE_TAG).csv

../output/khan_comparison_pd_transitions_$(DATE_TAG).csv: ../output/khan_comparison_summary_$(DATE_TAG).csv

../output/khan_comparison_sample_$(DATE_TAG).csv: ../output/khan_comparison_summary_$(DATE_TAG).csv

../output/khan_family_counts_$(DATE_TAG).csv: ../output/khan_comparison_summary_$(DATE_TAG).csv

../output/khan_far_rule_variants_$(DATE_TAG).csv: ../output/khan_comparison_summary_$(DATE_TAG).csv

../output/khan_pd_direction_decomposition_$(DATE_TAG).csv: ../output/khan_comparison_summary_$(DATE_TAG).csv

../input/zoning_matters_$(DATE_TAG).csv: ../../../build_zoning_dataset/output/zoning_matters_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/zoning_far_lookup_clean.csv: ../../../zoning_data_cleaning/output/zoning_far_lookup_clean.csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv: ../../../rezoning_hand_adjudications/output/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/pdf_text_$(DATE_TAG).csv: ../../../elms_pdf_processing/output/pdf_text_$(DATE_TAG).csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/khan_journal_2010_code_fills.csv: ../../../rezoning_hand_adjudications/output/journal_zoning_code_fills_20101101_20160831.csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/khan_sample_corrections.csv: ../../../rezoning_hand_adjudications/output/rezoning_sample_decisions_20101101_20160831.csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/khan_pd_transition_corrections.csv: ../../../rezoning_hand_adjudications/output/pd_transition_code_corrections_20101101_20160831.csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

../input/khan_destination_code_corrections.csv: ../../../rezoning_hand_adjudications/output/destination_code_corrections_20101101_20160831.csv | ../input
	@if [ "$$(readlink "$@")" != "$<" ]; then ln -sf "$<" "$@"; fi

link-inputs: ../input/zoning_matters_$(DATE_TAG).csv ../input/zoning_far_lookup_clean.csv ../input/pd_far_text_review_updates_$(PD_FAR_REVIEW_TAG).csv ../input/pdf_text_$(DATE_TAG).csv ../input/khan_journal_2010_code_fills.csv ../input/khan_sample_corrections.csv ../input/khan_pd_underlying_evidence.csv ../input/khan_pd_transition_corrections.csv ../input/khan_destination_code_corrections.csv ../input/khan_pd_subtype_overrides.csv ../input/khan_mixed_pd_allowed_use_decisions.csv

include ../../../shell_functions.make
include ../../../generic.make
