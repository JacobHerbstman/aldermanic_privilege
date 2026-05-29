SHELL := bash

all: \
	../output/rental_support_by_event_time.csv \
	../output/rental_support_by_calendar_time.csv \
	../output/rental_pretrend_tests.csv \
	../output/rental_spec_comparison.csv \
	../output/rental_audit_summary.txt \
	../output/sales_support_by_event_time.csv \
	../output/sales_support_by_calendar_time.csv \
	../output/sales_pretrend_tests.csv \
	../output/sales_spec_comparison.csv \
	../output/sales_audit_summary.txt

../output/rental_support_by_event_time.csv: audit_event_studies.R \
	../input/rental_listing_panel_support_by_calendar_time.csv \
	../input/rental_listing_panel_repeat_listing_diagnostics.csv \
	../input/rental_listing_panel_assignment_stability.csv \
	../input/sales_transaction_panel_support_by_calendar_time.csv \
	../input/sales_transaction_panel_assignment_stability.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	| ../output
	Rscript audit_event_studies.R

../output/rental_support_by_calendar_time.csv ../output/rental_pretrend_tests.csv \
../output/rental_spec_comparison.csv ../output/rental_audit_summary.txt \
../output/sales_support_by_event_time.csv ../output/sales_support_by_calendar_time.csv \
../output/sales_pretrend_tests.csv ../output/sales_spec_comparison.csv ../output/sales_audit_summary.txt: ../output/rental_support_by_event_time.csv
	@test -s "$@"

../input/rental_listing_panel_support_by_calendar_time.csv: ../../../audits/rental_event_study_audit/output/rental_listing_panel_support_by_calendar_time.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/rental_listing_panel_repeat_listing_diagnostics.csv: ../../../audits/rental_event_study_audit/output/rental_listing_panel_repeat_listing_diagnostics.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/rental_listing_panel_assignment_stability.csv: ../../../audits/rental_event_study_audit/output/rental_listing_panel_assignment_stability.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_support_by_calendar_time.csv: ../../../audits/sales_event_study_audit/output/sales_transaction_panel_support_by_calendar_time.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_assignment_stability.csv: ../../../audits/sales_event_study_audit/output/sales_transaction_panel_assignment_stability.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv: ../../../run_event_study_rental_disaggregate/output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv: ../../../run_event_study_rental_disaggregate/output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv: ../../../run_event_study_rental_disaggregate/output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv: ../../../run_event_study_rental_disaggregate/output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv: ../../../run_event_study_rental_disaggregate/output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_support_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_coefficients_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_support_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_coefficients_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv: ../../../run_event_study_sales_disaggregate/output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/rental_listing_panel_support_by_calendar_time.csv \
	../input/rental_listing_panel_repeat_listing_diagnostics.csv \
	../input/rental_listing_panel_assignment_stability.csv \
	../input/sales_transaction_panel_support_by_calendar_time.csv \
	../input/sales_transaction_panel_assignment_stability.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_1000ft_mf_no_hedonics_short.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_mf_short.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_mf_overlap.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv

include ../../../shell_functions.make
include ../../../generic.make
