SHELL := bash

all: \
	../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv \
	../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv \
	../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv \
	../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv \
	../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv \
	../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv \
	../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv \
	../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv \
	../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv \
	../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv \
	../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv \
	../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv \
	../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv \
	../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv \
	../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv \
	../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv \
	../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv \
	../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv \
	../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv \
	../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv \
	../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv

pooled_did: \
	../output/did_table_rental.tex \
	../output/did_table_rental.csv \
	../output/did_coefficients_rental.csv \
	../output/did_table_rental_clean.tex

../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv: ../../../run_event_study_rental_disaggregate/code/run_event_study_disaggregate.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../output
	$(MAKE) -C ../../../run_event_study_rental_disaggregate/code link-inputs
	cd ../../../run_event_study_rental_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/rental_event_study_audit/output Rscript run_event_study_disaggregate.R cohort_2023 yearly continuous TRUE triangular 250 multifamily_only strict_pair_x_year short segment twoway_block_segment

../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv: ../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_short.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv: ../../../run_event_study_rental_disaggregate/code/run_event_study_disaggregate.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../output
	$(MAKE) -C ../../../run_event_study_rental_disaggregate/code link-inputs
	cd ../../../run_event_study_rental_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/rental_event_study_audit/output Rscript run_event_study_disaggregate.R cohort_2023 yearly continuous_split TRUE triangular 250 multifamily_only strict_pair_x_year short segment twoway_block_segment

../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv: ../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_short.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv: ../../../run_event_study_rental_disaggregate/code/run_event_study_disaggregate.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../output
	$(MAKE) -C ../../../run_event_study_rental_disaggregate/code link-inputs
	cd ../../../run_event_study_rental_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/rental_event_study_audit/output Rscript run_event_study_disaggregate.R cohort_2023 yearly continuous FALSE triangular 250 multifamily_only strict_pair_x_year short segment twoway_block_segment

../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv: ../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_triangular_250m_mf_no_hedonics_short.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv: ../../../run_event_study_rental_disaggregate/code/run_event_study_disaggregate.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../output
	$(MAKE) -C ../../../run_event_study_rental_disaggregate/code link-inputs
	cd ../../../run_event_study_rental_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/rental_event_study_audit/output Rscript run_event_study_disaggregate.R cohort_2023 yearly continuous_split FALSE triangular 250 multifamily_only strict_pair_x_year short segment twoway_block_segment

../output/event_study_support_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv: ../output/event_study_coefficients_disaggregate_yearly_cohort_2023_continuous_split_triangular_250m_mf_no_hedonics_short.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv: ../../../run_event_study_rental_disaggregate/code/run_event_study_disaggregate.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2015.parquet | ../output
	$(MAKE) -C ../../../run_event_study_rental_disaggregate/code link-inputs
	cd ../../../run_event_study_rental_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/rental_event_study_audit/output Rscript run_event_study_disaggregate.R cohort_2015 yearly continuous TRUE triangular 250 multifamily_only strict_pair_x_year short segment twoway_block_segment

../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv: ../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_250m_mf_short.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv: ../../../run_event_study_rental_disaggregate/code/run_event_study_disaggregate.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../output
	$(MAKE) -C ../../../run_event_study_rental_disaggregate/code link-inputs
	cd ../../../run_event_study_rental_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/rental_event_study_audit/output Rscript run_event_study_disaggregate.R stacked_implementation yearly continuous TRUE triangular 250 multifamily_only strict_pair_x_year overlap segment twoway_block_segment

../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv: ../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_250m_mf_overlap.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv: ../../../run_event_study_rental_disaggregate/code/run_event_study_disaggregate.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../output
	$(MAKE) -C ../../../run_event_study_rental_disaggregate/code link-inputs
	cd ../../../run_event_study_rental_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/rental_event_study_audit/output Rscript run_event_study_disaggregate.R stacked_implementation yearly continuous_split TRUE triangular 250 multifamily_only strict_pair_x_year overlap segment twoway_block_segment

../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv: ../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_split_triangular_250m_mf_overlap.csv
	@test -s "$@"

../output/did_table_rental.tex: run_did_rental_disaggregate.R ../input/rental_listing_panel.parquet | ../output
	$(R) $< 250 triangular strict_pair_x_year segment twoway_block_segment

../output/did_table_rental.csv \
../output/did_coefficients_rental.csv \
../output/did_table_rental_clean.tex: ../output/did_table_rental.tex
	@test -s "$@"

../input/rental_listing_panel.parquet: ../../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_listing_panel.parquet

include ../../../shell_functions.make
include ../../../generic.make
