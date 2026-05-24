SHELL := bash

MAIN_BW_M ?= 304.8
MAIN_BW_LABEL ?= 1000ft

all: \
	../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
	../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv

../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../../../run_event_study_sales_disaggregate/code/run_event_study.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../output
	$(MAKE) -C ../../../run_event_study_sales_disaggregate/code link-inputs
	cd ../../../run_event_study_sales_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/sales_event_study_audit/output Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year uniform $(MAIN_BW_M) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../../../run_event_study_sales_disaggregate/code/run_event_study.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../output
	$(MAKE) -C ../../../run_event_study_sales_disaggregate/code link-inputs
	cd ../../../run_event_study_sales_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/sales_event_study_audit/output Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year uniform $(MAIN_BW_M) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../../../run_event_study_sales_disaggregate/code/run_event_study.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../output
	$(MAKE) -C ../../../run_event_study_sales_disaggregate/code link-inputs
	cd ../../../run_event_study_sales_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/sales_event_study_audit/output Rscript run_event_study.R stacked_implementation continuous TRUE yearly strict_pair_x_year uniform $(MAIN_BW_M) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv
	@test -s "$@"

../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../../../run_event_study_sales_disaggregate/code/run_event_study.R ../../../_lib/event_study_plot_helpers.R ../../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../output
	$(MAKE) -C ../../../run_event_study_sales_disaggregate/code link-inputs
	cd ../../../run_event_study_sales_disaggregate/code && WRITE_SIDECARS=1 SIDECAR_OUTPUT_DIR=../../audits/sales_event_study_audit/output Rscript run_event_study.R stacked_implementation continuous_split TRUE yearly strict_pair_x_year uniform $(MAIN_BW_M) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv
	@test -s "$@"

link-inputs:

include ../../../shell_functions.make
include ../../../generic.make
