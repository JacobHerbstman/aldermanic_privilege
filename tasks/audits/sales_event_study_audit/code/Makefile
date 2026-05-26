SHELL := bash

MAIN_BW_M ?= 304.8
MAIN_BW_LABEL ?= 1000ft

all:

event_study_sidecars: \
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

pooled_did: \
	../output/did_table_sales.tex \
	../output/did_table_sales.csv \
	../output/did_coefficients_sales.csv \
	../output/did_table_sales_clean.tex

all_redistricted_sales: \
	../output/all_redistricted_sales_2015_did.csv \
	../output/all_redistricted_sales_2015_sample_summary.csv \
	../output/all_redistricted_sales_2015_event_coefficients.csv \
	../output/all_redistricted_sales_2015_event_studies.pdf \
	../output/all_redistricted_sales_2015_event_studies.png \
	../output/all_redistricted_sales_2015_event_study_all_valid_event_pair_x_year.pdf \
	../output/all_redistricted_sales_2015_event_study_all_valid_block_event_pair_x_year.pdf \
	../output/all_redistricted_sales_2015_event_study_all_valid_block_year.pdf \
	../output/all_redistricted_sales_2015_event_study_all_valid_origin_year.pdf \
	../output/all_redistricted_sales_2015_event_study_all_valid_origin_x_year.pdf \
	../output/all_redistricted_sales_2015_event_study_boundary_1000ft_block_event_pair_x_year.pdf \
	../output/all_redistricted_sales_2015_event_study_redistricted_only_block_pair_x_year.pdf \
	../output/all_redistricted_sales_2015_event_study_redistricted_only_block_year.pdf \
	../output/all_redistricted_sales_2015_event_study_redistricted_only_origin_year.pdf \
	../output/all_redistricted_sales_2015_event_study_redistricted_only_pair_year.pdf

boundary_origin_destination_sales: \
	../output/boundary_origin_destination_sales_2015_did.csv \
	../output/boundary_origin_destination_sales_2015_sample_summary.csv \
	../output/boundary_origin_destination_sales_2015_event_coefficients.csv \
	../output/boundary_origin_destination_sales_2015_event_studies.pdf \
	../output/boundary_origin_destination_sales_2015_event_studies.png \
	../output/boundary_origin_destination_sales_2015_event_study_border_pair_side_pair_year.pdf \
	../output/boundary_origin_destination_sales_2015_event_study_block_pair_year.pdf \
	../output/boundary_origin_destination_sales_2015_event_study_block_year.pdf

nearest_boundary_local_control_sales: \
	../output/nearest_boundary_local_control_sales_2015_did.csv \
	../output/nearest_boundary_local_control_sales_2015_sample_summary.csv \
	../output/nearest_boundary_local_control_sales_2015_event_coefficients.csv \
	../output/nearest_boundary_local_control_sales_2015_event_studies.pdf \
	../output/nearest_boundary_local_control_sales_2015_event_studies.png \
	../output/nearest_boundary_local_control_sales_2015_event_study_local_pair_side_pair_year.pdf \
	../output/nearest_boundary_local_control_sales_2015_event_study_block_local_pair_year.pdf \
	../output/nearest_boundary_local_control_sales_2015_event_study_block_year.pdf

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

../output/did_table_sales.tex: run_did_sales_disaggregate.R ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet | ../output
	$(R) $< 300 triangular strict_pair_x_year segment twoway_block_segment 300m

../output/did_table_sales.csv \
../output/did_coefficients_sales.csv \
../output/did_table_sales_clean.tex: ../output/did_table_sales.tex
	@test -s "$@"

../output/all_redistricted_sales_2015_did.csv: run_all_redistricted_sales.R ../input/sales_transaction_panel_2015_all_valid.parquet | ../output
	$(R) $<

../output/all_redistricted_sales_2015_sample_summary.csv \
../output/all_redistricted_sales_2015_event_coefficients.csv \
../output/all_redistricted_sales_2015_event_studies.pdf \
../output/all_redistricted_sales_2015_event_studies.png \
../output/all_redistricted_sales_2015_event_study_all_valid_event_pair_x_year.pdf \
../output/all_redistricted_sales_2015_event_study_all_valid_block_event_pair_x_year.pdf \
../output/all_redistricted_sales_2015_event_study_all_valid_block_year.pdf \
../output/all_redistricted_sales_2015_event_study_all_valid_origin_year.pdf \
../output/all_redistricted_sales_2015_event_study_all_valid_origin_x_year.pdf \
../output/all_redistricted_sales_2015_event_study_boundary_1000ft_block_event_pair_x_year.pdf \
../output/all_redistricted_sales_2015_event_study_redistricted_only_block_pair_x_year.pdf \
../output/all_redistricted_sales_2015_event_study_redistricted_only_block_year.pdf \
../output/all_redistricted_sales_2015_event_study_redistricted_only_origin_year.pdf \
../output/all_redistricted_sales_2015_event_study_redistricted_only_pair_year.pdf: ../output/all_redistricted_sales_2015_did.csv
	@test -s "$@"

../output/boundary_origin_destination_sales_2015_did.csv: run_boundary_origin_destination_sales.R ../input/sales_transaction_panel_2015.parquet | ../output
	$(R) $<

../output/boundary_origin_destination_sales_2015_sample_summary.csv \
../output/boundary_origin_destination_sales_2015_event_coefficients.csv \
../output/boundary_origin_destination_sales_2015_event_studies.pdf \
../output/boundary_origin_destination_sales_2015_event_studies.png \
../output/boundary_origin_destination_sales_2015_event_study_border_pair_side_pair_year.pdf \
../output/boundary_origin_destination_sales_2015_event_study_block_pair_year.pdf \
../output/boundary_origin_destination_sales_2015_event_study_block_year.pdf: ../output/boundary_origin_destination_sales_2015_did.csv
	@test -s "$@"

../output/nearest_boundary_local_control_sales_2015_did.csv: run_nearest_boundary_local_control_sales.R ../input/sales_transaction_panel_2015_all_valid.parquet | ../output
	$(R) $<

../output/nearest_boundary_local_control_sales_2015_sample_summary.csv \
../output/nearest_boundary_local_control_sales_2015_event_coefficients.csv \
../output/nearest_boundary_local_control_sales_2015_event_studies.pdf \
../output/nearest_boundary_local_control_sales_2015_event_studies.png \
../output/nearest_boundary_local_control_sales_2015_event_study_local_pair_side_pair_year.pdf \
../output/nearest_boundary_local_control_sales_2015_event_study_block_local_pair_year.pdf \
../output/nearest_boundary_local_control_sales_2015_event_study_block_year.pdf: ../output/nearest_boundary_local_control_sales_2015_did.csv
	@test -s "$@"

../input/sales_transaction_panel_2012.parquet: ../../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2015_all_valid.parquet: ../../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015_all_valid.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2015_all_valid.parquet

include ../../../shell_functions.make
include ../../../generic.make
