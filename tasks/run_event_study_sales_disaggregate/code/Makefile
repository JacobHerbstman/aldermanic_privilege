SHELL := bash
include ../../shell_functions.make

MAIN_BW_M ?= 250
MAIN_BW_LABEL ?= $(MAIN_BW_M)m

all: \
	../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf \
	../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf \
	../output/did_table_sales_2015_uniform_$(MAIN_BW_LABEL)_geo_wardpair_clust_block.tex

../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf: run_event_study.R ../../_lib/event_study_plot_helpers.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year uniform $(MAIN_BW_M) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf: run_event_study.R ../../_lib/event_study_plot_helpers.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year uniform $(MAIN_BW_M) full ward_pair block amenity $(MAIN_BW_LABEL)

../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.csv: ../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_$(MAIN_BW_LABEL)_amenity_full_geo_wardpair_clust_block.pdf

../output/did_table_sales_2015_uniform_$(MAIN_BW_LABEL)_geo_wardpair_clust_block.tex: did_table_sales_2015.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript did_table_sales_2015.R $(MAIN_BW_M) uniform "$@" amenity

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/sales_transaction_panel_2015.parquet

include ../../generic.make
