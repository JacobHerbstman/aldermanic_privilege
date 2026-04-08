SHELL := bash
include ../../shell_functions.make

PAPER_PDFS := \
	../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_1000ft_full_geo_wardpair_clust_block.pdf \
	../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_1000ft_full_geo_wardpair_clust_block.pdf
PAPER_TABLES := ../output/did_table_sales_2015_uniform_1000ft_geo_wardpair_clust_block.tex

all: $(PAPER_PDFS) $(PAPER_TABLES)

../output/event_study_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year triangular 1000 full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_full.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year triangular 1000 full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_no_hedonics_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous FALSE yearly strict_pair_x_year triangular 1000 full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_no_hedonics_full.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_no_hedonics_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_no_hedonics_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_no_hedonics_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_no_hedonics_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_triangular_1000ft_no_hedonics_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split FALSE yearly strict_pair_x_year triangular 1000 full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
../output/event_study_support_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R cohort_2012 continuous TRUE yearly strict_pair_x_year triangular 1000 full segment twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
../output/event_study_support_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R stacked_announcement continuous TRUE yearly strict_pair_x_year triangular 1000 full segment twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.pdf \
../output/event_study_coefficients_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
../output/event_study_support_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
../output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
../output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R stacked_implementation continuous TRUE yearly strict_pair_x_year triangular 1000 full segment twoway_block_segment

../output/event_study_disaggregate_yearly_cohort_2015_continuous_uniform_1000ft_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_uniform_1000ft_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_uniform_1000ft_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_uniform_1000ft_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_uniform_1000ft_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous TRUE yearly strict_pair_x_year uniform 1000 full ward_pair block

../output/event_study_disaggregate_yearly_cohort_2015_continuous_split_uniform_1000ft_full_geo_wardpair_clust_block.pdf \
../output/event_study_combined_disaggregate_yearly_cohort_2015_continuous_split_uniform_1000ft_full_geo_wardpair_clust_block.pdf \
../output/event_study_coefficients_disaggregate_yearly_cohort_2015_continuous_split_uniform_1000ft_full_geo_wardpair_clust_block.csv \
../output/event_study_support_disaggregate_yearly_cohort_2015_continuous_split_uniform_1000ft_full_geo_wardpair_clust_block.csv \
../output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_split_uniform_1000ft_full_geo_wardpair_clust_block.csv \
../output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_split_uniform_1000ft_full_geo_wardpair_clust_block.csv: run_event_study.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study.R cohort_2015 continuous_split TRUE yearly strict_pair_x_year uniform 1000 full ward_pair block

../output/did_table_sales_clean_uniform_1000ft.tex: run_did_sales_disaggregate.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript run_did_sales_disaggregate.R 1000 uniform strict_pair_x_year segment twoway_block_segment
	cp ../output/did_table_sales_clean.tex "$@"

../output/did_table_sales_2015_uniform_1000ft_geo_wardpair_clust_block.tex: did_table_sales_2015.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript did_table_sales_2015.R 1000 uniform $@

../input/sales_transaction_panel.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_announcement.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_announcement.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2022.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2022.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2023.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_transaction_panel_2015.parquet

include ../../generic.make
