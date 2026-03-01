SHELL := bash
include ../../shell_functions.make

COHORT ?= 2015

all: \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics.pdf \
	../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics.pdf \
	../output/did_table_sales_clean_uniform_1000ft.tex

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics.pdf: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R continuous FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics.pdf: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R continuous_split FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment twoway_block_segment

../output/did_table_sales_clean_uniform_1000ft.tex: run_did_sales_disaggregate.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript run_did_sales_disaggregate.R 1000 uniform strict_pair_x_year segment twoway_block_segment
	cp ../output/did_table_sales_clean.tex "$@"

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics_geo_wardpair.pdf: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R continuous FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full ward_pair twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_uniform_1000ft_no_hedonics_clust_block.pdf: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R continuous FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment block

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics_geo_wardpair.pdf: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R continuous_split FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full ward_pair twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_announcement_continuous_split_uniform_1000ft_no_hedonics_clust_block.pdf: run_event_study.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet | ../output
	Rscript run_event_study.R continuous_split FALSE yearly strict_pair_x_year uniform 1000 TRUE announcement $(COHORT) full segment block

../output/did_table_sales_clean_geo_wardpair.tex: run_did_sales_disaggregate.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript run_did_sales_disaggregate.R 1000 uniform strict_pair_x_year ward_pair twoway_block_segment

../output/did_table_sales_clean_clust_block.tex: run_did_sales_disaggregate.R ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript run_did_sales_disaggregate.R 1000 uniform strict_pair_x_year segment block

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

link-inputs: ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2022.parquet ../input/sales_transaction_panel_2023.parquet

include ../../generic.make
