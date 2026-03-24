SHELL := bash
include ../../shell_functions.make

all: \
	../output/event_study_sales_permit_style_block_year_cohort_2012_main.pdf \
	../output/event_study_sales_permit_style_block_year_cohort_2015_main.pdf \
	../output/event_study_sales_permit_style_block_year_stacked_announcement_main.pdf \
	../output/event_study_sales_permit_style_block_year_stacked_implementation_main.pdf \
	../output/event_study_sales_permit_style_transaction_cohort_2012_no_hedonics.pdf \
	../output/event_study_sales_permit_style_transaction_cohort_2015_no_hedonics.pdf \
	../output/event_study_sales_permit_style_transaction_stacked_announcement_no_hedonics.pdf \
	../output/event_study_sales_permit_style_transaction_stacked_implementation_no_hedonics.pdf \
	../output/event_study_sales_permit_style_transaction_cohort_2012_with_hedonics.pdf \
	../output/event_study_sales_permit_style_transaction_cohort_2015_with_hedonics.pdf \
	../output/event_study_sales_permit_style_transaction_stacked_announcement_with_hedonics.pdf \
	../output/event_study_sales_permit_style_transaction_stacked_implementation_with_hedonics.pdf \
	../output/did_sales_permit_style_block_year_implementation.csv \
	../output/sales_permit_style_comparison.csv

../output/did_sales_permit_style_block_year_implementation.csv \
../output/did_sales_permit_style_block_year_implementation.tex: run_did_sales_permit_style.R ../input/sales_block_year_panel.csv | ../output
	Rscript run_did_sales_permit_style.R

../output/event_study_sales_permit_style_block_year_cohort_2012_main.pdf: run_event_study_sales_permit_style.R ../input/sales_block_year_panel.csv | ../output
	Rscript run_event_study_sales_permit_style.R block_year cohort_2012 main

../output/event_study_support_sales_permit_style_block_year_cohort_2012_main.csv \
../output/event_study_pretrend_sales_permit_style_block_year_cohort_2012_main.csv \
../output/event_study_metadata_sales_permit_style_block_year_cohort_2012_main.csv: ../output/event_study_sales_permit_style_block_year_cohort_2012_main.pdf
	test -f "$@"

../output/event_study_sales_permit_style_block_year_cohort_2015_main.pdf: run_event_study_sales_permit_style.R ../input/sales_block_year_panel.csv | ../output
	Rscript run_event_study_sales_permit_style.R block_year cohort_2015 main

../output/event_study_support_sales_permit_style_block_year_cohort_2015_main.csv \
../output/event_study_pretrend_sales_permit_style_block_year_cohort_2015_main.csv \
../output/event_study_metadata_sales_permit_style_block_year_cohort_2015_main.csv: ../output/event_study_sales_permit_style_block_year_cohort_2015_main.pdf
	test -f "$@"

../output/event_study_sales_permit_style_block_year_stacked_announcement_main.pdf: run_event_study_sales_permit_style.R ../input/sales_block_year_panel.csv | ../output
	Rscript run_event_study_sales_permit_style.R block_year stacked_announcement main

../output/event_study_support_sales_permit_style_block_year_stacked_announcement_main.csv \
../output/event_study_pretrend_sales_permit_style_block_year_stacked_announcement_main.csv \
../output/event_study_metadata_sales_permit_style_block_year_stacked_announcement_main.csv: ../output/event_study_sales_permit_style_block_year_stacked_announcement_main.pdf
	test -f "$@"

../output/event_study_sales_permit_style_block_year_stacked_implementation_main.pdf: run_event_study_sales_permit_style.R ../input/sales_block_year_panel.csv | ../output
	Rscript run_event_study_sales_permit_style.R block_year stacked_implementation main

../output/event_study_support_sales_permit_style_block_year_stacked_implementation_main.csv \
../output/event_study_pretrend_sales_permit_style_block_year_stacked_implementation_main.csv \
../output/event_study_metadata_sales_permit_style_block_year_stacked_implementation_main.csv: ../output/event_study_sales_permit_style_block_year_stacked_implementation_main.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_cohort_2012_no_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel_2012.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction cohort_2012 no_hedonics

../output/event_study_support_sales_permit_style_transaction_cohort_2012_no_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_cohort_2012_no_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_cohort_2012_no_hedonics.csv: ../output/event_study_sales_permit_style_transaction_cohort_2012_no_hedonics.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_cohort_2015_no_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction cohort_2015 no_hedonics

../output/event_study_support_sales_permit_style_transaction_cohort_2015_no_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_cohort_2015_no_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_cohort_2015_no_hedonics.csv: ../output/event_study_sales_permit_style_transaction_cohort_2015_no_hedonics.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_stacked_announcement_no_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction stacked_announcement no_hedonics

../output/event_study_support_sales_permit_style_transaction_stacked_announcement_no_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_stacked_announcement_no_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_stacked_announcement_no_hedonics.csv: ../output/event_study_sales_permit_style_transaction_stacked_announcement_no_hedonics.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_stacked_implementation_no_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction stacked_implementation no_hedonics

../output/event_study_support_sales_permit_style_transaction_stacked_implementation_no_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_stacked_implementation_no_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_stacked_implementation_no_hedonics.csv: ../output/event_study_sales_permit_style_transaction_stacked_implementation_no_hedonics.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_cohort_2012_with_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel_2012.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction cohort_2012 with_hedonics

../output/event_study_support_sales_permit_style_transaction_cohort_2012_with_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_cohort_2012_with_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_cohort_2012_with_hedonics.csv: ../output/event_study_sales_permit_style_transaction_cohort_2012_with_hedonics.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_cohort_2015_with_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel_2015.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction cohort_2015 with_hedonics

../output/event_study_support_sales_permit_style_transaction_cohort_2015_with_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_cohort_2015_with_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_cohort_2015_with_hedonics.csv: ../output/event_study_sales_permit_style_transaction_cohort_2015_with_hedonics.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_stacked_announcement_with_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel_announcement.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction stacked_announcement with_hedonics

../output/event_study_support_sales_permit_style_transaction_stacked_announcement_with_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_stacked_announcement_with_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_stacked_announcement_with_hedonics.csv: ../output/event_study_sales_permit_style_transaction_stacked_announcement_with_hedonics.pdf
	test -f "$@"

../output/event_study_sales_permit_style_transaction_stacked_implementation_with_hedonics.pdf: run_event_study_sales_permit_style.R ../input/sales_transaction_panel.parquet | ../output
	Rscript run_event_study_sales_permit_style.R transaction stacked_implementation with_hedonics

../output/event_study_support_sales_permit_style_transaction_stacked_implementation_with_hedonics.csv \
../output/event_study_pretrend_sales_permit_style_transaction_stacked_implementation_with_hedonics.csv \
../output/event_study_metadata_sales_permit_style_transaction_stacked_implementation_with_hedonics.csv: ../output/event_study_sales_permit_style_transaction_stacked_implementation_with_hedonics.pdf
	test -f "$@"

../output/sales_permit_style_comparison.csv: build_sales_permit_style_comparison.R \
	../output/event_study_pretrend_sales_permit_style_block_year_cohort_2012_main.csv \
	../output/event_study_metadata_sales_permit_style_block_year_cohort_2012_main.csv \
	../output/event_study_pretrend_sales_permit_style_block_year_cohort_2015_main.csv \
	../output/event_study_metadata_sales_permit_style_block_year_cohort_2015_main.csv \
	../output/event_study_pretrend_sales_permit_style_block_year_stacked_announcement_main.csv \
	../output/event_study_metadata_sales_permit_style_block_year_stacked_announcement_main.csv \
	../output/event_study_pretrend_sales_permit_style_block_year_stacked_implementation_main.csv \
	../output/event_study_metadata_sales_permit_style_block_year_stacked_implementation_main.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_cohort_2012_no_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_cohort_2012_no_hedonics.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_cohort_2015_no_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_cohort_2015_no_hedonics.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_stacked_announcement_no_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_stacked_announcement_no_hedonics.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_stacked_implementation_no_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_stacked_implementation_no_hedonics.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_cohort_2012_with_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_cohort_2012_with_hedonics.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_cohort_2015_with_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_cohort_2015_with_hedonics.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_stacked_announcement_with_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_stacked_announcement_with_hedonics.csv \
	../output/event_study_pretrend_sales_permit_style_transaction_stacked_implementation_with_hedonics.csv \
	../output/event_study_metadata_sales_permit_style_transaction_stacked_implementation_with_hedonics.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv | ../output
	Rscript build_sales_permit_style_comparison.R

../input/sales_block_year_panel.csv: ../../create_event_study_sales_data/output/sales_block_year_panel.csv | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_announcement.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_announcement.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2012.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2012.parquet | ../input
	ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv: ../../run_event_study_sales_disaggregate/output/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/sales_block_year_panel.csv ../input/sales_transaction_panel_announcement.parquet ../input/sales_transaction_panel.parquet ../input/sales_transaction_panel_2012.parquet ../input/sales_transaction_panel_2015.parquet \
	../input/event_study_metadata_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2012_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_cohort_2015_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_announcement_continuous_triangular_1000ft_full.csv \
	../input/event_study_metadata_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv \
	../input/event_study_pretrend_disaggregate_yearly_stacked_implementation_continuous_triangular_1000ft_full.csv
