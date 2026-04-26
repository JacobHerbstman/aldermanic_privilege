SHELL := bash
include ../../shell_functions.make

all: \
	../output/sales_balance_summary.csv \
	../output/sales_balance_summary.tex

../output/sales_balance_summary.csv: build_sales_balance.R ../input/sales_transaction_panel_2015.parquet ../input/block_group_controls.csv | ../output
	$(R) build_sales_balance.R ../input/sales_transaction_panel_2015.parquet ../input/block_group_controls.csv ../output/sales_balance_summary.csv ../output/sales_balance_summary.tex

../output/sales_balance_summary.tex: ../output/sales_balance_summary.csv
	test -f "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../create_block_group_controls/output/block_group_controls.csv | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/sales_transaction_panel_2015.parquet ../input/block_group_controls.csv

include ../../generic.make
