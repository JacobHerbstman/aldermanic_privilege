SHELL := bash

all: ../output/permit_balance_full.csv ../output/sales_balance_full.csv ../output/permit_balance_summary.csv ../output/sales_balance_summary.csv ../output/permit_balance_flags.csv ../output/sales_balance_flags.csv ../output/event_study_balance_memo.md

../output/.balance_audit_complete: run_event_study_balance_audit.R ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet ../input/block_group_controls.csv | ../output
	$(R) run_event_study_balance_audit.R
	touch $@

../output/permit_balance_full.csv ../output/sales_balance_full.csv ../output/permit_balance_summary.csv ../output/sales_balance_summary.csv ../output/permit_balance_flags.csv ../output/sales_balance_flags.csv ../output/event_study_balance_memo.md: ../output/.balance_audit_complete

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/block_group_controls.csv: ../../../create_block_group_controls/output/block_group_controls.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/sales_transaction_panel_2015.parquet ../input/block_group_controls.csv

include ../../../shell_functions.make
include ../../../generic.make
