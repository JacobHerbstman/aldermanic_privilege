SHELL := bash

all: ../output/event_geometry_contract_summary.csv ../output/event_geometry_contract_flags.csv ../output/event_geometry_contract_memo.md

../output/event_geometry_contract_summary.csv: run_event_geometry_contract_audit.R ../../../_lib/canonical_geometry_helpers.R ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel_2023.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2023.parquet ../input/sales_transaction_panel_event_geometry_diagnostics.csv ../input/boundary_segments_1320ft.gpkg | ../output
	$(R) run_event_geometry_contract_audit.R

../output/event_geometry_contract_flags.csv: ../output/event_geometry_contract_summary.csv
	@test -s "$@"

../output/event_geometry_contract_memo.md: ../output/event_geometry_contract_summary.csv
	@test -s "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/permit_block_year_panel_2023.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2023.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2015.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2015.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_2023.parquet: ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel_2023.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_transaction_panel_event_geometry_diagnostics.csv: ../../../audits/sales_event_study_audit/output/sales_transaction_panel_event_geometry_diagnostics.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet ../input/permit_block_year_panel_2023.parquet ../input/sales_transaction_panel_2015.parquet ../input/sales_transaction_panel_2023.parquet ../input/sales_transaction_panel_event_geometry_diagnostics.csv ../input/boundary_segments_1320ft.gpkg

include ../../../shell_functions.make
include ../../../generic.make
