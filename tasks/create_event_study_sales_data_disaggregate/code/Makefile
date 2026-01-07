SHELL := bash
include ../../shell_functions.make

../output/sales_transaction_panel.parquet: create_transaction_panel.R \
    ../input/sales_with_ward_distances.csv \
    ../input/residential_improvements_panel.parquet \
    ../input/block_treatment_panel.csv \
    ../input/census_blocks_2010.csv | ../output
	@echo "→ [create_event_study_sales_data_disaggregate] Creating transaction-level panel..."
	$(R) $<
	@echo "→ [create_event_study_sales_data_disaggregate] Done"

include ../../generic.make
