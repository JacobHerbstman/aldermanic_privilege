SHELL := bash
include ../../shell_functions.make

all: ../output/sales_transaction_panel.parquet

../output/sales_transaction_panel.parquet: create_transaction_panel.R ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv | ../output
	$(R) $<

../input/sales_with_ward_distances.csv: ../../calculate_sale_distances/output/sales_with_ward_distances.csv | ../input
	ln -sf $< $@

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf $< $@

../input/block_treatment_panel.csv: ../../create_block_treatment_panel/output/block_treatment_panel.csv | ../input
	ln -sf $< $@

../input/census_blocks_2010.csv: ../../../data_raw/CensusBlockTIGER2010_20250721.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv
