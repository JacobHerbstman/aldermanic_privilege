include ../../generic.make

# Input symlinks
../input/sales_transaction_panel.parquet:
	ln -sf ../../create_event_study_sales_data_disaggregate/output/sales_transaction_panel.parquet $@

../input/block_treatment_panel.csv:
	ln -sf ../../create_block_treatment_panel/output/block_treatment_panel.csv $@

../input/census_blocks_2010.csv:
	ln -sf ../../census_block_2020_cleaning/input/census_blocks_2010.csv $@

# Outputs
OUTPUTS = ../output/ward_pair_stability_summary.txt \
          ../output/ward_pair_stability_by_block.csv \
          ../output/unstable_blocks_map.pdf \
          ../output/fe_cell_balance.csv

# Run diagnostic script
$(OUTPUTS): ../input/sales_transaction_panel.parquet ../input/block_treatment_panel.csv ../input/census_blocks_2010.csv diagnose_ward_pair_stability.R
	Rscript diagnose_ward_pair_stability.R

all: $(OUTPUTS)

.PHONY: all
