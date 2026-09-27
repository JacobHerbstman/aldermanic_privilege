include ../../shared/code/shell_functions.make

all: ../output/boundary_cell_jumps.csv ../output/boundary_jump_heterogeneity.csv

../%/boundary_cell_jumps.csv ../%/boundary_jump_heterogeneity.csv: test_boundary_jumps.R \
	../input/new_construction_analysis_data.csv ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet ../input/price_boundary_estimates.csv | ../output ../report
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/rental_rd_characteristics_panel_bw1500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@
../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@
../input/price_boundary_estimates.csv: ../../price_boundary_results/output/price_boundary_estimates.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet ../input/price_boundary_estimates.csv

include ../../shared/code/generic.make
