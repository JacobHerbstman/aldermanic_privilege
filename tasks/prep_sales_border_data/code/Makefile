include ../../shared/code/shell_functions.make

DROP_INCONSISTENT_ROOMS ?= TRUE

all: ../output/sales_with_hedonics.parquet

../output/sales_with_hedonics.parquet: prep_sales_data.R Makefile ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< $(DROP_INCONSISTENT_ROOMS)

../input/sales_with_ward_distances.csv: ../../merge_sales_scores/output/sales_with_ward_distances_through2022.csv | ../input
	ln -sf $< $@

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet

include ../../shared/code/generic.make
