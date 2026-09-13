include ../../shared/code/shell_functions.make

DROP_INCONSISTENT_ROOMS ?= TRUE

all: ../output/sales_with_hedonics.parquet

../output/sales_with_hedonics.parquet: prep_sales_data.R Makefile ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet ../input/packages.R ../input/save_data.R ../input/report.py | ../output ../report
	$(R) $< $(DROP_INCONSISTENT_ROOMS)

../input/sales_with_ward_distances.csv: ../../merge_sales_scores/output/sales_with_ward_distances_through2022.csv | ../input
	ln -sf $< $@

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf $< $@

../input/packages.R: ../../setup_environment/code/packages.R | ../input
	ln -sf $< $@

../input/save_data.R: ../../shared/code/save_data.R | ../input
	ln -sf $< $@

../input/report.py: ../../shared/code/report.py | ../input
	ln -sf $< $@

link-inputs: ../input/packages.R ../input/save_data.R ../input/report.py ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet

include ../../shared/code/generic.make
