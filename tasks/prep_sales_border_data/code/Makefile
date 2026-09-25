include ../../shared/code/shell_functions.make

all: ../output/sales_with_hedonics.parquet ../output/sales_hedonics_sample_flow.csv

../%/sales_with_hedonics.parquet ../%/sales_hedonics_sample_flow.csv: prep_sales_data.R \
	../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet | ../output ../report
	$(R) $<

../input/sales_with_ward_distances.csv: ../../merge_sales_scores/output/sales_with_ward_distances_through2022.csv | ../input
	ln -sf $< $@

../input/residential_improvements_panel.parquet: ../../process_residential_improvements_full/output/residential_improvements_panel.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/sales_with_ward_distances.csv ../input/residential_improvements_panel.parquet

include ../../shared/code/generic.make
