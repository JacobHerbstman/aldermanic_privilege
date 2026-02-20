SHELL := bash
include ../../shell_functions.make

SAMPLE := FALSE

all: ../output/rent_pre_scores_full.parquet

../output/rent_pre_scores_full.parquet: calculate_rent_distances.R ../input/chicago_rent_panel.parquet ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	# arg order: sample
	$(R) calculate_rent_distances.R $(SAMPLE)

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/chicago_rent_panel.parquet ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv
