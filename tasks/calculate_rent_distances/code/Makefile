SHELL := bash
include ../../shell_functions.make

SAMPLE := FALSE

all: ../output/rent_with_ward_distances_full.parquet

../output/rent_with_ward_distances_full.parquet: calculate_rent_distances.R ../input/chicago_rent_panel.parquet ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/ward_controls.csv | ../output
	$(R) calculate_rent_distances.R --sample=$(SAMPLE)

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/alderman_restrictiveness_scores_month_FEs.csv: ../../create_alderman_strictness_scores/output/alderman_restrictiveness_scores_month_FEs.csv | ../input
	ln -sf $< $@

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/chicago_rent_panel.parquet ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/alderman_restrictiveness_scores_month_FEs.csv ../input/ward_controls.csv