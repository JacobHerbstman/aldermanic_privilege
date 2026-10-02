include ../../shared/code/shell_functions.make

all: ../output/block_treatment_pre_scores.csv

../output/block_treatment_pre_scores.csv: create_treatment_panel.R ../input/census_blocks_2010.csv ../input/ward_panel.gpkg Makefile | ../output ../report
	$(R) $<

../input/census_blocks_2010.csv: ../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/census_blocks_2010.csv ../input/ward_panel.gpkg

include ../../shared/code/generic.make
