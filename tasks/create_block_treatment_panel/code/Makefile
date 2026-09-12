include ../../shared/code/shell_functions.make

all: ../output/block_treatment_pre_scores.csv

../output/block_treatment_pre_scores.csv: create_treatment_panel.R ../../shared/code/canonical_geometry_helpers.R ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../../setup_environment/code/packages.R Makefile ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../input/census_blocks_2010.csv: ../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

link-inputs: ../input/census_blocks_2010.csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv

include ../../shared/code/generic.make
