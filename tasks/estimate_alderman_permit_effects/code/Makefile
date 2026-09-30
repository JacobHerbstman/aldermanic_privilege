include ../../shared/code/shell_functions.make

all: ../output/block_alderman_year_panel.parquet ../output/alderman_permit_effects.csv \
	../output/alderman_permit_effects_split_samples.csv

../output/block_alderman_year_panel.parquet: build_block_alderman_panel.R ../input/census_blocks_2010.csv \
	../input/building_permits_clean.gpkg ../input/manual_permit_block_assignments.csv \
	../input/block_treatment_pre_scores.csv ../input/chicago_alderman_terms.csv | ../output ../report
	$(R) $<

../%/alderman_permit_effects.csv ../%/alderman_permit_effects_split_samples.csv: estimate_alderman_permit_effects.R \
	../output/block_alderman_year_panel.parquet | ../output ../report
	$(R) $<

../input/census_blocks_2010.csv: ../../download_chicago_spatial_data/output/census_blocks_2010.csv | ../input
	ln -sf $< $@
../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean_2006_2022.gpkg | ../input
	ln -sf $< $@
../input/manual_permit_block_assignments.csv: ../../permit_block_hand_adjudications/output/manual_permit_block_assignments.csv | ../input
	ln -sf $< $@
../input/block_treatment_pre_scores.csv: ../../create_block_treatment_panel/output/block_treatment_pre_scores.csv | ../input
	ln -sf $< $@
../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	ln -sf $< $@

link-inputs: ../input/census_blocks_2010.csv ../input/building_permits_clean.gpkg \
	../input/manual_permit_block_assignments.csv ../input/block_treatment_pre_scores.csv \
	../input/chicago_alderman_terms.csv

include ../../shared/code/generic.make
