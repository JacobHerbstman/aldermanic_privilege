include ../../shared/code/shell_functions.make

START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/ward_controls_$(START_YEAR)_$(END_YEAR).csv

../output/ward_controls_$(START_YEAR)_$(END_YEAR).csv: create_ward_controls.R ../output/census_2000.gpkg ../output/census_2010.csv ../output/census_acs.csv ../output/census_block_groups.gpkg ../input/ward_panel.gpkg Makefile | ../output ../report
	$(R) $< $(START_YEAR) $(END_YEAR)

../output/census_2000.gpkg: ../sources/census_2000.gpkg.gz | ../output ../temp
	gzip -dc $< > ../temp/census_2000.gpkg
	mv ../temp/census_2000.gpkg $@

../output/census_2010.csv: ../sources/census_2010.csv.gz | ../output ../temp
	gzip -dc $< > ../temp/census_2010.csv
	mv ../temp/census_2010.csv $@

../output/census_acs.csv: ../sources/census_acs.csv.gz | ../output ../temp
	gzip -dc $< > ../temp/census_acs.csv
	mv ../temp/census_acs.csv $@

../output/census_block_groups.gpkg: ../sources/census_block_groups.gpkg.gz | ../output ../temp
	gzip -dc $< > ../temp/census_block_groups.gpkg
	mv ../temp/census_block_groups.gpkg $@

download-current: ../output/census_2000_current.gpkg ../output/census_2010_current.csv ../output/census_acs_current.csv ../output/census_block_groups_current.gpkg

../%/census_2000_current.gpkg \
	../%/census_2010_current.csv \
	../%/census_acs_current.csv \
	../%/census_block_groups_current.gpkg: download_census_controls.R Makefile | ../output ../temp ../report
	$(R) $< $(START_YEAR) $(END_YEAR)

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@


link-inputs: ../input/ward_panel.gpkg

include ../../shared/code/generic.make
