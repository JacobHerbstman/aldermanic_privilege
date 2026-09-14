all: ../output/official_building_footprints_2008.gpkg \
	../output/official_building_footprints_2022.gpkg

../output/official_building_footprints_2008.gpkg: ../input/official_building_footprints_2008.gpkg | ../output
	cp $< $@

../output/official_building_footprints_2022.gpkg: ../input/official_building_footprints_2022.gpkg | ../output
	cp $< $@

../input/official_building_footprints_2008.gpkg: ../../../data_raw/construction_review/official_building_footprints_2008.gpkg | ../input
	ln -sf $< $@

../input/official_building_footprints_2022.gpkg: ../../../data_raw/construction_review/official_building_footprints_2022.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/official_building_footprints_2008.gpkg ../input/official_building_footprints_2022.gpkg

include ../../shared/code/generic.make
