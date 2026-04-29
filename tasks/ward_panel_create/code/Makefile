SHELL := bash
include ../../shell_functions.make

all: ../output/ward_panel.gpkg

../output/ward_panel.gpkg: make_ward_panel.R ../input/ward1998.shp ../input/ward1998.dbf ../input/ward1998.shx ../input/ward1998.prj ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson | ../output
	$(R) make_ward_panel.R

../input/ward1998.shp: ../../../data_raw/ward1998.shp | ../input
	ln -sf $< $@

../input/ward1998.dbf: ../../../data_raw/ward1998.dbf | ../input
	ln -sf $< $@

../input/ward1998.shx: ../../../data_raw/ward1998.shx | ../input
	ln -sf $< $@

../input/ward1998.prj: ../../../data_raw/ward1998.prj | ../input
	ln -sf $< $@

../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@

../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	ln -sf $< $@

../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/ward1998.shp ../input/ward1998.dbf ../input/ward1998.shx ../input/ward1998.prj ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson
