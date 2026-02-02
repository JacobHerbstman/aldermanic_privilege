SHELL := bash
include ../../shell_functions.make

all: ../output/permits_for_uncertainty_index.csv

../output/permits_for_uncertainty_index.csv: prepare_uncertainty_data.R \
	../input/ward_panel.gpkg \
	../input/building_permits_clean.gpkg \
	../input/chicago_alderman_panel.csv \
	../input/ward_controls.csv \
	../input/parcel_proximity.csv \
	../input/community_areas.geojson | ../output
	$(R) $<

# Symlinks to upstream data
../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf $< $@

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	ln -sf $< $@

../input/parcel_proximity.csv: ../../../data_raw/Assessor_-_Parcel_Proximity_20251007.csv | ../input
	ln -sf $< $@

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/ward_panel.gpkg ../input/building_permits_clean.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/parcel_proximity.csv ../input/community_areas.geojson

