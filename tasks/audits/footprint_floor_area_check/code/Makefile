include ../../../shared/code/shell_functions.make

all: ../output/footprint_floor_area_buildings.csv ../output/footprint_floor_area_summary.csv

../%/footprint_floor_area_buildings.csv ../%/footprint_floor_area_summary.csv: compare_footprint_floor_area.R \
	../input/permit_construction.csv ../input/parcel_centroids.csv ../input/building_footprints_2022_chicago.gpkg | ../output ../report
	$(R) $<

../input/permit_construction.csv: ../../../prepare_permit_construction/output/permit_construction.csv | ../input
	ln -sf $< $@
../input/parcel_centroids.csv: ../../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@
../input/building_footprints_2022_chicago.gpkg: ../../../download_building_footprints_2022/output/building_footprints_2022_chicago.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/permit_construction.csv ../input/parcel_centroids.csv ../input/building_footprints_2022_chicago.gpkg

include ../../../shared/code/generic.make
