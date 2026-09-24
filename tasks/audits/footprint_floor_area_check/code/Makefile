include ../../../shared/code/shell_functions.make

MIN_HEIGHT_FT := 15
LAST_CONSTRUCTION_YEAR := 2021
SIZE_BREAKS := 20 50 100

all: ../output/footprint_floor_area_buildings.csv ../output/footprint_floor_area_summary.csv

../%/footprint_floor_area_buildings.csv ../%/footprint_floor_area_summary.csv: compare_footprint_floor_area.R Makefile \
	../input/permit_construction.csv ../input/parcel_centroids.csv ../input/building_footprints_2022_chicago.gpkg \
	../../../setup_environment/code/packages.R ../../../shared/code/save_data.R | ../output ../report
	$(R) $< $(MIN_HEIGHT_FT) $(LAST_CONSTRUCTION_YEAR) "$(SIZE_BREAKS)"

../input/permit_construction.csv: ../../../prepare_permit_construction/output/permit_construction.csv | ../input
	ln -sf $< $@
../input/parcel_centroids.csv: ../../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@
../input/building_footprints_2022_chicago.gpkg: ../../../download_building_footprints_2022/output/building_footprints_2022_chicago.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/permit_construction.csv ../input/parcel_centroids.csv ../input/building_footprints_2022_chicago.gpkg

include ../../../shared/code/generic.make
