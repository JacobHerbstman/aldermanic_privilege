include ../../shared/code/shell_functions.make

TILE_FT := 5280

all: ../output/building_footprints_2022_chicago.gpkg

../output/building_footprints_2022_chicago.gpkg: download_building_footprints_2022.R ../input/parcel_centroids.csv \
	../../setup_environment/code/packages.R ../../shared/code/save_data.R | ../output ../report
	$(R) $< $(TILE_FT)

../input/parcel_centroids.csv: ../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@

link-inputs: ../input/parcel_centroids.csv

include ../../shared/code/generic.make
