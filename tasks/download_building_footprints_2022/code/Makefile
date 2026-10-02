include ../../shared/code/shell_functions.make

all: ../output/building_footprints_2022_chicago.gpkg

../output/building_footprints_2022_chicago.gpkg: download_building_footprints_2022.R ../input/parcel_centroids.csv | ../output ../report
	$(R) $<

../input/parcel_centroids.csv: ../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@

link-inputs: ../input/parcel_centroids.csv

include ../../shared/code/generic.make
