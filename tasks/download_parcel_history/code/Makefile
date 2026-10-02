include ../../shared/code/shell_functions.make

all: ../output/parcel_history.gpkg

../output/parcel_history.gpkg: download_parcel_history.R ../input/parcel_centroids.csv | ../output ../report ../temp
	$(R) $<

../input/parcel_centroids.csv: ../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@

link-inputs: ../input/parcel_centroids.csv

include ../../shared/code/generic.make
