include ../../shared/code/shell_functions.make

all: ../output/parcel_centroids.csv

../output/parcel_centroids.csv: download_parcel_centroids.R | ../output ../report
	$(R) $<

include ../../shared/code/generic.make
