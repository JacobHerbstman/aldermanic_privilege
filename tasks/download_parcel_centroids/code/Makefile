include ../../shared/code/shell_functions.make

FIRST_YEAR := 1999
LAST_YEAR := 2025

all: ../output/parcel_centroids.csv

../output/parcel_centroids.csv: download_parcel_centroids.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R | ../output ../report
	$(R) $< $(FIRST_YEAR) $(LAST_YEAR)

include ../../shared/code/generic.make
