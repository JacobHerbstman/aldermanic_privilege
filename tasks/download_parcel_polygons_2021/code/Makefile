include ../../shared/code/shell_functions.make

all: ../output/parcel_lot_areas_2021.csv

../output/parcel_lot_areas_2021.csv: download_parcel_polygons.R | ../output ../report ../temp
	$(R) $<

include ../../shared/code/generic.make
