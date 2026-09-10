include ../../shared/code/shell_functions.make

PARCEL_YEAR := 2025
TRIAD_NAME := City

all: ../output/parcel_universe_2025_city_native_current.csv

../output/parcel_universe_2025_city_native_current.csv: download_parcel_universe_data.sh download_recipes.make | ../temp ../output
	bash $< $(PARCEL_YEAR) $(TRIAD_NAME)

include ../../shared/code/generic.make
