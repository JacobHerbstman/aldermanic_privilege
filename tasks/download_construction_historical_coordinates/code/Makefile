include ../../shared/code/shell_functions.make

HISTORY_START_YEAR ?= 1999
HISTORY_END_YEAR ?= 2025

all: ../output/density_historical_parcel_records.csv ../output/density_historical_address_records.csv

../output/density_historical_parcel_records.csv: download_historical_parcel_records.R ../../setup_environment/code/packages.R ../output/historical_parcel_requests.csv Makefile | ../output
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR)

../output/density_historical_address_records.csv: download_historical_address_records.R ../../setup_environment/code/packages.R ../output/historical_parcel_requests.csv Makefile | ../output
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR)

../output/historical_parcel_requests.csv: prepare_historical_parcel_requests.R ../../setup_environment/code/packages.R ../input/density_historical_building_universe.csv | ../output
	$(R) $<

../input/density_historical_building_universe.csv: ../../new_construction_cleaning/output/density_historical_building_universe.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/density_historical_building_universe.csv

include ../../shared/code/generic.make
