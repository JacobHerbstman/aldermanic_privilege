REZONING_DATE_TAG ?= 20101101_20201231
PARCEL_YEAR ?= 2025
NEAREST_MAX_DIFF ?= 50
MAX_ADDRESS_RANGE_SPAN ?= 500

all: ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv

../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv: stage1_parcel_geocode.py ../input/zoning_matters_far_$(REZONING_DATE_TAG).csv ../input/parcel_addresses_$(PARCEL_YEAR)_chicago.csv ../input/parcel_universe_2025_city.csv | ../output run.sbatch
	$(PYTHON) $< $(REZONING_DATE_TAG) $(PARCEL_YEAR) $(NEAREST_MAX_DIFF) $(MAX_ADDRESS_RANGE_SPAN)

../output/rezoning_geocode_stage1_unmatched_$(REZONING_DATE_TAG).csv: ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv

../output/rezoning_parcel_matches_$(REZONING_DATE_TAG).csv: ../output/rezoning_geocode_stage1_$(REZONING_DATE_TAG).csv

../input/zoning_matters_far_$(REZONING_DATE_TAG).csv: ../../rezoning_far_pre_geocode/output/zoning_matters_far_$(REZONING_DATE_TAG).csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_addresses_$(PARCEL_YEAR)_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_$(PARCEL_YEAR)_chicago.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/zoning_matters_far_$(REZONING_DATE_TAG).csv ../input/parcel_addresses_$(PARCEL_YEAR)_chicago.csv ../input/parcel_universe_2025_city.csv

include ../../shell_functions.make
include ../../generic.make
