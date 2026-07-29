PARCEL_ADDRESSES_DATASET_ID ?= 3723-97qp
PARCEL_YEAR ?= 2025
PARCEL_CITY ?= CHICAGO
MIN_EXPECTED_ROWS ?= 800000
BATCH_SIZE ?= 500000
ORDER_BY ?= pin
MAX_BATCHES ?= 0

all: ../output/parcel_addresses_$(PARCEL_YEAR)_chicago.csv

../output/parcel_addresses_$(PARCEL_YEAR)_chicago.csv: download_parcel_addresses.py | ../output run.sbatch
	$(PYTHON) $< \
		--dataset-id $(PARCEL_ADDRESSES_DATASET_ID) \
		--year $(PARCEL_YEAR) \
		--city $(PARCEL_CITY) \
		--min-expected-rows $(MIN_EXPECTED_ROWS) \
		--batch-size $(BATCH_SIZE) \
		--order-by $(ORDER_BY) \
		--max-batches $(MAX_BATCHES)

link-inputs:

include ../../shell_functions.make
include ../../generic.make
