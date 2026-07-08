SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3
PARCEL_ADDRESSES_DATASET_ID ?= 3723-97qp
PARCEL_UNIVERSE_DATASET_ID ?= pabr-t5kh
MIN_EXPECTED_ROWS ?= 1000000
BATCH_SIZE ?= 500000
ORDER_BY ?= pin
MAX_BATCHES ?= 0
EXTRA_ARGS ?=

all: ../output/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv ../output/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv

../output/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv: download_parcel_addresses.py | ../output run.sbatch
	$(PYTHON) download_parcel_addresses.py \
		--dataset-id $(PARCEL_ADDRESSES_DATASET_ID) \
		--min-expected-rows $(MIN_EXPECTED_ROWS) \
		--batch-size $(BATCH_SIZE) \
		--order-by $(ORDER_BY) \
		--max-batches $(MAX_BATCHES) \
		$(EXTRA_ARGS) \
		--out-csv ../output/parcel_addresses_$(PARCEL_ADDRESSES_DATASET_ID).csv

../output/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv: download_parcel_addresses.py | ../output run.sbatch
	$(PYTHON) download_parcel_addresses.py \
		--dataset-id $(PARCEL_UNIVERSE_DATASET_ID) \
		--min-expected-rows $(MIN_EXPECTED_ROWS) \
		--batch-size $(BATCH_SIZE) \
		--order-by $(ORDER_BY) \
		--max-batches $(MAX_BATCHES) \
		$(EXTRA_ARGS) \
		--out-csv ../output/parcel_universe_$(PARCEL_UNIVERSE_DATASET_ID).csv

test: test_download_parcel_addresses.py download_parcel_addresses.py
	$(PYTHON) -m unittest test_download_parcel_addresses.py
