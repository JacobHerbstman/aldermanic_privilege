SHELL=bash
include ../../shell_functions.make
include ../../generic.make

.DEFAULT_GOAL := all

PYTHON ?= python3

all: ../output/old_zoning_bulk_density_1957_2004.csv

diagnostics: ../output/old_zoning_district_mentions_1957_2004.csv

link-inputs: ../input/chicago_zoning_code1957.txt ../input/zoning_conversion_2004_crosswalk.csv

../input/chicago_zoning_code1957.txt: ../../../data_raw/chicago_zoning_code1957.txt | ../input
	ln -sf "$<" "$@"

../input/zoning_conversion_2004_crosswalk.csv: ../../../data_raw/zoning_conversion_2004_crosswalk.csv | ../input
	ln -sf "$<" "$@"

../output/old_zoning_bulk_density_1957_2004.csv: build_old_zoning_lookup.py ../input/chicago_zoning_code1957.txt ../input/zoning_conversion_2004_crosswalk.csv | ../output run.sbatch
	$(PYTHON) build_old_zoning_lookup.py \
		--in-old-zoning-txt ../input/chicago_zoning_code1957.txt \
		--in-crosswalk-csv ../input/zoning_conversion_2004_crosswalk.csv \
		--out-bulk-csv ../output/old_zoning_bulk_density_1957_2004.csv

../output/old_zoning_district_mentions_1957_2004.csv: build_old_zoning_lookup.py ../input/chicago_zoning_code1957.txt ../input/zoning_conversion_2004_crosswalk.csv | ../output run.sbatch
	$(PYTHON) build_old_zoning_lookup.py \
		--in-old-zoning-txt ../input/chicago_zoning_code1957.txt \
		--in-crosswalk-csv ../input/zoning_conversion_2004_crosswalk.csv \
		--out-bulk-csv ../output/old_zoning_bulk_density_1957_2004.csv \
		--out-mentions-csv ../output/old_zoning_district_mentions_1957_2004.csv

test: test_build_old_zoning_lookup.py build_old_zoning_lookup.py
	$(PYTHON) -m unittest test_build_old_zoning_lookup.py
