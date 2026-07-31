START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: ../output/renthub_manifest.csv

../output/renthub_manifest.csv: download_rent_data.R Makefile $(wildcard ../output/*.parquet) | ../output
	$(R) $< $(START_DATE) $(END_DATE)

include ../../shell_functions.make
include ../../generic.make
