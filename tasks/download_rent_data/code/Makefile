SHELL := bash

all: \
	../output/download.done \
	../output/renthub_file_manifest.csv \
	../output/renthub_download_metadata.csv \
	../output/renthub_download_file_audit.csv \
	../output/renthub_schema_audit.csv

../output/download.done: download_rent_data.R | ../output
	$(R) $<

../output/renthub_file_manifest.csv \
../output/renthub_download_metadata.csv \
../output/renthub_download_file_audit.csv \
../output/renthub_schema_audit.csv: ../output/download.done
	@test -s "$@"

../output/%.parquet: ../output/download.done
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
