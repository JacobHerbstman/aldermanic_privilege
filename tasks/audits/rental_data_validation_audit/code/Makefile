SHELL := bash

all: \
	../output/rental_data_validation_growth.pdf \
	../output/rental_data_validation_annual_growth.pdf \
	../output/rental_data_validation_growth.tex \
	../output/rental_data_validation_growth_series.csv \
	../output/rental_data_validation_source_status.csv

../output/rental_data_validation_growth_series.csv: build_rental_data_validation_audit.R ../input/chicago_rent_panel.parquet | ../output
	$(R) $<

../output/rental_data_validation_growth.pdf \
../output/rental_data_validation_annual_growth.pdf \
../output/rental_data_validation_growth.tex \
../output/rental_data_validation_source_status.csv: ../output/rental_data_validation_growth_series.csv
	@test -s "$@"

../input/chicago_rent_panel.parquet: ../../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/chicago_rent_panel.parquet

include ../../../shell_functions.make
include ../../../generic.make
