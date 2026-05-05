SHELL := bash
include ../../shell_functions.make

all: ../output/did_table_combined.tex

../output/did_table_combined.tex ../output/did_table_sales_announcement.tex: combine_tables.R ../input/did_coefficients_rental.csv ../input/did_coefficients_sales.csv | ../output
	Rscript combine_tables.R

../input/did_coefficients_rental.csv: ../../run_event_study_rental_disaggregate/output/did_coefficients_rental.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/did_coefficients_sales.csv: ../../run_event_study_sales_disaggregate/output/did_coefficients_sales.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/did_coefficients_rental.csv ../input/did_coefficients_sales.csv

include ../../generic.make
