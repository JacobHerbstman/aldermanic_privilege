SHELL := bash
include ../../shell_functions.make

all: ../output/rental_sumstats_table.tex

../output/rental_sumstats_table.tex: create_appendix_a.R ../input/rent_with_ward_distances_full.parquet | ../output
	$(R) create_appendix_a.R

../input/rent_with_ward_distances_full.parquet: ../../calculate_rent_distances/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/rent_with_ward_distances_full.parquet
