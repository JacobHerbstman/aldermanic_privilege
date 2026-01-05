# Makefile for create_appendix_a_sumstats task

OUTPUT = output/rental_sumstats_table.tex
CODE = code/create_appendix_a.R
INPUT = input/rent_with_ward_distances_full.parquet

$(OUTPUT): $(CODE) $(INPUT)
	cd code && Rscript create_appendix_a.R

# Symlinks
input/rent_with_ward_distances_full.parquet: ../calculate_rent_distances/output/rent_with_ward_distances_full.parquet
	mkdir -p input
	ln -sf ../../calculate_rent_distances/output/rent_with_ward_distances_full.parquet $@

.PHONY: clean
clean:
	rm -f $(OUTPUT)
