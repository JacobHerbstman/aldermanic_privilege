all: ../output/rental_sumstats_table.tex

../output/rental_sumstats_table.tex: create_appendix_a.R ../input/rent_with_ward_distances_full.parquet | ../output
	Rscript $<

../input/rent_with_ward_distances_full.parquet: ../../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances_full.parquet

include ../../../generic.make
