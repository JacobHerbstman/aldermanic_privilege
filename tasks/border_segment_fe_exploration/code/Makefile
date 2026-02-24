SHELL := bash
include ../../shell_functions.make

OUTPUTS := \
	../output/segment_assignment_coverage_sales.csv \
	../output/segment_assignment_coverage_rental.csv \
	../output/segment_fe_results_sales_all.csv \
	../output/segment_fe_results_sales_pruned.csv \
	../output/segment_fe_results_rental_all.csv \
	../output/segment_fe_results_rental_pruned.csv \
	../output/segment_fe_table_sales_all.tex \
	../output/segment_fe_table_sales_pruned.tex \
	../output/segment_fe_table_rental_all.tex \
	../output/segment_fe_table_rental_pruned.tex \
	../output/segment_fe_comparison_summary.md

INPUTS := \
	../input/sales_with_hedonics.parquet \
	../input/rent_with_ward_distances_full.parquet \
	../input/boundary_segments_1320ft.gpkg \
	../input/confounded_pair_era_flags.csv

all: $(OUTPUTS)

$(OUTPUTS): run_segment_fe_exploration.R $(INPUTS) | ../output
	Rscript run_segment_fe_exploration.R

../input/sales_with_hedonics.parquet: ../../prep_sales_border_data/output/sales_with_hedonics.parquet | ../input
	ln -sf "$<" "$@"

../input/rent_with_ward_distances_full.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUTS)

include ../../generic.make
