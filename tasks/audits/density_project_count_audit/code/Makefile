all: \
	../output/project_count_models.csv \
	../output/project_counts_by_band.csv \
	../output/project_count_by_band.pdf

../output/project_count_models.csv: build_project_count_audit.R ../input/new_construction_analysis_data.csv ../input/boundary_segments_1320ft.gpkg | ../output
	$(R) $<

../output/project_counts_by_band.csv ../output/project_count_by_band.pdf: ../output/project_count_models.csv
	@test -s "$@"

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_1320ft.gpkg: ../../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/boundary_segments_1320ft.gpkg

include ../../../shell_functions.make
include ../../../generic.make
