SHELL := bash
include ../../shell_functions.make

all: \
	../output/boundary_segments_1320ft.gpkg \
	../output/boundary_segments_2640ft.gpkg \
	../output/segment_classification.csv \
	../output/ward_pair_boundaries.gpkg \
	../output/ward_pair_boundary_summary.csv \
	../output/ward_pair_boundary_diagnostics.csv

../output/boundary_segments_1320ft.gpkg: build_boundary_segments.R ../../_lib/canonical_geometry_helpers.R ../input/canonical_boundary_segments_1320ft.gpkg ../input/canonical_boundary_segments_2640ft.gpkg ../input/canonical_segment_classification.csv ../input/canonical_ward_pair_boundaries.gpkg | ../output
	# optional env: REBUILD_MODE=snapshot|reuse|raw (default snapshot)
	Rscript build_boundary_segments.R

../output/boundary_segments_2640ft.gpkg \
../output/segment_classification.csv \
../output/ward_pair_boundaries.gpkg \
../output/ward_pair_boundary_summary.csv \
../output/ward_pair_boundary_diagnostics.csv: ../output/boundary_segments_1320ft.gpkg
	test -f $@

../input/canonical_boundary_segments_1320ft.gpkg: ../../../data_raw/boundaries/segments/boundary_segments_1320ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/canonical_boundary_segments_2640ft.gpkg: ../../../data_raw/boundaries/segments/boundary_segments_2640ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/canonical_segment_classification.csv: ../../../data_raw/boundaries/segments/segment_classification.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/canonical_ward_pair_boundaries.gpkg: ../../../data_raw/boundaries/segments/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/canonical_boundary_segments_1320ft.gpkg ../input/canonical_boundary_segments_2640ft.gpkg ../input/canonical_segment_classification.csv ../input/canonical_ward_pair_boundaries.gpkg

include ../../generic.make
