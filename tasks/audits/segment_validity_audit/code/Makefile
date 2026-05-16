SHELL := bash

SEGMENT_LENGTH_FT ?= 1320

all: ../output/segment_validity_audit.csv ../output/segment_analysis_id_map.csv ../output/segment_support_audit.csv ../output/short_segment_summary.csv ../output/invalid_segment_observation_audit.csv ../output/segment_validity_summary.md

../output/segment_validity_audit.csv: run_segment_validity_audit.R ../../../_lib/canonical_geometry_helpers.R ../input/ward_panel.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg ../input/parcels_with_ward_distances.csv ../input/sales_pre_scores_with_segments.csv ../input/confounded_segment_flags.csv | ../output
	Rscript run_segment_validity_audit.R

../output/segment_analysis_id_map.csv \
../output/segment_support_audit.csv \
../output/short_segment_summary.csv \
../output/invalid_segment_observation_audit.csv \
../output/segment_validity_summary.md: ../output/segment_validity_audit.csv
	@test -s "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: ../../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/sales_pre_scores_with_segments.csv: ../../../assign_segment_ids_sales_rental/output/sales_pre_scores_with_segments.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/confounded_segment_flags.csv: ../../../border_confound_pruning/output/confounded_segment_flags.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/ward_panel.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg ../input/parcels_with_ward_distances.csv ../input/sales_pre_scores_with_segments.csv ../input/confounded_segment_flags.csv

include ../../../generic.make
