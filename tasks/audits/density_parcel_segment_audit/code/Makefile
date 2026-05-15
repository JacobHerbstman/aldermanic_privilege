SHELL := bash

all: ../output/audit_summary.csv ../output/ward_pair_distance_issues.csv ../output/segment_assignment_issues.csv ../output/all_options_segment_comparison.csv ../output/global_nearest_segment_pair_differences.csv ../output/manual_spotcheck_sample.csv ../output/manual_spotcheck_sample.gpkg

external: ../output/cook_county_pin_spotcheck.csv

../input ../output:
	mkdir -p "$@"

../output/audit_summary.csv: run_density_parcel_segment_audit.R ../../../_lib/canonical_geometry_helpers.R ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/parcel_segment_ids.csv ../input/parcel_segment_pair_constraint_audit.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_400m.gpkg | ../output
	Rscript run_density_parcel_segment_audit.R

../output/ward_pair_distance_issues.csv \
../output/segment_assignment_issues.csv \
../output/all_options_segment_comparison.csv \
../output/global_nearest_segment_pair_differences.csv \
../output/manual_spotcheck_sample.csv \
../output/manual_spotcheck_sample.gpkg: ../output/audit_summary.csv
	@test -s "$@"

../output/cook_county_pin_spotcheck.csv: run_cook_county_pin_spotcheck.R ../output/manual_spotcheck_sample.csv | ../output
	Rscript run_cook_county_pin_spotcheck.R

../input/parcels_with_geometry.gpkg: ../../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_ward_distances.csv: ../../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_pair_constraint_audit.csv: ../../../assign_segment_ids/output/parcel_segment_pair_constraint_audit.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_400m.gpkg: ../../../border_segment_creation/output/boundary_segments_400m.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_geometry.gpkg ../input/parcels_with_ward_distances.csv ../input/parcel_segment_ids.csv ../input/parcel_segment_pair_constraint_audit.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/boundary_segments_400m.gpkg
