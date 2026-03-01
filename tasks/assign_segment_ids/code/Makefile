SHELL := bash
include ../../shell_functions.make

all: ../output/parcel_segment_ids.csv ../output/parcel_segment_ids_coverage.csv

../output/parcel_segment_ids.csv ../output/parcel_segment_ids_coverage.csv: assign_segment_ids.R ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg | ../output
	# arg order: parcels_pre_scores parcels_with_geometry segment_gpkg output_lookup output_coverage
	$(R) assign_segment_ids.R ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg ../output/parcel_segment_ids.csv ../output/parcel_segment_ids_coverage.csv

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	ln -sf $< $@

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	ln -sf $< $@

../input/boundary_segments_1320ft.gpkg: ../../border_segment_creation/output/boundary_segments_1320ft.gpkg | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_1320ft.gpkg
