SEGMENT_LENGTH_FT ?= 1320
SEGMENT_BUFFER_M ?= 250

all: ../output/parcel_segment_ids.csv

../output/parcel_segment_ids.csv: assign_segment_ids.R ../../_lib/canonical_geometry_helpers.R ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../output
	Rscript $< $(SEGMENT_LENGTH_FT) $(SEGMENT_BUFFER_M)

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/parcels_with_geometry.gpkg: ../../calculate_ward_boundary_distances/output/parcels_with_geometry.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/parcels_pre_scores.csv ../input/parcels_with_geometry.gpkg ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg

include ../../generic.make
