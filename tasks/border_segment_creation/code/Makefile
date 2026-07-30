SEGMENT_LENGTH_FT ?= 1320
SEGMENT_LAYER_BWS_M ?= 100 250 400

all: \
	../output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg \
	../output/ward_pair_boundaries.gpkg

../output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: build_boundary_segments.R \
	../../_lib/canonical_geometry_helpers.R \
	../input/ward_panel.gpkg \
	Makefile | ../output
	Rscript $< $(SEGMENT_LENGTH_FT) "$(SEGMENT_LAYER_BWS_M)"

../output/ward_pair_boundaries.gpkg: ../output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg
	@test -s "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/ward_panel.gpkg

include ../../generic.make
