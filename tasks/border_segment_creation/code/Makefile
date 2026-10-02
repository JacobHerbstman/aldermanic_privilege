include ../../shared/code/shell_functions.make

SEGMENT_LENGTH_FT ?= 1320
SEGMENT_LAYER_BWS_M ?= 100,250,400

all: ../output/ward_pair_boundaries.gpkg \
	../output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg

../%/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg \
	../%/ward_pair_boundaries.gpkg: build_boundary_segments.R \
	../input/ward_panel.gpkg \
	Makefile | ../output ../report
	$(R) $< $(SEGMENT_LENGTH_FT) $(SEGMENT_LAYER_BWS_M)

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/ward_panel.gpkg

include ../../shared/code/generic.make
