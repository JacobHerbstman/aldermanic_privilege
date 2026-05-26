SHELL := bash

SEGMENT_LENGTH_FT ?= 1320

all: ../output/pruned_boundaries_all_segments_by_era.pdf

../output/pruned_boundaries_all_segments_by_era.pdf: plot_pruned_boundary_maps.R \
	../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg \
	../input/ward_panel.gpkg | ../output
	SEGMENT_LENGTH_FT=$(SEGMENT_LENGTH_FT) Rscript plot_pruned_boundary_maps.R

../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg: ../../border_segment_creation/output/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/boundary_segments_$(SEGMENT_LENGTH_FT)ft.gpkg \
	../input/ward_panel.gpkg

include ../../shell_functions.make
include ../../generic.make
