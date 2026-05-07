SHELL := bash

BANDWIDTHS_M ?= 50 75 100 125 150 175 200 225 250 275 300
BANDWIDTHS_FT := $(shell Rscript -e 'cat(round(as.numeric(strsplit("$(BANDWIDTHS_M)", " ")[[1]]) / 0.3048), collapse = " ")')
SAMPLES := all multifamily

all: \
	../output/parcel_other_pair_distance.csv \
	../output/boundary_ambiguity_by_bw.csv \
	../output/boundary_ambiguity_top_pairs.csv \
	../output/boundary_ambiguity_share.pdf

../output/parcel_other_pair_distance.csv: build_boundary_ambiguity.R ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	Rscript build_boundary_ambiguity.R ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg ../output/parcel_other_pair_distance.csv ../output/boundary_ambiguity_by_bw.csv ../output/boundary_ambiguity_top_pairs.csv ../output/boundary_ambiguity_share.pdf "$(BANDWIDTHS_FT)" "$(SAMPLES)"

../output/boundary_ambiguity_by_bw.csv: ../output/parcel_other_pair_distance.csv
	test -f "$@"

../output/boundary_ambiguity_top_pairs.csv: ../output/parcel_other_pair_distance.csv
	test -f "$@"

../output/boundary_ambiguity_share.pdf: ../output/parcel_other_pair_distance.csv
	test -f "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg

include ../../generic.make
