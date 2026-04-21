SHELL := bash

BANDWIDTHS := 100 150 200 250 300 350 400 450 500 550 600 650 700 750 800 850 900 950 1000
SAMPLES := all multifamily

all: \
	../output/parcel_other_pair_distance.csv \
	../output/boundary_ambiguity_by_bw.csv \
	../output/boundary_ambiguity_top_pairs.csv \
	../output/boundary_ambiguity_share.pdf

../output/parcel_other_pair_distance.csv: build_boundary_ambiguity.R ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	Rscript build_boundary_ambiguity.R ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg ../output/parcel_other_pair_distance.csv ../output/boundary_ambiguity_by_bw.csv ../output/boundary_ambiguity_top_pairs.csv ../output/boundary_ambiguity_share.pdf "$(BANDWIDTHS)" "$(SAMPLES)"

../output/boundary_ambiguity_by_bw.csv: ../output/parcel_other_pair_distance.csv
	test -f "$@"

../output/boundary_ambiguity_top_pairs.csv: ../output/parcel_other_pair_distance.csv
	test -f "$@"

../output/boundary_ambiguity_share.pdf: ../output/parcel_other_pair_distance.csv
	test -f "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg

include ../../generic.make
