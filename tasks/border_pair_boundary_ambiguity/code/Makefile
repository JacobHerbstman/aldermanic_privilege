BANDWIDTHS_M ?= 76.2 152.4 304.8
SAMPLES := all multifamily

all: \
	../output/parcel_other_pair_distance.csv \
	../output/boundary_ambiguity_by_bw.csv \
	../output/boundary_ambiguity_top_pairs.csv \
	../output/boundary_ambiguity_share.pdf

../output/parcel_other_pair_distance.csv: build_boundary_ambiguity.R ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg | ../output
	Rscript build_boundary_ambiguity.R ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg ../output/parcel_other_pair_distance.csv ../output/boundary_ambiguity_by_bw.csv ../output/boundary_ambiguity_top_pairs.csv ../output/boundary_ambiguity_share.pdf "$(BANDWIDTHS_M)" "$(SAMPLES)"

../output/boundary_ambiguity_by_bw.csv: ../output/parcel_other_pair_distance.csv
	@test -s "$@"

../output/boundary_ambiguity_top_pairs.csv: ../output/parcel_other_pair_distance.csv
	@test -s "$@"

../output/boundary_ambiguity_share.pdf: ../output/parcel_other_pair_distance.csv
	@test -s "$@"

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv ../input/geocoded_residential_data.gpkg ../input/ward_pair_boundaries.gpkg

include ../../generic.make
