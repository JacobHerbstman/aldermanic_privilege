BW_M ?= 152.4
BW_LABEL ?= 500ft
START_CONSTRUCTION_YEAR ?= 2006
END_CONSTRUCTION_YEAR ?= 2022

all: \
	../output/fe_table_$(BW_LABEL)_all_corner_clean_compare.tex \
	../output/fe_table_$(BW_LABEL)_multifamily_corner_clean_compare.tex

../output/fe_table_$(BW_LABEL)_all_corner_clean_compare.tex: build_density_corner_clean_table.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv ../input/boundary_ambiguity_by_bw.csv | ../output
	Rscript $< $(BW_M) all $(BW_LABEL) $(START_CONSTRUCTION_YEAR) $(END_CONSTRUCTION_YEAR)

../output/fe_table_$(BW_LABEL)_multifamily_corner_clean_compare.tex: build_density_corner_clean_table.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv ../input/boundary_ambiguity_by_bw.csv | ../output
	Rscript $< $(BW_M) multifamily $(BW_LABEL) $(START_CONSTRUCTION_YEAR) $(END_CONSTRUCTION_YEAR)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_other_pair_distance.csv: ../../border_pair_boundary_ambiguity/output/parcel_other_pair_distance.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/boundary_ambiguity_by_bw.csv: ../../border_pair_boundary_ambiguity/output/boundary_ambiguity_by_bw.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/parcels_with_ward_distances.csv \
	../input/parcel_other_pair_distance.csv \
	../input/boundary_ambiguity_by_bw.csv

include ../../generic.make
