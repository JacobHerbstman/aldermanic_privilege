FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 152.4
BW_LABEL ?= 500ft
BINS_PER_SIDE ?= 5

all: \
	../output/nonparametric_rd_density_linear_display_4panel_$(BW_LABEL)_all_multifamily_bins$(BINS_PER_SIDE).pdf

../output/nonparametric_rd_density_linear_display_4panel_$(BW_LABEL)_all_multifamily_bins$(BINS_PER_SIDE).pdf: nonparametric_rd_density_linear_display_combined.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $(BW_M) $(FE_SPEC) $(BINS_PER_SIDE)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
