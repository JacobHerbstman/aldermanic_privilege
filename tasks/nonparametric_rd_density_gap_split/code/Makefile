FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 152.4
BW_LABEL ?= 500ft
BINS_PER_SIDE ?= 5
SAMPLES := all multifamily
GAP_SPLITS := above_median below_median
YVARS := density_far density_dupac

all: $(foreach sample,$(SAMPLES),$(foreach yvar,$(YVARS),$(foreach gap_split,$(GAP_SPLITS),../output/nonparametric_rd_density_gap_split_log_$(yvar)_$(BW_LABEL)_$(sample)_$(gap_split).pdf)))

../output/nonparametric_rd_density_gap_split_log_%_$(BW_LABEL)_all_above_median.pdf: nonparametric_rd_density_gap_split.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) all $(FE_SPEC) $(BINS_PER_SIDE) above_median

../output/nonparametric_rd_density_gap_split_log_%_$(BW_LABEL)_all_below_median.pdf: nonparametric_rd_density_gap_split.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) all $(FE_SPEC) $(BINS_PER_SIDE) below_median

../output/nonparametric_rd_density_gap_split_log_%_$(BW_LABEL)_multifamily_above_median.pdf: nonparametric_rd_density_gap_split.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) multifamily $(FE_SPEC) $(BINS_PER_SIDE) above_median

../output/nonparametric_rd_density_gap_split_log_%_$(BW_LABEL)_multifamily_below_median.pdf: nonparametric_rd_density_gap_split.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) multifamily $(FE_SPEC) $(BINS_PER_SIDE) below_median

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
