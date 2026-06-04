FE_SPEC := zonegroup_segment_year_additive
BW_M ?= 152.4
BW_LABEL ?= 500ft
BINS_PER_SIDE ?= 5
START_CONSTRUCTION_YEAR ?= 2006
END_CONSTRUCTION_YEAR ?= 2022
SAMPLES := all multifamily
PLACEBO_SHIFT_M ?= 152.4
PLACEBO_SHIFT_LABEL ?= 500ft
YVARS := density_far density_dupac
PLACEBO_DIRECTIONS := neg pos

all: $(foreach sample,$(SAMPLES), \
	$(foreach yvar,$(YVARS), \
	$(foreach direction,$(PLACEBO_DIRECTIONS), \
	../output/nonparametric_rd_density_placebo_log_$(yvar)_$(BW_LABEL)_$(sample)_shift_$(direction)$(PLACEBO_SHIFT_LABEL).pdf)))

../output/nonparametric_rd_density_placebo_log_%_$(BW_LABEL)_all_shift_neg$(PLACEBO_SHIFT_LABEL).pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) all $(FE_SPEC) $(BINS_PER_SIDE) -$(PLACEBO_SHIFT_M) $(START_CONSTRUCTION_YEAR) $(END_CONSTRUCTION_YEAR)

../output/nonparametric_rd_density_placebo_log_%_$(BW_LABEL)_all_shift_pos$(PLACEBO_SHIFT_LABEL).pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) all $(FE_SPEC) $(BINS_PER_SIDE) $(PLACEBO_SHIFT_M) $(START_CONSTRUCTION_YEAR) $(END_CONSTRUCTION_YEAR)

../output/nonparametric_rd_density_placebo_log_%_$(BW_LABEL)_multifamily_shift_neg$(PLACEBO_SHIFT_LABEL).pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) multifamily $(FE_SPEC) $(BINS_PER_SIDE) -$(PLACEBO_SHIFT_M) $(START_CONSTRUCTION_YEAR) $(END_CONSTRUCTION_YEAR)

../output/nonparametric_rd_density_placebo_log_%_$(BW_LABEL)_multifamily_shift_pos$(PLACEBO_SHIFT_LABEL).pdf: nonparametric_rd_density_placebo.R ../../_lib/border_pair_helpers.R ../input/parcels_with_ward_distances.csv | ../output
	Rscript $< $* $(BW_M) multifamily $(FE_SPEC) $(BINS_PER_SIDE) $(PLACEBO_SHIFT_M) $(START_CONSTRUCTION_YEAR) $(END_CONSTRUCTION_YEAR)

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../generic.make
