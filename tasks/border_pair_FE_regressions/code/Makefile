SHELL := bash
include ../../shell_functions.make

# Active uncertainty-spec FE table set used by slides_uncertainty.
FE_SPECS := pair_year_only pair_x_year zone_pair_year_additive
BWS_FT := 250 500 1000
SAMPLES := all multifamily
YVARS_TABLE := 'log(density_far)' 'log(density_dupac)' 'log(unitscount)'
SEG_FE_SPECS := segment_year zone_segment_year_additive
SEG_CLUSTERS := ward_pair segment
SEG_YVARS := 'log(density_far)' 'log(density_dupac)'

OUTPUT_FILES := $(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),../output/fe_table_bw$(bw)_$(sample)_$(fe).tex)))
OUTPUT_FILES_PRUNED := $(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),../output/fe_table_bw$(bw)_$(sample)_$(fe)_pruned.tex)))
SEG_OUTPUT_FILES := $(foreach sample,$(SAMPLES),$(foreach fe,$(SEG_FE_SPECS),$(foreach cluster,$(SEG_CLUSTERS),../output/fe_table_bw500_$(sample)_$(fe)_clust_$(cluster).tex)))

all: $(OUTPUT_FILES)
pruned: $(OUTPUT_FILES_PRUNED)
segment-robustness: $(SEG_OUTPUT_FILES)

define FE_template
../output/fe_table_bw$(1)_$(2)_$(3).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	# arg order: bw_ft sample fe_spec output_filename yvars...
	Rscript border_pair_FE_regressions.R $(1) $(2) $(3) "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),$(eval $(call FE_template,$(bw),$(sample),$(fe))))))

define FE_template_pruned
../output/fe_table_bw$(1)_$(2)_$(3)_pruned.tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv | ../output
	# arg order: bw_ft sample fe_spec output_filename yvars...
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv Rscript border_pair_FE_regressions.R $(1) $(2) $(3) "$$@" $(YVARS_TABLE)
endef
$(foreach bw,$(BWS_FT),$(foreach sample,$(SAMPLES),$(foreach fe,$(FE_SPECS),$(eval $(call FE_template_pruned,$(bw),$(sample),$(fe))))))

define FE_template_segment
../output/fe_table_bw500_$(1)_$(2)_clust_$(3).tex: border_pair_FE_regressions.R ../input/parcels_with_ward_distances.csv | ../output
	# arg order: bw_ft sample fe_spec output_filename yvars...
	CLUSTER_LEVEL=$(3) Rscript border_pair_FE_regressions.R 500 $(1) $(2) "$$@" $(SEG_YVARS)
endef
$(foreach sample,$(SAMPLES),$(foreach fe,$(SEG_FE_SPECS),$(foreach cluster,$(SEG_CLUSTERS),$(eval $(call FE_template_segment,$(sample),$(fe),$(cluster))))))

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

include ../../generic.make

link-inputs: ../input/parcels_with_ward_distances.csv ../input/confounded_pair_era_flags.csv
