SHELL := bash
include ../../shell_functions.make

INPUT := ../input/rent_with_ward_distances.parquet

FE_TEX := ../output/fe_table_rental_bw500_pre_2023.tex
FE_CSV := ../output/fe_table_rental_bw500_pre_2023.csv
YEAR_DIAG := ../output/year_diagnostics_bw500_pre_2023.csv
FE_GEOS := segment ward_pair
CLUSTERS := segment ward_pair
ROBUST_FE_OUTPUTS := $(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(filter segment,$(geo)),$(if $(filter segment,$(cl)),,../output/fe_table_rental_bw500_pre_2023_geo_$(geo)_clust_$(cl).tex),../output/fe_table_rental_bw500_pre_2023_geo_$(geo)_clust_$(cl).tex)))

LEVEL_PCT0 := ../output/level_plot_bw500_pct0_ctrl.pdf
LEVEL_PCT50 := ../output/level_plot_bw500_pct50_ctrl.pdf

.DEFAULT_GOAL := all
.PHONY: all link-inputs

all: $(FE_TEX) $(ROBUST_FE_OUTPUTS) $(LEVEL_PCT0) $(LEVEL_PCT50)

$(FE_TEX): rental_border_pair_fe.R $(INPUT) | ../output
	# arg order: input bw_ft window sample_filter output_tex output_csv output_year_diag fe_geo cluster_level
	Rscript rental_border_pair_fe.R "$(INPUT)" 500 pre_2023 all "$@" "$(FE_CSV)" "$(YEAR_DIAG)" segment segment

define FE_ROBUST_RULE
../output/fe_table_rental_bw500_pre_2023_geo_$(1)_clust_$(2).tex: rental_border_pair_fe.R $(INPUT) | ../output
	Rscript rental_border_pair_fe.R "$(INPUT)" 500 pre_2023 all "$$@" "../output/fe_table_rental_bw500_pre_2023_geo_$(1)_clust_$(2).csv" "../output/year_diagnostics_bw500_pre_2023_geo_$(1)_clust_$(2).csv" $(1) $(2)
endef
$(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(and $(filter segment,$(geo)),$(filter segment,$(cl))),,$(eval $(call FE_ROBUST_RULE,$(geo),$(cl))))))

$(LEVEL_PCT0): rental_level_plot.R $(INPUT) | ../output
	# arg order: input bw_ft window sample_filter use_controls bins_per_side min_strictness_diff_pctile output_pdf output_meta_csv output_bins_csv fe_geo cluster_level
	Rscript rental_level_plot.R "$(INPUT)" 500 pre_2023 all TRUE 15 0 "$@" NA NA segment segment

$(LEVEL_PCT50): rental_level_plot.R $(INPUT) | ../output
	# arg order: input bw_ft window sample_filter use_controls bins_per_side min_strictness_diff_pctile output_pdf output_meta_csv output_bins_csv fe_geo cluster_level
	Rscript rental_level_plot.R "$(INPUT)" 500 pre_2023 all TRUE 15 50 "$@" NA NA segment segment

$(INPUT): ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: $(INPUT)

include ../../generic.make
