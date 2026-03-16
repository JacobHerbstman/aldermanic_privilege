SHELL := bash
include ../../shell_functions.make

FE_GEOS := segment ward_pair
CLUSTERS := segment ward_pair
ROBUST_FE_OUTPUTS := $(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(filter segment,$(geo)),$(if $(filter segment,$(cl)),,../output/fe_table_rental_bw500_pre_2023_geo_$(geo)_clust_$(cl).tex),../output/fe_table_rental_bw500_pre_2023_geo_$(geo)_clust_$(cl).tex)))
SLIDE_WARD_PAIR_OUTPUTS := \
	../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex \
	../output/level_plot_bw500_pct0_ctrl_clust_ward_pair.pdf

all: $(SLIDE_WARD_PAIR_OUTPUTS) $(ROBUST_FE_OUTPUTS) ../output/fe_table_rental_bw500_pre_2023.tex ../output/level_plot_bw500_pct0_ctrl.pdf ../output/level_plot_bw500_pct50_ctrl.pdf

../output/fe_table_rental_bw500_pre_2023.tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all "$@" ../output/fe_table_rental_bw500_pre_2023.csv ../output/year_diagnostics_bw500_pre_2023.csv segment segment

../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all "$@" ../output/fe_table_rental_bw500_pre_2023_clust_ward_pair.csv ../output/year_diagnostics_bw500_pre_2023_clust_ward_pair.csv segment ward_pair

../output/fe_table_rental_bw500_pre_2023_zonegroup.tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet ../input/zoning_data_clean.gpkg | ../output
	USE_ZONE_GROUP_FE=TRUE ZONING_GPKG=../input/zoning_data_clean.gpkg Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all "$@" ../output/fe_table_rental_bw500_pre_2023_zonegroup.csv ../output/year_diagnostics_bw500_pre_2023_zonegroup.csv segment segment

../output/fe_table_rental_bw500_pre_2023_pruned.tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all "$@" ../output/fe_table_rental_bw500_pre_2023_pruned.csv ../output/year_diagnostics_bw500_pre_2023_pruned.csv segment segment

../output/fe_table_rental_bw500_pre_2023_pruned_zonegroup.tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv ../input/zoning_data_clean.gpkg | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv USE_ZONE_GROUP_FE=TRUE ZONING_GPKG=../input/zoning_data_clean.gpkg Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all "$@" ../output/fe_table_rental_bw500_pre_2023_pruned_zonegroup.csv ../output/year_diagnostics_bw500_pre_2023_pruned_zonegroup.csv segment segment

define FE_ROBUST_RULE
../output/fe_table_rental_bw500_pre_2023_geo_$(1)_clust_$(2).tex: rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_border_pair_fe.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all $$@ ../output/fe_table_rental_bw500_pre_2023_geo_$(1)_clust_$(2).csv ../output/year_diagnostics_bw500_pre_2023_geo_$(1)_clust_$(2).csv $(1) $(2)
endef
$(foreach geo,$(FE_GEOS),$(foreach cl,$(CLUSTERS),$(if $(and $(filter segment,$(geo)),$(filter segment,$(cl))),,$(eval $(call FE_ROBUST_RULE,$(geo),$(cl))))))

../output/level_plot_bw500_pct0_ctrl.pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all TRUE 15 0 "$@" NA NA segment segment

../output/level_plot_bw500_pct0_ctrl_clust_ward_pair.pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all TRUE 15 0 "$@" NA NA segment ward_pair

../output/level_plot_bw500_pct50_ctrl.pdf: rental_level_plot.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript rental_level_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all TRUE 15 50 "$@" NA NA segment segment

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv ../input/zoning_data_clean.gpkg

include ../../generic.make
