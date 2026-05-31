BANDWIDTH_FT ?= 500
MAX_CONSTRUCTION_YEAR ?= 2026

all: ../output/fe_table_$(BANDWIDTH_FT)ft_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex

../output/fe_table_$(BANDWIDTH_FT)ft_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex: build_low_discretion_density_robustness.R ../temp/parcels_with_ward_distances_low_discretion_residualized.csv | ../output
	Rscript $< $(BANDWIDTH_FT)

../temp/parcels_with_ward_distances_low_discretion_residualized.csv: merge_low_discretion_residualized_scores.R ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/aldermen_uncertainty_scores.csv | ../temp
	Rscript $< $(MAX_CONSTRUCTION_YEAR)

../input/parcels_pre_scores.csv: ../../calculate_ward_boundary_distances/output/parcels_pre_scores.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_segment_ids.csv: ../../assign_segment_ids/output/parcel_segment_ids.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/aldermen_uncertainty_scores.csv: ../../new_construction_score_variants/output/alderman_uncertainty_index_low_discretion_residualized.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_pre_scores.csv ../input/parcel_segment_ids.csv ../input/aldermen_uncertainty_scores.csv

include ../../generic.make
