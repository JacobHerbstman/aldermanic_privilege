BW_M ?= 152.4
BW_LABEL ?= 500ft

all: \
	../output/fe_table_$(BW_LABEL)_all_corner_clean_compare.tex \
	../output/fe_table_$(BW_LABEL)_multifamily_corner_clean_compare.tex

../output/fe_table_$(BW_LABEL)_all_corner_clean_compare.tex: build_density_corner_clean_table.R ../temp/fe_summary_$(BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../temp/fe_summary_$(BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv ../input/boundary_ambiguity_by_bw.csv | ../output
	Rscript $< $(BW_M) all $(BW_LABEL)

../output/fe_table_$(BW_LABEL)_multifamily_corner_clean_compare.tex: build_density_corner_clean_table.R ../temp/fe_summary_$(BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv ../temp/fe_summary_$(BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv ../input/boundary_ambiguity_by_bw.csv | ../output
	Rscript $< $(BW_M) multifamily $(BW_LABEL)

../temp/fe_summary_$(BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_RESULT_LOCATION=temp FE_RESULT_TAG=baseline Rscript $< $(BW_M) all zonegroup_segment_year_additive all ward_pair 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_$(BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_baseline.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) WRITE_TEX=FALSE FE_RESULT_LOCATION=temp FE_RESULT_TAG=baseline Rscript $< $(BW_M) multifamily zonegroup_segment_year_additive all ward_pair 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_$(BW_LABEL)_all_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) DROP_AMBIGUOUS_WITHIN_BW=TRUE WRITE_TEX=FALSE FE_RESULT_LOCATION=temp FE_RESULT_TAG=corner_clean Rscript $< $(BW_M) all zonegroup_segment_year_additive all ward_pair 'log(density_far)' 'log(density_dupac)'

../temp/fe_summary_$(BW_LABEL)_multifamily_zonegroup_segment_year_additive_clust_ward_pair_corner_clean.csv: ../../_lib/border_pair_fe_regression_runner.R ../input/parcels_with_ward_distances.csv ../input/parcel_other_pair_distance.csv | ../temp
	DISTANCE_DISPLAY_UNIT=ft BANDWIDTH_LABEL=$(BW_LABEL) DROP_AMBIGUOUS_WITHIN_BW=TRUE WRITE_TEX=FALSE FE_RESULT_LOCATION=temp FE_RESULT_TAG=corner_clean Rscript $< $(BW_M) multifamily zonegroup_segment_year_additive all ward_pair 'log(density_far)' 'log(density_dupac)'

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
