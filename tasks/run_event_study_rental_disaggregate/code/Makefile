SHELL := bash
include ../../shell_functions.make

FREQUENCIES := yearly
STACKINGS := TRUE FALSE
TREATMENT_TYPES := continuous continuous_split
INCLUDE_CONTROLS := TRUE FALSE
WEIGHTINGS := triangular uniform
BANDWIDTHS := 500 1000
SAMPLE_FILTERS := multifamily_only
POST_WINDOWS := short full
FE_TYPES ?= strict_pair_x_year

STACK_PREFIX_TRUE := stacked
STACK_PREFIX_FALSE := unstacked
CONTROLS_SUFFIX_TRUE :=
CONTROLS_SUFFIX_FALSE := _no_hedonics
SAMPLE_SUFFIX_full_sample :=
SAMPLE_SUFFIX_multifamily_only := _mf
POST_SUFFIX_full :=
POST_SUFFIX_short := _short
FE_SUFFIX_strict_pair_x_year :=
FE_SUFFIX_pair_trend_plus_year := _pairtrend
FE_SUFFIX_side_plus_year := _yearfe

define GEN_OUTPUTS
$(foreach freq,$(FREQUENCIES),\
$(foreach st,$(STACKINGS),\
$(foreach tt,$(TREATMENT_TYPES),\
$(foreach ic,$(INCLUDE_CONTROLS),\
$(foreach wt,$(WEIGHTINGS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sf,$(SAMPLE_FILTERS),\
$(foreach fe,$(FE_TYPES),\
$(foreach pw,$(POST_WINDOWS),\
../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic))$(FE_SUFFIX_$(fe))$(POST_SUFFIX_$(pw)).pdf\
)))))))))
endef

all: $(call GEN_OUTPUTS) ../output/did_table_rental.tex

define ES_RULE
../output/event_study_disaggregate_$(freq)_$(STACK_PREFIX_$(st))_$(tt)_$(wt)_$(bw)ft$(SAMPLE_SUFFIX_$(sf))$(CONTROLS_SUFFIX_$(ic))$(FE_SUFFIX_$(fe))$(POST_SUFFIX_$(pw)).pdf: run_event_study_disaggregate.R ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R $(freq) $(st) $(tt) $(ic) $(wt) $(bw) $(sf) $(fe) $(pw) segment twoway_block_segment
endef

$(foreach freq,$(FREQUENCIES),\
$(foreach st,$(STACKINGS),\
$(foreach tt,$(TREATMENT_TYPES),\
$(foreach ic,$(INCLUDE_CONTROLS),\
$(foreach wt,$(WEIGHTINGS),\
$(foreach bw,$(BANDWIDTHS),\
$(foreach sf,$(SAMPLE_FILTERS),\
$(foreach fe,$(FE_TYPES),\
$(foreach pw,$(POST_WINDOWS),\
$(eval $(ES_RULE))\
)))))))))

../output/did_table_rental.tex: run_did_rental_disaggregate.R ../input/rental_listing_panel.parquet | ../output
	Rscript run_did_rental_disaggregate.R 1000 triangular strict_pair_x_year segment twoway_block_segment

../output/did_table_rental_pairtrend.tex: run_did_rental_disaggregate.R ../input/rental_listing_panel.parquet | ../output
	Rscript run_did_rental_disaggregate.R 1000 triangular pair_trend_plus_year segment twoway_block_segment

../output/did_table_rental_yearfe.tex: run_did_rental_disaggregate.R ../input/rental_listing_panel.parquet | ../output
	Rscript run_did_rental_disaggregate.R 1000 triangular side_plus_year segment twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_continuous_triangular_1000ft_mf_short_geo_wardpair.pdf: run_event_study_disaggregate.R ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R yearly TRUE continuous TRUE triangular 1000 multifamily_only strict_pair_x_year short ward_pair twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_continuous_triangular_1000ft_mf_short_clust_block.pdf: run_event_study_disaggregate.R ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R yearly TRUE continuous TRUE triangular 1000 multifamily_only strict_pair_x_year short segment block

../output/event_study_disaggregate_yearly_stacked_continuous_split_triangular_1000ft_mf_short_geo_wardpair.pdf: run_event_study_disaggregate.R ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R yearly TRUE continuous_split TRUE triangular 1000 multifamily_only strict_pair_x_year short ward_pair twoway_block_segment

../output/event_study_disaggregate_yearly_stacked_continuous_split_triangular_1000ft_mf_short_clust_block.pdf: run_event_study_disaggregate.R ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet | ../output
	Rscript run_event_study_disaggregate.R yearly TRUE continuous_split TRUE triangular 1000 multifamily_only strict_pair_x_year short segment block

../output/did_table_rental_clean_geo_wardpair.tex: run_did_rental_disaggregate.R ../input/rental_listing_panel.parquet | ../output
	Rscript run_did_rental_disaggregate.R 1000 triangular strict_pair_x_year ward_pair twoway_block_segment

../output/did_table_rental_clean_clust_block.tex: run_did_rental_disaggregate.R ../input/rental_listing_panel.parquet | ../output
	Rscript run_did_rental_disaggregate.R 1000 triangular strict_pair_x_year segment block

../output/rental_effect_source_summary.csv \
../output/rental_effect_source_event_study_coefficients.csv \
../output/rental_effect_source_counterfactuals.csv \
../output/rental_effect_source_boundaries.csv \
../output/rental_effect_source_boundaries_top50.csv \
../output/rental_effect_source_wards.csv \
../output/rental_effect_source_wards_top50.csv \
../output/rental_effect_source_blocks_top200.csv \
../output/rental_effect_source_buildings.csv \
../output/rental_effect_source_buildings_top200.csv: diagnose_rental_effect_sources.R ../input/rental_listing_panel.parquet ../input/rent_with_ward_distances.parquet | ../output
	Rscript diagnose_rental_effect_sources.R ../input/rental_listing_panel.parquet ../input/rent_with_ward_distances.parquet 1000 triangular multifamily_only strict_pair_x_year 50 200 200 ../output/rental_effect_source_summary.csv ../output/rental_effect_source_event_study_coefficients.csv ../output/rental_effect_source_counterfactuals.csv ../output/rental_effect_source_boundaries.csv ../output/rental_effect_source_boundaries_top50.csv ../output/rental_effect_source_wards.csv ../output/rental_effect_source_wards_top50.csv ../output/rental_effect_source_blocks_top200.csv ../output/rental_effect_source_buildings.csv ../output/rental_effect_source_buildings_top200.csv

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf "$<" "$@"

../input/rental_listing_panel_2015.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2015.parquet | ../input
	ln -sf "$<" "$@"

../input/rental_listing_panel_2023.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel_2023.parquet | ../input
	ln -sf "$<" "$@"

../input/rent_with_ward_distances.parquet: ../../create_event_study_rental_data_disaggregate/input/rent_with_ward_distances.parquet | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rental_listing_panel.parquet ../input/rental_listing_panel_2015.parquet ../input/rental_listing_panel_2023.parquet ../input/rent_with_ward_distances.parquet

include ../../generic.make
