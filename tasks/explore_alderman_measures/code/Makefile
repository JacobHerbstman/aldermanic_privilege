include ../../shared/code/shell_functions.make

all: ../output/measure_correlations.csv ../output/measures_by_characteristic.csv \
	../output/measures_by_ward_characteristics.csv ../output/measure_principal_components.csv \
	../output/alderman_measures.csv ../output/zoning_trends_by_year.csv ../output/zoning_outcomes_by_direction.csv \
	../output/zoning_outcomes_by_ward_demographics.csv ../output/zoning_outcomes_by_boundary_distance.csv \
	../output/alderman_share_beyond_ward.csv ../output/boundary_side_counts.csv \
	../output/stall_rate_away_from_boundaries.csv ../output/density_by_zoning_measures.csv \
	../output/boundary_counts_placebo_lines.png ../output/stall_rate_by_year.png ../output/application_timeline.csv \
	../output/passage_curves.csv ../output/application_timing.csv ../output/alderman_stall_and_time.csv \
	../output/lame_duck_stall_rates.csv ../output/refiling_summary.csv ../output/passage_curves.png

../%/measure_correlations.csv ../%/measures_by_characteristic.csv ../%/measures_by_ward_characteristics.csv \
	../%/measure_principal_components.csv ../%/alderman_measures.csv: compare_alderman_measures.R \
	../input/alderman_uncertainty_index_through2022.csv ../input/alderman_uncertainty_index_through2014.csv \
	../input/alderman_permit_effects.csv ../input/alderman_zoning_measures.csv ../input/alderman_characteristics.csv \
	../input/prerogative_positions.csv ../input/community_zoning_processes.csv \
	../input/permits_for_uncertainty_index.csv | ../output ../report
	$(R) $<

../%/zoning_trends_by_year.csv ../%/zoning_outcomes_by_direction.csv ../%/zoning_outcomes_by_ward_demographics.csv \
	../%/zoning_outcomes_by_boundary_distance.csv ../%/alderman_share_beyond_ward.csv: explore_zoning_patterns.R \
	../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv ../input/permits_for_uncertainty_index.csv \
	../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson | ../output ../report
	$(R) $<

../%/boundary_side_counts.csv ../%/stall_rate_away_from_boundaries.csv: boundary_counts.R ../output/alderman_measures.csv \
	../input/new_construction_analysis_data.csv ../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv \
	../input/alderman_terms.csv ../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson \
	| ../output ../report
	$(R) $<

../%/application_timeline.csv ../%/passage_curves.csv ../%/application_timing.csv ../%/alderman_stall_and_time.csv \
	../%/lame_duck_stall_rates.csv ../%/refiling_summary.csv: explore_application_dynamics.R \
	../input/elms_zoning_matter_details.jsonl ../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv \
	../input/alderman_terms.csv | ../output ../report
	$(R) $<

../output/passage_curves.png: plot_passage_curves.R ../output/passage_curves.csv | ../output
	$(R) $<

../output/stall_rate_by_year.png: plot_stall_trends.R ../input/zoning_map_amendments.csv | ../output
	$(R) $<

../output/boundary_counts_placebo_lines.png: plot_boundary_counts.R ../output/boundary_side_counts.csv | ../output
	$(R) $<

../output/density_by_zoning_measures.csv: density_by_zoning_measures.R ../output/alderman_measures.csv \
	../output/stall_rate_away_from_boundaries.csv ../input/new_construction_analysis_data.csv | ../output ../report
	$(R) $<

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@
../input/alderman_uncertainty_index_through2014.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2014.csv | ../input
	ln -sf $< $@
../input/alderman_permit_effects.csv: ../../estimate_alderman_permit_effects/output/alderman_permit_effects.csv | ../input
	ln -sf $< $@
../input/alderman_zoning_measures.csv: ../../estimate_alderman_zoning_measures/output/alderman_zoning_measures.csv | ../input
	ln -sf $< $@
../input/alderman_characteristics.csv: ../../alderman_characteristics/sources/alderman_characteristics.csv | ../input
	ln -sf $< $@
../input/prerogative_positions.csv: ../../alderman_characteristics/sources/prerogative_positions.csv | ../input
	ln -sf $< $@
../input/community_zoning_processes.csv: ../../alderman_characteristics/sources/community_zoning_processes.csv | ../input
	ln -sf $< $@
../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@
../input/zoning_map_amendments.csv: ../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/zoning_amendment_wards.csv: ../../assign_zoning_amendment_wards/output/zoning_amendment_wards.csv | ../input
	ln -sf $< $@
../input/Wards_2014.geojson: ../../../data_raw/Wards_2014.geojson | ../input
	ln -sf $< $@
../input/Wards_2015.geojson: ../../../data_raw/Wards_2015.geojson | ../input
	ln -sf $< $@
../input/Wards_2024.geojson: ../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/alderman_terms.csv: ../../create_alderman_data/adjudication/alderman_terms.csv | ../input
	ln -sf $< $@

../input/elms_zoning_matter_details.jsonl: ../../download_elms_matters/output/elms_zoning_matter_details.jsonl | ../input
	ln -sf $< $@

link-inputs: ../input/elms_zoning_matter_details.jsonl ../input/new_construction_analysis_data.csv ../input/alderman_terms.csv \
	../input/alderman_uncertainty_index_through2022.csv ../input/alderman_uncertainty_index_through2014.csv \
	../input/alderman_permit_effects.csv ../input/alderman_zoning_measures.csv ../input/alderman_characteristics.csv \
	../input/prerogative_positions.csv ../input/community_zoning_processes.csv \
	../input/permits_for_uncertainty_index.csv ../input/zoning_map_amendments.csv ../input/zoning_amendment_wards.csv \
	../input/Wards_2014.geojson ../input/Wards_2015.geojson ../input/Wards_2024.geojson

include ../../shared/code/generic.make
