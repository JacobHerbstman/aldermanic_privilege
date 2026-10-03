include ../../../shared/code/shell_functions.make

all: ../output/scores_through_2026.csv ../output/score_comparison.csv ../output/results_with_scores_through_2026.csv \
	../output/density_side_flips_by_pair.csv ../output/density_without_near_ties.csv \
	../output/density_continuous_rd.pdf

../output/permits_2023_2026_for_index.csv: prepare_permits_2023_2026.R ../input/building_permits_clean_2023_2026.gpkg \
	../input/ward_panel.gpkg ../input/Wards_2024.geojson ../sources/alderman_terms_through_2026.csv ../input/ward_controls.csv \
	../input/community_areas.geojson ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg | ../output
	$(R) $<

../%/scores_through_2026.csv ../%/score_comparison.csv: build_scores_through_2026.R \
	../input/permits_for_uncertainty_index.csv ../output/permits_2023_2026_for_index.csv \
	../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $<

../output/results_with_scores_through_2026.csv: results_with_scores_through_2026.R ../output/scores_through_2026.csv \
	../input/alderman_uncertainty_index_through2022.csv ../input/new_construction_analysis_data.csv \
	../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet | ../output
	$(R) $<

../%/density_side_flips_summary.csv ../%/density_side_flips_by_pair.csv ../%/density_side_flips_by_alderman.csv: \
	density_side_flips.R ../output/scores_through_2026.csv ../input/alderman_uncertainty_index_through2022.csv \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/density_without_near_ties.csv: density_without_near_ties.R ../output/scores_through_2026.csv \
	../input/alderman_uncertainty_index_through2022.csv ../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/density_continuous_rd.pdf: density_continuous_plots.R ../output/scores_through_2026.csv \
	../input/alderman_uncertainty_index_through2022.csv ../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../input/building_permits_clean_2023_2026.gpkg: ../../../clean_building_permits/output/building_permits_clean_2023_2026.gpkg | ../input
	ln -sf $< $@
../input/ward_panel.gpkg: ../../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@
../input/Wards_2024.geojson: ../../../../data_raw/Wards_2024.geojson | ../input
	ln -sf $< $@
../input/ward_controls.csv: ../../../create_ward_controls/output/ward_controls_2006_2022.csv | ../input
	ln -sf $< $@
../input/community_areas.geojson: ../../../download_chicago_spatial_data/output/community_areas.geojson | ../input
	ln -sf $< $@
../input/cta_stops.gpkg: ../../../prepare_amenities_data/output/cta_stops.gpkg | ../input
	ln -sf $< $@
../input/gis_osm_water_a_free_1.%: ../../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	ln -sf $< $@
../input/permits_for_uncertainty_index.csv: ../../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	ln -sf $< $@
../input/alderman_uncertainty_index_through2022.csv: ../../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@
../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@
../input/rental_rd_characteristics_panel_bw1500.parquet: ../../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@
../input/sales_with_hedonics_amenities.parquet: ../../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_clean_2023_2026.gpkg ../input/ward_panel.gpkg ../input/Wards_2024.geojson \
	../input/ward_controls.csv ../input/community_areas.geojson ../input/cta_stops.gpkg ../input/gis_osm_water_a_free_1.shp \
	../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx \
	../input/gis_osm_water_a_free_1.cpg ../input/permits_for_uncertainty_index.csv \
	../input/alderman_uncertainty_index_through2022.csv ../input/new_construction_analysis_data.csv \
	../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet

include ../../../shared/code/generic.make
