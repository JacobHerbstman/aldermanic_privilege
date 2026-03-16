SHELL := bash
include ../../shell_functions.make

all: \
	../output/char_fe_table_bw500.tex \
	../output/rent_with_ward_distances_amenities.parquet \
	../output/rental_amenity_distance_diagnostics.csv \
	../output/amenity_char_fe_table_bw500_pre_2023.tex \
	../output/amenity_char_fe_table_bw500_pre_2023.csv \
	../output/amenity_char_fe_table_bwall_pre_2023.tex \
	../output/amenity_char_fe_table_bwall_pre_2023.csv \
	../output/rent_amenity_attenuation_bw500_pre_2023.tex \
	../output/rent_amenity_attenuation_bw500_pre_2023.csv \
	../output/rent_amenity_attenuation_bwall_pre_2023.tex \
	../output/rent_amenity_attenuation_bwall_pre_2023.csv \
	../output/listing_density_bw500_pre_2023_all_pct0.pdf \
	../output/listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf \
	../output/listing_units_bw500_pre_2023_all_pct0.pdf \
	../output/listing_units_side_comparison_bw500_pre_2023.pdf \
	../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.pdf \
	../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.csv \
	../output/listing_units_fe_table_bw500_pre_2023_all_pct0.tex \
	../output/listing_units_fe_table_bw500_pre_2023_all_pct0.csv \
	../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex \
	../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv \
	../output/listing_units_ppml_robustness_bw500_pre_2023.tex \
	../output/listing_units_ppml_robustness_bw500_pre_2023.csv \
	../output/listing_units_ppml_main_bw500_pre_2023.tex \
	../output/listing_units_ppml_main_bw500_pre_2023.csv

../output/char_fe_table_bw500.tex: characteristic_fe_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript characteristic_fe_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 "$@" ../output/char_fe_table_bw500.csv

../output/rent_with_ward_distances_amenities.parquet: enrich_rent_with_amenity_distances.R ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript enrich_rent_with_amenity_distances.R ../input/rent_with_ward_distances.parquet ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp "$@" ../output/rental_amenity_distance_diagnostics.csv

../output/rental_amenity_distance_diagnostics.csv: ../output/rent_with_ward_distances_amenities.parquet
	test -f "$@"

../output/amenity_char_fe_table_bw500_pre_2023.tex: amenity_characteristic_fe_table.R ../output/rent_with_ward_distances_amenities.parquet | ../output
	Rscript amenity_characteristic_fe_table.R ../output/rent_with_ward_distances_amenities.parquet 500 pre_2023 "$@" ../output/amenity_char_fe_table_bw500_pre_2023.csv

../output/amenity_char_fe_table_bw500_pre_2023.csv: ../output/amenity_char_fe_table_bw500_pre_2023.tex

../output/amenity_char_fe_table_bwall_pre_2023.tex: amenity_characteristic_fe_table.R ../output/rent_with_ward_distances_amenities.parquet | ../output
	Rscript amenity_characteristic_fe_table.R ../output/rent_with_ward_distances_amenities.parquet all pre_2023 "$@" ../output/amenity_char_fe_table_bwall_pre_2023.csv

../output/amenity_char_fe_table_bwall_pre_2023.csv: ../output/amenity_char_fe_table_bwall_pre_2023.tex

../output/rent_amenity_attenuation_bw500_pre_2023.tex: rent_amenity_attenuation.R ../output/rent_with_ward_distances_amenities.parquet | ../output
	Rscript rent_amenity_attenuation.R ../output/rent_with_ward_distances_amenities.parquet 500 pre_2023 "$@" ../output/rent_amenity_attenuation_bw500_pre_2023.csv

../output/rent_amenity_attenuation_bw500_pre_2023.csv: ../output/rent_amenity_attenuation_bw500_pre_2023.tex

../output/rent_amenity_attenuation_bwall_pre_2023.tex: rent_amenity_attenuation.R ../output/rent_with_ward_distances_amenities.parquet | ../output
	Rscript rent_amenity_attenuation.R ../output/rent_with_ward_distances_amenities.parquet all pre_2023 "$@" ../output/rent_amenity_attenuation_bwall_pre_2023.csv

../output/rent_amenity_attenuation_bwall_pre_2023.csv: ../output/rent_amenity_attenuation_bwall_pre_2023.tex

../output/listing_density_bw500_pre_2023_all_pct0.pdf: listing_density_plot.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript listing_density_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all 0 15 "$@"

../output/listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf: listing_units_plot_cached.R ../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet ../output/listing_units_bin_cells_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.parquet | ../output
	Rscript listing_units_plot_cached.R ../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet ../output/listing_units_bin_cells_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.parquet "$@"

../output/listing_units_bw500_pre_2023_all_pct0.pdf: ../output/listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf | ../output
	ln -sf "listing_units_unit_proxy_bw500_pre_2023_all_pct0.pdf" "$@"

../output/listing_units_side_comparison_bw500_pre_2023.pdf: listing_units_side_comparison.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript listing_units_side_comparison.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy 0 "$@"

../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.pdf: listing_units_matched_pair_ratio_plot.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	Rscript listing_units_matched_pair_ratio_plot.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy 0 "$@" ../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.csv

../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.csv: ../output/listing_units_matched_pair_ratio_bw500_pre_2023_all_pct0_unit_proxy.pdf

../output/listing_units_fe_table_bw500_pre_2023_all_pct0.tex: listing_units_fe_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_units_fe_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all 0 "$@" ../output/listing_units_fe_table_bw500_pre_2023_all_pct0.csv

../output/listing_units_fe_table_bw500_pre_2023_all_pct0.csv: ../output/listing_units_fe_table_bw500_pre_2023_all_pct0.tex

../output/listing_units_cache_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.csv: build_listing_units_cache.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript build_listing_units_cache.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy 0 8 all NA ../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet ../output/listing_units_bin_cells_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.parquet "$@"

../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet: ../output/listing_units_cache_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.csv
	test -f "$@"

../output/listing_units_bin_cells_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.parquet: ../output/listing_units_cache_bw500_pre_2023_all_pct0_unit_proxy_bins8_all.csv
	test -f "$@"

../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex: listing_units_ppml_fe_table_cached.R ../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet | ../output
	Rscript listing_units_ppml_fe_table_cached.R ../output/listing_units_side_panel_bw500_pre_2023_all_pct0_unit_proxy_all.parquet "$@" ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv

../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.csv: ../output/listing_units_ppml_fe_table_bw500_pre_2023_all_pct0.tex

../output/listing_units_ppml_robustness_bw500_pre_2023.tex: listing_units_ppml_robustness.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_units_ppml_robustness.R ../input/rent_with_ward_distances.parquet 500 pre_2023 "$@" ../output/listing_units_ppml_robustness_bw500_pre_2023.csv

../output/listing_units_ppml_robustness_bw500_pre_2023.csv: ../output/listing_units_ppml_robustness_bw500_pre_2023.tex

../output/listing_units_ppml_main_bw500_pre_2023.tex: listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet | ../output
	Rscript listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy "$@" ../output/listing_units_ppml_main_bw500_pre_2023.csv

../output/listing_units_ppml_main_bw500_pre_2023.csv: ../output/listing_units_ppml_main_bw500_pre_2023.tex

../output/listing_units_ppml_main_bw500_pre_2023_pruned.tex: listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv | ../output
	PRUNE_SAMPLE=pruned CONFOUND_FLAGS_PATH=../input/confounded_pair_era_flags.csv Rscript listing_units_ppml_main_table.R ../input/rent_with_ward_distances.parquet 500 pre_2023 all unit_proxy "$@" ../output/listing_units_ppml_main_bw500_pre_2023_pruned.csv

../output/listing_units_ppml_main_bw500_pre_2023_pruned.csv: ../output/listing_units_ppml_main_bw500_pre_2023_pruned.tex

../input/rent_with_ward_distances.parquet: ../../merge_event_study_scores/output/rent_with_ward_distances_full.parquet | ../input
	ln -sf "$<" "$@"

../input/confounded_pair_era_flags.csv: ../../border_confound_pruning/output/confounded_pair_era_flags.csv | ../input
	ln -sf "$<" "$@"

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	ln -sf "$<" "$@"

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	ln -sf "$<" "$@"

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shp | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.dbf: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.dbf | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.prj: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.prj | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shx: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.shx | ../input
	ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.cpg: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.cpg | ../input
	ln -sf "$<" "$@"

link-inputs: ../input/rent_with_ward_distances.parquet ../input/confounded_pair_era_flags.csv ../input/schools_2015.gpkg ../input/parks.gpkg ../input/major_streets.gpkg ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
