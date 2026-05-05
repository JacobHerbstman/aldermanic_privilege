SHELL := bash
include ../../shell_functions.make

UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022
MAP_YEAR ?= 2022
MAP_SERIES_END_YEAR ?= 2022

all: ../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/ward_alderman_summary_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/within_ward_dotplot_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/variance_decomposition_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/low_vs_high_processing_correlation_summary_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/residualized_low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/residualized_low_vs_high_processing_correlation_summary_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/low_discretion_falsification_summary_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/low_discretion_falsification_uncertainty_$(UNCERTAINTY_SPEC).tex \
	../output/low_discretion_falsification_compact_uncertainty_$(UNCERTAINTY_SPEC).tex \
	../output/split_half_stability_scores_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/split_half_stability_summary_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/split_half_stability_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf: analyze_within_ward_variation.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv | ../output
	Rscript analyze_within_ward_variation.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv uncertainty_index "Regulatory Stringency Index" uncertainty_$(UNCERTAINTY_SPEC)

../output/ward_alderman_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/within_ward_dotplot_uncertainty_$(UNCERTAINTY_SPEC).pdf ../output/variance_decomposition_uncertainty_$(UNCERTAINTY_SPEC).csv: ../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../output/low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf: analyze_low_vs_high_processing_correlation.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	Rscript analyze_low_vs_high_processing_correlation.R $(UNCERTAINTY_SPEC) ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../output/low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/low_vs_high_processing_correlation_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../output/low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/low_vs_high_processing_correlation_summary_uncertainty_$(UNCERTAINTY_SPEC).csv: ../output/low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../output/residualized_low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf: analyze_residualized_high_low_processing_correlation.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/cta_stations.geojson ../input/gis_osm_water_a_free_1.shp | ../output
	Rscript analyze_residualized_high_low_processing_correlation.R $(UNCERTAINTY_SPEC) ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/cta_stations.geojson ../input/gis_osm_water_a_free_1.shp ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/residualized_low_vs_high_processing_correlation_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/residualized_low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/residualized_low_vs_high_processing_correlation_summary_uncertainty_$(UNCERTAINTY_SPEC).csv: ../output/residualized_low_vs_high_processing_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../output/low_discretion_falsification_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/low_discretion_falsification_uncertainty_$(UNCERTAINTY_SPEC).tex ../output/low_discretion_falsification_compact_uncertainty_$(UNCERTAINTY_SPEC).tex: run_low_discretion_falsification_regression.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv | ../output
	Rscript run_low_discretion_falsification_regression.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/low_discretion_falsification_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/low_discretion_falsification_uncertainty_$(UNCERTAINTY_SPEC).tex ../output/low_discretion_falsification_compact_uncertainty_$(UNCERTAINTY_SPEC).tex

../output/split_half_stability_scores_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/split_half_stability_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/split_half_stability_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf: analyze_split_half_tenure_stability.R ../input/permits_for_uncertainty_index.csv ../input/chicago_alderman_panel.csv | ../output
	Rscript analyze_split_half_tenure_stability.R $(UNCERTAINTY_SPEC) ../input/permits_for_uncertainty_index.csv ../input/chicago_alderman_panel.csv ../output/split_half_stability_scores_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/split_half_stability_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/split_half_stability_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf

../output/residualized_high_low_map_uncertainty_$(UNCERTAINTY_SPEC)_2022.pdf ../output/residualized_high_low_map_uncertainty_$(UNCERTAINTY_SPEC)_2022.png ../output/residualized_high_low_map_data_uncertainty_$(UNCERTAINTY_SPEC)_2022.csv ../output/residualized_high_low_map_summary_uncertainty_$(UNCERTAINTY_SPEC)_2022.csv: map_residualized_high_low_ward_year.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/cta_stations.geojson ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript map_residualized_high_low_ward_year.R $(UNCERTAINTY_SPEC) 2022 ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/cta_stations.geojson ../input/gis_osm_water_a_free_1.shp ../output/residualized_high_low_map_data_uncertainty_$(UNCERTAINTY_SPEC)_2022.csv ../output/residualized_high_low_map_summary_uncertainty_$(UNCERTAINTY_SPEC)_2022.csv ../output/residualized_high_low_map_uncertainty_$(UNCERTAINTY_SPEC)_2022.pdf ../output/residualized_high_low_map_uncertainty_$(UNCERTAINTY_SPEC)_2022.png

../output/residualized_high_low_current_map_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).pdf ../output/residualized_high_low_current_map_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).png ../output/residualized_high_low_current_map_data_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).csv ../output/residualized_high_low_current_map_summary_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).csv: map_residualized_high_low_current_aldermen.R ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	Rscript map_residualized_high_low_current_aldermen.R ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv $(MAP_YEAR) ../output/residualized_high_low_current_map_data_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).csv ../output/residualized_high_low_current_map_summary_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).csv ../output/residualized_high_low_current_map_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).pdf ../output/residualized_high_low_current_map_uncertainty_$(UNCERTAINTY_SPEC)_$(MAP_YEAR).png

../output/residualized_high_low_map_year_series_uncertainty_$(UNCERTAINTY_SPEC).pdf ../output/residualized_high_low_map_year_series_uncertainty_$(UNCERTAINTY_SPEC).png ../output/residualized_high_low_map_year_series_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/residualized_high_low_map_period_summary_uncertainty_$(UNCERTAINTY_SPEC).csv: summarize_residualized_high_low_map_years.R ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv | ../output
	Rscript summarize_residualized_high_low_map_years.R ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv $(MAP_SERIES_END_YEAR) ../output/residualized_high_low_map_year_series_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/residualized_high_low_map_period_summary_uncertainty_$(UNCERTAINTY_SPEC).csv ../output/residualized_high_low_map_year_series_uncertainty_$(UNCERTAINTY_SPEC).pdf ../output/residualized_high_low_map_year_series_uncertainty_$(UNCERTAINTY_SPEC).png

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/permits_for_uncertainty_index.csv: ../../data_for_alderman_uncertainty_index/output/permits_for_uncertainty_index.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_controls.csv: ../../create_ward_controls/output/ward_controls_2000_2023.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/cta_stations.geojson: ../../../data_raw/cta_stations.geojson | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg: ../input/gis_osm_water_a_free_1.%: ../../../data_raw/illinois-250919-free/gis_osm_water_a_free_1.% | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/permits_for_uncertainty_index.csv ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/ward_controls.csv ../input/cta_stations.geojson ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../generic.make
