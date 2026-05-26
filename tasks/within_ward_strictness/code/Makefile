SHELL := bash

UNCERTAINTY_SPEC ?= ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022

all: ../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf \
	../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv \
	../output/low_discretion_falsification_compact_uncertainty_$(UNCERTAINTY_SPEC).tex

../output/predecessor_successor_scatter_uncertainty_$(UNCERTAINTY_SPEC).pdf: analyze_within_ward_variation.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv | ../output
	Rscript analyze_within_ward_variation.R $(UNCERTAINTY_SPEC)

../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv: analyze_residualized_high_low_processing_correlation.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/chicago_alderman_panel.csv ../input/ward_controls.csv ../input/cta_stations.geojson ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg | ../output
	Rscript analyze_residualized_high_low_processing_correlation.R $(UNCERTAINTY_SPEC)

../output/low_discretion_falsification_compact_uncertainty_$(UNCERTAINTY_SPEC).tex: run_low_discretion_falsification_regression.R ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../output/residualized_low_vs_high_processing_alderman_wide_uncertainty_$(UNCERTAINTY_SPEC).csv | ../output
	Rscript run_low_discretion_falsification_regression.R $(UNCERTAINTY_SPEC)

../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
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

link-inputs: ../input/alderman_uncertainty_index_$(UNCERTAINTY_SPEC).csv ../input/chicago_alderman_panel.csv ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg ../input/ward_controls.csv ../input/cta_stations.geojson ../input/gis_osm_water_a_free_1.shp ../input/gis_osm_water_a_free_1.dbf ../input/gis_osm_water_a_free_1.prj ../input/gis_osm_water_a_free_1.shx ../input/gis_osm_water_a_free_1.cpg

include ../../shell_functions.make
include ../../generic.make
