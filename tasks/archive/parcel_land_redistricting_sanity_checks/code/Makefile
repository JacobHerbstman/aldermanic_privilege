SHELL := bash
include ../../shell_functions.make

all: ../output/parcel_land_citywide_taxyear_summary.csv ../output/parcel_land_geography_taxyear_summary.csv ../output/parcel_land_citywide_series_panel.png ../output/parcel_land_ward_heatmap_panel.png ../output/parcel_land_community_area_heatmap_panel.png

../output/parcel_land_citywide_taxyear_summary.csv: build_sanity_checks.R ../../_lib/aggregate_geography_helpers.R ../input/parcel_land_redistricting_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	$(R) build_sanity_checks.R ../input/parcel_land_redistricting_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson ../output/parcel_land_citywide_taxyear_summary.csv ../output/parcel_land_geography_taxyear_summary.csv ../output/parcel_land_citywide_series_panel.pdf ../output/parcel_land_citywide_series_panel.png ../output/parcel_land_ward_heatmap_panel.pdf ../output/parcel_land_ward_heatmap_panel.png ../output/parcel_land_community_area_heatmap_panel.pdf ../output/parcel_land_community_area_heatmap_panel.png

../output/parcel_land_geography_taxyear_summary.csv \
../output/parcel_land_citywide_series_panel.pdf \
../output/parcel_land_citywide_series_panel.png \
../output/parcel_land_ward_heatmap_panel.pdf \
../output/parcel_land_ward_heatmap_panel.png \
../output/parcel_land_community_area_heatmap_panel.pdf \
../output/parcel_land_community_area_heatmap_panel.png: ../output/parcel_land_citywide_taxyear_summary.csv
	test -f $@

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_redistricting_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson
