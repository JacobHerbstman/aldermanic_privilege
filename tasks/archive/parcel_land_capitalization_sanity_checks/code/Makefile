SHELL := bash
include ../../shell_functions.make

all: \
	../output/parcel_land_capitalization_support_summary.csv \
	../output/parcel_land_capitalization_diagnostics.csv \
	../output/parcel_land_capitalization_distribution_summary.csv \
	../output/parcel_land_capitalization_map_summary.csv \
	../output/parcel_land_capitalization_change_summary.csv \
	../output/parcel_land_capitalization_distribution_levels.pdf \
	../output/parcel_land_capitalization_distribution_levels.png \
	../output/parcel_land_capitalization_distribution_logs.pdf \
	../output/parcel_land_capitalization_distribution_logs.png \
	../output/parcel_land_capitalization_ward_count_maps.pdf \
	../output/parcel_land_capitalization_ward_count_maps.png \
	../output/parcel_land_capitalization_ward_land_psf_maps.pdf \
	../output/parcel_land_capitalization_ward_land_psf_maps.png \
	../output/parcel_land_capitalization_community_count_maps.pdf \
	../output/parcel_land_capitalization_community_count_maps.png \
	../output/parcel_land_capitalization_community_land_psf_maps.pdf \
	../output/parcel_land_capitalization_community_land_psf_maps.png \
	../output/parcel_land_capitalization_ward_count_change_maps.pdf \
	../output/parcel_land_capitalization_ward_count_change_maps.png \
	../output/parcel_land_capitalization_ward_land_psf_change_maps.pdf \
	../output/parcel_land_capitalization_ward_land_psf_change_maps.png \
	../output/parcel_land_capitalization_community_count_change_maps.pdf \
	../output/parcel_land_capitalization_community_count_change_maps.png \
	../output/parcel_land_capitalization_community_land_psf_change_maps.pdf \
	../output/parcel_land_capitalization_community_land_psf_change_maps.png

../output/parcel_land_capitalization_support_summary.csv: build_capitalization_sanity_checks.R ../../_lib/aggregate_geography_helpers.R ../input/parcel_land_redistricting_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	$(R) build_capitalization_sanity_checks.R ../input/parcel_land_redistricting_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson ../output

../output/parcel_land_capitalization_diagnostics.csv \
../output/parcel_land_capitalization_distribution_summary.csv \
../output/parcel_land_capitalization_map_summary.csv \
../output/parcel_land_capitalization_change_summary.csv \
../output/parcel_land_capitalization_distribution_levels.pdf \
../output/parcel_land_capitalization_distribution_levels.png \
../output/parcel_land_capitalization_distribution_logs.pdf \
../output/parcel_land_capitalization_distribution_logs.png \
../output/parcel_land_capitalization_ward_count_maps.pdf \
../output/parcel_land_capitalization_ward_count_maps.png \
../output/parcel_land_capitalization_ward_land_psf_maps.pdf \
../output/parcel_land_capitalization_ward_land_psf_maps.png \
../output/parcel_land_capitalization_community_count_maps.pdf \
../output/parcel_land_capitalization_community_count_maps.png \
../output/parcel_land_capitalization_community_land_psf_maps.pdf \
../output/parcel_land_capitalization_community_land_psf_maps.png \
../output/parcel_land_capitalization_ward_count_change_maps.pdf \
../output/parcel_land_capitalization_ward_count_change_maps.png \
../output/parcel_land_capitalization_ward_land_psf_change_maps.pdf \
../output/parcel_land_capitalization_ward_land_psf_change_maps.png \
../output/parcel_land_capitalization_community_count_change_maps.pdf \
../output/parcel_land_capitalization_community_count_change_maps.png \
../output/parcel_land_capitalization_community_land_psf_change_maps.pdf \
../output/parcel_land_capitalization_community_land_psf_change_maps.png: ../output/parcel_land_capitalization_support_summary.csv
	test -f $@

../input/parcel_land_redistricting_panel.parquet: ../../create_parcel_land_redistricting_dataset/output/parcel_land_redistricting_panel.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/parcel_land_redistricting_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson
