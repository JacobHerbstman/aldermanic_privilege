SHELL := bash
include ../../shell_functions.make

all: \
	../output/land_transaction_citywide_incidence_summary.csv \
	../output/land_transaction_citywide_price_summary.csv \
	../output/land_transaction_map_summary.csv \
	../output/land_transaction_price_distribution_summary.csv \
	../output/land_transaction_citywide_incidence_series.pdf \
	../output/land_transaction_citywide_incidence_series.png \
	../output/land_transaction_citywide_price_series.pdf \
	../output/land_transaction_citywide_price_series.png \
	../output/land_transaction_ward_incidence_maps.pdf \
	../output/land_transaction_ward_incidence_maps.png \
	../output/land_transaction_community_incidence_maps.pdf \
	../output/land_transaction_community_incidence_maps.png \
	../output/land_transaction_ward_price_maps.pdf \
	../output/land_transaction_ward_price_maps.png \
	../output/land_transaction_community_price_maps.pdf \
	../output/land_transaction_community_price_maps.png \
	../output/land_transaction_price_distributions.pdf \
	../output/land_transaction_price_distributions.png

../output/land_transaction_citywide_incidence_summary.csv: build_land_transaction_sanity_checks.R ../../_lib/aggregate_geography_helpers.R ../input/land_transaction_sale_events.parquet ../input/land_transaction_incidence_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson | ../output
	$(R) build_land_transaction_sanity_checks.R ../input/land_transaction_sale_events.parquet ../input/land_transaction_incidence_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson ../output

../output/land_transaction_citywide_price_summary.csv \
../output/land_transaction_map_summary.csv \
../output/land_transaction_price_distribution_summary.csv \
../output/land_transaction_citywide_incidence_series.pdf \
../output/land_transaction_citywide_incidence_series.png \
../output/land_transaction_citywide_price_series.pdf \
../output/land_transaction_citywide_price_series.png \
../output/land_transaction_ward_incidence_maps.pdf \
../output/land_transaction_ward_incidence_maps.png \
../output/land_transaction_community_incidence_maps.pdf \
../output/land_transaction_community_incidence_maps.png \
../output/land_transaction_ward_price_maps.pdf \
../output/land_transaction_ward_price_maps.png \
../output/land_transaction_community_price_maps.pdf \
../output/land_transaction_community_price_maps.png \
../output/land_transaction_price_distributions.pdf \
../output/land_transaction_price_distributions.png: ../output/land_transaction_citywide_incidence_summary.csv
	test -f $@

../input/land_transaction_sale_events.parquet: ../../build_land_transaction_event_data/output/land_transaction_sale_events.parquet | ../input
	ln -sf $< $@

../input/land_transaction_incidence_panel.parquet: ../../build_land_transaction_incidence_panel/output/land_transaction_incidence_panel.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

../input/community_areas.geojson: ../../../data_raw/Boundaries_-_Community_Areas_20250920.geojson | ../input
	ln -sf $< $@

include ../../generic.make

link-inputs: ../input/land_transaction_sale_events.parquet ../input/land_transaction_incidence_panel.parquet ../input/ward_panel.gpkg ../input/community_areas.geojson
