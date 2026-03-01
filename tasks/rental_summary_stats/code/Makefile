SHELL := bash
include ../../shell_functions.make

all: ../output/ward42_neighbors_definition.csv \
	../output/yearly_rent_trends.csv \
	../output/yearly_building_type_shares.csv \
	../output/yearly_building_type_other_missing_diagnostics.csv \
	../output/yearly_size_bin_shares.csv \
	../output/overall_area_stats.csv \
	../output/yearly_leave_out_comparison.csv \
	../output/fig_ward42_neighbor_maps.pdf \
	../output/fig_yearly_median_rent_trends.pdf \
	../output/fig_yearly_rent_distribution_ribbon.pdf \
	../output/fig_yearly_area_share.pdf \
	../output/fig_building_type_shares.pdf \
	../output/fig_size_bin_shares.pdf

../output/ward42_neighbors_definition.csv \
../output/yearly_rent_trends.csv \
../output/yearly_building_type_shares.csv \
../output/yearly_building_type_other_missing_diagnostics.csv \
../output/yearly_size_bin_shares.csv \
../output/overall_area_stats.csv \
../output/yearly_leave_out_comparison.csv \
../output/fig_ward42_neighbor_maps.pdf \
../output/fig_yearly_median_rent_trends.pdf \
../output/fig_yearly_rent_distribution_ribbon.pdf \
../output/fig_yearly_area_share.pdf \
../output/fig_building_type_shares.pdf \
../output/fig_size_bin_shares.pdf: run_rental_summary_stats.R ../input/rental_listing_panel.parquet ../input/chicago_rent_panel.parquet ../input/ward_panel.gpkg | ../output
	$(R) run_rental_summary_stats.R

../input/rental_listing_panel.parquet: ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

../input/chicago_rent_panel.parquet: ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/rental_listing_panel.parquet ../input/chicago_rent_panel.parquet ../input/ward_panel.gpkg

include ../../generic.make
