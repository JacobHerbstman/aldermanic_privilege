SHELL := bash
include ../../shell_functions.make

OUTPUTS := \
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
	../output/fig_size_bin_shares.pdf

SCRIPT := run_rental_summary_stats.R

INPUT_SAMPLE := ../input/rental_listing_panel.parquet
INPUT_FULL := ../input/chicago_rent_panel.parquet
INPUT_WARDS := ../input/ward_panel.gpkg

all: $(OUTPUTS)

$(OUTPUTS): $(SCRIPT) $(INPUT_SAMPLE) $(INPUT_FULL) $(INPUT_WARDS) | ../output
	$(R) $(SCRIPT)

$(INPUT_SAMPLE): ../../create_event_study_rental_data_disaggregate/output/rental_listing_panel.parquet | ../input
	ln -sf $< $@

$(INPUT_FULL): ../../process_rent_data/output/chicago_rent_panel.parquet | ../input
	ln -sf $< $@

$(INPUT_WARDS): ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@

link-inputs: $(INPUT_SAMPLE) $(INPUT_FULL) $(INPUT_WARDS)

include ../../generic.make
