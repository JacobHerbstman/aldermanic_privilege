include ../../shared/code/shell_functions.make

OFFICIAL_PROPERTY_TYPE_FE = TRUE
START_YEAR = 2006
END_YEAR = 2022
BANDWIDTH_FT = 500
BIN_WIDTH_FT = 100
PLACEBO_FT = 1000
DONUT_INNER_FT = 25
DONUT_OUTER_FT = 50
RENT_CONTROLS = log_sqft + beds_factor + log_baths + nearest_school_dist_kft + nearest_park_dist_kft + nearest_major_road_dist_kft + nearest_cta_stop_dist_kft + lake_michigan_dist_kft
SALES_CONTROLS = log_sqft + log_land_sqft + log_building_age + log_bedrooms + log_baths + has_garage + nearest_school_dist_ft + nearest_park_dist_ft + nearest_major_road_dist_ft + nearest_cta_stop_dist_ft + lake_michigan_dist_ft
RENT_FIXED_EFFECTS = segment_id^year_month
SALES_FIXED_EFFECTS = segment_id^year_quarter
CLUSTER = ward_pair
RENT_START_YEAR = 2014

all: ../output/price_boundary_main.pdf \
	../output/price_boundary_placebos.pdf \
	../output/price_boundary_straight.pdf \
	../output/price_boundary_donuts.pdf

../%/price_boundary_main.pdf \
	../%/price_boundary_placebos.pdf \
	../%/price_boundary_straight.pdf \
	../%/price_boundary_donuts.pdf: build_price_boundary_results.R Makefile ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R ../../shared/code/price_boundary_helpers.R ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/ward_pair_boundaries.gpkg ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $< "$(OFFICIAL_PROPERTY_TYPE_FE)" $(START_YEAR) $(END_YEAR) $(BANDWIDTH_FT) $(BIN_WIDTH_FT) $(PLACEBO_FT) $(DONUT_INNER_FT) $(DONUT_OUTER_FT) "$(RENT_CONTROLS)" "$(SALES_CONTROLS)" "$(RENT_FIXED_EFFECTS)" "$(SALES_FIXED_EFFECTS)" "$(CLUSTER)" $(RENT_START_YEAR)

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	ln -sf $< $@

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	ln -sf $< $@

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet \
	../input/ward_pair_boundaries.gpkg

include ../../shared/code/generic.make
