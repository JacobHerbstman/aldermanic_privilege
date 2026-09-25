include ../../shared/code/shell_functions.make

all: ../output/price_boundary_main.pdf \
	../output/price_boundary_placebos.pdf \
	../output/price_boundary_straight.pdf \
	../output/price_boundary_donuts.pdf

../%/price_boundary_main.pdf \
	../%/price_boundary_placebos.pdf \
	../%/price_boundary_straight.pdf \
	../%/price_boundary_donuts.pdf: build_price_boundary_results.R ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet ../input/ward_pair_boundaries.gpkg | ../output
	$(R) $<

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
