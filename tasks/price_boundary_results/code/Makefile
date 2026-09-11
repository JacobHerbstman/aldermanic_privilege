include ../../shared/code/shell_functions.make

OFFICIAL_PROPERTY_TYPE_FE = TRUE

all: ../output/price_boundary_main.pdf \
	../output/price_boundary_placebos.pdf \
	../output/price_boundary_straight.pdf \
	../output/price_boundary_donuts.pdf \
	../output/price_boundary_property_type_fe_comparison.pdf \
	../output/price_boundary_property_type_fe_estimates.csv \
	../report/price_boundary_property_type_fe_estimates.csv.log

ifneq ($(filter-out $(wildcard ../output/price_boundary_main.pdf ../output/price_boundary_placebos.pdf ../output/price_boundary_straight.pdf ../output/price_boundary_donuts.pdf ../output/price_boundary_property_type_fe_comparison.pdf ../output/price_boundary_property_type_fe_estimates.csv),../output/price_boundary_main.pdf ../output/price_boundary_placebos.pdf ../output/price_boundary_straight.pdf ../output/price_boundary_donuts.pdf ../output/price_boundary_property_type_fe_comparison.pdf ../output/price_boundary_property_type_fe_estimates.csv),)
.PHONY: ../output/price_boundary_main.pdf
endif

../output/price_boundary_main.pdf: build_price_boundary_results.R Makefile ../../setup_environment/code/packages.R ../../shared/code/canonical_geometry_helpers.R ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/ward_pair_boundaries.gpkg | ../output
	$(R) $< $(OFFICIAL_PROPERTY_TYPE_FE)

../output/price_boundary_placebos.pdf: ../output/price_boundary_main.pdf

../output/price_boundary_straight.pdf: ../output/price_boundary_main.pdf

../output/price_boundary_donuts.pdf: ../output/price_boundary_main.pdf

../output/price_boundary_property_type_fe_comparison.pdf: ../output/price_boundary_main.pdf

../output/price_boundary_property_type_fe_estimates.csv: ../output/price_boundary_main.pdf

../report/price_boundary_property_type_fe_estimates.csv.log: ../../shared/code/report.py ../output/price_boundary_main.pdf ../output/price_boundary_property_type_fe_estimates.csv | ../report
	$(PYTHON) $< ../output/price_boundary_property_type_fe_estimates.csv $@ market specification

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet \
	../input/ward_pair_boundaries.gpkg

include ../../shared/code/generic.make
