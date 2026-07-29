all: ../output/price_boundary_main.pdf \
	../output/price_boundary_placebos.pdf \
	../output/price_boundary_straight.pdf \
	../output/price_boundary_donuts.pdf

../output/price_boundary_main.pdf: build_price_boundary_results.R ../../_lib/canonical_geometry_helpers.R ../input/rental_rd_characteristics_panel_bw1500.parquet ../input/sales_with_hedonics_amenities.parquet ../input/ward_pair_boundaries.gpkg | ../output
	$(R) $<

../output/price_boundary_placebos.pdf: ../output/price_boundary_main.pdf
	@test -s "$@"

../output/price_boundary_straight.pdf: ../output/price_boundary_main.pdf
	@test -s "$@"

../output/price_boundary_donuts.pdf: ../output/price_boundary_main.pdf
	@test -s "$@"

../input/rental_rd_characteristics_panel_bw1500.parquet: ../../rental_rd_characteristics/output/rental_rd_characteristics_panel_bw1500.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/sales_with_hedonics_amenities.parquet: ../../sales_border_pair_fe/output/sales_with_hedonics_amenities.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/rental_rd_characteristics_panel_bw1500.parquet \
	../input/sales_with_hedonics_amenities.parquet \
	../input/ward_pair_boundaries.gpkg

include ../../shell_functions.make
include ../../generic.make
