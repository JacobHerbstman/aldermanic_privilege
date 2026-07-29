all: ../output/sales_pre_scores.csv

../output/sales_pre_scores.csv: calculate_sale_distances.R ../../_lib/canonical_geometry_helpers.R ../input/parcel_sales.csv ../input/parcel_universe_2025_city.csv ../input/historical_sale_parcel_coordinates_2006_2022.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_terms.csv ../input/fred_cpi_cuura207sa0.csv | ../output
	Rscript $<

../input/parcel_sales.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/historical_sale_parcel_coordinates_2006_2022.csv: ../../download_historical_sale_parcel_coordinates/output/historical_sale_parcel_coordinates_2006_2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_terms.csv: ../../create_alderman_data/output/chicago_alderman_terms.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/fred_cpi_cuura207sa0.csv: ../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcel_sales.csv ../input/parcel_universe_2025_city.csv ../input/historical_sale_parcel_coordinates_2006_2022.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_terms.csv ../input/fred_cpi_cuura207sa0.csv

include ../../generic.make
