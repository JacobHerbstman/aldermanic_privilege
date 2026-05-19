SHELL := bash

SAMPLE := FALSE

all: \
	../output/sales_pre_scores.csv \
	../output/sales_geometry_diagnostics.csv \
	../output/sales_geometry_contract_audit.csv \
	../output/sales_ward_hit_multiplicity_audit.csv \
	../output/sales_ward_hit_multiplicity_detail.csv

../output/sales_pre_scores.csv: calculate_sale_distances.R ../../_lib/canonical_geometry_helpers.R ../input/parcel_sales.csv ../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/fred_cpi_cuura207sa0.csv | ../output
	# arg order: sample
	$(R) calculate_sale_distances.R $(SAMPLE)

../output/sales_geometry_diagnostics.csv \
../output/sales_geometry_contract_audit.csv \
../output/sales_ward_hit_multiplicity_audit.csv \
../output/sales_ward_hit_multiplicity_detail.csv: ../output/sales_pre_scores.csv
	@test -s "$@"

../input/parcel_sales.csv: ../../download_parcel_sales_data/output/parcel_sales_city.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/fred_cpi_cuura207sa0.csv: ../../download_fred_cpi/output/fred_cpi_cuura207sa0.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"


link-inputs: ../input/parcel_sales.csv ../input/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg ../input/chicago_alderman_panel.csv ../input/fred_cpi_cuura207sa0.csv

include ../../shell_functions.make
include ../../generic.make
