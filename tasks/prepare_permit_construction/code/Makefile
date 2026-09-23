include ../../shared/code/shell_functions.make

FIRST_ISSUE_YEAR := 2006
LAST_ISSUE_YEAR := 2022
FIRST_YEAR_BUILT := 2006
LAST_YEAR_BUILT := 2022
LAST_UNPERMITTED_YEAR_BUILT := 2007
REPEAT_PERMIT_YEARS := 3
ASSESSOR_YEAR_LEAD := 2
MAX_BUILD_LAG_YEARS := 4
UNIT_TOLERANCE := 0.2
MIN_SQFT_PER_UNIT := 300
MAX_LAND_SQFT_PER_UNIT := 43560
REBUILT_AREA_GROWTH := 0.25
LOT_DISTANCE_FT := 150
TOWNHOUSE_DISTANCE_FT := 300
LARGE_BUILDING_UNITS := 20
LARGE_LOT_DISTANCE_FT := 300
BOUNDARY_WINDOW_FT := 1500
MAIN_BOUNDARY_WINDOW_FT := 500

all: ../output/construction_buildings.csv ../output/permit_construction.csv

../output/permit_construction.csv: build_ledger.R ../output/construction_buildings.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg \
	../../shared/code/canonical_geometry_helpers.R ../../shared/code/save_data.R | ../output ../report
	$(R) $< $(BOUNDARY_WINDOW_FT) $(MAIN_BOUNDARY_WINDOW_FT)

../output/construction_buildings.csv: build_construction_buildings.R construction_rules.R ../output/permit_buildings.csv \
	../output/construction_permits.csv ../adjudication/manual_decisions.csv \
	../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv ../input/commercial_valuation_data.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/parcel_centroids.csv ../../shared/code/assessor_classification.R \
	../../shared/code/normalize_chicago_address.R ../../shared/code/save_data.R | ../output ../report
	$(R) $< $(FIRST_YEAR_BUILT) $(LAST_YEAR_BUILT) $(LAST_UNPERMITTED_YEAR_BUILT) $(ASSESSOR_YEAR_LEAD) $(MAX_BUILD_LAG_YEARS) $(UNIT_TOLERANCE) \
		$(MIN_SQFT_PER_UNIT) $(MAX_LAND_SQFT_PER_UNIT) $(LOT_DISTANCE_FT) $(TOWNHOUSE_DISTANCE_FT) \
		$(LARGE_BUILDING_UNITS) $(LARGE_LOT_DISTANCE_FT)

../output/permit_buildings.csv: measure_buildings.R construction_rules.R ../output/construction_permits.csv \
	../adjudication/manual_decisions.csv ../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv \
	../input/commercial_valuation_data.csv ../input/parcel_addresses_2025_chicago.csv ../../shared/code/normalize_chicago_address.R \
	../../shared/code/street_key.R ../../shared/code/assessor_classification.R ../../shared/code/save_data.R | ../output ../report
	$(R) $< $(ASSESSOR_YEAR_LEAD) $(MAX_BUILD_LAG_YEARS) $(UNIT_TOLERANCE) $(MIN_SQFT_PER_UNIT) $(MAX_LAND_SQFT_PER_UNIT) \
		$(REBUILT_AREA_GROWTH)

../output/construction_permits.csv: select_permits.R ../input/building_permits_full.csv ../../shared/code/normalize_chicago_address.R \
	../../shared/code/permit_unit_patterns.R ../../shared/code/save_data.R | ../output ../report
	$(R) $< $(FIRST_ISSUE_YEAR) $(LAST_ISSUE_YEAR) $(REPEAT_PERMIT_YEARS)

../input/building_permits_full.csv: ../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@
../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@
../input/condominium_characteristics.csv: ../../download_condominium_characteristics/output/condominium_characteristics.csv | ../input
	ln -sf $< $@
../input/commercial_valuation_data.csv: ../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@
../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@
../input/parcel_centroids.csv: ../../download_parcel_centroids/output/parcel_centroids.csv | ../input
	ln -sf $< $@
../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf $< $@
../input/ward_pair_boundaries.gpkg: ../../border_segment_creation/output/ward_pair_boundaries.gpkg | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_full.csv ../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv \
	../input/commercial_valuation_data.csv ../input/parcel_addresses_2025_chicago.csv ../input/parcel_centroids.csv ../input/ward_panel.gpkg ../input/ward_pair_boundaries.gpkg

include ../../shared/code/generic.make
