include ../../shared/code/shell_functions.make

all: ../output/new_buildings.csv

../output/new_buildings.csv: combine_new_buildings.R new_building_rules.R ../output/house_events.csv ../output/condominium_buildings.csv \
	../output/apartment_events.csv ../output/parcel_lineage.csv ../input/assessed_values.parquet ../input/parcel_history.gpkg \
	../input/commercial_valuation_data.csv | ../output ../report
	$(R) $<

../output/apartment_events.csv: detect_apartment_events.R new_building_rules.R ../output/parcel_lineage.csv ../output/condominium_buildings.csv \
	../input/assessed_values.parquet ../input/commercial_valuation_data.csv | ../output ../report
	$(R) $<

../output/condominium_buildings.csv: detect_condominium_buildings.R ../input/condominium_characteristics.csv | ../output ../report
	$(R) $<

../output/house_events.csv: detect_house_events.R ../input/residential_assessor_history.parquet | ../output ../report
	$(R) $<

../output/parcel_lineage.csv: build_parcel_lineage.R ../input/assessed_values.parquet ../input/parcel_history.gpkg | ../output ../report
	$(R) $<

../input/assessed_values.parquet: ../../download_assessed_values/output/assessed_values.parquet | ../input
	ln -sf $< $@
../input/parcel_history.gpkg: ../../download_parcel_history/output/parcel_history.gpkg | ../input
	ln -sf $< $@
../input/condominium_characteristics.csv: ../../download_condominium_characteristics/output/condominium_characteristics.csv | ../input
	ln -sf $< $@
../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@
../input/commercial_valuation_data.csv: ../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@

link-inputs: ../input/assessed_values.parquet ../input/parcel_history.gpkg ../input/condominium_characteristics.csv \
	../input/residential_assessor_history.parquet ../input/commercial_valuation_data.csv

include ../../shared/code/generic.make
