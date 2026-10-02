include ../../../shared/code/shell_functions.make

all: ../output/recall_against_permits.csv ../output/precision_against_permits.csv ../output/checks_before_2006.csv ../output/census_comparison.csv

../output/recall_against_permits.csv: check_recall.R building_sites.R ../input/new_buildings.csv ../input/parcel_lineage.csv \
	../input/construction_buildings.csv | ../output ../report
	$(R) $<

../output/precision_against_permits.csv: check_precision.R building_sites.R ../input/new_buildings.csv ../input/parcel_lineage.csv \
	../input/assessed_values.parquet ../input/parcel_history.gpkg ../input/construction_permits.csv | ../output ../report
	$(R) $<

../output/checks_before_2006.csv: check_before_2006.R building_sites.R ../input/new_buildings.csv ../input/parcel_lineage.csv \
	../input/commercial_valuation_data.csv ../input/energy_benchmarking.csv ../input/parcel_history.gpkg | ../output ../report
	$(R) $<

../output/census_comparison.csv: compare_census_permits.R building_sites.R ../input/new_buildings.csv ../input/chicago_building_permits_survey.csv | ../output ../report
	$(R) $<

../input/new_buildings.csv: ../../../build_assessor_new_buildings/output/new_buildings.csv | ../input
	ln -sf $< $@
../input/parcel_lineage.csv: ../../../build_assessor_new_buildings/output/parcel_lineage.csv | ../input
	ln -sf $< $@
../input/construction_buildings.csv: ../../../prepare_permit_construction/output/construction_buildings.csv | ../input
	ln -sf $< $@
../input/construction_permits.csv: ../../../prepare_permit_construction/output/construction_permits.csv | ../input
	ln -sf $< $@
../input/assessed_values.parquet: ../../../download_assessed_values/output/assessed_values.parquet | ../input
	ln -sf $< $@
../input/parcel_history.gpkg: ../../../download_parcel_history/output/parcel_history.gpkg | ../input
	ln -sf $< $@
../input/commercial_valuation_data.csv: ../../../../data_raw/construction_review/commercial_valuation_data.csv | ../input
	ln -sf $< $@
../input/energy_benchmarking.csv: ../../../download_energy_benchmarking/output/energy_benchmarking.csv | ../input
	ln -sf $< $@
../input/chicago_building_permits_survey.csv: ../../../download_census_building_permits/output/chicago_building_permits_survey.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_buildings.csv ../input/parcel_lineage.csv ../input/construction_buildings.csv ../input/construction_permits.csv \
	../input/assessed_values.parquet ../input/parcel_history.gpkg ../input/commercial_valuation_data.csv ../input/energy_benchmarking.csv \
	../input/chicago_building_permits_survey.csv

include ../../../shared/code/generic.make
