include ../../../shared/code/shell_functions.make

all: ../output/lot_review.csv ../output/townhouse_review.csv ../output/queue_review.csv ../output/validation_sample.csv

../output/lot_review.csv: draw_lot_review.R ../lot_link_reviews.csv ../input/construction_buildings.csv ../input/building_permits_full.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/residential_assessor_history.parquet | ../output ../report
	$(R) $<

../output/townhouse_review.csv: draw_townhouse_review.R ../input/construction_buildings.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/residential_assessor_history.parquet | ../output ../report
	$(R) $<

../output/queue_review.csv: draw_queue_review.R ../input/construction_buildings.csv ../input/building_permits_full.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/residential_assessor_history.parquet | ../output ../report
	$(R) $<

../output/validation_sample.csv: draw_validation_sample.R ../input/construction_buildings.csv ../input/building_permits_full.csv \
	../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv | ../output ../report
	$(R) $<

../input/construction_buildings.csv: ../../../prepare_permit_construction/output/construction_buildings.csv | ../input
	ln -sf $< $@
../input/building_permits_full.csv: ../../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@
../input/parcel_addresses_2025_chicago.csv: ../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@
../input/residential_assessor_history.parquet: ../../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@
../input/condominium_characteristics.csv: ../../../download_condominium_characteristics/output/condominium_characteristics.csv | ../input
	ln -sf $< $@

link-inputs: ../input/construction_buildings.csv ../input/building_permits_full.csv ../input/parcel_addresses_2025_chicago.csv \
	../input/residential_assessor_history.parquet ../input/condominium_characteristics.csv

include ../../../shared/code/generic.make
