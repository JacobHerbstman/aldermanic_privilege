include ../../../shared/code/shell_functions.make

REVIEW_SEED := 20260922
SAMPLE_SIZE := 40
TOWNHOUSE_SAMPLE_SIZE := 25
FIRST_RECORD_YEAR_BUILT := 2004

all: ../output/lot_review.csv ../output/townhouse_review.csv ../output/queue_review.csv

../output/lot_review.csv: draw_lot_review.R ../lot_link_reviews.csv ../input/construction_buildings.csv ../input/building_permits_full.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/residential_assessor_history.parquet \
	../../../shared/code/normalize_chicago_address.R ../../../shared/code/street_key.R ../../../shared/code/save_data.R | ../output ../report
	$(R) $< $(REVIEW_SEED) $(SAMPLE_SIZE)

../output/townhouse_review.csv: draw_townhouse_review.R ../input/construction_buildings.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/residential_assessor_history.parquet ../../../shared/code/save_data.R | ../output ../report
	$(R) $< $(REVIEW_SEED) $(TOWNHOUSE_SAMPLE_SIZE) $(FIRST_RECORD_YEAR_BUILT)

../output/queue_review.csv: draw_queue_review.R ../input/construction_buildings.csv ../input/building_permits_full.csv \
	../input/parcel_addresses_2025_chicago.csv ../input/residential_assessor_history.parquet \
	../../../shared/code/normalize_chicago_address.R ../../../shared/code/street_key.R ../../../shared/code/save_data.R | ../output ../report
	$(R) $< $(FIRST_RECORD_YEAR_BUILT)

../input/construction_buildings.csv: ../../../prepare_permit_construction/output/construction_buildings.csv | ../input
	ln -sf $< $@
../input/building_permits_full.csv: ../../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@
../input/parcel_addresses_2025_chicago.csv: ../../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@
../input/residential_assessor_history.parquet: ../../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

link-inputs: ../input/construction_buildings.csv ../input/building_permits_full.csv ../input/parcel_addresses_2025_chicago.csv \
	../input/residential_assessor_history.parquet

include ../../../shared/code/generic.make
