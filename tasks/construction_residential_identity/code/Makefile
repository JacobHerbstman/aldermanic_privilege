include ../../shared/code/shell_functions.make

AUTOMATIC_CARD_BUILDING_GAP := 0.02
EPISODE_YEAR_WINDOW := 2
SUCCESSOR_POINT_TOLERANCE_FT := 1
SUCCESSOR_LAND_TOLERANCE := 0.005

all: ../output/residential_assessor_project_candidates.csv

../%/residential_tieback_temporal_lineage_evidence.csv \
	../%/residential_tieback_temporal_snapshots.csv: build_residential_tieback_temporal_evidence.R \
	../input/residential_assessor_history.parquet \
	../input/residential_fractional_base_groups.csv \
	../input/residential_project_candidate_inventory.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $<

../output/residential_assessor_project_candidates.csv: build_residential_assessor_projects.R \
	../input/construction_modifications.csv \
	../input/residential_reviewed_source_duplicates.csv \
	../input/residential_reviewed_building_components.csv \
	../input/residential_reviewed_source_exclusions.csv \
	../input/residential_assessor_history.parquet \
	../input/residential_reviewed_home_replacements.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../../shared/code/assessor_classification.R \
	../input/residential_project_candidate_inventory.csv \
	../output/residential_tieback_temporal_lineage_evidence.csv \
	../input/residential_multicard_cards.csv \
	../input/parcel_universe_2025_city.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/density_historical_address_records.csv \
	../input/density_historical_parcel_records.csv \
	../input/geocoding_parcel_history.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/predecessor_parcel_history.csv \
	Makefile | ../output ../report
	$(R) $< $(AUTOMATIC_CARD_BUILDING_GAP) $(EPISODE_YEAR_WINDOW) $(SUCCESSOR_POINT_TOLERANCE_FT) $(SUCCESSOR_LAND_TOLERANCE)

../input/construction_modifications.csv: ../../new_construction_cleaning/output/construction_modifications.csv | ../input
	ln -sf $< $@

../input/density_historical_address_records.csv: ../../../data_raw/construction_review/density_historical_address_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/density_historical_parcel_records.csv: ../../../data_raw/construction_review/density_historical_parcel_records_2026-09-04.csv | ../input
	ln -sf $< $@

../input/geocoding_parcel_history.csv: ../../download_construction_historical_parcels/output/geocoding_parcel_history.csv | ../input
	ln -sf $< $@

../input/parcel_addresses_2025_chicago.csv: ../../cook_parcel_addresses_download/output/parcel_addresses_2025_chicago.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/predecessor_parcel_history.csv: ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

../input/residential_fractional_base_groups.csv: ../../construction_project_candidates/output/residential_fractional_base_groups.csv | ../input
	ln -sf $< $@

../input/residential_multicard_cards.csv: ../../construction_project_candidates/output/residential_multicard_cards.csv | ../input
	ln -sf $< $@

../input/residential_project_candidate_inventory.csv: ../../construction_project_candidates/output/residential_project_candidate_inventory.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_building_components.csv: ../../new_construction_cleaning/output/residential_reviewed_building_components.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_home_replacements.csv: ../../new_construction_cleaning/output/residential_reviewed_home_replacements.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_source_duplicates.csv: ../../new_construction_cleaning/output/residential_reviewed_source_duplicates.csv | ../input
	ln -sf $< $@

../input/residential_reviewed_source_exclusions.csv: ../../new_construction_cleaning/output/residential_reviewed_source_exclusions.csv | ../input
	ln -sf $< $@

link-inputs: ../input/construction_modifications.csv \
	../input/density_historical_address_records.csv \
	../input/density_historical_parcel_records.csv \
	../input/geocoding_parcel_history.csv \
	../input/parcel_addresses_2025_chicago.csv \
	../input/parcel_universe_2025_city.csv \
	../input/predecessor_parcel_history.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/residential_assessor_history.parquet \
	../input/residential_fractional_base_groups.csv \
	../input/residential_multicard_cards.csv \
	../input/residential_project_candidate_inventory.csv \
	../input/residential_reviewed_building_components.csv \
	../input/residential_reviewed_home_replacements.csv \
	../input/residential_reviewed_source_duplicates.csv \
	../input/residential_reviewed_source_exclusions.csv

include ../../shared/code/generic.make
