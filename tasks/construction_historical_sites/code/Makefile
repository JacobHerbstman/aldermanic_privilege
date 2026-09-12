include ../../shared/code/shell_functions.make

all: ../output/historical_project_year_geometry.gpkg \
	../output/preferred_historical_parcels.gpkg

../%/historical_project_parcel_requests.csv \
	../%/historical_project_parcel_coverage.csv \
	../%/missing_project_reference_points.csv \
	../%/historical_project_predecessor_resolution.csv \
	../%/historical_project_year_geometry.gpkg: build_historical_project_geography.R \
	../input/new_construction_project_components.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/historical_project_parcel_source.gpkg \
	../input/historical_project_parcel_source_queries.csv \
	../input/predecessor_parcel_history.csv \
	../input/predecessor_history_queries.csv \
	../input/historical_predecessor_parcel_source.gpkg \
	../input/historical_predecessor_source_queries.csv \
	Makefile | ../output ../report
	$(R) $<

../%/preferred_project_geography_requests.csv \
	../%/preferred_historical_parcels.gpkg \
	../%/preferred_historical_parcel_coverage.csv: select_project_parcels.R \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_residential_project_candidates.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_historical_parcel_source_queries.csv \
	Makefile | ../output ../report
	$(R) $<

../input/historical_predecessor_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/historical_predecessor_source_queries.csv: ../../download_construction_historical_parcels/output/historical_predecessor_source_queries.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_source.gpkg: ../../download_construction_historical_parcels/output/historical_project_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_queries.csv: ../../download_construction_historical_parcels/output/historical_project_parcel_source_queries.csv | ../input
	ln -sf $< $@

../input/new_construction_project_components.csv: ../../construction_project_permits/output/new_construction_project_components.csv | ../input
	ln -sf $< $@

../input/predecessor_history_queries.csv: ../../download_construction_historical_parcels/output/predecessor_history_queries.csv | ../input
	ln -sf $< $@

../input/predecessor_parcel_history.csv: ../../download_construction_historical_parcels/output/predecessor_parcel_history.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_project_candidates.csv: ../../construction_commercial_measurements/output/preferred_commercial_project_candidates.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source.gpkg: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source_queries.csv: ../../download_construction_historical_parcels/output/preferred_historical_parcel_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_residential_project_candidates.csv: ../../construction_residential_measurements/output/preferred_residential_project_candidates.csv | ../input
	ln -sf $< $@

link-inputs: ../input/historical_predecessor_parcel_source.gpkg \
	../input/historical_predecessor_source_queries.csv \
	../input/historical_project_parcel_source.gpkg \
	../input/historical_project_parcel_source_queries.csv \
	../input/new_construction_project_components.csv \
	../input/predecessor_history_queries.csv \
	../input/predecessor_parcel_history.csv \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_historical_parcel_source_queries.csv \
	../input/preferred_residential_project_candidates.csv

include ../../shared/code/generic.make
