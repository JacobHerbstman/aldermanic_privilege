include ../../shared/code/shell_functions.make

PARCEL_YEARS := 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
HISTORY_START_YEAR := 1999
HISTORY_END_YEAR := 2025

all: ../report/historical_parcel_queries_additional.csv.log $(foreach year,$(PARCEL_YEARS),../output/historical_parcels_additional_$(year).gpkg ../report/historical_parcels_additional_$(year).gpkg.log)

all: ../output/historical_parcels_additional.gpkg ../report/historical_parcels_additional.gpkg.log

all: ../report/predecessor_parcel_history_download.csv.log ../report/predecessor_history_queries_download.csv.log

all: ../report/predecessor_parcels_download.gpkg.log ../report/predecessor_spatial_queries_download.csv.log

all: ../report/preferred_predecessor_parcels_download.gpkg.log ../report/preferred_predecessor_spatial_queries_download.csv.log

all: ../report/history_reference_parcels_download.gpkg.log ../report/history_reference_spatial_queries_download.csv.log

../output/history_reference_parcels_download.gpkg: download_predecessor_parcels.R download_recipes.make ../output/history_reference_spatial_queries_download.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< history_reference

../output/history_reference_spatial_queries_download.csv: prepare_history_reference_queries.R ../output/predecessor_parcel_history.csv ../input/preferred_predecessor_reference_points.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_address_geocode_requests.csv ../output/geocoding_parcel_history.csv | ../output
	$(R) $<

../report/history_reference_parcels_download.gpkg.log: ../../shared/code/report.py ../output/history_reference_parcels_download.gpkg | ../report
	$(PYTHON) $< ../output/history_reference_parcels_download.gpkg $@ target_year object_id

../report/history_reference_spatial_queries_download.csv.log: ../../shared/code/report.py ../output/history_reference_spatial_queries_download.csv | ../report
	$(PYTHON) $< ../output/history_reference_spatial_queries_download.csv $@ target_year reference_x_3435 reference_y_3435

../output/preferred_predecessor_parcels_download.gpkg: download_predecessor_parcels.R download_recipes.make ../output/preferred_predecessor_spatial_queries_download.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< preferred

../output/preferred_predecessor_spatial_queries_download.csv: prepare_preferred_predecessor_queries.R ../input/preferred_predecessor_reference_points.csv ../input/preferred_predecessor_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/preferred_predecessor_parcels_download.gpkg.log: ../../shared/code/report.py ../output/preferred_predecessor_parcels_download.gpkg | ../report
	$(PYTHON) $< ../output/preferred_predecessor_parcels_download.gpkg $@ target_year object_id

../report/preferred_predecessor_spatial_queries_download.csv.log: ../../shared/code/report.py ../output/preferred_predecessor_spatial_queries_download.csv | ../report
	$(PYTHON) $< ../output/preferred_predecessor_spatial_queries_download.csv $@ target_year reference_x_3435 reference_y_3435

../output/predecessor_parcels_download.gpkg: download_predecessor_parcels.R download_recipes.make ../output/predecessor_spatial_queries_download.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< initial

../output/predecessor_spatial_queries_download.csv: prepare_predecessor_spatial_queries.R ../input/missing_project_reference_points.csv ../input/historical_predecessor_queries_2026-07-27.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/predecessor_parcels_download.gpkg.log: ../../shared/code/report.py ../output/predecessor_parcels_download.gpkg | ../report
	$(PYTHON) $< ../output/predecessor_parcels_download.gpkg $@ target_year object_id

../report/predecessor_spatial_queries_download.csv.log: ../../shared/code/report.py ../output/predecessor_spatial_queries_download.csv | ../report
	$(PYTHON) $< ../output/predecessor_spatial_queries_download.csv $@ target_year reference_x_3435 reference_y_3435

all: ../report/geocoding_parcel_history_download.csv.log ../report/geocoding_history_queries_download.csv.log

../output/geocoding_parcel_history_download.csv: download_predecessor_parcel_history.R download_recipes.make ../output/geocoding_history_queries_download.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) geocoding

../output/geocoding_history_queries_download.csv: prepare_geocoding_history_queries.R ../input/preferred_address_geocode_requests.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/geocoding_parcel_history_download.csv.log: ../../shared/code/report.py ../output/geocoding_parcel_history_download.csv | ../report
	$(PYTHON) $< ../output/geocoding_parcel_history_download.csv $@ pin year

../report/geocoding_history_queries_download.csv.log: ../../shared/code/report.py ../output/geocoding_history_queries_download.csv | ../report
	$(PYTHON) $< ../output/geocoding_history_queries_download.csv $@ pin

../output/predecessor_parcel_history_download.csv: download_predecessor_parcel_history.R download_recipes.make ../output/predecessor_history_queries_download.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) initial

../output/predecessor_history_queries_download.csv: prepare_predecessor_history_queries.R ../input/historical_project_parcel_coverage.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/predecessor_parcel_history_download.csv.log: ../../shared/code/report.py ../output/predecessor_parcel_history_download.csv | ../report
	$(PYTHON) $< ../output/predecessor_parcel_history_download.csv $@ row_id

../report/predecessor_history_queries_download.csv.log: ../../shared/code/report.py ../output/predecessor_history_queries_download.csv | ../report
	$(PYTHON) $< ../output/predecessor_history_queries_download.csv $@ pin

../output/historical_parcels_additional.gpkg: combine_additional_parcel_years.R $(foreach year,$(PARCEL_YEARS),../output/historical_parcels_additional_$(year).gpkg) ../../setup_environment/code/packages.R | ../output
	$(R) $< historical

../report/historical_parcels_additional.gpkg.log: ../../shared/code/report.py ../output/historical_parcels_additional.gpkg | ../report
	$(PYTHON) $< ../output/historical_parcels_additional.gpkg $@ target_year object_id

../output/historical_parcels_additional_%.gpkg: download_historical_parcels.R download_recipes.make ../output/historical_parcel_queries_additional.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< $* historical

../output/historical_parcel_queries_additional.csv: prepare_additional_parcel_queries.R ../input/historical_project_parcel_requests.csv ../input/historical_project_parcel_queries_2026-07-27.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/historical_parcel_queries_additional.csv.log: ../../shared/code/report.py ../output/historical_parcel_queries_additional.csv | ../report
	$(PYTHON) $< ../output/historical_parcel_queries_additional.csv $@ target_year pin10

../report/historical_parcels_additional_%.gpkg.log: ../../shared/code/report.py ../output/historical_parcels_additional_%.gpkg | ../report
	$(PYTHON) $< ../output/historical_parcels_additional_$*.gpkg $@ target_year object_id

all: ../report/preferred_parcels_additional.gpkg.log ../report/preferred_parcel_queries_additional.csv.log $(foreach year,$(PARCEL_YEARS),../output/preferred_parcels_additional_$(year).gpkg ../report/preferred_parcels_additional_$(year).gpkg.log)

../output/preferred_parcels_additional.gpkg: combine_additional_parcel_years.R $(foreach year,$(PARCEL_YEARS),../output/preferred_parcels_additional_$(year).gpkg) ../../setup_environment/code/packages.R | ../output
	$(R) $< preferred

../output/preferred_parcels_additional_%.gpkg: download_historical_parcels.R download_recipes.make ../output/preferred_parcel_queries_additional.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< $* preferred

../output/preferred_parcel_queries_additional.csv: prepare_preferred_parcel_queries.R ../input/preferred_project_geography_requests.csv ../output/preferred_historical_parcel_source_queries.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/preferred_parcel_queries_additional.csv.log: ../../shared/code/report.py ../output/preferred_parcel_queries_additional.csv | ../report
	$(PYTHON) $< ../output/preferred_parcel_queries_additional.csv $@ target_year pin10

../report/preferred_parcels_additional.gpkg.log: ../../shared/code/report.py ../output/preferred_parcels_additional.gpkg | ../report
	$(PYTHON) $< ../output/preferred_parcels_additional.gpkg $@ target_year object_id

../report/preferred_parcels_additional_%.gpkg.log: ../../shared/code/report.py ../output/preferred_parcels_additional_%.gpkg | ../report
	$(PYTHON) $< ../output/preferred_parcels_additional_$*.gpkg $@ target_year object_id

../input/preferred_project_geography_requests.csv: ../../new_construction_cleaning/output/preferred_project_geography_requests.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_historical_parcel_source_queries.csv: ../../../data_raw/construction_review/preferred_historical_parcel_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_queries_2026-09-07.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_project_geography_requests.csv ../input/preferred_historical_parcel_source_queries.csv ../input/historical_project_parcel_queries_2026-09-07.csv

../input/historical_project_parcel_requests.csv: ../../new_construction_cleaning/output/historical_project_parcel_requests.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_coverage.csv: ../../new_construction_cleaning/output/historical_project_parcel_coverage.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/missing_project_reference_points.csv: ../../new_construction_cleaning/output/missing_project_reference_points.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_predecessor_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_predecessor_queries_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/historical_project_parcel_coverage.csv ../input/missing_project_reference_points.csv ../input/historical_predecessor_queries_2026-07-27.csv

../input/historical_project_parcel_layers.csv: ../../../data_raw/construction_review/historical_project_parcel_layers_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_requests.csv ../input/historical_project_parcel_layers.csv

../input/preferred_predecessor_reference_points.csv: ../../new_construction_cleaning/output/preferred_predecessor_reference_points.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/preferred_predecessor_source_queries.csv: ../../../data_raw/construction_review/preferred_predecessor_source_queries.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_predecessor_reference_points.csv ../input/preferred_predecessor_source_queries.csv

../input/preferred_address_geocode_requests.csv: ../../new_construction_cleaning/output/preferred_address_geocode_requests.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_address_geocode_requests.csv

include ../../shared/code/generic.make
