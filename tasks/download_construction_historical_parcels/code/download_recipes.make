include ../../shared/code/shell_functions.make

PARCEL_YEARS := 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022

all: ../report/historical_parcel_queries_additional.csv.log $(foreach year,$(PARCEL_YEARS),../output/historical_parcels_additional_$(year).gpkg ../report/historical_parcels_additional_$(year).gpkg.log)

all: ../output/historical_parcels_additional.gpkg ../report/historical_parcels_additional.gpkg.log

../output/historical_parcels_additional.gpkg: combine_additional_parcel_years.R $(foreach year,$(PARCEL_YEARS),../output/historical_parcels_additional_$(year).gpkg) ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/historical_parcels_additional.gpkg.log: ../../shared/code/report.py ../output/historical_parcels_additional.gpkg | ../report
	$(PYTHON) $< ../output/historical_parcels_additional.gpkg $@ target_year object_id

../output/historical_parcels_additional_%.gpkg: download_historical_parcels.R download_recipes.make ../output/historical_parcel_queries_additional.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< $*

../output/historical_parcel_queries_additional.csv: prepare_additional_parcel_queries.R ../input/historical_project_parcel_requests.csv ../input/historical_project_parcel_queries_2026-07-27.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/historical_parcel_queries_additional.csv.log: ../../shared/code/report.py ../output/historical_parcel_queries_additional.csv | ../report
	$(PYTHON) $< ../output/historical_parcel_queries_additional.csv $@ target_year pin10

../report/historical_parcels_additional_%.gpkg.log: ../../shared/code/report.py ../output/historical_parcels_additional_%.gpkg | ../report
	$(PYTHON) $< ../output/historical_parcels_additional_$*.gpkg $@ target_year object_id

../input/historical_project_parcel_requests.csv: ../../new_construction_cleaning/output/historical_project_parcel_requests.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_layers.csv: ../../../data_raw/construction_review/historical_project_parcel_layers_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/historical_project_parcel_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-07-27.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_requests.csv ../input/historical_project_parcel_layers.csv

include ../../shared/code/generic.make
