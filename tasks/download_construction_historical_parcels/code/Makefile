include ../../shared/code/shell_functions.make

REVIEWED_PARCEL_YEARS = 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022

all: ../output/historical_project_parcel_source.gpkg \
	../output/historical_predecessor_parcel_source.gpkg \
	../output/preferred_historical_parcel_source.gpkg \
	../output/preferred_predecessor_parcel_source.gpkg \
	../output/geocoding_parcel_history.csv \
	../output/predecessor_parcel_history.csv \
	../output/history_reference_parcels.gpkg

../output/historical_project_parcel_source.gpkg \
../output/historical_predecessor_parcel_source.gpkg \
../output/preferred_historical_parcel_source.gpkg \
../output/preferred_predecessor_parcel_source.gpkg \
../output/geocoding_parcel_history.csv: combine_recorded_parcels.R Makefile \
	../input/historical_project_parcel_queries_2026-07-27.csv \
	../input/historical_project_parcel_queries_2026-09-07.csv \
	../input/historical_project_parcel_queries_2026-09-08.csv \
	../input/historical_project_parcel_source_2026-07-27.gpkg \
	../input/historical_project_parcel_source_2026-09-07.gpkg \
	../input/historical_project_parcel_source_2026-09-08.gpkg \
	../input/historical_predecessor_queries_2026-07-27.csv \
	../input/historical_predecessor_queries_2026-09-07.csv \
	../input/historical_predecessor_parcels_2026-07-27.gpkg \
	../input/historical_predecessor_parcels_2026-09-07.gpkg \
	../input/preferred_historical_parcel_source_queries.csv \
	../input/preferred_parcel_supplement_queries_2026-09-07.csv \
	../input/preferred_parcel_supplement_queries_2026-09-08.csv \
	../input/preferred_parcel_evidence_queries_2026-09-08.csv \
	../input/lake_park_direct_queries_2026-09-08.csv \
	../input/east_64th_queries_2026-09-08.csv \
	../input/geneva_maud_queries_2026-09-08.csv \
	../input/campbell_queries_2026-09-08.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_parcel_supplement_2026-09-08.gpkg \
	../input/preferred_parcel_evidence_2026-09-08.gpkg \
	../input/lake_park_direct_parcels_2026-09-08.gpkg \
	../input/east_64th_parcels_2026-09-08.gpkg \
	../input/geneva_maud_parcels_2026-09-08.gpkg \
	../input/campbell_parcels_2026-09-08.gpkg \
	../input/preferred_parcel_supplement_2026-09-07.gpkg \
	../input/preferred_predecessor_source_queries.csv \
	../input/preferred_predecessor_queries_2026-09-07.csv \
	../input/preferred_predecessor_troy_queries_2026-09-07.csv \
	../input/lake_park_predecessor_queries_2026-09-08.csv \
	../input/campbell_predecessor_queries_2026-09-08.csv \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/preferred_predecessor_parcels_2026-09-07.gpkg \
	../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg \
	../input/lake_park_predecessor_parcels_2026-09-08.gpkg \
	../input/campbell_predecessor_parcels_2026-09-08.gpkg \
	../input/geocoding_parcel_history_2026-09-07.csv \
	../input/history_reference_parcels_complete_2026-09-08.gpkg \
	../input/history_reference_queries_complete_2026-09-08.csv \
	../input/geocoding_history_queries_2026-09-07.csv \
	../input/historical_project_parcel_layers.csv \
	../input/predecessor_parcel_history_2026-09-07.csv \
	../input/predecessor_history_queries_2026-09-07.csv \
	reviewed_parcel_queries.csv \
	reviewed_predecessor_queries.csv \
	../sources/reviewed_predecessor_parcels.gpkg \
	../sources/reviewed_parcel_history.csv \
	predecessor_snapshot.sha256 \
	preferred_predecessor_snapshot.sha256 \
	geocoding_history_snapshot.sha256 \
	source_snapshot.sha256 \
	history_reference_snapshot.sha256 \
	preferred_snapshot.sha256 \
	history_snapshot.sha256 \
	$(foreach year,$(REVIEWED_PARCEL_YEARS),../sources/reviewed_parcels_additional_$(year).gpkg) \
	../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c predecessor_snapshot.sha256 preferred_predecessor_snapshot.sha256 geocoding_history_snapshot.sha256 source_snapshot.sha256 history_reference_snapshot.sha256 preferred_snapshot.sha256 history_snapshot.sha256
	$(R) $<

../output/predecessor_parcel_history.csv: ../input/predecessor_parcel_history_2026-09-07.csv ../../shared/code/report.py history_snapshot.sha256 ../input/predecessor_history_queries_2026-09-07.csv | ../output ../report
	shasum -a 256 -c history_snapshot.sha256
	cp $< $@
	$(PYTHON) ../../shared/code/report.py $@ ../report/predecessor_parcel_history.csv.log pin year

../output/history_reference_parcels.gpkg: ../input/history_reference_parcels_complete_2026-09-08.gpkg ../../shared/code/report.py history_reference_snapshot.sha256 ../input/history_reference_queries_complete_2026-09-08.csv | ../output ../report
	shasum -a 256 -c history_reference_snapshot.sha256
	cp $< $@
	$(PYTHON) ../../shared/code/report.py $@ ../report/history_reference_parcels.gpkg.log target_year object_id

../input/historical_project_parcel_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-07-27.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_queries_2026-09-07.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_queries_2026-09-08.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_2026-07-27.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-07-27.gpkg | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_2026-09-07.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_2026-09-08.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/historical_predecessor_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_predecessor_queries_2026-07-27.csv | ../input
	ln -sf $< $@

../input/historical_predecessor_queries_2026-09-07.csv: ../../../data_raw/construction_review/historical_predecessor_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/historical_predecessor_parcels_2026-07-27.gpkg: ../../../data_raw/construction_review/historical_predecessor_parcels_2026-07-27.gpkg | ../input
	ln -sf $< $@

../input/historical_predecessor_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/historical_predecessor_parcels_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source_queries.csv: ../../../data_raw/construction_review/preferred_historical_parcel_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_parcel_supplement_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_queries_2026-09-08.csv: ../../../data_raw/construction_review/preferred_parcel_supplement_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/preferred_parcel_evidence_queries_2026-09-08.csv: ../../../data_raw/construction_review/preferred_parcel_evidence_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/lake_park_direct_queries_2026-09-08.csv: ../../../data_raw/construction_review/lake_park_direct_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/east_64th_queries_2026-09-08.csv: ../../../data_raw/construction_review/east_64th_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/geneva_maud_queries_2026-09-08.csv: ../../../data_raw/construction_review/geneva_maud_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/campbell_queries_2026-09-08.csv: ../../../data_raw/construction_review/campbell_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_2026-09-08.gpkg: ../../../data_raw/construction_review/preferred_parcel_supplement_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/preferred_parcel_evidence_2026-09-08.gpkg: ../../../data_raw/construction_review/preferred_parcel_evidence_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/lake_park_direct_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/lake_park_direct_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/east_64th_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/east_64th_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/geneva_maud_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/geneva_maud_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/campbell_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/campbell_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_parcel_supplement_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_source_queries.csv: ../../../data_raw/construction_review/preferred_predecessor_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_predecessor_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_troy_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_predecessor_troy_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/lake_park_predecessor_queries_2026-09-08.csv: ../../../data_raw/construction_review/lake_park_predecessor_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/campbell_predecessor_queries_2026-09-08.csv: ../../../data_raw/construction_review/campbell_predecessor_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcels_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_predecessor_troy_parcels_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/lake_park_predecessor_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/lake_park_predecessor_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/campbell_predecessor_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/campbell_predecessor_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/geocoding_parcel_history_2026-09-07.csv: ../../../data_raw/construction_review/geocoding_parcel_history_2026-09-07.csv | ../input
	ln -sf $< $@

../input/history_reference_parcels_complete_2026-09-08.gpkg: ../../../data_raw/construction_review/history_reference_parcels_complete_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/history_reference_queries_complete_2026-09-08.csv: ../../../data_raw/construction_review/history_reference_queries_complete_2026-09-08.csv | ../input
	ln -sf $< $@

../input/geocoding_history_queries_2026-09-07.csv: ../../../data_raw/construction_review/geocoding_history_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_layers.csv: ../../../data_raw/construction_review/historical_project_parcel_layers_2026-07-27.csv | ../input
	ln -sf $< $@

../input/predecessor_parcel_history_2026-09-07.csv: ../../../data_raw/construction_review/predecessor_parcel_history_2026-09-07.csv | ../input
	ln -sf $< $@

../input/predecessor_history_queries_2026-09-07.csv: ../../../data_raw/construction_review/predecessor_history_queries_2026-09-07.csv | ../input
	ln -sf $< $@

link-inputs: ../input/geocoding_history_queries_2026-09-07.csv ../input/historical_project_parcel_layers.csv ../input/predecessor_parcel_history_2026-09-07.csv ../input/predecessor_history_queries_2026-09-07.csv ../input/historical_project_parcel_queries_2026-07-27.csv \
	../input/historical_project_parcel_queries_2026-09-07.csv \
	../input/historical_project_parcel_queries_2026-09-08.csv \
	../input/historical_project_parcel_source_2026-07-27.gpkg \
	../input/historical_project_parcel_source_2026-09-07.gpkg \
	../input/historical_project_parcel_source_2026-09-08.gpkg \
	../input/historical_predecessor_queries_2026-07-27.csv \
	../input/historical_predecessor_queries_2026-09-07.csv \
	../input/historical_predecessor_parcels_2026-07-27.gpkg \
	../input/historical_predecessor_parcels_2026-09-07.gpkg \
	../input/preferred_historical_parcel_source_queries.csv \
	../input/preferred_parcel_supplement_queries_2026-09-07.csv \
	../input/preferred_parcel_supplement_queries_2026-09-08.csv \
	../input/preferred_parcel_evidence_queries_2026-09-08.csv \
	../input/lake_park_direct_queries_2026-09-08.csv \
	../input/east_64th_queries_2026-09-08.csv \
	../input/geneva_maud_queries_2026-09-08.csv \
	../input/campbell_queries_2026-09-08.csv \
	../input/preferred_historical_parcel_source.gpkg \
	../input/preferred_parcel_supplement_2026-09-08.gpkg \
	../input/preferred_parcel_evidence_2026-09-08.gpkg \
	../input/lake_park_direct_parcels_2026-09-08.gpkg \
	../input/east_64th_parcels_2026-09-08.gpkg \
	../input/geneva_maud_parcels_2026-09-08.gpkg \
	../input/campbell_parcels_2026-09-08.gpkg \
	../input/preferred_parcel_supplement_2026-09-07.gpkg \
	../input/preferred_predecessor_source_queries.csv \
	../input/preferred_predecessor_queries_2026-09-07.csv \
	../input/preferred_predecessor_troy_queries_2026-09-07.csv \
	../input/lake_park_predecessor_queries_2026-09-08.csv \
	../input/campbell_predecessor_queries_2026-09-08.csv \
	../input/preferred_predecessor_parcel_source.gpkg \
	../input/preferred_predecessor_parcels_2026-09-07.gpkg \
	../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg \
	../input/lake_park_predecessor_parcels_2026-09-08.gpkg \
	../input/campbell_predecessor_parcels_2026-09-08.gpkg \
	../input/geocoding_parcel_history_2026-09-07.csv \
	../input/history_reference_parcels_complete_2026-09-08.gpkg \
	../input/history_reference_queries_complete_2026-09-08.csv

include ../../shared/code/generic.make
