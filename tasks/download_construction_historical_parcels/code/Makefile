include ../../shared/code/shell_functions.make

REVIEWED_PARCEL_YEARS := 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022

all: $(foreach year,$(REVIEWED_PARCEL_YEARS),../output/reviewed_parcels_additional_$(year).gpkg) ../output/reviewed_parcel_queries_additional.csv \
	../output/historical_project_parcel_source.gpkg ../output/historical_project_parcel_source_queries.csv \
	../output/predecessor_parcel_history.csv ../output/predecessor_history_queries.csv \
	../output/historical_predecessor_parcel_source.gpkg ../output/historical_predecessor_source_queries.csv \
	../output/preferred_historical_parcel_source.gpkg ../output/preferred_historical_parcel_source_queries.csv \
	../output/preferred_predecessor_parcel_source.gpkg ../output/preferred_predecessor_source_queries.csv \
	../output/history_reference_parcels.gpkg ../output/history_reference_queries.csv \
	../output/reviewed_predecessor_parcels.gpkg \
	../output/reviewed_parcel_history.csv \
	../output/geocoding_parcel_history.csv ../output/geocoding_history_queries.csv \
	../output/remaining_home_parcels_2018.geojson \
	../output/remaining_home_parcels_2019.geojson \
	../output/remaining_home_parcels_2025.geojson

../output/reviewed_parcels_additional_%.gpkg: ../sources/reviewed_parcels_additional_%.gpkg | ../output
	cp $< $@

../output/reviewed_predecessor_parcels.gpkg: ../sources/reviewed_predecessor_parcels.gpkg | ../output
	cp $< $@

../output/reviewed_parcel_history.csv: ../sources/reviewed_parcel_history.csv | ../output
	cp $< $@

../output/reviewed_parcel_queries_additional.csv: reviewed_parcel_queries.csv | ../output ../report
	cp $< $@

../output/history_reference_parcels.gpkg: ../input/history_reference_parcels_complete_2026-09-08.gpkg ../output/history_reference_queries.csv | ../output ../report
	cp $< $@

../output/history_reference_queries.csv: ../input/history_reference_queries_complete_2026-09-08.csv ../input/history_reference_parcels_complete_2026-09-08.gpkg history_reference_snapshot.sha256 | ../output ../report
	shasum -a 256 -c history_reference_snapshot.sha256
	cp $< $@

../output/preferred_predecessor_parcel_source.gpkg: combine_preferred_predecessor_parcels.R ../output/reviewed_predecessor_parcels.gpkg ../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/lake_park_predecessor_parcels_2026-09-08.gpkg ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../output/preferred_predecessor_source_queries.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/preferred_predecessor_source_queries.csv: combine_preferred_predecessor_queries.R reviewed_predecessor_queries.csv ../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/campbell_predecessor_queries_2026-09-08.csv ../input/lake_park_predecessor_parcels_2026-09-08.gpkg ../input/lake_park_predecessor_queries_2026-09-08.csv ../input/preferred_predecessor_source_queries.csv ../input/preferred_predecessor_queries_2026-09-07.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_queries_2026-09-07.csv preferred_predecessor_snapshot.sha256 ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c preferred_predecessor_snapshot.sha256
	$(R) $<

../output/geocoding_parcel_history.csv: combine_geocoding_history.R ../output/reviewed_parcel_history.csv ../input/geocoding_parcel_history_2026-09-07.csv ../input/geocoding_history_queries_2026-09-07.csv geocoding_history_snapshot.sha256 ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c geocoding_history_snapshot.sha256
	$(R) $<

../output/geocoding_history_queries.csv: reviewed_history_queries.csv ../input/geocoding_history_queries_2026-09-07.csv ../input/geocoding_parcel_history_2026-09-07.csv geocoding_history_snapshot.sha256 | ../output ../report
	shasum -a 256 -c geocoding_history_snapshot.sha256
	(head -n 1 $<; tail -n +2 ../input/geocoding_history_queries_2026-09-07.csv; tail -n +2 $<) > $@

../output/preferred_historical_parcel_source.gpkg: combine_preferred_parcel_sources.R $(foreach year,$(REVIEWED_PARCEL_YEARS),../output/reviewed_parcels_additional_$(year).gpkg) ../input/campbell_parcels_2026-09-08.gpkg ../input/geneva_maud_parcels_2026-09-08.gpkg ../input/east_64th_parcels_2026-09-08.gpkg ../input/lake_park_direct_parcels_2026-09-08.gpkg ../input/preferred_parcel_evidence_2026-09-08.gpkg ../input/preferred_parcel_supplement_2026-09-08.gpkg ../input/preferred_historical_parcel_source.gpkg ../input/preferred_historical_parcel_source_queries.csv ../output/historical_project_parcel_source.gpkg ../input/preferred_parcel_supplement_2026-09-07.gpkg ../output/preferred_historical_parcel_source_queries.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/preferred_historical_parcel_source_queries.csv: combine_preferred_parcel_queries.R ../output/reviewed_parcel_queries_additional.csv ../input/campbell_parcels_2026-09-08.gpkg ../input/campbell_queries_2026-09-08.csv ../input/geneva_maud_parcels_2026-09-08.gpkg ../input/geneva_maud_queries_2026-09-08.csv ../input/east_64th_parcels_2026-09-08.gpkg ../input/east_64th_queries_2026-09-08.csv ../input/lake_park_direct_parcels_2026-09-08.gpkg ../input/lake_park_direct_queries_2026-09-08.csv ../input/preferred_parcel_evidence_2026-09-08.gpkg ../input/preferred_parcel_evidence_queries_2026-09-08.csv ../input/preferred_parcel_supplement_2026-09-08.gpkg ../input/preferred_parcel_supplement_queries_2026-09-08.csv ../input/preferred_historical_parcel_source_queries.csv ../input/preferred_historical_parcel_source.gpkg ../input/preferred_parcel_supplement_queries_2026-09-07.csv ../input/preferred_parcel_supplement_2026-09-07.gpkg ../output/historical_project_parcel_source_queries.csv preferred_snapshot.sha256 ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c preferred_snapshot.sha256
	$(R) $<

../output/historical_predecessor_parcel_source.gpkg: combine_predecessor_parcels.R ../input/historical_predecessor_parcels_2026-07-27.gpkg ../input/historical_predecessor_parcels_2026-09-07.gpkg ../output/historical_predecessor_source_queries.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/historical_predecessor_source_queries.csv: combine_predecessor_queries.R ../input/historical_predecessor_queries_2026-07-27.csv ../input/historical_predecessor_queries_2026-09-07.csv ../input/historical_predecessor_parcels_2026-07-27.gpkg ../input/historical_predecessor_parcels_2026-09-07.gpkg predecessor_snapshot.sha256 ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c predecessor_snapshot.sha256
	$(R) $<

../output/predecessor_parcel_history.csv: ../input/predecessor_parcel_history_2026-09-07.csv ../input/predecessor_history_queries_2026-09-07.csv history_snapshot.sha256 | ../output ../report
	shasum -a 256 -c history_snapshot.sha256
	cp $< $@

../output/predecessor_history_queries.csv: ../input/predecessor_history_queries_2026-09-07.csv ../input/predecessor_parcel_history_2026-09-07.csv history_snapshot.sha256 | ../output ../report
	shasum -a 256 -c history_snapshot.sha256
	cp $< $@

../output/historical_project_parcel_source.gpkg: combine_parcel_sources.R ../input/historical_project_parcel_source_2026-09-08.gpkg ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../output/historical_project_parcel_source_queries.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/historical_project_parcel_source_queries.csv: combine_parcel_queries.R ../input/historical_project_parcel_queries_2026-09-08.csv ../input/historical_project_parcel_source_2026-09-08.gpkg ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_queries_2026-09-07.csv ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../input/historical_project_parcel_layers.csv source_snapshot.sha256 ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c source_snapshot.sha256
	$(R) $<

../output/remaining_home_parcels_2018.geojson: ../input/remaining_home_parcels_2018_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/remaining_home_parcels_2019.geojson: ../input/remaining_home_parcels_2019_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/remaining_home_parcels_2025.geojson: ../input/remaining_home_parcels_2025_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/lincoln_footprints_2022.geojson: ../input/lincoln_footprints_2022_review_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/campbell_footprints_2022.geojson: ../input/campbell_footprints_2022_review_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/seeley_footprints_2022.geojson: ../input/seeley_footprints_2022_review_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/street38_footprints_2022.geojson: ../input/street38_footprints_2022_review_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/calumet_footprints_2022.geojson: ../input/calumet_footprints_2022_review_2026-09-08.geojson | ../output ../report
	cp $< $@

../output/marquette_footprints_2022.geojson: ../input/marquette_footprints_2022_review_2026-09-08.geojson | ../output ../report
	cp $< $@

../input/lake_park_predecessor_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/lake_park_predecessor_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/lake_park_predecessor_queries_2026-09-08.csv: ../../../data_raw/construction_review/lake_park_predecessor_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/lake_park_direct_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/lake_park_direct_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/lake_park_direct_queries_2026-09-08.csv: ../../../data_raw/construction_review/lake_park_direct_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/preferred_parcel_evidence_2026-09-08.gpkg: ../../../data_raw/construction_review/preferred_parcel_evidence_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/preferred_parcel_evidence_queries_2026-09-08.csv: ../../../data_raw/construction_review/preferred_parcel_evidence_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_2026-09-08.gpkg: ../../../data_raw/construction_review/preferred_parcel_supplement_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_queries_2026-09-08.csv: ../../../data_raw/construction_review/preferred_parcel_supplement_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_2026-09-08.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/historical_project_parcel_queries_2026-09-08.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_2026-07-27.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-07-27.gpkg | ../input
	ln -sf $< $@

../input/historical_project_parcel_source_2026-09-07.gpkg: ../../../data_raw/construction_review/historical_project_parcel_source_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/historical_project_parcel_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-07-27.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_queries_2026-09-07.csv: ../../../data_raw/construction_review/historical_project_parcel_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_layers.csv: ../../../data_raw/construction_review/historical_project_parcel_layers_2026-07-27.csv | ../input
	ln -sf $< $@

../input/predecessor_parcel_history_2026-09-07.csv: ../../../data_raw/construction_review/predecessor_parcel_history_2026-09-07.csv | ../input
	ln -sf $< $@

../input/predecessor_history_queries_2026-09-07.csv: ../../../data_raw/construction_review/predecessor_history_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/historical_predecessor_parcels_2026-07-27.gpkg: ../../../data_raw/construction_review/historical_predecessor_parcels_2026-07-27.gpkg | ../input
	ln -sf $< $@

../input/historical_predecessor_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/historical_predecessor_parcels_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/historical_predecessor_queries_2026-07-27.csv: ../../../data_raw/construction_review/historical_predecessor_queries_2026-07-27.csv | ../input
	ln -sf $< $@

../input/historical_predecessor_queries_2026-09-07.csv: ../../../data_raw/construction_review/historical_predecessor_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_historical_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_historical_parcel_source_queries.csv: ../../../data_raw/construction_review/preferred_historical_parcel_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_parcel_supplement_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/preferred_parcel_supplement_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_parcel_supplement_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcel_source.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcel_source.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_source_queries.csv: ../../../data_raw/construction_review/preferred_predecessor_source_queries.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_predecessor_parcels_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_predecessor_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/geocoding_parcel_history_2026-09-07.csv: ../../../data_raw/construction_review/geocoding_parcel_history_2026-09-07.csv | ../input
	ln -sf $< $@

../input/geocoding_history_queries_2026-09-07.csv: ../../../data_raw/construction_review/geocoding_history_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg: ../../../data_raw/construction_review/preferred_predecessor_troy_parcels_2026-09-07.gpkg | ../input
	ln -sf $< $@

../input/preferred_predecessor_troy_queries_2026-09-07.csv: ../../../data_raw/construction_review/preferred_predecessor_troy_queries_2026-09-07.csv | ../input
	ln -sf $< $@

../input/history_reference_parcels_complete_2026-09-08.gpkg: ../../../data_raw/construction_review/history_reference_parcels_complete_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/history_reference_queries_complete_2026-09-08.csv: ../../../data_raw/construction_review/history_reference_queries_complete_2026-09-08.csv | ../input
	ln -sf $< $@

../input/east_64th_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/east_64th_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/east_64th_queries_2026-09-08.csv: ../../../data_raw/construction_review/east_64th_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/geneva_maud_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/geneva_maud_parcels_2026-09-08.gpkg | ../input
	ln -sf ../../../data_raw/construction_review/geneva_maud_parcels_2026-09-08.gpkg "$@"

../input/geneva_maud_queries_2026-09-08.csv: ../../../data_raw/construction_review/geneva_maud_queries_2026-09-08.csv | ../input
	ln -sf ../../../data_raw/construction_review/geneva_maud_queries_2026-09-08.csv "$@"

../input/campbell_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/campbell_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/campbell_queries_2026-09-08.csv: ../../../data_raw/construction_review/campbell_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/campbell_predecessor_parcels_2026-09-08.gpkg: ../../../data_raw/construction_review/campbell_predecessor_parcels_2026-09-08.gpkg | ../input
	ln -sf $< $@

../input/campbell_predecessor_queries_2026-09-08.csv: ../../../data_raw/construction_review/campbell_predecessor_queries_2026-09-08.csv | ../input
	ln -sf $< $@

../input/remaining_home_parcels_2018_2026-09-08.geojson: ../../../data_raw/construction_review/remaining_home_parcels_2018_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/remaining_home_parcels_2019_2026-09-08.geojson: ../../../data_raw/construction_review/remaining_home_parcels_2019_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/remaining_home_parcels_2025_2026-09-08.geojson: ../../../data_raw/construction_review/remaining_home_parcels_2025_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/lincoln_footprints_2022_review_2026-09-08.geojson: ../../../data_raw/construction_review/lincoln_footprints_2022_review_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/campbell_footprints_2022_review_2026-09-08.geojson: ../../../data_raw/construction_review/campbell_footprints_2022_review_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/seeley_footprints_2022_review_2026-09-08.geojson: ../../../data_raw/construction_review/seeley_footprints_2022_review_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/street38_footprints_2022_review_2026-09-08.geojson: ../../../data_raw/construction_review/street38_footprints_2022_review_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/calumet_footprints_2022_review_2026-09-08.geojson: ../../../data_raw/construction_review/calumet_footprints_2022_review_2026-09-08.geojson | ../input
	ln -sf $< $@

../input/marquette_footprints_2022_review_2026-09-08.geojson: ../../../data_raw/construction_review/marquette_footprints_2022_review_2026-09-08.geojson | ../input
	ln -sf $< $@

link-inputs: ../input/preferred_historical_parcel_source.gpkg ../input/preferred_historical_parcel_source_queries.csv ../input/preferred_parcel_supplement_2026-09-07.gpkg ../input/preferred_parcel_supplement_queries_2026-09-07.csv ../input/historical_predecessor_parcels_2026-07-27.gpkg ../input/historical_predecessor_parcels_2026-09-07.gpkg ../input/historical_predecessor_queries_2026-07-27.csv ../input/historical_predecessor_queries_2026-09-07.csv \
	../input/predecessor_parcel_history_2026-09-07.csv ../input/predecessor_history_queries_2026-09-07.csv ../input/historical_project_parcel_source_2026-07-27.gpkg ../input/historical_project_parcel_source_2026-09-07.gpkg ../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_queries_2026-09-07.csv ../input/historical_project_parcel_layers.csv \
	../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_source_queries.csv ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_queries_2026-09-07.csv \
	../input/geocoding_parcel_history_2026-09-07.csv ../input/geocoding_history_queries_2026-09-07.csv \
	../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_queries_2026-09-07.csv \
	../input/history_reference_parcels_complete_2026-09-08.gpkg ../input/history_reference_queries_complete_2026-09-08.csv \
	../input/east_64th_parcels_2026-09-08.gpkg ../input/east_64th_queries_2026-09-08.csv \
	../input/geneva_maud_parcels_2026-09-08.gpkg \
	../input/geneva_maud_queries_2026-09-08.csv \
	../input/campbell_parcels_2026-09-08.gpkg ../input/campbell_queries_2026-09-08.csv \
	../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/campbell_predecessor_queries_2026-09-08.csv \
	../input/preferred_historical_parcel_source_queries.csv ../input/historical_project_parcel_queries_2026-09-07.csv \
	../input/historical_predecessor_queries_2026-07-27.csv \
	../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_layers.csv \
	../input/preferred_predecessor_source_queries.csv

include ../../shared/code/generic.make
