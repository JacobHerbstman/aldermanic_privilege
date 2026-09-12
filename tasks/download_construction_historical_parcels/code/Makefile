include ../../shared/code/shell_functions.make

REVIEWED_PARCEL_YEARS := 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022

PARCEL_YEARS := 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
HISTORY_START_YEAR := 1999
HISTORY_END_YEAR := 2025

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

download-current: ../output/historical_parcel_queries_additional.csv $(foreach year,$(PARCEL_YEARS),../output/historical_parcels_additional_$(year).gpkg) \
	../output/historical_parcels_additional.gpkg \
	../output/predecessor_parcel_history_download.csv ../output/predecessor_history_queries_download.csv \
	../output/predecessor_parcels_download.gpkg ../output/predecessor_spatial_queries_download.csv \
	../output/preferred_predecessor_parcels_download.gpkg ../output/preferred_predecessor_spatial_queries_download.csv \
	../output/history_reference_parcels_download.gpkg ../output/history_reference_spatial_queries_download.csv \
	../output/geocoding_parcel_history_download.csv ../output/geocoding_history_queries_download.csv \
	../output/preferred_parcels_additional.gpkg ../output/preferred_parcel_queries_additional.csv $(foreach year,$(PARCEL_YEARS),../output/preferred_parcels_additional_$(year).gpkg)

download-reviewed-current: $(foreach year,$(REVIEWED_PARCEL_YEARS),../output/reviewed_parcels_additional_$(year)_current.gpkg) ../output/reviewed_predecessor_parcels_current.gpkg ../output/reviewed_parcel_history_current.csv ../output/remaining_home_parcels_2018_current.geojson ../output/remaining_home_parcels_2019_current.geojson ../output/remaining_home_parcels_2025_current.geojson

../output/reviewed_parcels_additional_%.gpkg: ../sources/reviewed_parcels_additional_%.gpkg | ../output
	cp $< $@

../output/reviewed_predecessor_parcels.gpkg: ../sources/reviewed_predecessor_parcels.gpkg | ../output
	cp $< $@

../output/reviewed_parcel_history.csv: ../sources/reviewed_parcel_history.csv | ../output
	cp $< $@

../output/reviewed_parcels_additional_%_current.gpkg: download_historical_parcels.R ../../shared/code/save_data.R ../../shared/code/report.py ../output/reviewed_parcel_queries_additional.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< $* reviewed

../output/reviewed_parcel_queries_additional.csv: reviewed_parcel_queries.csv | ../output ../report
	cp $< $@

../output/history_reference_parcels.gpkg: ../input/history_reference_parcels_complete_2026-09-08.gpkg ../output/history_reference_queries.csv | ../output ../report
	cp $< $@

../output/history_reference_queries.csv: ../input/history_reference_queries_complete_2026-09-08.csv ../input/history_reference_parcels_complete_2026-09-08.gpkg history_reference_snapshot.sha256 | ../output ../report
	shasum -a 256 -c history_reference_snapshot.sha256
	cp $< $@

../output/reviewed_predecessor_parcels_current.gpkg: download_predecessor_parcels.R ../../shared/code/save_data.R ../../shared/code/report.py reviewed_predecessor_queries.csv ../input/historical_project_parcel_layers.csv | ../output ../temp ../report
	$(R) $< reviewed

../output/preferred_predecessor_parcel_source.gpkg: combine_preferred_predecessor_parcels.R ../output/reviewed_predecessor_parcels.gpkg ../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/lake_park_predecessor_parcels_2026-09-08.gpkg ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../output/preferred_predecessor_source_queries.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/preferred_predecessor_source_queries.csv: combine_preferred_predecessor_queries.R reviewed_predecessor_queries.csv ../input/campbell_predecessor_parcels_2026-09-08.gpkg ../input/campbell_predecessor_queries_2026-09-08.csv ../input/lake_park_predecessor_parcels_2026-09-08.gpkg ../input/lake_park_predecessor_queries_2026-09-08.csv ../input/preferred_predecessor_source_queries.csv ../input/preferred_predecessor_queries_2026-09-07.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_predecessor_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_parcels_2026-09-07.gpkg ../input/preferred_predecessor_troy_queries_2026-09-07.csv preferred_predecessor_snapshot.sha256 ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	shasum -a 256 -c preferred_predecessor_snapshot.sha256
	$(R) $<

../output/reviewed_parcel_history_current.csv: download_predecessor_parcel_history.R ../../shared/code/save_data.R ../../shared/code/report.py reviewed_history_queries.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< 1999 2025 reviewed

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

../output/history_reference_parcels_download.gpkg: download_predecessor_parcels.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../output/history_reference_spatial_queries_download.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< history_reference

../output/history_reference_spatial_queries_download.csv: prepare_history_reference_queries.R ../output/predecessor_parcel_history.csv ../input/preferred_predecessor_reference_points.csv ../input/preferred_predecessor_parcel_source.gpkg ../input/preferred_address_geocode_requests.csv ../output/geocoding_parcel_history.csv ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/preferred_predecessor_parcels_download.gpkg: download_predecessor_parcels.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../output/preferred_predecessor_spatial_queries_download.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< preferred

../output/preferred_predecessor_spatial_queries_download.csv: prepare_preferred_predecessor_queries.R ../input/preferred_predecessor_reference_points.csv ../output/preferred_predecessor_source_queries.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/predecessor_parcels_download.gpkg: download_predecessor_parcels.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../output/predecessor_spatial_queries_download.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< initial

../output/predecessor_spatial_queries_download.csv: prepare_predecessor_spatial_queries.R ../input/missing_project_reference_points.csv ../input/historical_predecessor_queries_2026-07-27.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/geocoding_parcel_history_download.csv: download_predecessor_parcel_history.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../output/geocoding_history_queries_download.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) geocoding

../output/geocoding_history_queries_download.csv: prepare_geocoding_history_queries.R ../input/preferred_address_geocode_requests.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/predecessor_parcel_history_download.csv: download_predecessor_parcel_history.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../output/predecessor_history_queries_download.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< $(HISTORY_START_YEAR) $(HISTORY_END_YEAR) initial

../output/predecessor_history_queries_download.csv: prepare_predecessor_history_queries.R ../input/historical_project_parcel_coverage.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/historical_parcels_additional.gpkg: combine_additional_parcel_years.R ../../shared/code/save_data.R ../../shared/code/report.py $(foreach year,$(PARCEL_YEARS),../output/historical_parcels_additional_$(year).gpkg) ../../setup_environment/code/packages.R | ../output ../report
	$(R) $< historical

../output/historical_parcels_additional_%.gpkg: download_historical_parcels.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../output/historical_parcel_queries_additional.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< $* historical

../output/historical_parcel_queries_additional.csv: prepare_additional_parcel_queries.R ../input/historical_project_parcel_requests.csv ../input/historical_project_parcel_queries_2026-07-27.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/preferred_parcels_additional.gpkg: combine_additional_parcel_years.R ../../shared/code/save_data.R ../../shared/code/report.py $(foreach year,$(PARCEL_YEARS),../output/preferred_parcels_additional_$(year).gpkg) ../../setup_environment/code/packages.R | ../output ../report
	$(R) $< preferred

../output/preferred_parcels_additional_%.gpkg: download_historical_parcels.R ../../shared/code/save_data.R ../../shared/code/report.py Makefile ../output/preferred_parcel_queries_additional.csv ../input/historical_project_parcel_layers.csv ../../setup_environment/code/packages.R | ../output ../temp ../report
	$(R) $< $* preferred

../output/preferred_parcel_queries_additional.csv: prepare_preferred_parcel_queries.R parcel_evidence_requests.csv ../input/preferred_project_geography_requests.csv ../output/preferred_historical_parcel_source_queries.csv ../../setup_environment/code/packages.R ../../shared/code/save_data.R ../../shared/code/report.py | ../output ../report
	$(R) $<

../output/remaining_home_parcels_2018_current.geojson: Makefile | ../output ../temp ../report
	curl --fail --location --get 'https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/20/query' --data-urlencode "where=PIN10 IN ('2023213097','2023213098')" --data-urlencode 'outFields=*' --data-urlencode 'returnGeometry=true' --data-urlencode 'outSR=4326' --data-urlencode 'f=geojson' --output ../temp/remaining_home_parcels_2018_current.geojson && python3 -c 'import json; d=json.load(open("../temp/remaining_home_parcels_2018_current.geojson")); assert d["type"]=="FeatureCollection" and not d.get("exceededTransferLimit", False); assert len(d["features"])==2; assert all(f["geometry"] for f in d["features"])' && mv ../temp/remaining_home_parcels_2018_current.geojson $@

../output/remaining_home_parcels_2019_current.geojson: Makefile | ../output ../temp ../report
	curl --fail --location --get 'https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/21/query' --data-urlencode "where=PIN10 IN ('2023213110','2023213111')" --data-urlencode 'outFields=*' --data-urlencode 'returnGeometry=true' --data-urlencode 'outSR=4326' --data-urlencode 'f=geojson' --output ../temp/remaining_home_parcels_2019_current.geojson && python3 -c 'import json; d=json.load(open("../temp/remaining_home_parcels_2019_current.geojson")); assert d["type"]=="FeatureCollection" and not d.get("exceededTransferLimit", False); assert len(d["features"])==2; assert all(f["geometry"] for f in d["features"])' && mv ../temp/remaining_home_parcels_2019_current.geojson $@

../output/remaining_home_parcels_2025_current.geojson: Makefile | ../output ../temp ../report
	curl --fail --location --get 'https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/2025/query' --data-urlencode "where=PIN10 IN ('2003400084','2003400085','2003400086','2003400087','2003400088','1428319116','1428319117')" --data-urlencode 'outFields=*' --data-urlencode 'returnGeometry=true' --data-urlencode 'outSR=4326' --data-urlencode 'f=geojson' --output ../temp/remaining_home_parcels_2025_current.geojson && python3 -c 'import json; d=json.load(open("../temp/remaining_home_parcels_2025_current.geojson")); assert d["type"]=="FeatureCollection" and not d.get("exceededTransferLimit", False); assert len(d["features"])==7; assert all(f["geometry"] for f in d["features"])' && mv ../temp/remaining_home_parcels_2025_current.geojson $@

../output/lincoln_footprints_2022_current.geojson: Makefile | ../output ../report
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1158988%2C1932960%2C1159429%2C1933401&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "95429d3b7f9dc30200d7ae7be063983ab4cb60516cfca398fcd23ec84e8b961a  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/campbell_footprints_2022_current.geojson: Makefile | ../output ../report
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1159091%2C1911530%2C1159532%2C1911971&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "a5e5366041bf563f194cc6c0d7ee6718fecc35d69f113c4dd78c6b9a56c19964  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/seeley_footprints_2022_current.geojson: Makefile | ../output ../report
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1161929%2C1923337%2C1162370%2C1923778&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "b08e050d3d4db53c1dbfeebd623021904711e942ccf96b3d864b0efb1323bcca  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/street38_footprints_2022_current.geojson: Makefile | ../output ../report
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1181563%2C1879640%2C1182004%2C1880081&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "3d540cb9d3c82bbd97632cadbc718963ef6b825134725dde95f85a8629c1d895  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/calumet_footprints_2022_current.geojson: Makefile | ../output ../report
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1178914%2C1873428%2C1179355%2C1873869&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "1cdbbc6706fb1957b6f0e61fc89df9fd23ee4edca64688ed0b603d3e8ac901eb  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/marquette_footprints_2022_current.geojson: Makefile | ../output ../report
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1174279%2C1860171%2C1174720%2C1860612&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "31af688432a7f9093b86a3800b9efba9167001a77dec09133ed3a75c6a4b4b6d  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

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

../input/preferred_project_geography_requests.csv: ../../new_construction_cleaning/output/preferred_project_geography_requests.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_requests.csv: ../../new_construction_cleaning/output/historical_project_parcel_requests.csv | ../input
	ln -sf $< $@

../input/historical_project_parcel_coverage.csv: ../../new_construction_cleaning/output/historical_project_parcel_coverage.csv | ../input
	ln -sf $< $@

../input/missing_project_reference_points.csv: ../../new_construction_cleaning/output/missing_project_reference_points.csv | ../input
	ln -sf $< $@

../input/preferred_predecessor_reference_points.csv: ../../new_construction_cleaning/output/preferred_predecessor_reference_points.csv | ../input
	ln -sf $< $@

../input/preferred_address_geocode_requests.csv: ../../new_construction_cleaning/output/preferred_address_geocode_requests.csv | ../input
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
	../input/preferred_project_geography_requests.csv ../input/preferred_historical_parcel_source_queries.csv ../input/historical_project_parcel_queries_2026-09-07.csv \
	../input/historical_project_parcel_coverage.csv ../input/missing_project_reference_points.csv ../input/historical_predecessor_queries_2026-07-27.csv \
	../input/historical_project_parcel_queries_2026-07-27.csv ../input/historical_project_parcel_requests.csv ../input/historical_project_parcel_layers.csv \
	../input/preferred_predecessor_reference_points.csv ../input/preferred_predecessor_source_queries.csv \
	../input/preferred_address_geocode_requests.csv

.PHONY: download-current download-reviewed-current

include ../../shared/code/generic.make
