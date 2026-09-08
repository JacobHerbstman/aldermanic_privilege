all: ../output/remaining_home_parcels_2018.geojson ../output/remaining_home_parcels_2019.geojson ../output/remaining_home_parcels_2025.geojson

../output/remaining_home_parcels_2018.geojson: remaining_review_sources.make | ../output ../temp
	curl --fail --location --get 'https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/20/query' --data-urlencode "where=PIN10 IN ('2023213097','2023213098')" --data-urlencode 'outFields=*' --data-urlencode 'returnGeometry=true' --data-urlencode 'outSR=4326' --data-urlencode 'f=geojson' --output ../temp/remaining_home_parcels_2018.geojson && python3 -c 'import json; d=json.load(open("../temp/remaining_home_parcels_2018.geojson")); assert d["type"]=="FeatureCollection" and not d.get("exceededTransferLimit", False); assert len(d["features"])==2; assert all(f["geometry"] for f in d["features"])' && mv ../temp/remaining_home_parcels_2018.geojson $@

../output/remaining_home_parcels_2019.geojson: remaining_review_sources.make | ../output ../temp
	curl --fail --location --get 'https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/21/query' --data-urlencode "where=PIN10 IN ('2023213110','2023213111')" --data-urlencode 'outFields=*' --data-urlencode 'returnGeometry=true' --data-urlencode 'outSR=4326' --data-urlencode 'f=geojson' --output ../temp/remaining_home_parcels_2019.geojson && python3 -c 'import json; d=json.load(open("../temp/remaining_home_parcels_2019.geojson")); assert d["type"]=="FeatureCollection" and not d.get("exceededTransferLimit", False); assert len(d["features"])==2; assert all(f["geometry"] for f in d["features"])' && mv ../temp/remaining_home_parcels_2019.geojson $@

../output/remaining_home_parcels_2025.geojson: remaining_review_sources.make | ../output ../temp
	curl --fail --location --get 'https://gis.cookcountyil.gov/traditional/rest/services/parcelHistorical/MapServer/2025/query' --data-urlencode "where=PIN10 IN ('2003400084','2003400085','2003400086','2003400087','2003400088','1428319116','1428319117')" --data-urlencode 'outFields=*' --data-urlencode 'returnGeometry=true' --data-urlencode 'outSR=4326' --data-urlencode 'f=geojson' --output ../temp/remaining_home_parcels_2025.geojson && python3 -c 'import json; d=json.load(open("../temp/remaining_home_parcels_2025.geojson")); assert d["type"]=="FeatureCollection" and not d.get("exceededTransferLimit", False); assert len(d["features"])==7; assert all(f["geometry"] for f in d["features"])' && mv ../temp/remaining_home_parcels_2025.geojson $@

../output/lincoln_footprints_2022.geojson: remaining_review_sources.make | ../output
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1158988%2C1932960%2C1159429%2C1933401&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "95429d3b7f9dc30200d7ae7be063983ab4cb60516cfca398fcd23ec84e8b961a  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/campbell_footprints_2022.geojson: remaining_review_sources.make | ../output
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1159091%2C1911530%2C1159532%2C1911971&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "a5e5366041bf563f194cc6c0d7ee6718fecc35d69f113c4dd78c6b9a56c19964  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/seeley_footprints_2022.geojson: remaining_review_sources.make | ../output
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1161929%2C1923337%2C1162370%2C1923778&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "b08e050d3d4db53c1dbfeebd623021904711e942ccf96b3d864b0efb1323bcca  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/street38_footprints_2022.geojson: remaining_review_sources.make | ../output
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1181563%2C1879640%2C1182004%2C1880081&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "3d540cb9d3c82bbd97632cadbc718963ef6b825134725dde95f85a8629c1d895  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/calumet_footprints_2022.geojson: remaining_review_sources.make | ../output
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1178914%2C1873428%2C1179355%2C1873869&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "1cdbbc6706fb1957b6f0e61fc89df9fd23ee4edca64688ed0b603d3e8ac901eb  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@

../output/marquette_footprints_2022.geojson: remaining_review_sources.make | ../output
	curl --fail --location --retry 3 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?f=geojson&where=1%3D1&geometry=1174279%2C1860171%2C1174720%2C1860612&geometryType=esriGeometryEnvelope&inSR=3435&outSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&resultRecordCount=2000' -o $@.tmp
	python3 -c 'import json; x=json.load(open("$@.tmp")); assert x.get("type")=="FeatureCollection" and not x.get("exceededTransferLimit", False); assert len(x["features"])>0 and all(f.get("geometry") for f in x["features"])'
	echo "31af688432a7f9093b86a3800b9efba9167001a77dec09133ed3a75c6a4b4b6d  $@.tmp" | shasum -a 256 -c -
	mv $@.tmp $@
