all: ../output/eastgate_pd986.pdf \
	../output/park_place_2013_submission.pdf \
	../output/grove_pd1395.pdf \
	../output/northmarq_chicago_2025q2.pdf \
	../output/natchez_pd1345.pdf \
	../output/natchez_footprints_2022.geojson \
	../output/deming_pd853.pdf \
	../output/huron_pd356_2011.pdf \
	../output/huron_energy_benchmarking.csv \
	../output/huron_pd356.pdf \
	../output/cascade_developer.html \
	../output/cirrus_developer.html \
	../output/one_chicago_pd1401.pdf

../output/natchez_pd1345.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD1345.pdf' --output ../temp/natchez_pd1345.pdf && pdfinfo ../temp/natchez_pd1345.pdf >/dev/null && printf '%s  %s\n' e45fa4cb48b27b438fa7b3e1ba040724f5a2ef8d8d4bae5e85ee7141b4bd6ba5 ../temp/natchez_pd1345.pdf | shasum -a 256 -c - && mv ../temp/natchez_pd1345.pdf $@

../output/natchez_footprints_2022.geojson: Makefile | ../output ../temp
	curl --fail --location 'https://gis.cookcountyil.gov/traditional/rest/services/buildingFootprint_2022/MapServer/0/query?where=1%3D1&geometry=1131800%2C1912900%2C1132900%2C1914500&geometryType=esriGeometryEnvelope&inSR=3435&spatialRel=esriSpatialRelIntersects&outFields=OBJECTID%2CArea_SQFT%2CYear%2CHeight%2CGlobalID&returnGeometry=true&outSR=4326&orderByFields=OBJECTID&resultRecordCount=2000&f=geojson' --output ../temp/natchez_footprints_2022.geojson && python3 -c 'import json; d=json.load(open("../temp/natchez_footprints_2022.geojson")); assert d["type"] == "FeatureCollection" and d["features"] and not d.get("exceededTransferLimit", False)' && printf '%s  %s\n' e4a9cbe1ccb05d103a3bba285323b0ca07012d8055680b9b0dbaa5810154f50f ../temp/natchez_footprints_2022.geojson | shasum -a 256 -c - && mv ../temp/natchez_footprints_2022.geojson $@

../output/deming_pd853.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD853.pdf' --output ../temp/deming_pd853.pdf && pdfinfo ../temp/deming_pd853.pdf >/dev/null && printf '%s  %s\n' b92300cf4a7f08069aa7880aacd150750bda56abcdcc802c3fe57652e91de092 ../temp/deming_pd853.pdf | shasum -a 256 -c - && mv ../temp/deming_pd853.pdf $@

../output/huron_pd356_2011.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://occprodstoragev1.blob.core.usgovcloudapi.net/lsmatterattachmentspublic/30a5edca-e7c8-4504-b855-5b0e55389c2b.pdf' --output ../temp/huron_pd356_2011.pdf && pdfinfo ../temp/huron_pd356_2011.pdf >/dev/null && printf '%s  %s\n' 37e4beb335af385a23b9e39dac4331e04d9cef48ce608d4f118dbcd4cc0c5a40 ../temp/huron_pd356_2011.pdf | shasum -a 256 -c - && mv ../temp/huron_pd356_2011.pdf $@

../output/huron_energy_benchmarking.csv: Makefile | ../output ../temp
	curl --fail --location --get 'https://data.cityofchicago.org/resource/xq83-jr8c.csv' --data-urlencode '$$where=upper(address) like "%910%HURON%"' --data-urlencode '$$limit=1000' --data-urlencode '$$order=data_year,id' --output ../temp/huron_energy_benchmarking.csv && python3 -c 'import csv; r=list(csv.DictReader(open("../temp/huron_energy_benchmarking.csv"))); assert r and len(r)<1000; assert len({x["row_id"] for x in r})==len(r); assert all(x["id"]=="251926" for x in r)' && printf '%s  %s\n' 45b2f5621e471b976451dc6ba0b44e5dd966f67fca38ac0a37cfdb7aa5738140 ../temp/huron_energy_benchmarking.csv | shasum -a 256 -c - && mv ../temp/huron_energy_benchmarking.csv $@

../output/huron_pd356.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD356.pdf' --output ../temp/huron_pd356.pdf && pdfinfo ../temp/huron_pd356.pdf >/dev/null && printf '%s  %s\n' 363b5a3ab52957ceb567364620a9c0c98e36b409622a587671d6f68dc1a08cdf ../temp/huron_pd356.pdf | shasum -a 256 -c - && mv ../temp/huron_pd356.pdf $@

../output/montclare_pd1412.pdf: Makefile montclare_pd1412.sha256 | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD1412.pdf' --output ../temp/montclare_pd1412.pdf && pdfinfo ../temp/montclare_pd1412.pdf >/dev/null && shasum -a 256 -c montclare_pd1412.sha256 && printf '%s  %s\n' f10c074d4e37e55ff29a34a7c90b6808cd7c44ed17c4e2b584662d15272b212e ../temp/montclare_pd1412.pdf | shasum -a 256 -c - && mv ../temp/montclare_pd1412.pdf $@

../output/cascade_developer.html: Makefile | ../output ../temp
	curl --fail --location --retry 3 'https://www.magellandevelopment.com/projects/cascade/' --output ../temp/cascade_developer.html
	python3 -c 'from pathlib import Path; s=Path("../temp/cascade_developer.html").read_text(); assert "Site Size" in s and "Initial Occupancy" in s and "Building Size" in s'
	printf '%s  %s\n' f1c372a8c360df3112d72c81d7f6be88e51d2cd12d98a1cac07cdff8818422e1 ../temp/cascade_developer.html | shasum -a 256 -c - && mv ../temp/cascade_developer.html $@

../output/cirrus_developer.html: Makefile | ../output ../temp
	curl --fail --location --retry 3 'https://www.magellandevelopment.com/projects/cirrus/' --output ../temp/cirrus_developer.html
	python3 -c 'from pathlib import Path; s=Path("../temp/cirrus_developer.html").read_text(); assert "Site Size" in s and "Initial Occupancy" in s and "Building Size" in s'
	printf '%s  %s\n' 34fc718059329b4c2806b12226e1c2662e196f8ef318f2fd8a5acd7d47afc697 ../temp/cirrus_developer.html | shasum -a 256 -c - && mv ../temp/cirrus_developer.html $@

../output/one_chicago_pd1401.pdf: Makefile | ../output ../temp
	curl --fail --location --retry 3 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD1401.pdf' --output ../temp/one_chicago_pd1401.pdf
	pdfinfo ../temp/one_chicago_pd1401.pdf >/dev/null
	printf '%s  %s\n' 3b6a0da3cd39aaf4fe8050d39ffb86686e7c25611a1d81da2ba9b78e73439a64 ../temp/one_chicago_pd1401.pdf | shasum -a 256 -c - && mv ../temp/one_chicago_pd1401.pdf $@

../output/northmarq_chicago_2025q2.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://www.northmarq.com/sites/default/files/docs/NM%20Market%20Insights_2025Q2_Chicago_0.pdf' --output ../temp/northmarq_chicago_2025q2.pdf && pdfinfo ../temp/northmarq_chicago_2025q2.pdf >/dev/null && printf '%s  %s\n' 81a58cc64d1bd55ceb357f142156d137fc587e50d5902064998583b10540c828 ../temp/northmarq_chicago_2025q2.pdf | shasum -a 256 -c - && mv ../temp/northmarq_chicago_2025q2.pdf $@

../output/grove_pd1395.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD1395.pdf' --output ../temp/grove_pd1395.pdf && pdfinfo ../temp/grove_pd1395.pdf >/dev/null && printf '%s  %s\n' e6d26ed69e0eb7d5e12c01d874e03e941c74f8560a051741c17dc075d9b96b0e ../temp/grove_pd1395.pdf | shasum -a 256 -c - && mv ../temp/grove_pd1395.pdf $@

../output/eastgate_pd986.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/pd986.pdf' --output ../temp/eastgate_pd986.pdf && pdfinfo ../temp/eastgate_pd986.pdf >/dev/null && printf '%s  %s\n' 08c4c39862389a071b1788e9155d233876158483d90922d7790cf8d8fb7769ed ../temp/eastgate_pd986.pdf | shasum -a 256 -c - && mv ../temp/eastgate_pd986.pdf $@

../output/park_place_2013_submission.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://occprodstoragev1.blob.core.usgovcloudapi.net/lsmatterattachmentspublic/844d10cb-411f-4793-be9c-b7a4b75f4069.pdf' --output ../temp/park_place_2013_submission.pdf && pdfinfo ../temp/park_place_2013_submission.pdf >/dev/null && printf '%s  %s\n' dc00f538762c4a2085df5b9209ed2eac208019029417780c8af3bb75ae3a3ea9 ../temp/park_place_2013_submission.pdf | shasum -a 256 -c - && mv ../temp/park_place_2013_submission.pdf $@

include ../../shared/code/generic.make
