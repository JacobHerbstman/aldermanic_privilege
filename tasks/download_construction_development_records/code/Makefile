include ../../shared/code/shell_functions.make

all: ../output/eastgate_pd986.pdf ../output/park_place_2013_submission.pdf ../output/grove_pd1395.pdf ../output/northmarq_chicago_2025q2.pdf

all: ../output/natchez_pd1345.pdf ../output/natchez_footprints_2022.geojson

include natchez_sources.make
include remaining_sources.make

../output/northmarq_chicago_2025q2.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://www.northmarq.com/sites/default/files/docs/NM%20Market%20Insights_2025Q2_Chicago_0.pdf' --output ../temp/northmarq_chicago_2025q2.pdf && pdfinfo ../temp/northmarq_chicago_2025q2.pdf >/dev/null && mv ../temp/northmarq_chicago_2025q2.pdf $@

../output/grove_pd1395.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/PD1395.pdf' --output ../temp/grove_pd1395.pdf && pdfinfo ../temp/grove_pd1395.pdf >/dev/null && mv ../temp/grove_pd1395.pdf $@

../output/eastgate_pd986.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://gisapps.chicago.gov/gisimages/zoning_pds/pd986.pdf' --output ../temp/eastgate_pd986.pdf && pdfinfo ../temp/eastgate_pd986.pdf >/dev/null && mv ../temp/eastgate_pd986.pdf $@

../output/park_place_2013_submission.pdf: Makefile | ../output ../temp
	curl --fail --location 'https://occprodstoragev1.blob.core.usgovcloudapi.net/lsmatterattachmentspublic/844d10cb-411f-4793-be9c-b7a4b75f4069.pdf' --output ../temp/park_place_2013_submission.pdf && pdfinfo ../temp/park_place_2013_submission.pdf >/dev/null && mv ../temp/park_place_2013_submission.pdf $@

include ../../shared/code/generic.make
