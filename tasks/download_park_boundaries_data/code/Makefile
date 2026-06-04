all: ../output/cpd_park_boundaries.geojson

../output/cpd_park_boundaries.geojson: download_park_boundaries_data.sh | ../output
	bash $<

include ../../generic.make
