all: ../output/building_permits.csv

../output/building_permits.csv: download_building_permits.sh | ../output
	bash $<

include ../../generic.make
