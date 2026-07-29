FIRST_YEAR := 2004
LAST_YEAR := 2022

all: ../output/census_bps_place_midwest.csv

../output/census_bps_place_midwest.csv: download_census_bps_place.sh | ../output
	bash $< $(FIRST_YEAR) $(LAST_YEAR)

include ../../generic.make
