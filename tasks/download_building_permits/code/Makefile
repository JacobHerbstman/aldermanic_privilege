START_YEAR ?= 2006
END_YEAR ?= 2022

all: ../output/building_permits_$(START_YEAR)_$(END_YEAR).csv

../output/building_permits_$(START_YEAR)_$(END_YEAR).csv: download_building_permits.sh | ../output
	bash $< $(START_YEAR) $(END_YEAR)

include ../../generic.make
