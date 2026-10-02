include ../../shared/code/shell_functions.make

all: ../output/chicago_building_permits_survey.csv

../output/chicago_building_permits_survey.csv: download_census_building_permits.R | ../output ../report
	$(R) $<

include ../../shared/code/generic.make
