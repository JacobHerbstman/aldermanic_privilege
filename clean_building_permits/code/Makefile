SHELL := bash
include ../../shell_functions.make
include ../../generic.make

all: ../output/building_permits_clean.shp

../output/building_permits_clean.shp: \
    building_permits_cleaning.R \
    ../input/Building_Permits_20250109.csv \
    | ../output
	Rscript $<


