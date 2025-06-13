SHELL := bash
include ../../shell_functions.make

.DEFAULT_GOAL := ../output/building_permits_clean.shp   # ← NEW

../output/building_permits_clean.shp: \
    building_permits_cleaning.R \
    ../input/Building_Permits_20250109.csv \
    | ../output
	Rscript $<

include ../../generic.make




