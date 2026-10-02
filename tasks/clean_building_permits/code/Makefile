include ../../shared/code/shell_functions.make

PERIODS := 2006_2022 2023_2026

all: $(foreach period,$(PERIODS),../output/building_permits_clean_$(period).gpkg)

../output/building_permits_clean_%.gpkg: building_permits_cleaning.R ../input/building_permits_%.csv | ../output ../report
	$(R) $< $(subst _, ,$*)

../input/building_permits_%.csv: ../../download_building_permits/output/building_permits_%.csv | ../input
	ln -sf $< $@

link-inputs: $(foreach period,$(PERIODS),../input/building_permits_$(period).csv)

include ../../shared/code/generic.make
