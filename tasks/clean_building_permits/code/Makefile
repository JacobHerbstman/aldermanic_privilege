all: ../output/building_permits_clean.gpkg

../output/building_permits_clean.gpkg: building_permits_cleaning.R ../input/building_permits.csv | ../output
	Rscript $<

../input/building_permits.csv: ../../download_building_permits/output/building_permits.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits.csv

include ../../generic.make
