all: ../output/building_permits_text_features.csv.gz

../output/building_permits_text_features.csv.gz: building_permits_scraping.R ../input/building_permits.csv | ../output
	Rscript $<

../input/building_permits.csv: ../../download_building_permits/output/building_permits.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/building_permits.csv

include ../../generic.make
