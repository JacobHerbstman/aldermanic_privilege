include ../../shared/code/shell_functions.make

all: ../output/substitute_changes.csv

../output/substitute_changes.csv: compare_substitute_ordinances.R ../input/zoning_map_amendments.csv \
	../input/application_form_pages.parquet ../input/attached_application_pages.parquet \
	../input/zoning-code-summary-district-types.csv | ../output ../report
	$(R) $<

../input/zoning_map_amendments.csv: ../../clean_zoning_map_amendments/output/zoning_map_amendments.csv | ../input
	ln -sf $< $@
../input/application_form_pages.parquet: \
	../../extract_zoning_application_forms/output/application_form_pages.parquet | ../input
	ln -sf $< $@
../input/attached_application_pages.parquet: \
	../../extract_zoning_application_forms/output/attached_application_pages.parquet | ../input
	ln -sf $< $@
../input/zoning-code-summary-district-types.csv: \
	../../download_second_city_zoning_districts/output/zoning-code-summary-district-types.csv | ../input
	ln -sf $< $@

link-inputs: ../input/zoning_map_amendments.csv ../input/application_form_pages.parquet \
	../input/attached_application_pages.parquet ../input/zoning-code-summary-district-types.csv

include ../../shared/code/generic.make
