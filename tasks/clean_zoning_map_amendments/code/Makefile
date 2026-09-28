include ../../shared/code/shell_functions.make

all: ../output/zoning_map_amendments.csv

../output/zoning_map_amendments.csv: clean_zoning_map_amendments.R ../input/elms_zoning_matter_details.jsonl \
	../input/zoning_legislation_text.parquet ../input/zoning-code-summary-district-types.csv | ../output ../report
	$(R) $<

../input/elms_zoning_matter_details.jsonl: ../../download_elms_matters/output/elms_zoning_matter_details.jsonl | ../input
	ln -sf $< $@
../input/zoning_legislation_text.parquet: ../../extract_zoning_legislation_text/output/zoning_legislation_text.parquet | ../input
	ln -sf $< $@
../input/zoning-code-summary-district-types.csv: ../../download_second_city_zoning_districts/output/zoning-code-summary-district-types.csv | ../input
	ln -sf $< $@

link-inputs: ../input/elms_zoning_matter_details.jsonl ../input/zoning_legislation_text.parquet \
	../input/zoning-code-summary-district-types.csv

include ../../shared/code/generic.make
