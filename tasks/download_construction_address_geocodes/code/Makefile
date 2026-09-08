include ../../shared/code/shell_functions.make

GEOCODERS := census chicago

all: $(foreach provider,$(GEOCODERS),../output/address_geocodes_$(provider).csv ../report/address_geocodes_$(provider).csv.log)

../output/address_geocodes_%.csv: combine_address_geocodes.R ../input/address_geocodes_census_2026-09-07.csv ../input/address_geocodes_chicago_2026-09-07.csv ../input/address_geocodes_census_additional_2026-09-07.csv ../input/address_geocodes_chicago_additional_2026-09-07.csv source_snapshot.sha256 Makefile | ../output
	shasum -a 256 -c source_snapshot.sha256
	$(R) $< $*

../report/address_geocodes_%.csv.log: ../../shared/code/report.py ../output/address_geocodes_%.csv | ../report
	$(PYTHON) $< ../output/address_geocodes_$*.csv $@ selected_address

../input/address_geocodes_census_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_census_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/address_geocodes_chicago_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_chicago_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/address_geocodes_census_additional_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_census_additional_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

../input/address_geocodes_chicago_additional_2026-09-07.csv: ../../../data_raw/construction_review/address_geocodes_chicago_additional_2026-09-07.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/address_geocodes_census_2026-09-07.csv ../input/address_geocodes_chicago_2026-09-07.csv ../input/address_geocodes_census_additional_2026-09-07.csv ../input/address_geocodes_chicago_additional_2026-09-07.csv

include ../../shared/code/generic.make
