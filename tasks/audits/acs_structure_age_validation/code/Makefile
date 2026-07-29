ACS_YEAR := 2019
FIRST_COMPLETION_YEAR := 2010
LAST_COMPLETION_YEAR := 2019

all: ../output/acs_b25127_chicago_built_2010_or_later.csv \
	../output/acs_ledger_bps_comparison.csv \
	../output/acs_ledger_bps_summary.csv

../output/acs_b25127_chicago_built_2010_or_later.csv: download_acs_structure_age.R | ../output
	$(R) $< $(ACS_YEAR)

../output/acs_ledger_bps_comparison.csv: compare_acs_to_new_construction.R \
	../output/acs_b25127_chicago_built_2010_or_later.csv \
	../input/final_new_construction_audit_ledger.csv \
	../input/census_bps_place_midwest.csv | ../output
	$(R) $< $(FIRST_COMPLETION_YEAR) $(LAST_COMPLETION_YEAR)

../output/acs_ledger_bps_summary.csv: ../output/acs_ledger_bps_comparison.csv
	@test -s "$@"

../input/final_new_construction_audit_ledger.csv:
	@source="../../commercial_new_construction_sample_audit/output/final_new_construction_audit_ledger.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/census_bps_place_midwest.csv:
	@source="../../download_census_bps/output/census_bps_place_midwest.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/final_new_construction_audit_ledger.csv \
	../input/census_bps_place_midwest.csv

include ../../../shell_functions.make
include ../../../generic.make
