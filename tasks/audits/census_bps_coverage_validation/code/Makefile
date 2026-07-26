FIRST_COMPLETION_YEAR := 2006
LAST_COMPLETION_YEAR := 2022
MAX_LAG := 3

all: ../output/bps_ledger_annual_comparison.csv \
	../output/bps_ledger_cumulative_coverage.csv \
	../output/bps_ledger_size_composition.csv \
	../output/bps_ledger_coverage_summary.csv \
	../output/small_multifamily_gap_decomposition.csv \
	../output/small_multifamily_gap_by_era.csv

../output/bps_ledger_annual_comparison.csv: compare_bps_to_new_construction.R \
	../input/census_bps_place_midwest.csv \
	../input/final_new_construction_audit_ledger.csv | ../output
	$(R) $< $(FIRST_COMPLETION_YEAR) $(LAST_COMPLETION_YEAR) $(MAX_LAG)

../output/bps_ledger_cumulative_coverage.csv: ../output/bps_ledger_annual_comparison.csv
	@test -s "$@"

../output/bps_ledger_size_composition.csv: ../output/bps_ledger_annual_comparison.csv
	@test -s "$@"

../output/bps_ledger_coverage_summary.csv: ../output/bps_ledger_annual_comparison.csv
	@test -s "$@"

../output/small_multifamily_gap_decomposition.csv: decompose_small_multifamily_gap.R \
	../input/census_bps_place_midwest.csv \
	../input/final_new_construction_audit_ledger.csv \
	../input/multifamily_classification_decisions.csv \
	../input/residential_cross_section.csv | ../output
	$(R) $< $(FIRST_COMPLETION_YEAR) $(LAST_COMPLETION_YEAR)

../output/small_multifamily_gap_by_era.csv: ../output/small_multifamily_gap_decomposition.csv
	@test -s "$@"

../input/multifamily_classification_decisions.csv:
	@source="../../new_construction_universe_validation/output/multifamily_classification_decisions.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/residential_cross_section.csv:
	@source="../../../residential_improvements_data_cleaning/output/residential_cross_section.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/census_bps_place_midwest.csv:
	@source="../../download_census_bps/output/census_bps_place_midwest.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

../input/final_new_construction_audit_ledger.csv:
	@source="../../commercial_new_construction_sample_audit/output/final_new_construction_audit_ledger.csv"; \
	test -f "$$source"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/census_bps_place_midwest.csv \
	../input/final_new_construction_audit_ledger.csv \
	../input/multifamily_classification_decisions.csv \
	../input/residential_cross_section.csv

include ../../../shell_functions.make
include ../../../generic.make
