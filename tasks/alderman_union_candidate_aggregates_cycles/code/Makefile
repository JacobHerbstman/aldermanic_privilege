R := Rscript
COMMITTEE_DEFINITIONS ?= reviewed_named_recipient strict_candidate

.SECONDARY: $(COMMITTEE_DEFINITIONS:%=../../alderman_union_donations_cycles/output/alderman_cycle_union_donations_%.csv)

all: ../output/alderman_union_donations_main.csv $(COMMITTEE_DEFINITIONS:%=../output/alderman_cycle_union_category_aggregates_%.csv) $(COMMITTEE_DEFINITIONS:%=../output/alderman_candidate_union_category_totals_%.csv)

../output/alderman_union_donations_main.csv: ../output/alderman_candidate_union_category_totals_reviewed_named_recipient.csv | ../output
	@test "$$(readlink "$@" 2>/dev/null)" = "alderman_candidate_union_category_totals_reviewed_named_recipient.csv" || ln -sf "alderman_candidate_union_category_totals_reviewed_named_recipient.csv" "$@"

../output/alderman_cycle_union_category_aggregates_%.csv: build_union_candidate_aggregates_cycles.R ../input/alderman_cycle_union_donations_%.csv | ../output
	$(R) $< $* cycle

../output/alderman_candidate_union_category_totals_%.csv: build_union_candidate_aggregates_cycles.R ../input/alderman_cycle_union_donations_%.csv | ../output
	$(R) $< $* candidate_total

../input/alderman_cycle_union_donations_%.csv: ../../alderman_union_donations_cycles/output/alderman_cycle_union_donations_%.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: $(COMMITTEE_DEFINITIONS:%=../input/alderman_cycle_union_donations_%.csv)

include ../../generic.make
