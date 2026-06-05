R := Rscript
COMMITTEE_DEFINITIONS ?= reviewed_named_recipient strict_candidate

all: $(COMMITTEE_DEFINITIONS:%=../output/alderman_cycle_union_donations_%.csv)

../output/alderman_cycle_union_donations_%.csv: build_union_donation_panel_cycles.R ../input/isbe_receipts_full_cycles_raw.tsv ../input/alderman_committee_crosswalk_cycles.csv ../input/donor_union_crosswalk_cycles.csv ../input/alderman_cycle_targets.csv | ../output
	$(R) $< $*

../input/isbe_receipts_full_cycles_raw.tsv: ../../isbe_campaign_disclosure_cycle_extract/output/isbe_receipts_full_cycles_raw.tsv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_committee_crosswalk_cycles.csv: ../../alderman_union_committee_crosswalk_cycles/output/alderman_committee_crosswalk_cycles.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/donor_union_crosswalk_cycles.csv: ../../alderman_union_donor_classify_cycles/output/donor_union_crosswalk_cycles.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_cycle_targets.csv: ../../create_alderman_cycle_targets/output/alderman_cycle_targets.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/isbe_receipts_full_cycles_raw.tsv ../input/alderman_committee_crosswalk_cycles.csv ../input/donor_union_crosswalk_cycles.csv ../input/alderman_cycle_targets.csv

include ../../generic.make
