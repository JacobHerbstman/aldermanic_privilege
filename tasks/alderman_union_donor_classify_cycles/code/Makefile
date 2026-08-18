R := Rscript
HIGH_DOLLAR_DONOR_MIN ?= 10000

all: ../output/donor_union_crosswalk_cycles.csv ../output/donor_union_crosswalk_auto_cycles.csv

../output/donor_union_crosswalk_cycles.csv: build_donor_union_crosswalk_cycles.R donor_manual_review_decisions_cycles.csv ../input/isbe_receipts_full_cycles_raw.tsv ../input/alderman_committee_crosswalk_cycles.csv | ../output
	$(R) $< $(HIGH_DOLLAR_DONOR_MIN)

../output/donor_union_crosswalk_auto_cycles.csv: ../output/donor_union_crosswalk_cycles.csv

../input/isbe_receipts_full_cycles_raw.tsv: ../../isbe_campaign_disclosure_cycle_extract/output/isbe_receipts_full_cycles_raw.tsv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_committee_crosswalk_cycles.csv: ../../alderman_union_committee_crosswalk_cycles/output/alderman_committee_crosswalk_cycles.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/isbe_receipts_full_cycles_raw.tsv ../input/alderman_committee_crosswalk_cycles.csv

include ../../generic.make
