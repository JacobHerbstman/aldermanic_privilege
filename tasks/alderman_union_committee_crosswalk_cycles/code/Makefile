R := Rscript

all: ../output/alderman_committee_crosswalk_cycles.csv

../output/alderman_committee_crosswalk_auto_cycles.csv: build_committee_crosswalk_cycles.R ../input/isbe_committees_raw.tsv ../input/alderman_cycle_targets.csv | ../output
	$(R) $<

../output/alderman_committee_crosswalk_cycles.csv: apply_committee_manual_decisions_cycles.R committee_manual_review_decisions_cycles.csv ../output/alderman_committee_crosswalk_auto_cycles.csv ../input/isbe_committees_raw.tsv ../input/alderman_cycle_targets.csv | ../output
	$(R) $<

../input/isbe_committees_raw.tsv: ../../isbe_campaign_disclosure_cycle_extract/output/isbe_committees_raw.tsv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_cycle_targets.csv: ../../create_alderman_cycle_targets/output/alderman_cycle_targets.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/isbe_committees_raw.tsv ../input/alderman_cycle_targets.csv

include ../../generic.make
