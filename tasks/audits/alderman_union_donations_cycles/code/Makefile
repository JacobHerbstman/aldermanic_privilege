R := Rscript
HIGH_DOLLAR_DONOR_MIN ?= 10000

all: ../output/committee_coverage_status_cycles.csv ../output/committee_manual_review_queue_cycles.csv ../output/committee_definition_bridge_cycles.csv ../output/committee_resolution_packet_cycles.md ../output/isbe_receipts_extract_summary_cycles.csv ../output/donor_union_review_cycles.csv ../output/isbe_d2totals_reconciliation_cycles.csv ../output/union_donation_parity_report.csv ../output/second_opinion_packet.md

../output/committee_coverage_status_cycles.csv: build_committee_coverage_audit_cycles.R ../input/alderman_committee_crosswalk_cycles.csv ../input/isbe_committees_raw.tsv ../input/alderman_cycle_targets.csv | ../output
	$(R) $<

../output/committee_manual_review_queue_cycles.csv: ../output/committee_coverage_status_cycles.csv

../output/committee_definition_bridge_cycles.csv: ../output/committee_coverage_status_cycles.csv

../output/committee_resolution_packet_cycles.md: ../output/committee_coverage_status_cycles.csv

../output/isbe_receipts_extract_summary_cycles.csv: build_union_donation_audit_cycles.R ../input/isbe_receipts_full_cycles_raw.tsv ../input/alderman_committee_crosswalk_cycles.csv ../input/donor_union_crosswalk_cycles.csv ../input/alderman_cycle_targets.csv ../input/alderman_cycle_union_donations_reviewed_named_recipient.csv ../input/alderman_cycle_union_donations_strict_candidate.csv ../input/alderman_candidate_union_category_totals_reviewed_named_recipient.csv ../input/alderman_candidate_union_category_totals_strict_candidate.csv ../output/isbe_d2totals_raw.tsv ../output/isbe_filed_docs_raw.tsv | ../output
	$(R) $< $(HIGH_DOLLAR_DONOR_MIN)

../output/donor_union_review_cycles.csv: ../output/isbe_receipts_extract_summary_cycles.csv

../output/isbe_d2totals_reconciliation_cycles.csv: ../output/isbe_receipts_extract_summary_cycles.csv

../output/union_donation_parity_report.csv: ../output/isbe_receipts_extract_summary_cycles.csv

../output/second_opinion_packet.md: ../output/isbe_receipts_extract_summary_cycles.csv

../output/isbe_d2totals_raw.tsv: | ../output
	curl -L https://www.elections.il.gov/campaigndisclosuredatafiles/D2Totals.txt -o "$@"

../output/isbe_filed_docs_raw.tsv: | ../output
	curl -L https://www.elections.il.gov/campaigndisclosuredatafiles/FiledDocs.txt -o "$@"

../input/alderman_committee_crosswalk_cycles.csv: ../../../alderman_union_committee_crosswalk_cycles/output/alderman_committee_crosswalk_cycles.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/isbe_committees_raw.tsv: ../../../isbe_campaign_disclosure_cycle_extract/output/isbe_committees_raw.tsv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_cycle_targets.csv: ../../../create_alderman_cycle_targets/output/alderman_cycle_targets.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/isbe_receipts_full_cycles_raw.tsv: ../../../isbe_campaign_disclosure_cycle_extract/output/isbe_receipts_full_cycles_raw.tsv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/donor_union_crosswalk_cycles.csv: ../../../alderman_union_donor_classify_cycles/output/donor_union_crosswalk_cycles.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_cycle_union_donations_reviewed_named_recipient.csv: ../../../alderman_union_donations_cycles/output/alderman_cycle_union_donations_reviewed_named_recipient.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_cycle_union_donations_strict_candidate.csv: ../../../alderman_union_donations_cycles/output/alderman_cycle_union_donations_strict_candidate.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_candidate_union_category_totals_reviewed_named_recipient.csv: ../../../alderman_union_candidate_aggregates_cycles/output/alderman_candidate_union_category_totals_reviewed_named_recipient.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_candidate_union_category_totals_strict_candidate.csv: ../../../alderman_union_candidate_aggregates_cycles/output/alderman_candidate_union_category_totals_strict_candidate.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/alderman_committee_crosswalk_cycles.csv ../input/isbe_committees_raw.tsv ../input/alderman_cycle_targets.csv ../input/isbe_receipts_full_cycles_raw.tsv ../input/donor_union_crosswalk_cycles.csv ../input/alderman_cycle_union_donations_reviewed_named_recipient.csv ../input/alderman_cycle_union_donations_strict_candidate.csv ../input/alderman_candidate_union_category_totals_reviewed_named_recipient.csv ../input/alderman_candidate_union_category_totals_strict_candidate.csv

include ../../../generic.make
