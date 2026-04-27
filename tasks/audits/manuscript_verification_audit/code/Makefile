HOSTILE_OUTPUTS = \
	../output/hostile_claim_ledger.csv \
	../output/claim_artifact_map_hostile.csv \
	../output/package_integrity_findings.csv \
	../output/replication_package_release_checklist.md \
	../output/result_stability_scoreboard.csv \
	../output/challenge_results.csv \
	../output/density_challenge_results.csv \
	../output/permit_challenge_results.csv \
	../output/sales_challenge_results.csv \
	../output/density_balance_tests.csv \
	../output/density_sample_ladder.csv \
	../output/permit_sample_ladder.csv \
	../output/sales_sample_ladder.csv \
	../output/density_sidecar_inventory.csv \
	../output/data_integrity_checks.csv \
	../output/density_influence_ward_pair.csv \
	../output/density_influence_segment.csv \
	../output/sales_influence_ward_pair.csv \
	../output/sales_influence_block.csv \
	../output/hostile_red_team_memo.md

STAMP = ../output/.hostile_audit_complete

all: $(STAMP)

$(STAMP): run_hostile_replication_audit.R ../output/findings_ledger.csv ../output/artifact_map.csv ../output/coverage_matrix.csv
	Rscript run_hostile_replication_audit.R
	touch $(STAMP)

link-inputs:
	@true
