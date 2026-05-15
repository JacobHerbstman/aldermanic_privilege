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

all: $(HOSTILE_OUTPUTS)

../output/hostile_claim_ledger.csv: run_hostile_replication_audit.R | ../output
	Rscript run_hostile_replication_audit.R

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
../output/hostile_red_team_memo.md: ../output/hostile_claim_ledger.csv
	@test -s "$@"

include ../../../generic.make
