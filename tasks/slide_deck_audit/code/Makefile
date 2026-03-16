SHELL := bash
include ../../shell_functions.make

all: \
	../output/slide_asset_inventory.csv \
	../output/slide_claim_manifest.csv \
	../output/slide_task_lineage.csv \
	../output/object_verification_index.csv \
	../output/slide_claim_status.csv \
	../output/geometry_geocoding_checks.csv \
	../output/rerun_reproducibility_log.csv \
	../output/slides_audit_findings.csv \
	../output/slides_audit_findings.md

../output/slide_asset_inventory.csv \
../output/slide_claim_manifest.csv: build_slide_inventory.R ../input/slides ../input/manual_slide_claims.csv | ../output
	Rscript build_slide_inventory.R ../input/slides ../input/manual_slide_claims.csv ../output/slide_asset_inventory.csv ../output/slide_claim_manifest.csv

../output/slide_task_lineage.csv \
../output/object_verification_index.csv: build_lineage_map.R ../output/slide_asset_inventory.csv ../output/slide_claim_manifest.csv | ../output
	Rscript build_lineage_map.R ../output/slide_asset_inventory.csv ../output/slide_claim_manifest.csv ../output/slide_task_lineage.csv ../output/object_verification_index.csv

../output/slide_claim_status.csv \
../output/geometry_geocoding_checks.csv \
../output/rerun_reproducibility_log.csv \
../output/slides_audit_findings.csv: run_slide_audit.R ../output/slide_asset_inventory.csv ../output/slide_claim_manifest.csv ../output/slide_task_lineage.csv ../output/object_verification_index.csv | ../output
	Rscript run_slide_audit.R ../output/slide_asset_inventory.csv ../output/slide_claim_manifest.csv ../output/slide_task_lineage.csv ../output/object_verification_index.csv ../output/slide_claim_status.csv ../output/geometry_geocoding_checks.csv ../output/rerun_reproducibility_log.csv ../output/slides_audit_findings.csv

../output/slides_audit_findings.md: render_findings_report.R ../output/slides_audit_findings.csv ../output/slide_claim_status.csv ../output/object_verification_index.csv ../output/rerun_reproducibility_log.csv ../output/geometry_geocoding_checks.csv | ../output
	Rscript render_findings_report.R ../output/slides_audit_findings.csv ../output/slide_claim_status.csv ../output/object_verification_index.csv ../output/rerun_reproducibility_log.csv ../output/geometry_geocoding_checks.csv ../output/slides_audit_findings.md

../input/slides: ../../../slides | ../input
	ln -sfn "$<" "$@"

link-inputs: ../input/slides

include ../../generic.make
