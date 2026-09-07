include ../tasks/shared/code/shell_functions.make

all: logbook.pdf

logbook.pdf: entries/2026-09-07-general-construction-rules.tex ../tasks/new_construction_cleaning/adjudication/multicard_external_web_reviews.csv ../tasks/new_construction_cleaning/adjudication/multicard_manual_episode_decisions.csv ../tasks/new_construction_cleaning/adjudication/residential_unresolved_source_dispositions.csv entries/2026-09-07-manual-dependence.tex ../tasks/working_paper_release_audit/manual_review_inventory.md ../tasks/working_paper_release_audit/output/manual_review_increment.csv ../tasks/working_paper_release_audit/output/manual_episode_increment.csv ../tasks/working_paper_release_audit/output/multicard_parent_coverage.csv ../tasks/working_paper_release_audit/output/multicard_followup_permits.csv ../tasks/working_paper_release_audit/output/density_exclusion_comparison.csv entries/2026-09-07-density-decisions.tex ../tasks/new_construction_cleaning/adjudication/density_denominator_decisions.csv logbook.tex entries/2026-09-06-construction-replication.tex \
	../tasks/working_paper_release_audit/restored_stage_checks.csv \
	../tasks/working_paper_release_audit/current_assessor_temporal_candidate_changes.csv \
	../tasks/working_paper_release_audit/output/multicard_land_scope.csv \
	../tasks/working_paper_release_audit/output/denominator_case_review.csv \
	../tasks/working_paper_release_audit/denominator_review.md ../tasks/working_paper_release_audit/output/denominator_permit_evidence.csv \
	../data_raw/construction_review/SHA256SUMS Makefile
	pdflatex -halt-on-error -interaction=nonstopmode logbook.tex
	pdflatex -halt-on-error -interaction=nonstopmode logbook.tex

include ../tasks/shared/code/generic.make
