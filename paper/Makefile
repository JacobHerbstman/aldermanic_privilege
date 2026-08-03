all: paper.pdf

paper.pdf: Makefile paper.tex sections/*.tex sections/aldermanic_privilege.bib \
	../tasks/strictness_score_map/output/uncertainty_score_map_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022_2022-01.pdf \
	../tasks/create_alderman_uncertainty_index/output/stage1_regression_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex \
	../tasks/strictness_score_map/output/uncertainty_score_distribution_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/strictness_score_map/output/uncertainty_score_predecessor_successor_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/density_main_results/output/density_rd.pdf \
	../tasks/density_main_results/output/density_sample_summary.tex \
	../tasks/density_appendix_results/output/density_placebo_neg1000ft.pdf \
	../tasks/density_appendix_results/output/density_placebo_pos1000ft.pdf \
	../tasks/density_appendix_results/output/density_donut25ft.pdf \
	../tasks/density_appendix_results/output/density_donut50ft.pdf \
	../tasks/density_boundary_checks/output/density_boundary_robustness.tex \
	../tasks/density_boundary_checks/output/density_location_continuity.tex \
	../tasks/density_score_robustness/output/density_score_robustness.tex \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_separate_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_all_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_appendix_500ft.tex \
	../tasks/permit_summary_stats/output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../tasks/permit_summary_stats/output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../tasks/permit_summary_stats/output/permit_processing_time_volume_correlation.tex \
	../tasks/permit_itt_balance/output/permit_itt_balance_500ft.tex \
	../tasks/price_boundary_results/output/price_boundary_main.pdf \
	../tasks/price_boundary_results/output/price_boundary_placebos.pdf \
	../tasks/price_boundary_results/output/price_boundary_straight.pdf \
	../tasks/price_boundary_results/output/price_boundary_donuts.pdf

.SECONDEXPANSION:
paper.pdf: | osm-check $$(addprefix task-check/,$$(patsubst ../tasks/%,%,$$(filter ../tasks/%,$$^)))
	pdflatex -interaction=nonstopmode paper.tex
	bibtex paper
	pdflatex -interaction=nonstopmode paper.tex
	pdflatex -interaction=nonstopmode paper.tex
	pdflatex -interaction=nonstopmode paper.tex

osm-check:
	@cd .. && shasum -a 256 -c data_raw/illinois-250919-free.sha256

../tasks/%: | task-check/%
	@test -e "$@" || { echo "Task check did not produce $@"; false; }

task-check/%: FORCE
	@task=$$(printf '%s\n' "$*" | sed 's#/output/.*##'); \
	output=$$(printf '%s\n' "$*" | sed 's#^.*/output/#../output/#'); \
	$(MAKE) -C "../tasks/$$task/code" "$$output"

.PHONY: FORCE osm-check
FORCE:
