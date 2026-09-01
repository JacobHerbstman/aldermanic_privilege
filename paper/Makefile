all: paper.pdf online_appendix.pdf working_paper.pdf word_count.pdf

paper.pdf: Makefile paper.tex sections/intro.tex sections/data.tex sections/model.tex \
	sections/empirics_results.tex sections/discussion.tex sections/conclusion.tex \
	sections/aldermanic_privilege.bib \
	../tasks/permit_summary_stats/output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../tasks/density_main_results/output/density_rd.pdf \
	../tasks/density_main_results/output/density_sample_summary.tex \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_separate_500ft.pdf \
	../tasks/price_boundary_results/output/price_boundary_main.pdf

online_appendix.pdf: Makefile online_appendix.tex paper.pdf sections/appendix_A_data.tex \
	sections/appendix_B_alderman_fe.tex sections/appendix_C_permits.tex \
	sections/appendix_D_density.tex sections/appendix_E_price_rd.tex \
	../tasks/strictness_score_map/output/uncertainty_score_map_comparison.pdf \
	../tasks/create_alderman_uncertainty_index/output/stage1_regression_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex \
	../tasks/strictness_score_map/output/uncertainty_score_distribution_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/strictness_score_map/output/uncertainty_score_predecessor_successor_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/density_appendix_results/output/density_placebo_neg1000ft.pdf \
	../tasks/density_appendix_results/output/density_placebo_pos1000ft.pdf \
	../tasks/density_appendix_results/output/density_donut25ft.pdf \
	../tasks/density_appendix_results/output/density_donut50ft.pdf \
	../tasks/density_boundary_checks/output/density_boundary_robustness.tex \
	../tasks/density_boundary_checks/output/density_location_continuity.tex \
	../tasks/density_score_robustness/output/density_score_robustness.tex \
	../tasks/run_event_study_permit/output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_all_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_appendix_500ft.tex \
	../tasks/permit_summary_stats/output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../tasks/permit_summary_stats/output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../tasks/permit_summary_stats/output/permit_processing_time_volume_correlation.tex \
	../tasks/permit_itt_balance/output/permit_itt_balance_500ft.tex \
	../tasks/price_boundary_results/output/price_boundary_placebos.pdf \
	../tasks/price_boundary_results/output/price_boundary_straight.pdf \
	../tasks/price_boundary_results/output/price_boundary_donuts.pdf

working_paper.pdf: Makefile working_paper.tex paper.tex sections/intro.tex \
	sections/data.tex sections/model.tex sections/empirics_results.tex \
	sections/discussion.tex sections/conclusion.tex sections/appendix_A_data.tex \
	sections/appendix_B_alderman_fe.tex sections/appendix_C_permits.tex \
	sections/appendix_D_density.tex sections/appendix_E_price_rd.tex \
	sections/aldermanic_privilege.bib \
	../tasks/strictness_score_map/output/uncertainty_score_map_comparison.pdf \
	../tasks/create_alderman_uncertainty_index/output/stage1_regression_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex \
	../tasks/strictness_score_map/output/uncertainty_score_distribution_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/strictness_score_map/output/uncertainty_score_predecessor_successor_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/permit_summary_stats/output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../tasks/density_main_results/output/density_rd.pdf \
	../tasks/density_main_results/output/density_sample_summary.tex \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_separate_500ft.pdf \
	../tasks/price_boundary_results/output/price_boundary_main.pdf \
	../tasks/density_appendix_results/output/density_placebo_neg1000ft.pdf \
	../tasks/density_appendix_results/output/density_placebo_pos1000ft.pdf \
	../tasks/density_appendix_results/output/density_donut25ft.pdf \
	../tasks/density_appendix_results/output/density_donut50ft.pdf \
	../tasks/density_boundary_checks/output/density_boundary_robustness.tex \
	../tasks/density_boundary_checks/output/density_location_continuity.tex \
	../tasks/density_score_robustness/output/density_score_robustness.tex \
	../tasks/run_event_study_permit/output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_all_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_appendix_500ft.tex \
	../tasks/permit_summary_stats/output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../tasks/permit_summary_stats/output/permit_processing_time_volume_correlation.tex \
	../tasks/permit_itt_balance/output/permit_itt_balance_500ft.tex \
	../tasks/price_boundary_results/output/price_boundary_placebos.pdf \
	../tasks/price_boundary_results/output/price_boundary_straight.pdf \
	../tasks/price_boundary_results/output/price_boundary_donuts.pdf

word_count.pdf: Makefile word_count.tex paper.pdf

.SECONDEXPANSION:
paper.pdf: | osm-check $$(addprefix task-check/,$$(patsubst ../tasks/%,%,$$(filter ../tasks/%,$$^)))
	pdflatex -interaction=nonstopmode paper.tex
	bibtex paper
	pdflatex -interaction=nonstopmode paper.tex
	pdflatex -interaction=nonstopmode paper.tex

online_appendix.pdf: | osm-check $$(addprefix task-check/,$$(patsubst ../tasks/%,%,$$(filter ../tasks/%,$$^)))
	pdflatex -interaction=nonstopmode online_appendix.tex
	pdflatex -interaction=nonstopmode online_appendix.tex

working_paper.pdf: | osm-check $$(addprefix task-check/,$$(patsubst ../tasks/%,%,$$(filter ../tasks/%,$$^)))
	pdflatex -interaction=nonstopmode working_paper.tex
	bibtex working_paper
	pdflatex -interaction=nonstopmode working_paper.tex
	pdflatex -interaction=nonstopmode working_paper.tex
	pdflatex -interaction=nonstopmode working_paper.tex

word_count.pdf:
	pdflatex -interaction=nonstopmode word_count.tex

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
