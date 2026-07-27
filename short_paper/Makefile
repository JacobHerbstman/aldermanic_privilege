all: paper.pdf

paper.pdf: Makefile paper.tex sections/*.tex sections/aldermanic_privilege.bib \
	../tasks/audits/income_score_current_results/output/income_score_map_2022.pdf \
	../tasks/audits/income_score_current_results/output/income_score_stage1_2022.tex \
	../tasks/audits/income_score_current_results/output/income_score_distribution_2022.pdf \
	../tasks/audits/income_score_current_results/output/income_score_predecessor_successor_2022.pdf \
	../tasks/density_main_results/output/density_rd.pdf \
	../tasks/density_main_results/output/density_sample_summary.tex \
	../tasks/density_main_results/output/density_main_table.tex \
	../tasks/density_covariate_balance/output/density_covariate_balance.tex \
	../tasks/density_appendix_results/output/density_placebo_neg1000ft.pdf \
	../tasks/density_appendix_results/output/density_placebo_pos1000ft.pdf \
	../tasks/density_appendix_results/output/density_donut25ft.pdf \
	../tasks/density_appendix_results/output/density_donut50ft.pdf \
	../tasks/audits/permit_event_study_audit/output/binary_signed_directional_event_study_high_discretion_income_added_back_500ft_paper.pdf \
	../tasks/audits/permit_event_study_audit/output/binary_joint_directional_event_study_high_discretion_income_added_back_500ft_paper.pdf \
	../tasks/audits/permit_event_study_audit/output/binary_signed_directional_event_study_low_discretion_nosigns_income_added_back_500ft_paper.pdf \
	../tasks/audits/permit_event_study_audit/output/binary_signed_directional_event_study_high_discretion_income_added_back_500ft_stable_paper.pdf \
	../tasks/audits/permit_event_study_audit/output/binary_event_study_appendix_income_added_back_500ft.tex \
	../tasks/permit_summary_stats/output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../tasks/permit_summary_stats/output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../tasks/permit_summary_stats/output/permit_processing_time_volume_correlation.tex \
	../tasks/permit_itt_balance/output/permit_itt_balance_500ft.tex \
	../tasks/price_boundary_regressions/output/price_boundary_regressions_500ft.tex \
	../tasks/price_boundary_regressions/output/price_boundary_rent_500ft.pdf \
	../tasks/price_boundary_regressions/output/price_boundary_sales_500ft.pdf \
	../tasks/price_rd_stress_tests/output/rent_placebo_rd_main_style.pdf \
	../tasks/price_rd_stress_tests/output/sales_placebo_rd_main_style.pdf

.SECONDEXPANSION:
paper.pdf: | $$(addprefix task-check/,$$(patsubst ../tasks/%,%,$$(filter ../tasks/%,$$^)))
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make paper.tex

paper.bbl: paper.aux sections/aldermanic_privilege.bib aea.bst
	bibtex paper

../tasks/%: | task-check/%
	@test -e "$@" || { echo "Task check did not produce $@"; false; }

task-check/%: FORCE
	@task=$$(printf '%s\n' "$*" | sed 's#/output/.*##'); \
	output=$$(printf '%s\n' "$*" | sed 's#^.*/output/#../output/#'); \
	$(MAKE) -C "../tasks/$$task/code" "$$output"

.PHONY: FORCE
FORCE:

clean:
	latexmk -CA
