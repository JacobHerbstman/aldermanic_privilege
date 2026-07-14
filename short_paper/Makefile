all: paper.pdf

paper.pdf: Makefile paper.tex sections/*.tex sections/aldermanic_privilege.bib \
	../tasks/create_alderman_uncertainty_index/output/stage1_regression_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex \
	../tasks/create_alderman_uncertainty_index/output/uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/new_construction_density_robustness/output/fe_table_500ft_all_zonegroup_segment_year_additive_clust_ward_pair_low_discretion_residualized_mainoutcomes.tex \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_dupac_500ft_all_donut25ft.pdf \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_dupac_500ft_all_donut50ft.pdf \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_dupac_500ft_multifamily_donut25ft.pdf \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_dupac_500ft_multifamily_donut50ft.pdf \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_far_500ft_all_donut25ft.pdf \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_far_500ft_all_donut50ft.pdf \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_far_500ft_multifamily_donut25ft.pdf \
	../tasks/nonparametric_rd_density_donut/output/nonparametric_rd_density_donut_log_density_far_500ft_multifamily_donut50ft.pdf \
	../tasks/nonparametric_rd_density_linear_display/output/nonparametric_rd_density_linear_display_4panel_500ft_all_multifamily_bins5.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_dupac_500ft_all_shift_neg500ft.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_dupac_500ft_all_shift_pos500ft.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_dupac_500ft_multifamily_shift_neg500ft.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_dupac_500ft_multifamily_shift_pos500ft.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_far_500ft_all_shift_neg500ft.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_far_500ft_all_shift_pos500ft.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_far_500ft_multifamily_shift_neg500ft.pdf \
	../tasks/nonparametric_rd_density_placebo/output/nonparametric_rd_density_placebo_log_density_far_500ft_multifamily_shift_pos500ft.pdf \
	../tasks/permit_summary_stats/output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../tasks/permit_summary_stats/output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../tasks/permit_summary_stats/output/permit_processing_time_volume_correlation.tex \
	../tasks/run_event_study_permit/output/did_table_permit_2015_application_frozen2014_stable_preperiod_controls_500ft_clust_ward_pair.tex \
	../tasks/run_event_study_permit/output/event_study_permit_2015_high_discretion_application_frozen2014_stable_preperiod_controls_500ft_clust_ward_pair.pdf \
	../tasks/run_event_study_permit/output/event_study_permit_2015_low_discretion_nosigns_application_frozen2014_stable_preperiod_controls_500ft_clust_ward_pair.pdf \
	../tasks/rental_rd/output/rental_rd_flat_bw500_2014_2022_all_controls.pdf \
	../tasks/price_rd_stress_tests/output/rent_placebo_rd_main_style.pdf \
	../tasks/price_rd_stress_tests/output/sales_placebo_rd_main_style.pdf \
	../tasks/sales_border_pair_fe/output/sales_rd_flat_bw500_year_quarter_amenity_clust_segment.pdf \
	../tasks/strictness_score_map/output/uncertainty_score_map_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022_2022-01.pdf \
	../tasks/summary_stats_new_construction/output/summary_stats.tex \
	../tasks/permit_stringency_table/output/permit_validation_table_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex \
	../tasks/within_ward_strictness/output/low_discretion_falsification_compact_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex \
	../tasks/within_ward_strictness/output/predecessor_successor_scatter_uncertainty_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf

.SECONDEXPANSION:
paper.pdf: | $$(addprefix task-check/,$$(patsubst ../tasks/%,%,$$(filter ../tasks/%,$$^)))
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make paper.tex

paper.bbl: paper.aux sections/aldermanic_privilege.bib aea.bst
	bibtex paper

../tasks/%: | task-check/%
	@test -e "$@" || { echo "Task check did not produce $@"; false; }

task-check/%: FORCE
	@task=$$(printf '%s\n' "$*" | sed 's#/output/.*##'); \
	output=$$(printf '%s\n' "$*" | sed 's#^[^/]*/output/#../output/#'); \
	$(MAKE) -C "../tasks/$$task/code" "$$output"

.PHONY: FORCE
FORCE:

clean:
	latexmk -CA
