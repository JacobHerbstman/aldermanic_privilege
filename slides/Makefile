all: slides.pdf

slides.pdf: Makefile slides.tex ../paper/sections/aldermanic_privilege.bib \
	sections/appendix_alderman_fe.tex \
	sections/appendix_density.tex \
	sections/appendix_permits.tex \
	sections/appendix_rents.tex \
	sections/appendix_summary_stats.tex \
	sections/conclusion.tex \
	sections/density_analysis.tex \
	sections/index_construction.tex \
	sections/intro.tex \
	sections/literature.tex \
	sections/permit_results.tex \
	sections/permits_and_pipeline.tex \
	sections/rental_market.tex \
	sections/theory.tex \
	figures/pipeline_tikz.tex \
	images/ad_hoc_zoning_chicago.png \
	images/alderman_fight.png \
	images/home_price_growth.png \
	images/hud_report_headline.png \
	images/lightfoot_order.png \
	images/los_angeles_dysfunctional_metropolis.png \
	images/new_york_member_deference.png \
	images/old_town_towers.png \
	images/philadelphia_councilmanic_prerogative.png \
	images/rent_growth_headline.png \
	images/sterling_bay_fight.png \
	../tasks/create_alderman_uncertainty_index/output/stage1_regression_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.tex \
	../tasks/density_main_results/output/density_sample_summary.tex \
	../tasks/permit_itt_balance/output/permit_itt_balance_500ft.tex \
	../tasks/permit_summary_stats/output/alderman_mean_processing_time_high_vs_low_density.pdf \
	../tasks/permit_summary_stats/output/permit_processing_time_high_vs_low_alderman_summary.tex \
	../tasks/permit_summary_stats/output/permit_processing_time_volume_correlation.tex \
	../tasks/presentation_figures/output/density_boundary_robustness_slides.tex \
	../tasks/presentation_figures/output/density_donut25ft_slides.pdf \
	../tasks/presentation_figures/output/density_donut50ft_slides.pdf \
	../tasks/presentation_figures/output/density_location_continuity_slides.tex \
	../tasks/presentation_figures/output/density_placebo_neg1000ft_slides.pdf \
	../tasks/presentation_figures/output/density_placebo_pos1000ft_slides.pdf \
	../tasks/presentation_figures/output/density_rd_slides.pdf \
	../tasks/presentation_figures/output/price_boundary_donuts_slides.pdf \
	../tasks/presentation_figures/output/price_boundary_main_slides.pdf \
	../tasks/presentation_figures/output/price_boundary_placebos_slides.pdf \
	../tasks/presentation_figures/output/price_boundary_straight_slides.pdf \
	../tasks/presentation_figures/output/ward_map_2024.pdf \
	../tasks/presentation_figures/output/ward_pair_1-26_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_appendix_500ft.tex \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_all_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_separate_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_high_discretion_stable_signed_500ft.pdf \
	../tasks/run_event_study_permit/output/permit_event_study_low_discretion_nosigns_stable_signed_500ft.pdf \
	../tasks/strictness_score_map/output/uncertainty_score_distribution_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.pdf \
	../tasks/strictness_score_map/output/uncertainty_score_map_comparison.pdf
	pdflatex -halt-on-error -interaction=nonstopmode -draftmode slides.tex
	bibtex slides
	pdflatex -halt-on-error -interaction=nonstopmode -draftmode slides.tex
	pdflatex -halt-on-error -interaction=nonstopmode slides.tex

include ../tasks/shared/code/generic.make
