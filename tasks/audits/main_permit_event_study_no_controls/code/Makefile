all: \
	../output/main_permit_event_study_no_controls_500ft.pdf \
	../output/appendix_low_discretion_event_study_no_controls_500ft.pdf \
	../output/appendix_stable_incumbent_event_study_no_controls_500ft.pdf

../output/main_permit_event_study_no_controls_500ft.pdf: run_no_controls.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< main

../output/appendix_low_discretion_event_study_no_controls_500ft.pdf: run_no_controls.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< low_discretion

../output/appendix_stable_incumbent_event_study_no_controls_500ft.pdf: run_no_controls.R ../input/permit_block_year_panel_2015.parquet | ../output
	Rscript $< stable_incumbent

../output/main_permit_event_study_no_controls_500ft.png \
../output/main_permit_event_study_no_controls_500ft_estimates.csv \
../output/main_permit_event_study_no_controls_500ft_summary.csv: ../output/main_permit_event_study_no_controls_500ft.pdf
	@test -s "$@"

../output/appendix_low_discretion_event_study_no_controls_500ft.png \
../output/appendix_low_discretion_event_study_no_controls_500ft_estimates.csv \
../output/appendix_low_discretion_event_study_no_controls_500ft_summary.csv: ../output/appendix_low_discretion_event_study_no_controls_500ft.pdf
	@test -s "$@"

../output/appendix_stable_incumbent_event_study_no_controls_500ft.png \
../output/appendix_stable_incumbent_event_study_no_controls_500ft_estimates.csv \
../output/appendix_stable_incumbent_event_study_no_controls_500ft_summary.csv: ../output/appendix_stable_incumbent_event_study_no_controls_500ft.pdf
	@test -s "$@"

../input/permit_block_year_panel_2015.parquet: ../../../create_event_study_permit_data/output/permit_block_year_panel_2015.parquet | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/permit_block_year_panel_2015.parquet

include ../../../generic.make
