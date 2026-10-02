include ../../shared/code/shell_functions.make

all: ../output/density_placebo_neg1000ft.pdf ../output/density_placebo_pos1000ft.pdf \
	../output/density_donut25ft.pdf ../output/density_donut50ft.pdf

../%/density_placebo_neg1000ft.pdf ../%/density_placebo_pos1000ft.pdf \
	../%/density_donut25ft.pdf ../%/density_donut50ft.pdf: \
	build_density_appendix_results.R ../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv

include ../../shared/code/generic.make
