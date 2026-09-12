include ../../shared/code/shell_functions.make

all: ../output/density_placebo_neg1000ft.pdf \
	../output/density_placebo_pos1000ft.pdf \
	../output/density_donut25ft.pdf \
	../output/density_donut50ft.pdf

../%/density_placebo_neg1000ft.pdf \
	../%/density_placebo_pos1000ft.pdf \
	../%/density_donut25ft.pdf \
	../%/density_donut50ft.pdf: \
	build_density_appendix_results.R Makefile ../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv \
	../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	ln -sf $< $@

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	ln -sf $< $@

link-inputs: ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv

include ../../shared/code/generic.make
