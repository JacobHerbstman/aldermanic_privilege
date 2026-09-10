include ../../shared/code/shell_functions.make

all: ../output/density_rd.pdf \
	../output/density_sample_summary.tex

ifneq ($(filter-out $(wildcard ../output/density_rd.pdf ../output/density_sample_summary.tex),../output/density_rd.pdf ../output/density_sample_summary.tex),)
.PHONY: ../output/density_rd.pdf
endif

../output/density_rd.pdf: \
	build_density_main_results.R Makefile ../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv \
	../input/alderman_uncertainty_index_through2022.csv | ../output
	$(R) $<

../output/density_sample_summary.tex: \
	../output/density_rd.pdf

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/new_construction_analysis_data.csv ../input/alderman_uncertainty_index_through2022.csv

include ../../shared/code/generic.make
