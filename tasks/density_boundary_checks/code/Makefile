include ../../shared/code/shell_functions.make

all: ../output/density_boundary_robustness.tex \
	../output/density_location_continuity.tex

ifneq ($(filter-out $(wildcard ../output/density_boundary_robustness.tex ../output/density_location_continuity.tex),../output/density_boundary_robustness.tex ../output/density_location_continuity.tex),)
.PHONY: ../output/density_boundary_robustness.tex
endif

../output/density_boundary_robustness.tex: \
	build_density_boundary_checks.R Makefile ../../setup_environment/code/packages.R \
	../input/new_construction_analysis_data.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/density_boundary_characteristics.csv | ../output
	$(R) $<

../output/density_location_continuity.tex: \
	../output/density_boundary_robustness.tex

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/alderman_uncertainty_index_through2022.csv: ../../create_alderman_uncertainty_index/output/alderman_uncertainty_index_ptfeTRUE_rtfeTRUE_porchTRUE_cafeFALSE_2stage_volLAG1_BOTH_through2022.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_boundary_characteristics.csv: ../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/alderman_uncertainty_index_through2022.csv \
	../input/density_boundary_characteristics.csv

include ../../shared/code/generic.make
