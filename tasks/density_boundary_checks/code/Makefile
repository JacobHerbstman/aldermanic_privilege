all: ../output/density_boundary_robustness.tex \
	../output/density_location_continuity.tex

../output/density_boundary_robustness.tex: \
	build_density_boundary_checks.R \
	../input/new_construction_analysis_data.csv \
	../input/density_boundary_characteristics.csv | ../output
	$(R) $<

../output/density_location_continuity.tex: \
	../output/density_boundary_robustness.tex
	@test -s "$@"

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_boundary_characteristics.csv: ../../density_boundary_characteristics/output/density_boundary_characteristics.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: \
	../input/new_construction_analysis_data.csv \
	../input/density_boundary_characteristics.csv

include ../../shell_functions.make
include ../../generic.make
