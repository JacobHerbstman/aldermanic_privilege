all: ../output/density_rd.pdf \
	../output/density_sample_summary.tex

../output/density_rd.pdf: \
	build_density_main_results.R \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/density_sample_summary.tex: \
	../output/density_rd.pdf
	@test -s "$@"

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/new_construction_analysis_data.csv

include ../../shell_functions.make
include ../../generic.make
