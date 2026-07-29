all: ../output/density_placebo_neg1000ft.pdf \
	../output/density_placebo_pos1000ft.pdf \
	../output/density_donut25ft.pdf \
	../output/density_donut50ft.pdf

../output/density_placebo_neg1000ft.pdf: \
	build_density_appendix_results.R \
	../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/density_placebo_pos1000ft.pdf: \
	../output/density_placebo_neg1000ft.pdf
	@test -s "$@"

../output/density_donut25ft.pdf: \
	../output/density_placebo_neg1000ft.pdf
	@test -s "$@"

../output/density_donut50ft.pdf: \
	../output/density_placebo_neg1000ft.pdf
	@test -s "$@"

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/new_construction_analysis_data.csv

include ../../shell_functions.make
include ../../generic.make
