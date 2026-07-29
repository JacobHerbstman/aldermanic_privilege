all: \
	../output/current_density_no_zoning_estimates.csv \
	../output/current_density_no_zoning.pdf

../output/current_density_no_zoning_estimates.csv: build_density_no_zoning_audit.R ../input/new_construction_analysis_data.csv | ../output
	$(R) $<

../output/current_density_no_zoning.pdf: ../output/current_density_no_zoning_estimates.csv
	@test -s "$@"

../input/new_construction_analysis_data.csv: ../../../new_construction_analysis_data/output/new_construction_analysis_data.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/new_construction_analysis_data.csv

include ../../../shell_functions.make
include ../../../generic.make
