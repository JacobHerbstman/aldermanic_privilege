all: ../output/density_covariate_balance.tex

../output/density_covariate_balance.tex: build_density_covariate_balance.R ../input/parcels_with_ward_distances.csv | ../output
	$(R) $<

../input/parcels_with_ward_distances.csv: ../../merge_in_scores/output/parcels_with_ward_distances.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcels_with_ward_distances.csv

include ../../shell_functions.make
include ../../generic.make
