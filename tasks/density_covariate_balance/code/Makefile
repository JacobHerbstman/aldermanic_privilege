all: ../output/density_covariate_balance.tex

../output/density_covariate_balance.tex: \
	build_density_covariate_balance.R \
	../input/new_construction_analysis_data.csv
	$(R) $<

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/new_construction_analysis_data.csv

include ../../shell_functions.make
include ../../generic.make
