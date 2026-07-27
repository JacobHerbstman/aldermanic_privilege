all: ../output/density_main_table.tex \
	../output/density_sample_summary.tex \
	../output/density_rd.pdf \
	../output/density_rd_estimates.csv

../output/density_main_table.tex: \
	build_density_main_results.R \
	../input/new_construction_analysis_data.csv
	$(R) $<

../output/density_sample_summary.tex: \
	../output/density_main_table.tex

../output/density_rd.pdf: \
	../output/density_main_table.tex

../output/density_rd_estimates.csv: \
	../output/density_main_table.tex

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/new_construction_analysis_data.csv

include ../../shell_functions.make
include ../../generic.make
