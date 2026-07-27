BINS_PER_SIDE ?= 5
CHECKS := placebo_neg1000ft placebo_pos1000ft donut25ft donut50ft

all: $(foreach check,$(CHECKS),../output/density_$(check).pdf)

../output/density_%.pdf: \
	build_density_appendix_plot.R \
	../input/new_construction_analysis_data.csv
	$(R) $< $* $(BINS_PER_SIDE)

../input/new_construction_analysis_data.csv: ../../new_construction_analysis_data/output/new_construction_analysis_data.csv
	@source="$<"; \
	target=$$(cd "$$(dirname "$$source")" && pwd)/$$(basename "$$source"); \
	if [ "$$(readlink "$@")" != "$$target" ]; then ln -sf "$$target" "$@"; fi

link-inputs: ../input/new_construction_analysis_data.csv

include ../../shell_functions.make
include ../../generic.make
