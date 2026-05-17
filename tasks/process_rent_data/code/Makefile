SHELL := bash

all: ../output/chicago_rent_panel.parquet

../output/chicago_rent_panel.parquet: aggregate_rents.R process_rent_data.R ../../download_rent_data/output/download.done | ../input ../temp ../output
	@set -euo pipefail; \
	files=(../../download_rent_data/output/*.parquet); \
	if [ ! -e "$${files[0]}" ]; then \
		echo "No rent parquet files found after download_rent_data." >&2; \
		exit 1; \
	fi; \
	for raw in "$${files[@]}"; do \
		base=$$(basename "$$raw"); \
		link="../input/$$base"; \
		current=$$(readlink "$$link" 2>/dev/null || true); \
		test "$$current" = "$$raw" || ln -sf "$$raw" "$$link"; \
		date=$${base%%--*}; \
		$(R) process_rent_data.R "$$link" "../temp/$$date.csv"; \
	done
	$(R) aggregate_rents.R
	@test -s "$@"

../input/%.parquet: ../../download_rent_data/output/%.parquet | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../../download_rent_data/output/download.done | ../input
	@set -euo pipefail; \
	files=(../../download_rent_data/output/*.parquet); \
	if [ ! -e "$${files[0]}" ]; then \
		echo "No rent parquet files found after download_rent_data." >&2; \
		exit 1; \
	fi; \
	for raw in "$${files[@]}"; do \
		base=$$(basename "$$raw"); \
		link="../input/$$base"; \
		current=$$(readlink "$$link" 2>/dev/null || true); \
		test "$$current" = "$$raw" || ln -sf "$$raw" "$$link"; \
	done

include ../../shell_functions.make
include ../../generic.make
