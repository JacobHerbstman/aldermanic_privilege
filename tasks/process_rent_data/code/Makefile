SHELL := bash

START_DATE ?= 2014-01-01
END_DATE ?= 2022-12-31

all: \
	../output/chicago_rent_panel.parquet

../output/chicago_rent_panel.parquet: build_renthub_clean_panel.R ../input/renthub_raw | ../output
	$(R) $< $(START_DATE) $(END_DATE)

../input/renthub_raw: ../../download_rent_data/output/download.done | ../input
	@target="../../download_rent_data/output"; test "$$(readlink "$@" 2>/dev/null)" = "$$target" || ln -sfn "$$target" "$@"

link-inputs: ../input/renthub_raw

include ../../shell_functions.make
include ../../generic.make
