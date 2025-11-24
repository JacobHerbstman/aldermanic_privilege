SHELL := bash
include ../../shell_functions.make

../output/download.done: download_rent_data.R | ../output ../input
	@echo "→ [download_renthub] Downloading data via Dewey API..."
	$(R) $<
	touch $@

include ../../generic.make