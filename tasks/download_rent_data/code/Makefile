SHELL := bash

all: ../output/download.done

../output/download.done: download_rent_data.R | ../output
	$(R) $<
	touch $@

../output/%.parquet: ../output/download.done
	@test -s "$@"

include ../../shell_functions.make
include ../../generic.make
