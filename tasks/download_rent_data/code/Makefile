SHELL := bash
include ../../shell_functions.make

all: ../output/download.done

../output/download.done: download_rent_data.R | ../output
	$(R) $<
	touch $@

../output/%.parquet: ../output/download.done
	test -f "$@"

include ../../generic.make
