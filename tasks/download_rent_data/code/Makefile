SHELL := bash
include ../../shell_functions.make

all: ../output/download.done

../output/download.done: download_rent_data.R | ../output
	$(R) $<
	touch $@

include ../../generic.make