SHELL := bash

all: ../output/R_packages.txt

../output/R_packages.txt: packages.R | ../output
	$(R) $<

include ../../shell_functions.make
include ../../generic.make
