SHELL := bash

all: ../output/stata_packages.txt ../output/R_packages.txt

../output/stata_packages.txt: packages.do | ../output
	$(STATA) $<

../output/R_packages.txt: packages.R | ../output
	$(R) $<

include ../../shell_functions.make
include ../../generic.make
