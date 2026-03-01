SHELL := bash
include ../../shell_functions.make

all: ../output/stata_packages.txt ../output/R_packages.txt

../output/stata_packages.txt: packages.do | ../output
	$(STATA) $<

../output/R_packages.txt: packages.R | ../output
	$(R) $<

link-inputs:

include ../../generic.make
