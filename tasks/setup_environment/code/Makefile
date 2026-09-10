include ../../shared/code/shell_functions.make

SHELL := bash

all: ../output/R_packages.txt ../output/Python_report_packages.txt

../output/R_packages.txt: packages.R | ../output
	$(R) $<

../output/Python_report_packages.txt: setup_python_reports.py report_requirements.txt | ../output
	$(PYTHON) $<

include ../../shared/code/generic.make
