include ../../shared/code/shell_functions.make

SHELL := bash

all: ../output/R_packages.txt \
	../output/Python_report_packages.txt

../output/R_packages.txt: packages.R | ../output
	$(R) $<

../output/Python_report_packages.txt: setup_python_env.sh report_requirements.txt | ../output
	bash $<

include ../../shared/code/generic.make
