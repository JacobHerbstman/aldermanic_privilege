SHELL=bash
include ../../shell_functions.make

all: $(foreach lang,stata R,../output/$(lang)_packages.txt) #python

#Recipes
../output/stata_packages.txt: packages.do | ../output run.sbatch
	$(STATA) $<
../output/R_packages.txt: packages.R | ../output run.sbatch
	$(R) $<

../input ../output slurmlogs:
	mkdir $@
run.sbatch: ../../setup_environment/code/run.sbatch | slurmlogs
	ln -s $< $@
