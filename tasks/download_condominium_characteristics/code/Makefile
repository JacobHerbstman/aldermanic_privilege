include ../../shared/code/shell_functions.make

all: ../output/condominium_characteristics.csv

../output/condominium_characteristics.csv: download_condominiums.R | ../output ../report
	$(R) $<

include ../../shared/code/generic.make
