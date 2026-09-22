include ../../shared/code/shell_functions.make

FIRST_YEAR_BUILT := 2000

all: ../output/condominium_characteristics.csv

../output/condominium_characteristics.csv: download_condominiums.R ../../setup_environment/code/packages.R ../../shared/code/save_data.R | ../output ../report
	$(R) $< $(FIRST_YEAR_BUILT)

include ../../shared/code/generic.make
