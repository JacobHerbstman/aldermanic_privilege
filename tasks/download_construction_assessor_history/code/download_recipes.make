include ../../shared/code/shell_functions.make

all: ../output/construction_condominium_history_current.csv

../output/construction_condominium_history_current.csv: download_construction_condos.R construction_condominium_queries.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $<

include ../../shared/code/generic.make
