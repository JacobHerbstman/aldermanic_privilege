include ../../shared/code/shell_functions.make

all: ../output/energy_benchmarking.csv

../output/energy_benchmarking.csv: download_energy_benchmarking.R | ../output ../report
	$(R) $<

include ../../shared/code/generic.make
