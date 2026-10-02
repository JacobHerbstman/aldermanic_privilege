include ../../shared/code/shell_functions.make

all: ../output/assessed_values.parquet

../output/assessed_values.parquet: download_assessed_values.R | ../output ../report
	$(R) $<

include ../../shared/code/generic.make
