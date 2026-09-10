include ../../shared/code/shell_functions.make

SERIES_ID := CUURA207SA0

all: ../output/fred_cpi_cuura207sa0_current.csv

../output/fred_cpi_cuura207sa0_current.csv: download_fred_cpi.R download_recipes.make | ../output
	$(R) $< $(SERIES_ID)

include ../../shared/code/generic.make
