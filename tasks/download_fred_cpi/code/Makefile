SHELL := bash
include ../../shell_functions.make

SERIES_ID := CUURA207SA0

all: ../output/fred_cpi_cuura207sa0.csv

../output/fred_cpi_cuura207sa0.csv ../output/fred_cpi_cuura207sa0_metadata.csv: download_fred_cpi.R | ../output
	$(R) download_fred_cpi.R $(SERIES_ID) ../output/fred_cpi_cuura207sa0.csv ../output/fred_cpi_cuura207sa0_metadata.csv

include ../../generic.make
