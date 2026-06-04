SERIES_ID := CUURA207SA0

all: ../output/fred_cpi_cuura207sa0.csv

../output/fred_cpi_cuura207sa0.csv: download_fred_cpi.R | ../output
	Rscript $< $(SERIES_ID)

include ../../generic.make
