all: ../output/renthub_manifest.csv

../output/renthub_manifest.csv: build_renthub_manifest.R ../output/download.done $(wildcard ../output/*.parquet) ../output
	Rscript $<

../output/download.done: download_rent_data.R | ../output
	Rscript $<

include ../../generic.make
