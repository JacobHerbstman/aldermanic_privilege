all: ../output/zillow_zori_city.csv

../output/zillow_zori_city.csv: download_zillow_zori.R | ../output
	Rscript $<

include ../../generic.make
