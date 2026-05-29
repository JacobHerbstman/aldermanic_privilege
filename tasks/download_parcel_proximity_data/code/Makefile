PROXIMITY_YEAR := 2023

all: ../output/parcel_proximity_2023.csv

../output/parcel_proximity_2023.csv: download_parcel_proximity_data.sh | ../output
	bash $< $(PROXIMITY_YEAR)

include ../../generic.make
