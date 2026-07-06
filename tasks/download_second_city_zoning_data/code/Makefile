all: ../output/zoning-code-summary-district-types.csv

../output/zoning-code-summary-district-types.csv: download_second_city_zoning_data.sh | ../output ../temp
	bash $<

include ../../generic.make
