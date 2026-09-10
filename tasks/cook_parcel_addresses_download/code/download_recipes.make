include ../../shared/code/shell_functions.make

all: ../output/parcel_addresses_2025_chicago_current.csv

../output/parcel_addresses_2025_chicago_current.csv: download_recipes.make | ../output
	wget -O $@.tmp 'https://datacatalog.cookcountyil.gov/resource/3723-97qp.csv?%24where=year%3D2025%20AND%20upper(prop_address_city_name)%3D%27CHICAGO%27&%24order=pin&%24limit=2000000'
	mv $@.tmp $@

include ../../shared/code/generic.make
