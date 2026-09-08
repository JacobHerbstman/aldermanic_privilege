include ../../shared/code/shell_functions.make

GEOCODERS := census chicago

all: $(foreach provider,$(GEOCODERS),../output/address_geocodes_$(provider)_download.csv ../report/address_geocodes_$(provider)_download.csv.log) ../report/address_geocode_queries.csv.log

../output/address_geocodes_%_download.csv: download_address_geocodes.R download_recipes.make ../output/address_geocode_queries.csv ../../setup_environment/code/packages.R | ../output ../temp
	$(R) $< $*

../output/address_geocode_queries.csv: prepare_address_geocode_queries.R ../input/preferred_address_geocode_requests.csv ../../setup_environment/code/packages.R | ../output
	$(R) $<

../report/address_geocodes_%_download.csv.log: ../../shared/code/report.py ../output/address_geocodes_%_download.csv | ../report
	$(PYTHON) $< ../output/address_geocodes_$*_download.csv $@ selected_address

../report/address_geocode_queries.csv.log: ../../shared/code/report.py ../output/address_geocode_queries.csv | ../report
	$(PYTHON) $< ../output/address_geocode_queries.csv $@ selected_address

../input/preferred_address_geocode_requests.csv: ../../new_construction_cleaning/output/preferred_address_geocode_requests.csv | ../input
	@test "$$(readlink "$@")" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/preferred_address_geocode_requests.csv

include ../../shared/code/generic.make
