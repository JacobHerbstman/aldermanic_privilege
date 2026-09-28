include ../../shared/code/shell_functions.make

all: ../output/zoning-code-summary-district-types.csv

# Second City Zoning's summary of each zoning district (DataMade); the checksum records the September 2026 vintage.
../output/zoning-code-summary-district-types.csv: zoning-code-summary-district-types.sha256 | ../output ../temp
	curl --fail --silent --show-error --location --retry 5 -o ../temp/zoning-code-summary-district-types.csv \
		https://secondcityzoning.org/resources/import/zoning-code-summary-district-types.csv
	cd ../temp && shasum -a 256 -c ../code/zoning-code-summary-district-types.sha256
	mv ../temp/zoning-code-summary-district-types.csv $@

include ../../shared/code/generic.make
