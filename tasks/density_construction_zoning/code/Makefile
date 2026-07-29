all: ../output/density_construction_zoning.csv

../output/density_construction_zoning.csv:
	@test -s "$@" || { echo "Missing frozen construction-year zoning file: $@"; false; }

link-inputs:

include ../../generic.make
