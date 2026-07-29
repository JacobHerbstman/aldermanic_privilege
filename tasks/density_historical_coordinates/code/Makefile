all: ../output/density_historical_coordinates.csv

../output/density_historical_coordinates.csv:
	@test -s "$@" || { echo "Missing frozen density coordinate file: $@"; false; }

link-inputs:

include ../../generic.make
