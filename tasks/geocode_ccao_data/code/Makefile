all: ../output/geocoded_residential_data.gpkg

../output/geocoded_residential_data.gpkg: geocode_residential_data.R ../input/parcel_proximity.csv ../input/residential_cross_section.csv ../input/parcels.csv ../input/multifamily_data_cleaned.csv ../input/density_historical_coordinates.csv | ../output
	Rscript $<

../input/residential_cross_section.csv: ../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcels.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/parcel_proximity.csv: ../../download_parcel_proximity_data/output/parcel_proximity_2023.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/multifamily_data_cleaned.csv: ../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

../input/density_historical_coordinates.csv: ../../density_historical_coordinates/output/density_historical_coordinates.csv | ../input
	@test "$$(readlink "$@" 2>/dev/null)" = "$<" || ln -sf "$<" "$@"

link-inputs: ../input/parcel_proximity.csv ../input/residential_cross_section.csv ../input/parcels.csv ../input/multifamily_data_cleaned.csv ../input/density_historical_coordinates.csv

include ../../generic.make
