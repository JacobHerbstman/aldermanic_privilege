SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs clean

# Added the new multifamily input
ALL_INPUTS := \
  ../input/parcel_proximity.csv \
  ../input/residential_cross_section.csv \
  ../input/parcels.csv \
  ../input/multifamily_data_cleaned.csv

# Target updated to .gpkg to match sf_write in script
all: ../output/geocoded_residential_data.gpkg

# Build
# Added script as a dependency so changes to logic trigger a rebuild
# Removed input args from command (script now hardcodes input paths and only reads args[1] for output)
../output/geocoded_residential_data.gpkg: geocode_residential_data.R $(ALL_INPUTS) | ../output
	$(R) $< $@

# -------------------- Symlinks --------------------

../input/residential_cross_section.csv: ../../residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	ln -sf "$<" "$@"

../input/parcels.csv: ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	ln -sf "$<" "$@"

../input/parcel_proximity.csv: ../../../data_raw/Assessor_-_Parcel_Proximity_20251007.csv | ../input
	ln -sf "$<" "$@"

# NEW: Check upstream path. Assumed task name: multifamily_data_cleaning
../input/multifamily_data_cleaned.csv: ../../commercial_value_data_cleaning/output/multifamily_data_cleaned.csv | ../input
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(ALL_INPUTS)

include ../../generic.make