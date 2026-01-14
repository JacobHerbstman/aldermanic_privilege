SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs clean

ALL_INPUTS := \
  ../input/land_value_raw.csv \
  ../input/parcels.csv

# build the Parquet output used in your spatial analysis
all: ../output/land_values_geo.parquet

# Build: pass inputs explicitly to preserve argument order for the R script
../output/land_values_geo.parquet: $(ALL_INPUTS) | ../output
	$(R) land_value_cleaning.R \
	  ../input/land_value_raw.csv \
	  ../input/parcels.csv \
	  $@

# -------------------- Symlinks --------------------
../input/land_value_raw.csv: ../../../data_raw/Assessor_-_Assessed_Values_20251011.csv | ../input
	ln -sf "$<" "$@"

../input/parcels.csv: ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	ln -sf "$<" "$@"

# Convenience
link-inputs: $(ALL_INPUTS)

include ../../generic.make
