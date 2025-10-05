SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs clean

ALL_INPUTS := \
  ../input/condos_cross_section.csv \
  ../input/residential_cross_section.csv \
  ../input/parcels.csv

all: ../output/geocoded_residential_data.csv

# Build: pass inputs explicitly to preserve argument order for the R script
../output/geocoded_residential_data.csv: $(ALL_INPUTS) | ../output
	$(R) geocode_residential_data.R \
	  ../input/condos_cross_section.csv \
	  ../input/residential_cross_section.csv \
	  ../input/parcels.csv \
	  $@

# -------------------- Symlinks --------------------
../input/condos_cross_section.csv: ../../../tasks/condos_data_cleaning/output/condos_cross_section.csv | ../input
	ln -sf "$<" "$@"

../input/residential_cross_section.csv: ../../../tasks/residential_improvements_data_cleaning/output/residential_cross_section.csv | ../input
	ln -sf "$<" "$@"

../input/parcels.csv: ../../../data_raw/Assessor_-_Parcel_Universe__Current_Year_Only__20251004.csv | ../input
	ln -sf "$<" "$@"


# Convenience
link-inputs: $(ALL_INPUTS)


include ../../generic.make
