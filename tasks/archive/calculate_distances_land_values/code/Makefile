	SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs clean

ALL_INPUTS := \
  ../input/land_values_geo.parquet \
  ../input/ward_panel.gpkg \
  ../input/alderman_panel.csv

# build the Parquet output used in your spatial analysis
all: ../output/land_values_aug.parquet

# Build: pass inputs explicitly to preserve argument order for the R script
../output/land_values_aug.parquet: $(ALL_INPUTS) | ../output
	$(R) calculate_distances_land_values.R \
	  ../input/land_values_geo.parquet \
	  ../input/ward_panel.gpkg \
	  ../input/alderman_panel.csv
	  $@

# -------------------- Symlinks --------------------
../input/land_values_geo.parquet: ../../land_value_data_cleaning/output/land_values_geo.parquet
	ln -sf "$<" "$@"

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	ln -sf "$<" "$@"

../input/alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	ln -sf "$<" "$@"


# Convenience
link-inputs: $(ALL_INPUTS)

include ../../generic.make
