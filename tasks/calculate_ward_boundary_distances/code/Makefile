# calculate_ward_boundary_distances/Makefile

SHELL := bash
include ../../shell_functions.make

.PHONY: all clean link-inputs

# Input files
YEAR_BUILT_SAMPLE := ../input/year_built_sample.gpkg
WARD_PANEL := ../input/ward_panel.gpkg  
ALDERMAN_PANEL := ../input/chicago_alderman_panel.csv

# Output files
PARCELS_WITH_DISTANCES := ../output/parcels_with_ward_distances.gpkg
PARCELS_PARQUET := ../output/parcels_with_ward_distances.parquet
SUMMARY_STATS := ../output/boundary_distance_summary.csv

# Main target
all: $(PARCELS_WITH_DISTANCES) $(PARCELS_PARQUET) $(SUMMARY_STATS)

include ../../generic.make

# Link inputs from upstream tasks
link-inputs:
	@ln -sf ../../clean_attom_historical/output/year_built_sample.gpkg ../input/year_built_sample.gpkg
	@ln -sf ../../ward_panel_create/output/ward_panel.gpkg ../input/ward_panel.gpkg
	@ln -sf ../../create_alderman_data/output/chicago_alderman_panel.csv ../input/chicago_alderman_panel.csv

# Main processing rule
$(PARCELS_WITH_DISTANCES) $(PARCELS_PARQUET) $(SUMMARY_STATS): $(YEAR_BUILT_SAMPLE) $(WARD_PANEL) $(ALDERMAN_PANEL) | ../output ../temp
	Rscript calculate_distances.R

