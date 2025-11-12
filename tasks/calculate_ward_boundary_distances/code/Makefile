SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# --- Main Goal ---
all: ../output/parcels_with_ward_distances.csv ../output/parcels_with_geometry.gpkg

include ../../generic.make

# --- Main processing rule ---
../output/parcels_with_ward_distances.csv ../output/parcels_with_geometry.gpkg: \
  calculate_distances.R \
  ../input/geocoded_residential_data.gpkg \
  ../input/ward_panel.gpkg \
  ../input/chicago_alderman_panel.csv \
  ../input/aldermen_strictness_scores.csv \
  ../input/zoning_data_clean.gpkg \
  ../input/cta_stops.gpkg \
  ../input/major_streets.gpkg \
  ../input/parks.gpkg \
  ../input/schools_2015.gpkg | ../output
	@Rscript calculate_distances.R

# --- Rules that make each symlink depend on its source file ---
../input/year_built_sample.gpkg: ../../process_attom_assessor/output/chicago_attom_2023.gpkg | ../input
	@ln -sf $< $@

../input/geocoded_residential_data.gpkg: ../../geocode_ccao_data/output/geocoded_residential_data.gpkg | ../input
	@ln -sf $< $@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@ln -sf $< $@

../input/chicago_alderman_panel.csv: ../../create_alderman_data/output/chicago_alderman_panel.csv | ../input
	@ln -sf $< $@

../input/aldermen_strictness_scores.csv: ../../rank_aldermen/output/aldermen_strictness_scores.csv | ../input
	@ln -sf $< $@

../input/zoning_data_clean.gpkg: ../../zoning_data_cleaning/output/zoning_data_clean.gpkg | ../input
	@ln -sf $< $@

../input/cta_stops.gpkg: ../../download_amenities_data/output/cta_stops.gpkg | ../input
	@ln -sf $< $@

../input/major_streets.gpkg: ../../download_amenities_data/output/major_streets.gpkg | ../input
	@ln -sf $< $@

../input/parks.gpkg: ../../download_amenities_data/output/parks.gpkg | ../input
	@ln -sf $< $@

../input/schools_2015.gpkg: ../../download_amenities_data/output/schools_2015.gpkg | ../input
	@ln -sf $< $@

# --- Helper to create all links at once ---
link-inputs: \
  ../input/year_built_sample.gpkg \
  ../input/geocoded_residential_data.gpkg \
  ../input/ward_panel.gpkg \
  ../input/chicago_alderman_panel.csv \
  ../input/aldermen_strictness_scores.csv \
  ../input/zoning_data_clean.gpkg \
  ../input/cta_stops.gpkg \
  ../input/major_streets.gpkg \
  ../input/parks.gpkg \
  ../input/schools_2015.gpkg

