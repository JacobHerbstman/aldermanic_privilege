SHELL := bash
include ../../shell_functions.make

.PHONY: all link-inputs

# Build both maps by default
all: ../output/permit_map_2003_2014.pdf ../output/permit_map_2015_2023.pdf

# Period-specific targets call the same script with flags
../output/permit_map_2003_2014.pdf: permit_map.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg | ../output
	@echo "→ Generating permit map for 2003–2014 (wards2014)…"
	Rscript permit_map.R --period=2003-2014 --out=$@

../output/permit_map_2015_2023.pdf: permit_map.R ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg | ../output
	@echo "→ Generating permit map for 2015–2023 (wards2015)…"
	Rscript permit_map.R --period=2015-2023 --out=$@

../input/ward_panel.gpkg: ../../ward_panel_create/output/ward_panel.gpkg | ../input
	@ln -sf $< $@

# Symlink inputs
../input/building_permits_clean.gpkg: ../../clean_building_permits/output/building_permits_clean.gpkg | ../input
	@ln -sf $< $@

# Convenience: create all links at once
link-inputs: ../input/building_permits_clean.gpkg ../input/ward_panel.gpkg

include ../../generic.make
