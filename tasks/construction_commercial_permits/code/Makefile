include ../../shared/code/shell_functions.make

all: ../output/commercial_address_permit_history.csv

../%/commercial_city_building_footprints.gpkg \
	../%/commercial_ground_up_evidence.csv: identify_commercial_new_buildings.R \
	../input/chicago_building_footprints_2015.zip \
	../input/preferred_project_year_geometry.gpkg \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/building_permits_for_verification.gpkg \
	../input/new_construction_exact_permit_matches.csv \
	../input/new_construction_spatial_permit_matches.csv \
	../input/preferred_commercial_project_candidates.csv \
	../input/project_permit_chain_links.csv \
	Makefile | ../output ../report
	$(R) $<

../output/commercial_address_permit_history.csv: build_commercial_address_permit_history.R \
	../input/building_permits_for_verification.gpkg \
	../output/commercial_ground_up_evidence.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $<

../input/building_permits_for_verification.gpkg: ../../construction_project_permits/output/building_permits_for_verification.gpkg | ../input
	ln -sf $< $@

../input/chicago_building_footprints_2015.zip: ../../download_city_building_footprints/output/chicago_building_footprints_2015.zip | ../input
	ln -sf $< $@

../input/new_construction_exact_permit_matches.csv: ../../construction_project_permits/output/new_construction_exact_permit_matches.csv | ../input
	ln -sf $< $@

../input/new_construction_spatial_permit_matches.csv: ../../construction_project_permits/output/new_construction_spatial_permit_matches.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_project_candidates.csv: ../../construction_commercial_measurements/output/preferred_commercial_project_candidates.csv | ../input
	ln -sf $< $@

../input/preferred_project_year_geometry.gpkg: ../../construction_project_locations/output/preferred_project_year_geometry.gpkg | ../input
	ln -sf $< $@

../input/project_permit_chain_links.csv: ../../construction_project_permits/output/project_permit_chain_links.csv | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_for_verification.gpkg \
	../input/chicago_building_footprints_2015.zip \
	../input/new_construction_exact_permit_matches.csv \
	../input/new_construction_spatial_permit_matches.csv \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_project_year_geometry.gpkg \
	../input/project_permit_chain_links.csv

include ../../shared/code/generic.make
