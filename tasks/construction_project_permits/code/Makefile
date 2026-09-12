include ../../shared/code/shell_functions.make

all: ../output/project_permit_chain_links.csv

../%/building_permits_for_verification.gpkg \
	../%/new_construction_exact_permit_matches.csv \
	../%/new_construction_permit_unit_mentions.csv \
	../%/new_construction_project_components.csv: match_construction_permits.R \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/building_permits_full.csv \
	../input/commercial_entity_component_candidates.csv \
	../input/commercial_production_family_members.csv \
	../input/residential_project_candidate_inventory.csv \
	../input/residential_tieback_groups_full.csv \
	../input/residential_tieback_members_full.csv \
	../../shared/code/permit_unit_patterns.R \
	Makefile | ../output ../report
	$(R) $<

../%/new_construction_spatial_permit_matches.csv \
	../%/project_permit_chain_links.csv \
	../%/project_permit_chain_unit_mentions.csv \
	../%/project_permit_chain_summary.csv: match_spatial_permits_and_revisions.R \
	../output/building_permits_for_verification.gpkg \
	../input/historical_project_year_geometry.gpkg \
	../output/new_construction_exact_permit_matches.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../../shared/code/permit_unit_patterns.R \
	Makefile | ../output ../report
	$(R) $<

../input/building_permits_full.csv: ../../../data_raw/construction_review/building_permits_full.csv | ../input
	ln -sf $< $@

../input/commercial_entity_component_candidates.csv: ../../construction_project_candidates/output/commercial_entity_component_candidates.csv | ../input
	ln -sf $< $@

../input/commercial_production_family_members.csv: ../../construction_project_candidates/output/commercial_production_family_members.csv | ../input
	ln -sf $< $@

../input/historical_project_year_geometry.gpkg: ../../construction_historical_sites/output/historical_project_year_geometry.gpkg | ../input
	ln -sf $< $@

../input/residential_project_candidate_inventory.csv: ../../construction_project_candidates/output/residential_project_candidate_inventory.csv | ../input
	ln -sf $< $@

../input/residential_tieback_groups_full.csv: ../../construction_project_candidates/output/residential_tieback_groups_full.csv | ../input
	ln -sf $< $@

../input/residential_tieback_members_full.csv: ../../construction_project_candidates/output/residential_tieback_members_full.csv | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_full.csv \
	../input/commercial_entity_component_candidates.csv \
	../input/commercial_production_family_members.csv \
	../input/historical_project_year_geometry.gpkg \
	../input/residential_project_candidate_inventory.csv \
	../input/residential_tieback_groups_full.csv \
	../input/residential_tieback_members_full.csv

include ../../shared/code/generic.make
