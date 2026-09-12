include ../../shared/code/shell_functions.make

all: ../output/preferred_commercial_projects.csv

../%/preferred_commercial_project_candidates.csv \
	../%/preferred_commercial_project_components.csv \
	../%/commercial_adjudication_queue.csv: build_preferred_commercial_candidates.R \
	../input/commercial_verified_case_review.csv \
	../input/commercial_address_family_candidates.csv \
	../input/commercial_entity_version_candidates.csv \
	../input/commercial_family_vintage_summary.csv \
	../input/commercial_project_family_review.csv \
	../input/project_permit_chain_links.csv \
	../input/project_permit_chain_summary.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	Makefile | ../output ../report
	$(R) $<

../%/commercial_post_evidence_resolution.csv \
	../%/preferred_commercial_projects.csv \
	../%/preferred_commercial_source_disposition.csv: select_commercial_measurements.R \
	../input/commercial_manual_decisions.csv \
	../input/commercial_family_vintage_summary.csv \
	../input/project_permit_chain_links.csv \
	../input/commercial_address_permit_history.csv \
	../input/new_construction_spatial_permit_matches.csv \
	../input/preferred_project_boundary_scope.csv \
	../input/preferred_adjudication_scope.csv \
	../output/preferred_commercial_project_candidates.csv \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/commercial_reported_land_decisions.csv \
	../input/commercial_component_overrides.csv \
	../input/commercial_cross_family_decisions.csv \
	../input/construction_modifications.csv \
	../input/commercial_entity_version_candidates.csv \
	Makefile | ../output ../report
	$(R) $<

../input/commercial_address_family_candidates.csv: ../../construction_project_candidates/output/commercial_address_family_candidates.csv | ../input
	ln -sf $< $@

../input/commercial_address_permit_history.csv: ../../construction_commercial_permits/output/commercial_address_permit_history.csv | ../input
	ln -sf $< $@

../input/commercial_component_overrides.csv: ../../new_construction_cleaning/output/commercial_component_overrides.csv | ../input
	ln -sf $< $@

../input/commercial_cross_family_decisions.csv: ../../new_construction_cleaning/output/commercial_cross_family_decisions.csv | ../input
	ln -sf $< $@

../input/commercial_entity_version_candidates.csv: ../../construction_project_candidates/output/commercial_entity_version_candidates.csv | ../input
	ln -sf $< $@

../input/commercial_family_vintage_summary.csv: ../../construction_project_candidates/output/commercial_family_vintage_summary.csv | ../input
	ln -sf $< $@

../input/commercial_manual_decisions.csv: ../../new_construction_cleaning/output/commercial_manual_decisions.csv | ../input
	ln -sf $< $@

../input/commercial_project_family_review.csv: ../../construction_project_candidates/output/commercial_project_family_review.csv | ../input
	ln -sf $< $@

../input/commercial_reported_land_decisions.csv: ../../new_construction_cleaning/output/commercial_reported_land_decisions.csv | ../input
	ln -sf $< $@

../input/commercial_verified_case_review.csv: ../../new_construction_cleaning/output/commercial_verified_case_review.csv | ../input
	ln -sf $< $@

../input/construction_modifications.csv: ../../new_construction_cleaning/output/construction_modifications.csv | ../input
	ln -sf $< $@

../input/new_construction_spatial_permit_matches.csv: ../../construction_project_permits/output/new_construction_spatial_permit_matches.csv | ../input
	ln -sf $< $@

../input/preferred_adjudication_scope.csv: ../../construction_project_locations/output/preferred_adjudication_scope.csv | ../input
	ln -sf $< $@

../input/preferred_project_boundary_scope.csv: ../../construction_project_locations/output/preferred_project_boundary_scope.csv | ../input
	ln -sf $< $@

../input/project_permit_chain_links.csv: ../../construction_project_permits/output/project_permit_chain_links.csv | ../input
	ln -sf $< $@

../input/project_permit_chain_summary.csv: ../../construction_project_permits/output/project_permit_chain_summary.csv | ../input
	ln -sf $< $@

link-inputs: ../input/commercial_address_family_candidates.csv \
	../input/commercial_address_permit_history.csv \
	../input/commercial_component_overrides.csv \
	../input/commercial_cross_family_decisions.csv \
	../input/commercial_entity_version_candidates.csv \
	../input/commercial_family_vintage_summary.csv \
	../input/commercial_manual_decisions.csv \
	../input/commercial_project_family_review.csv \
	../input/commercial_reported_land_decisions.csv \
	../input/commercial_verified_case_review.csv \
	../input/construction_modifications.csv \
	../input/new_construction_spatial_permit_matches.csv \
	../input/preferred_adjudication_scope.csv \
	../input/preferred_project_boundary_scope.csv \
	../input/project_permit_chain_links.csv \
	../input/project_permit_chain_summary.csv

include ../../shared/code/generic.make
