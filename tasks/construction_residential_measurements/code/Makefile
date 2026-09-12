include ../../shared/code/shell_functions.make

EPISODE_YEAR_WINDOW := 2
AUTOMATIC_CARD_BUILDING_GAP := 0.02
SUCCESSOR_LAND_TOLERANCE := 0.005

all: ../output/residential_selected_buildings.csv

../%/residential_selected_assessments.csv \
	../%/preferred_residential_project_candidates.csv \
	../%/preferred_residential_project_components.csv \
	../%/residential_adjudication_queue.csv: select_and_correct_residential_measurements.R \
	../input/project_permit_chain_links.csv \
	../input/project_permit_chain_unit_mentions.csv \
	../input/residential_assessor_project_candidates.csv \
	../input/residential_project_candidate_inventory.csv \
	../input/residential_tieback_temporal_lineage_evidence.csv \
	../../shared/code/assessor_classification.R \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/construction_modifications.csv \
	../input/residential_source_decisions.csv \
	../input/building_permits_for_verification.gpkg \
	../input/residential_unresolved_source_dispositions.csv \
	../input/construction_condominium_history.csv \
	../input/residential_class297_component_overrides.csv \
	../input/residential_overlap_decisions.csv \
	../input/eligibility_manual_exceptions.csv \
	Makefile | ../output ../report
	$(R) $<

../%/residential_successor_condo_requests.csv \
	../%/residential_review_current_parcel_links.csv \
	../%/residential_tieback_episode_resolution.csv \
	../%/residential_class297_resolution.csv \
	../%/residential_class297_source_disposition.csv \
	../%/residential_overlap_resolution.csv \
	../%/residential_selected_buildings.csv \
	../%/residential_review_source_dispositions.csv: select_residential_buildings.R \
	../../setup_environment/code/packages.R \
	../../shared/code/save_data.R \
	../../shared/code/report.py \
	../input/parcel_universe_2025_city.csv \
	../input/preferred_project_year_geometry.gpkg \
	../output/preferred_residential_project_candidates.csv \
	../input/residential_assessor_history.parquet \
	../input/preferred_project_year_centroids.gpkg \
	../input/project_permit_chain_links.csv \
	../input/project_permit_chain_unit_mentions.csv \
	../input/residential_project_candidate_inventory.csv \
	../input/construction_condominium_history.csv \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_commercial_source_disposition.csv \
	../input/residential_overlap_decisions.csv \
	../input/preferred_commercial_projects.csv \
	../input/residential_additional_candidate_decisions.csv \
	Makefile | ../output ../report
	$(R) $< $(EPISODE_YEAR_WINDOW) $(AUTOMATIC_CARD_BUILDING_GAP) $(SUCCESSOR_LAND_TOLERANCE)

../input/building_permits_for_verification.gpkg: ../../construction_project_permits/output/building_permits_for_verification.gpkg | ../input
	ln -sf $< $@

../input/construction_condominium_history.csv: ../../download_construction_assessor_history/output/construction_condominium_history.csv | ../input
	ln -sf $< $@

../input/construction_modifications.csv: ../../new_construction_cleaning/output/construction_modifications.csv | ../input
	ln -sf $< $@

../input/eligibility_manual_exceptions.csv: ../../new_construction_cleaning/output/eligibility_manual_exceptions.csv | ../input
	ln -sf $< $@

../input/parcel_universe_2025_city.csv: ../../download_parcel_universe_data/output/parcel_universe_2025_city.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_project_candidates.csv: ../../construction_commercial_measurements/output/preferred_commercial_project_candidates.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_projects.csv: ../../construction_commercial_measurements/output/preferred_commercial_projects.csv | ../input
	ln -sf $< $@

../input/preferred_commercial_source_disposition.csv: ../../construction_commercial_measurements/output/preferred_commercial_source_disposition.csv | ../input
	ln -sf $< $@

../input/preferred_project_year_centroids.gpkg: ../../construction_project_locations/output/preferred_project_year_centroids.gpkg | ../input
	ln -sf $< $@

../input/preferred_project_year_geometry.gpkg: ../../construction_project_locations/output/preferred_project_year_geometry.gpkg | ../input
	ln -sf $< $@

../input/project_permit_chain_links.csv: ../../construction_project_permits/output/project_permit_chain_links.csv | ../input
	ln -sf $< $@

../input/project_permit_chain_unit_mentions.csv: ../../construction_project_permits/output/project_permit_chain_unit_mentions.csv | ../input
	ln -sf $< $@

../input/residential_additional_candidate_decisions.csv: ../../new_construction_cleaning/output/residential_additional_candidate_decisions.csv | ../input
	ln -sf $< $@

../input/residential_assessor_history.parquet: ../../prepare_construction_assessor_history/output/residential_assessor_history.parquet | ../input
	ln -sf $< $@

../input/residential_assessor_project_candidates.csv: ../../construction_residential_identity/output/residential_assessor_project_candidates.csv | ../input
	ln -sf $< $@

../input/residential_class297_component_overrides.csv: ../../new_construction_cleaning/output/residential_class297_component_overrides.csv | ../input
	ln -sf $< $@

../input/residential_overlap_decisions.csv: ../../new_construction_cleaning/output/residential_overlap_decisions.csv | ../input
	ln -sf $< $@

../input/residential_project_candidate_inventory.csv: ../../construction_project_candidates/output/residential_project_candidate_inventory.csv | ../input
	ln -sf $< $@

../input/residential_source_decisions.csv: ../../new_construction_cleaning/output/residential_source_decisions.csv | ../input
	ln -sf $< $@

../input/residential_tieback_temporal_lineage_evidence.csv: ../../construction_residential_identity/output/residential_tieback_temporal_lineage_evidence.csv | ../input
	ln -sf $< $@

../input/residential_unresolved_source_dispositions.csv: ../../new_construction_cleaning/output/residential_unresolved_source_dispositions.csv | ../input
	ln -sf $< $@

link-inputs: ../input/building_permits_for_verification.gpkg \
	../input/construction_condominium_history.csv \
	../input/construction_modifications.csv \
	../input/eligibility_manual_exceptions.csv \
	../input/parcel_universe_2025_city.csv \
	../input/preferred_commercial_project_candidates.csv \
	../input/preferred_commercial_projects.csv \
	../input/preferred_commercial_source_disposition.csv \
	../input/preferred_project_year_centroids.gpkg \
	../input/preferred_project_year_geometry.gpkg \
	../input/project_permit_chain_links.csv \
	../input/project_permit_chain_unit_mentions.csv \
	../input/residential_additional_candidate_decisions.csv \
	../input/residential_assessor_history.parquet \
	../input/residential_assessor_project_candidates.csv \
	../input/residential_class297_component_overrides.csv \
	../input/residential_overlap_decisions.csv \
	../input/residential_project_candidate_inventory.csv \
	../input/residential_source_decisions.csv \
	../input/residential_tieback_temporal_lineage_evidence.csv \
	../input/residential_unresolved_source_dispositions.csv

include ../../shared/code/generic.make
